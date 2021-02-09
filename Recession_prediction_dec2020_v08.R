
library(tidyverse)
library(quantmod)
library(data.table)
library(readxl)
library(xlsx)
library(caret)
library(vip)
library(Metrics)
library(rsample)
library(pdp)
library(pROC)
library(ggplot2)
library(caTools)
library(hmeasure)
##########################################################################

##########################################################################
options(digits = 5)
options(scipen = 999)

rm(list = ls())
##########################################################################
#NBER<- read_xlsx("NBER_RECESSIONS.xlsx")

getSymbols('USREC', src='FRED')
NBER <- fortify.zoo(window(USREC, start = '1950-01-01', end = '2020-12-31'))
colnames(NBER)<-c("date","NBER_RECESSION")


getSymbols("^GSPC",src="yahoo",from = as.Date("1900-01-01"))#SP500 INDEX

#fred Macro economic data
getSymbols("CPIAUCSL",src="FRED")#CPI
getSymbols("CUUR0000SEHA",src="FRED")#CPI: Rent of Primary Residence in U.S. City Average
getSymbols("DGS10",src="FRED")#1OY RATE
#getSymbols("DGS2",src="FRED")#2Y RATE
#getSymbols("DGS30",src="FRED")#30Y RATE
#getSymbols("DGS3MO",src="FRED")#3M RATE
getSymbols("FEDFUNDS",src="FRED")#EFFECTIVE FED FUNDS RATE
getSymbols("GDPC1",src="FRED")#GDP QUARTERLY
getSymbols("INDPRO",src="FRED")# INDUSTRIAL PRODUCTION
#getSymbols("M2",src="FRED")#M2 FOR US
getSymbols("M2V",src="FRED")#M2 VELOCITY
getSymbols("MABMM301USM189S",src="FRED")#M3 FOR US
getSymbols("PAYEMS",src="FRED")#Total Nonfarm Payroll
#getSymbols("T10Y3M",src="FRED") #10-Year Minus 3-Month Treasury Constant Maturity
getSymbols("UNEMPLOY",src="FRED")#UNEMPLOYMENT RATE
#getSymbols("USSTHPI",src="FRED")#HOUSE Price Index
getSymbols("W875RX1",src="FRED")#PIX: Real personal income excluding current transfer receipts

gc(reset=T)

#########################################################################################
#########################################################################################
#########################################################################################

series <- c("CUUR0000SEHA",#m1914
            "CPIAUCSL",#m1947
            "DGS10",#d1962
            #"DGS2",#d1976
            #"DGS30",#d1977
            #"DGS3MO",#1982
            "FEDFUNDS",#m1954
            "GDPC1",#q1947
            "INDPRO",#m1919
            #"M2",#w1980
            "M2V",#q1959
            #"MABMM301USM189S",#m1960
            "PAYEMS",#m1939
            #"T10Y3M",#d1982
            #"USSTHPI",#q1975
            "UNEMPLOY",#m1948
            "W875RX1"#m1959
            )  

item <- list()

for (i in 1:length(series)) {
items <- get(series[i])
items<-to.monthly(items,indexAt = "month",OHLC=F)

  colnames(items) <- series[i]
  item[[i]] <- items %>% as.data.frame(.) %>% 
    #mutate(date = index(items)) %>% 
    mutate(yy = year(index(items))) %>% 
    mutate(mm = month(index(items))) %>% filter(yy>1955)
}

gc(reset=T)

########################################################################
########################################################################

base<-item[[1]] %>% select(yy,mm,everything())

for (i in 2:length(series)) {
  base <- full_join(base,item[[i]], by = c("yy","mm"))
}
 
GSPC_mm <- to.monthly(GSPC)
GSPC_mm <- as.data.frame(GSPC_mm) %>% 
  select(.,GSPC.Adjusted) %>% 
  mutate(yy = year(index(GSPC_mm)),
         mm = month(index(GSPC_mm)))

NBER <- NBER %>% mutate(yy = year(date),
                      mm = month(date)) %>% 
  select(everything(), NBER_RECESSION)


DV_IV <- inner_join(NBER,GSPC_mm,by = c("yy","mm")) %>% 
  inner_join(.,base,by = c("yy","mm")) %>% 
  select(date, yy , mm ,everything()) %>% 
  #filter(yy != 2020) %>% 
  filter(yy > 1958)

#imputation/interpolation/back-fill
DV_IV$GDPC1<-lag.xts(na.spline(DV_IV$GDPC1),k = 2)#LAG GDP 3 MONTHS
DV_IV$M2V<-na.locf(DV_IV$M2V)
#DV_IV$M3<-log(DV_IV$MABMM301USM189S/10000000)
#DV_IV$MABMM301USM189S<-NULL
#DV_IV$M3<-na.locf(DV_IV$M3, fromLast = TRUE)#backfill
DV_IV$DGS10<-na.locf(DV_IV$DGS10, fromLast = TRUE)
DV_IV$UNEMPLOY<-DV_IV$UNEMPLOY/1000
DV_IV$CPIAUCSL<-lag.xts(na.spline(DV_IV$CPIAUCSL),k=2)#LAG CPI 3 MONTHS
DV_IV$YC<- DV_IV$DGS10 - DV_IV$FEDFUNDS
#DV_IV$W875RX1 <- og(DV_IV$W875RX1)

DV_IV<-rename(DV_IV,
              c("RECESSION"="NBER_RECESSION",
                "TY10"="DGS10",
                "SP500"="GSPC.Adjusted", 
                "RENTHPI"="CUUR0000SEHA",
                "CPI"="CPIAUCSL",
                "GDP"="GDPC1",
                "PIX"="W875RX1"))

#write.csv(DV_IV,"DV_IV.csv",row.names=F)

rm(list=setdiff(ls(), c("DV_IV")))
gc(reset=T)

########################################################################
########################################################################
#applying transformations to make data stationary
names(DV_IV)

DV_IV_MOM<-DV_IV %>% select(M2V,FEDFUNDS,UNEMPLOY,TY10) %>% 
  apply(.,2,lag) %>% #lagging everything
  apply(.,2,momentum) %>% as.data.frame(.)
  
DV_IV_ROC<-DV_IV %>% select(-M2V,-FEDFUNDS,-UNEMPLOY,-TY10) %>% 
  select(-date,-yy,-mm, -RECESSION,-YC) %>% 
  apply(.,2,lag) %>% #lagging everything
  apply(.,2,ROC) %>% as.data.frame(.)

DV_IV_NOTRANS<-DV_IV %>% select(date,yy,mm,RECESSION,YC)

DV_IV_TRANSFORMED<-cbind.data.frame(DV_IV_NOTRANS,DV_IV_MOM,DV_IV_ROC) %>% 
  na.locf(.,fromLast = TRUE) %>% 
  select(-RENTHPI,-M2V,-TY10) %>% 
  select(date,yy,mm,RECESSION,order(colnames(.))) %>% 
  na.omit(.)

########################################################
########################################################
#spectral clustering-based regime indicator/feature
library(kernlab)
nc<-ncol(DV_IV_TRANSFORMED)

covar_df<- cbind.data.frame(DV_IV_TRANSFORMED[,5:nc],
                            lag(DV_IV_TRANSFORMED[,5:nc],3),
                            lag(DV_IV_TRANSFORMED[,5:nc],6),
                            lag(DV_IV_TRANSFORMED[,5:nc],12)
                            ) %>% na.locf(.,fromLast=TRUE)

kpca_sig <- specc(as.matrix(covar_df),
                  centers=2,
                  kernel="rbfdot",kpar=list(sigma=0.1)
                 )

DV_IV_TRANSFORMED <- DV_IV_TRANSFORMED %>% mutate(REGIME = kpca_sig@.Data)

table(DV_IV_TRANSFORMED$RECESSION,DV_IV_TRANSFORMED$REGIME)/nrow(DV_IV_TRANSFORMED)

########################################################################
########################################################################
library(solitude)

covar_df2<- cbind.data.frame(DV_IV_TRANSFORMED[,5:nc]
                            #,lag(DV_IV_TRANSFORMED[,5:nc],3)
                            #,lag(DV_IV_TRANSFORMED[,5:nc],6)
                            #,lag(DV_IV_TRANSFORMED[,5:nc],12)
) %>% na.locf(.,fromLast=TRUE)

# initiate an isolation forest
set.seed(1234567)
iso = isolationForest$new(sample_size = nrow(covar_df2), num_trees= 1500, seed=1234567)
iso$fit(dataset = covar_df2)
an<-iso$predict(covar_df2) # scores for train data

DV_IV_TRANSFORMED <- DV_IV_TRANSFORMED %>% mutate(REGIME3 = an$anomaly_score - 0.5)
#table(DV_IV_TRANSFORMED$RECESSION,DV_IV_TRANSFORMED$REGIME3)/nrow(DV_IV_TRANSFORMED)
#vioplot::vioplot(DV_IV_TRANSFORMED$REGIME3~DV_IV_TRANSFORMED$RECESSION)

########################################################################
########################################################################

corrplot::corrplot(cor(DV_IV_TRANSFORMED[,5:nc]),method="number",type="lower")

########################################################################
########################################################################
#DV_IV_TRANSFORMED$RECL1 <- lag(DV_IV_TRANSFORMED$RECESSION) %>% na.locf(.,fromLast = TRUE)

DV_IV_TRANSFORMED$RECESSION <- as.factor(if_else(DV_IV_TRANSFORMED$RECESSION==0,"n","y"))

#rm(list=setdiff(ls(), c("DV_IV_TRANSFORMED")))

########################################################################
########################################################################
#Train and test splits predicting next 5 years

DV_IV_TRAIN <- DV_IV_TRANSFORMED %>% filter(yy < 2007)
DV_IV_TEST <- setdiff(DV_IV_TRANSFORMED,DV_IV_TRAIN) %>% filter(yy < 2010)

#DV_IV_TRAIN <- DV_IV_TRANSFORMED %>% filter(yy < 2018)
#DV_IV_TEST <- setdiff(DV_IV_TRANSFORMED,DV_IV_TRAIN) %>% filter(yy < 2021)

table(DV_IV_TRAIN$RECESSION)
table(DV_IV_TEST$RECESSION)

nrow(DV_IV_TRAIN)/(nrow(DV_IV_TRAIN)+nrow(DV_IV_TEST))

########################################################################
########################################################################
write.csv(DV_IV_TRANSFORMED,"DV_IV_TRANSFORMED.csv",row.names=F)
write.csv(DV_IV_TRAIN,"DV_IV_TRAIN.csv",row.names=F)
write.csv(DV_IV_TEST,"DV_IV_TEST.csv",row.names=F)

########################################################################
########################################################################
formula <- RECESSION ~ . - date - yy - mm
#formula <- RECESSION ~ . 

set.seed(1234567)
ctrl<-trainControl(#method="repeatedcv", number=10, repeats=20,
                   method="timeslice",
                   initialWindow = 36,#24
                   horizon = 6,#3
                   fixedWindow = T, 
                   verboseIter=F,
                   #search="random",
                   returnResamp="final",
                   savePredictions="final",
                   selectionFunction="best",#oneSE
                   summaryFunction=twoClassSummary,
                   sampling="down",#smote
                   classProbs=T
                   )

prep = c("YeoJohnson")

set.seed(1234567)
############################################################
############################################################
############################################################
#We run glm classification
set.seed(1234567)
caret_glm <- train(formula,
                   data=DV_IV_TRAIN,
                   method = "glm",family = "binomial",
                   na.action=na.omit,
                   metric = "Sens",
                   #preProc = prep,
                   trControl = ctrl)

caret_glm
summary(caret_glm)
confusionMatrix(caret_glm)
plot(varImp(caret_glm))
vip(caret_glm, bar = T, horizontal = T, size = 1) 

caret_glm_pred_tr<-predict(caret_glm,DV_IV_TRAIN[,-4]) %>% as.data.frame(.)
colnames(caret_glm_pred_tr)<-"glm_predicted"

caret_glm_pred_te<-predict(caret_glm,DV_IV_TEST[,-4]) %>% as.data.frame(.)
colnames(caret_glm_pred_te)<-"glm_predicted"

#performance evaluation
#Training Sample
confusionMatrix(DV_IV_TRAIN$RECESSION,caret_glm_pred_tr$glm_predicted,positive='y')

#Test Sample
confusionMatrix(DV_IV_TEST$RECESSION,caret_glm_pred_te$glm_predicted,positive='y')

#extract probabilities
caret_glm_pr_tr <- predict(caret_glm,DV_IV_TRAIN[,-4],type="prob")
caret_glm_pr_te <- predict(caret_glm,DV_IV_TEST[,-4],type="prob")

head(caret_glm_pr_tr)
head(caret_glm_pr_te)

#####################################
#ROC curve
#training sample
glm_ROC_tr <- roc(predictor=caret_glm_pr_tr$y,
                  response=DV_IV_TRAIN$RECESSION,
                  levels=rev(levels(DV_IV_TRAIN$RECESSION)))
glm_ROC_tr$auc
plot(glm_ROC_tr,main="glm ROC Training", print.thres = 0.5)

# Plot the propability of poor segmentation
histogram(~caret_glm_pr_tr$y|DV_IV_TRAIN$RECESSION,xlab="Probability of Poor Segmentation Training")

#testing sample
glm_ROC_te <- roc(predictor=caret_glm_pr_te$y,
                  response=DV_IV_TEST$RECESSION,
                  levels=rev(levels(DV_IV_TEST$RECESSION)))
glm_ROC_te$auc
ci.auc(glm_ROC_te)
plot(glm_ROC_te,main="glm ROC Test" , print.thres = 0.5)

# Plot the propability of poor segmentation
histogram(~caret_glm_pr_te$y|DV_IV_TEST$RECESSION,xlab="Probability of Poor Segmentation Test")

#####################################
#1D Partial Dependence Plot
partial(caret_glm, 
        pred.data = DV_IV_TRAIN, 
        pred.var = "YC",
        prob = TRUE, plot = TRUE,rug = TRUE,
        chull = TRUE, progress = "text")

partial(caret_glm, 
        pred.data = DV_IV_TRAIN, 
        pred.var = "UNEMPLOY",
        prob = TRUE, plot = TRUE,rug = TRUE,
        chull = TRUE, progress = "text")

#2D Partial Dependence Plot
partial(caret_glm, 
        pred.data = DV_IV_TRAIN, 
        pred.var = c("YC","PAYEMS"),
        prob = TRUE, plot = TRUE,rug = TRUE,
        chull = TRUE, progress = "text")

gc(reset=T)

############################################################
############################################################
############################################################
#We run svm classification
set.seed(1234567)
caret_svm<-train(formula,
                 data=DV_IV_TRAIN,
                 method = "svmRadial",
                 #tuneGrid=expand.grid(sigma=0.01,C=c(10,100,500)),
                 na.action=na.omit,
                 metric = "Sens",
                 #preProc = prep,
                 trControl = ctrl)

caret_svm
confusionMatrix(caret_svm)
plot(caret_svm)
plot(varImp(caret_svm))
vip(caret_svm, bar = T, horizontal = T, size = 1) 

caret_svm_pred_tr<-predict(caret_svm,DV_IV_TRAIN[,-4]) %>% as.data.frame(.)
colnames(caret_svm_pred_tr)<-"svm_predicted"

caret_svm_pred_te<-predict(caret_svm,DV_IV_TEST[,-4]) %>% as.data.frame(.)
colnames(caret_svm_pred_te)<-"svm_predicted"

#performance evaluation
#Training Sample
confusionMatrix(DV_IV_TRAIN$RECESSION,caret_svm_pred_tr$svm_predicted,positive='y')

#Test Sample
confusionMatrix(DV_IV_TEST$RECESSION,caret_svm_pred_te$svm_predicted,positive='y')

#extract probabilities
caret_svm_pr_tr <- predict(caret_svm,DV_IV_TRAIN[,-4],type="prob")
caret_svm_pr_te <- predict(caret_svm,DV_IV_TEST[,-4],type="prob")

head(caret_svm_pr_tr)
head(caret_svm_pr_te)


#####################################
#ROC curve
#training sample
svm_ROC_tr <- roc(predictor=caret_svm_pr_tr$y,
                  response=DV_IV_TRAIN$RECESSION,
                  levels=rev(levels(DV_IV_TRAIN$RECESSION)))
svm_ROC_tr$auc
plot(svm_ROC_tr,main="svm ROC Training", print.thres = 0.5)

# Plot the propability of poor segmentation
histogram(~caret_svm_pr_tr$y|DV_IV_TRAIN$RECESSION,xlab="Probability of Poor Segmentation Training")

#testing sample
svm_ROC_te <- roc(predictor=caret_svm_pr_te$y,
                  response=DV_IV_TEST$RECESSION,
                  levels=rev(levels(DV_IV_TEST$RECESSION)))
svm_ROC_te$auc
ci.auc(svm_ROC_te)
plot(svm_ROC_te,main="svm ROC Test" , print.thres = 0.5)

# Plot the propability of poor segmentation
histogram(~caret_svm_pr_te$y|DV_IV_TEST$RECESSION,xlab="Probability of Poor Segmentation Test")

#####################################
#1D Partial Dependence Plot
partial(caret_svm, 
        pred.data = DV_IV_TRAIN, 
        pred.var = "YC",
        prob = TRUE, plot = TRUE,rug = TRUE,
        chull = TRUE, progress = "text")

#2D Partial Dependence Plot
partial(caret_svm, 
        pred.data = DV_IV_TRAIN, 
        pred.var = c("YC","PAYEMS"),
        prob = TRUE, plot = TRUE,rug = TRUE,
        chull = TRUE, progress = "text")

gc(reset=T)



############################################################
############################################################
############################################################
#We run rf classification
set.seed(1234567)
caret_rf <- train(formula,
                  data=DV_IV_TRAIN,
                  method = "rf",#ntrees=1000,
                  #method = "ranger",importance='permutation',#impurity
                  #method = "extraTrees",
                  #tuneGrid=expand.grid(mtry=2),
                  na.action=na.omit,
                  metric = "Sens",
                  #preProc = prep,
                  trControl = ctrl)

caret_rf
plot(caret_rf)
confusionMatrix(caret_rf)
plot(varImp(caret_rf))
vip(caret_rf, bar = T, horizontal = T, size = 1, ,fill="steelblue") 

caret_rf_pred_tr<-predict(caret_rf,DV_IV_TRAIN[,-4]) %>% as.data.frame(.)
colnames(caret_rf_pred_tr)<-"rf_predicted"

caret_rf_pred_te<-predict(caret_rf,DV_IV_TEST[,-4]) %>% as.data.frame(.)
colnames(caret_rf_pred_te)<-"rf_predicted"

#performance evaluation
#Training Sample
confusionMatrix(DV_IV_TRAIN$RECESSION,caret_rf_pred_tr$rf_predicted,positive='y')

#Test Sample
confusionMatrix(DV_IV_TEST$RECESSION,caret_rf_pred_te$rf_predicted,positive='y')

#extract probabilities
caret_rf_pr_tr <- predict(caret_rf,DV_IV_TRAIN[,-4],type="prob")
caret_rf_pr_te <- predict(caret_rf,DV_IV_TEST[,-4],type="prob")

head(caret_rf_pr_tr)
head(caret_rf_pr_te)

#####################################
#ROC curve
#training sample
rf_ROC_tr <- roc(predictor=caret_rf_pr_tr$y,
                 response=DV_IV_TRAIN$RECESSION,
                 levels=rev(levels(DV_IV_TRAIN$RECESSION)))
rf_ROC_tr$auc
plot(rf_ROC_tr,main="rf ROC Training", print.thres = 0.5)

# Plot the propability of poor segmentation
histogram(~caret_rf_pr_tr$y|DV_IV_TRAIN$RECESSION,xlab="Probability of Poor Segmentation Training")

#testing sample
rf_ROC_te <- roc(predictor=caret_rf_pr_te$y,
                 response=DV_IV_TEST$RECESSION,
                 levels=rev(levels(DV_IV_TEST$RECESSION)))
rf_ROC_te$auc
ci.auc(rf_ROC_te)
plot(rf_ROC_te,main="rf ROC Test" , print.thres = 0.5)

# Plot the propability of poor segmentation
histogram(~caret_rf_pr_te$y|DV_IV_TEST$RECESSION,xlab="Probability of Poor Segmentation Test")

#####################################
#1D Partial Dependence Plot
partial(caret_rf, 
        pred.data = DV_IV_TRAIN, 
        pred.var = "YC",
        prob = TRUE, plot = TRUE,rug = TRUE,
        chull = TRUE, progress = "text")

#2D Partial Dependence Plot
partial(caret_rf, 
        pred.data = DV_IV_TRAIN, 
        pred.var = c("YC","PAYEMS"),
        prob = TRUE, plot = TRUE,rug = TRUE,
        chull = TRUE, progress = "text")


gc(reset=T)

############################################################
############################################################
############################################################
#We run xgb classification
set.seed(1234567)
caret_xgb<-train(formula,
                 data=DV_IV_TRAIN,
                 method = "xgbTree",#xgbTree
                 #tuneGrid=expand.grid(n.trees=20,interaction.depth=3,shrinkage=0.01,n.minobsinnode=10),
                 na.action=na.omit,
                 metric = "Sens",#"Sens"
                 #preProc = prep,
                 trControl = ctrl)

caret_xgb
confusionMatrix(caret_xgb)
#plot(caret_xgb)
plot(varImp(caret_xgb))
vip(caret_xgb, bar = T, horizontal = T, size = 1, fill="steelblue") 

caret_xgb_pred_tr<-predict(caret_xgb,DV_IV_TRAIN[,-4]) %>% as.data.frame(.)
colnames(caret_xgb_pred_tr)<-"xgb_predicted"

caret_xgb_pred_te<-predict(caret_xgb,DV_IV_TEST[,-4]) %>% as.data.frame(.)
colnames(caret_xgb_pred_te)<-"xgb_predicted"

#performance evaluation
#Training Sample
confusionMatrix(DV_IV_TRAIN$RECESSION,caret_xgb_pred_tr$xgb_predicted,positive='y')

#Test Sample
confusionMatrix(DV_IV_TEST$RECESSION,caret_xgb_pred_te$xgb_predicted,positive='y')

#extract probabilities
caret_xgb_pr_tr <- predict(caret_xgb,DV_IV_TRAIN[,-4],type="prob")
caret_xgb_pr_te <- predict(caret_xgb,DV_IV_TEST[,-4],type="prob")

head(caret_xgb_pr_tr)
head(caret_xgb_pr_te)


#####################################
#ROC curve
#training sample
xgb_ROC_tr <- roc(predictor=caret_xgb_pr_tr$y,
                  response=DV_IV_TRAIN$RECESSION,
                  levels=rev(levels(DV_IV_TRAIN$RECESSION)))
xgb_ROC_tr$auc
plot(xgb_ROC_tr,main="xgb ROC Training", print.thres = 0.5)

# Plot the propability of poor segmentation
histogram(~caret_xgb_pr_tr$y|DV_IV_TRAIN$RECESSION,xlab="Probability of Poor Segmentation Training")

#testing sample
xgb_ROC_te <- roc(predictor=caret_xgb_pr_te$y,
                  response=DV_IV_TEST$RECESSION,
                  levels=rev(levels(DV_IV_TEST$RECESSION)))
xgb_ROC_te$auc
ci.auc(xgb_ROC_te)
plot(xgb_ROC_te,main="xgb ROC Test" , print.thres = 0.5)

# Plot the propability of poor segmentation
histogram(~caret_xgb_pr_te$y|DV_IV_TEST$RECESSION,xlab="Probability of Poor Segmentation Test")

#####################################
#1D Partial Dependence Plot
partial(caret_xgb, 
        pred.data = DV_IV_TRAIN, 
        pred.var = "YC",
        prob = TRUE, plot = TRUE,rug = TRUE,
        chull = TRUE, progress = "text")

#2D Partial Dependence Plot
partial(caret_xgb, 
        pred.data = DV_IV_TRAIN, 
        pred.var = c("YC","PAYEMS"),
        prob = TRUE, plot = TRUE,rug = TRUE,
        chull = TRUE, progress = "text")

gc(reset=T)

############################################################
############################################################
############################################################
#We run nnet classification2
set.seed(1234567)
caret_nnet<-train(formula,
                  data=DV_IV_TRAIN,
                  method = "avNNet",#nnet
                  #tuneLength=10,
                  na.action=na.omit,
                  metric = "Sens",
                  #preProc = c("center","scale"),
                  trControl = ctrl)

caret_nnet
confusionMatrix(caret_nnet)
plot(caret_nnet)
plot(varImp(caret_nnet))
vip(caret_nnet, bar = T, horizontal = T, size = 1) 

caret_nnet_pred_tr<-predict(caret_nnet,DV_IV_TRAIN[,-4]) %>% as.data.frame(.)
colnames(caret_nnet_pred_tr)<-"nnet_predicted"

caret_nnet_pred_te<-predict(caret_nnet,DV_IV_TEST[,-4]) %>% as.data.frame(.)
colnames(caret_nnet_pred_te)<-"nnet_predicted"

#performance evaluation
#Training Sample
confusionMatrix(DV_IV_TRAIN$RECESSION,caret_nnet_pred_tr$nnet_predicted,positive='y')

#Test Sample
confusionMatrix(DV_IV_TEST$RECESSION,caret_nnet_pred_te$nnet_predicted,positive='y')

#extract probabilities
caret_nnet_pr_tr <- predict(caret_nnet,DV_IV_TRAIN[,-4],type="prob")
caret_nnet_pr_te <- predict(caret_nnet,DV_IV_TEST[,-4],type="prob")

head(caret_nnet_pr_tr)
head(caret_nnet_pr_te)


#####################################
#ROC curve
#training sample
nnet_ROC_tr <- roc(predictor=caret_nnet_pr_tr$y,
                   response=DV_IV_TRAIN$RECESSION,
                   levels=rev(levels(DV_IV_TRAIN$RECESSION)))
nnet_ROC_tr$auc
plot(nnet_ROC_tr,main="nnet ROC Training", print.thres = 0.5)

# Plot the probability of poor segmentation
histogram(~caret_nnet_pr_tr$y|DV_IV_TRAIN$RECESSION,xlab="Probability of Poor Segmentation Training")

#testing sample
nnet_ROC_te <- roc(predictor=caret_nnet_pr_te$y,
                   response=DV_IV_TEST$RECESSION,
                   levels=rev(levels(DV_IV_TEST$RECESSION)))
nnet_ROC_te$auc
ci.auc(nnet_ROC_te)
plot(nnet_ROC_te,main="nnet ROC Test" , print.thres = 0.5)

# Plot the propability of poor segmentation
histogram(~caret_nnet_pr_te$y|DV_IV_TEST$RECESSION,xlab="Probability of Poor Segmentation Test")

#####################################
#1D Partial Dependence Plot
partial(caret_nnet, 
        pred.data = DV_IV_TRAIN, 
        pred.var = "YC",
        prob = TRUE, plot = TRUE,rug = TRUE,
        chull = TRUE, progress = "text")

#2D Partial Dependence Plot
partial(caret_nnet, 
        pred.data = DV_IV_TRAIN, 
        pred.var = c("YC","PAYEMS"),
        prob = TRUE, plot = TRUE,rug = TRUE,
        chull = TRUE, progress = "text")

gc(reset=T)

##########################################################################
##########################################################################
##########################################################################
#Aggregating results

results <- resamples(list(glm=caret_glm,
                          svm=caret_svm,
                          rf=caret_rf,
                          xgb=caret_xgb,
                          nnet=caret_nnet))
head(results$values)
summary(results)

# boxplot for ROC comparison
#bwplot(results,metric="ROC",main="mode ROC comparisons")	
dotplot(results,metric="ROC",main="mode ROC comparisons")

##########################################################################
##########################################################################
##########################################################################
relab<-function(x){ifelse(x=='y',1,0)}

TRAIN_PREDS<- DV_IV_TRAIN %>% select(date,yy,mm,RECESSION) %>% 
  mutate(RECESSION=relab(RECESSION)) %>% 
  cbind.data.frame(relab(caret_glm_pred_tr),
                   relab(caret_svm_pred_tr),
                   relab(caret_rf_pred_tr),
                   relab(caret_xgb_pred_tr),
                   relab(caret_nnet_pred_tr))


TEST_PREDS<- DV_IV_TEST %>% select(date,yy,mm,RECESSION) %>% 
  mutate(RECESSION=relab(RECESSION)) %>% 
  cbind.data.frame(relab(caret_glm_pred_te),
                   relab(caret_svm_pred_te),
                   relab(caret_rf_pred_te),
                   relab(caret_xgb_pred_te),
                   relab(caret_nnet_pred_te))

FULL_PREDS<- rbind.data.frame(TRAIN_PREDS, TEST_PREDS)

##############################################################
##############################################################
##############################################################
#TRAIN PERFORMANCE SUMMARY
metrics_summary_tr<-function(model_preds){
  cbind(
    accuracy(TRAIN_PREDS$RECESSION,model_preds),
    ce(TRAIN_PREDS$RECESSION,model_preds),
    fbeta_score(TRAIN_PREDS$RECESSION,model_preds,beta=0.1),
    ModelMetrics::sensitivity(TRAIN_PREDS$RECESSION,model_preds),#recall/sensitivity/tpr
    ModelMetrics::specificity(TRAIN_PREDS$RECESSION,model_preds),#specificity
    ModelMetrics::auc(TRAIN_PREDS$RECESSION,model_preds),
    ModelMetrics::logLoss(TRAIN_PREDS$RECESSION,model_preds),
    ModelMetrics::precision(TRAIN_PREDS$RECESSION,model_preds),
    ModelMetrics::kappa(TRAIN_PREDS$RECESSION,model_preds),
    ModelMetrics::tnr(TRAIN_PREDS$RECESSION,model_preds),#specificity/tnr
    HMeasure(TRAIN_PREDS$RECESSION,model_preds)$metrics[1],#H-Measure
    HMeasure(TRAIN_PREDS$RECESSION,model_preds)$metrics[5]#KS
    
  )
}


perf_summary_tr<-rbind.data.frame(metrics_summary_tr(TRAIN_PREDS$glm_predicted),
                                  metrics_summary_tr(TRAIN_PREDS$svm_predicted),
                                  metrics_summary_tr(TRAIN_PREDS$rf_predicted),
                                  metrics_summary_tr(TRAIN_PREDS$xgb_predicted),
                                  metrics_summary_tr(TRAIN_PREDS$nnet_predicted)) %>% 
  mutate(model=c("glm","svm","rf","xgb","nnet")) %>% select(model,everything())

#rownames(perf_summary_tr)<-c("glm","rf","xgb","nnet")

colnames(perf_summary_tr)<-c("model","accuracy","ce","fbeta_score",
                             "sensitivity","specificity","auc","logLoss",
                             "precision","kappa","tnr","H","KS")

#############################################
#TEST PERFORMANCE SUMMARY
metrics_summary_te<-function(model_preds){
  cbind(
    
    accuracy(TEST_PREDS$RECESSION,model_preds),
    ce(TEST_PREDS$RECESSION,model_preds),
    fbeta_score(TEST_PREDS$RECESSION,model_preds,beta=1),
    ModelMetrics::sensitivity(TEST_PREDS$RECESSION,model_preds),#recall/sensitivity/tpr
    ModelMetrics::specificity(TEST_PREDS$RECESSION,model_preds),
    ModelMetrics::auc(TEST_PREDS$RECESSION,model_preds),
    ModelMetrics::logLoss(TEST_PREDS$RECESSION,model_preds),
    ModelMetrics::precision(TEST_PREDS$RECESSION,model_preds),
    ModelMetrics::kappa(TEST_PREDS$RECESSION,model_preds),
    ModelMetrics::tnr(TEST_PREDS$RECESSION,model_preds),#specificity/tnr
    HMeasure(TEST_PREDS$RECESSION,model_preds)$metrics[1],#H-Measure
    HMeasure(TEST_PREDS$RECESSION,model_preds)$metrics[5]#KS
  )
}

perf_summary_te<-rbind.data.frame(metrics_summary_te(TEST_PREDS$glm_predicted),
                                  metrics_summary_te(TEST_PREDS$svm_predicted),
                                  metrics_summary_te(TEST_PREDS$rf_predicted),
                                  metrics_summary_te(TEST_PREDS$xgb_predicted),
                                  metrics_summary_te(TEST_PREDS$nnet_predicted))%>% 
  mutate(model=c("glm","svm","rf","xgb","nnet")) %>% select(model,everything())

#rownames(perf_summary_te)<-c("glm","rf","xgb","nnet")

colnames(perf_summary_te)<-c("model","accuracy","ce","fbeta_score",
                             "sensitivity","specificity","auc","logLoss",
                             "precision","kappa","tnr","H","KS")

##############################################################################
##############################################################################

colAUC(X=TRAIN_PREDS[,5:10], 
       y=TRAIN_PREDS$RECESSION, 
       plotROC=T, 
       alg=c("ROC"))

colAUC(X=TEST_PREDS[,5:10], 
       y=TEST_PREDS$RECESSION, 
       plotROC=T, 
       alg=c("ROC"))

colAUC(X=FULL_PREDS[,5:10], 
       y=FULL_PREDS$RECESSION, 
       plotROC=T, 
       alg=c("ROC"))

################################################################
################################################################
plot(smooth(glm_ROC_tr),main="Model ROCs (Train Sample)",col=1,lwd=2,lty=1,grid=T,xlim=c(1,0))
lines(smooth(svm_ROC_tr),col=3,lwd=2,lty=3)
lines(smooth(rf_ROC_tr),col=4,lwd=3,lty=1)
lines(smooth(xgb_ROC_tr),col=5,lwd=3,lty=2)
lines(smooth(nnet_ROC_tr),col=6,lwd=3,lty=3)
legend("bottomright", 
       legend=c("glm","svm","rf","xgb","nnet"),
       col=1:6, lwd=3, lty=c(1,3,1,2,3))


plot(smooth(glm_ROC_te),main="Model ROCs (Test Sample)",col=1,lwd=2,lty=1,grid=T, xlim=c(1,0))
lines(smooth(svm_ROC_te),col=3,lwd=2,lty=3)
lines(smooth(rf_ROC_te),col=4,lwd=3,lty=1)
lines(smooth(xgb_ROC_te),col=5,lwd=3,lty=2)
lines(smooth(nnet_ROC_te),col=6,lwd=3,lty=3)
legend("bottomright", 
       legend=c("glm","svm","rf","xgb","nnet"),
       col=1:6, lwd=3, lty=c(1,3,1,2,3))



################################################################
################################################################
################################################################
#training roc curves
sm_tr_glm_roc<-smooth(glm_ROC_tr)[1:2] %>% as.data.frame(.) %>% mutate(model="PROBIT")
sm_tr_svm_roc<-smooth(svm_ROC_tr)[1:2] %>% as.data.frame(.) %>% mutate(model="SVM")
sm_tr_rf_roc<-smooth(rf_ROC_tr)[1:2] %>% as.data.frame(.) %>% mutate(model="RF")
sm_tr_xgb_roc<-smooth(xgb_ROC_tr)[1:2] %>% as.data.frame(.) %>% mutate(model="XGB")
sm_tr_nnet_roc<-smooth(nnet_ROC_tr)[1:2] %>% as.data.frame(.) %>% mutate(model="NNET")

sm_tr_all_rocs<-rbind.data.frame(sm_tr_glm_roc,
                                 sm_tr_svm_roc,
                                 sm_tr_rf_roc,
                                 sm_tr_xgb_roc,
                                 sm_tr_nnet_roc)



ggplot(data=sm_tr_all_rocs, aes(1-specificities,sensitivities,group=model)) + 
  geom_line(size=1,aes(linetype=model, color=model)) +
  scale_color_manual(values=c('black','steelblue','red','pink','darkblue'))+
  scale_linetype_manual(values=c(2,3,4,1,5,6))+
  ggtitle("Model ROC Comparison (Training Sample)")+
  theme(legend.position="right")+
  theme(plot.title = element_text(hjust = 0.5,size=15))+
  xlab("Specificity")+
  ylab("Sensitivity")+
  theme(axis.title.x = element_text(size = rel(1.1)))+
  theme(axis.title.y = element_text(size = rel(1.1)))+
  theme(axis.text = element_text(size=10,colour="black"))+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")


################################################
################################################
#test roc curves
sm_te_glm_roc<-smooth(glm_ROC_te)[1:2] %>% as.data.frame(.) %>% mutate(model="PROBIT")
sm_te_svm_roc<-smooth(svm_ROC_te)[1:2] %>% as.data.frame(.) %>% mutate(model="SVM")
sm_te_rf_roc<-smooth(rf_ROC_te)[1:2] %>% as.data.frame(.) %>% mutate(model="RF")
sm_te_xgb_roc<-smooth(xgb_ROC_te)[1:2] %>% as.data.frame(.) %>% mutate(model="XGB")
sm_te_nnet_roc<-smooth(nnet_ROC_te)[1:2] %>% as.data.frame(.) %>% mutate(model="NNET")

sm_te_all_rocs<-rbind.data.frame(sm_te_glm_roc,
                                 sm_te_svm_roc,
                                 sm_te_rf_roc,
                                 sm_te_xgb_roc,
                                 sm_te_nnet_roc)


ggplot(data=sm_te_all_rocs, aes(1-specificities,sensitivities,group=model)) + 
  geom_line(size=1,aes(linetype=model, color=model)) +
  scale_color_manual(values=c('black','steelblue','red','pink','darkblue'))+
  scale_linetype_manual(values=c(2,3,4,1,5,6))+
  ggtitle("Model ROC Comparison (Test Sample)")+
  theme(legend.position="right")+
  theme(plot.title = element_text(hjust = 0.5,size=15))+
  xlab("Specificity")+ylab("Sensitivity")+
  theme(axis.title.x = element_text(size = rel(1.1)))+
  theme(axis.title.y = element_text(size = rel(1.1)))+
  theme(axis.text = element_text(size=10,colour="black"))+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")

################################################
########################################################################
########################################################################
########################################################################
#glm visualization and comparisons
caret_glm_full_preds<-rbind.data.frame(
  cbind(TRAIN_PREDS,caret_glm_pr_tr),
  cbind(TEST_PREDS,caret_glm_pr_te)) %>% 
  select(date,RECESSION,glm_predicted,n,y)

ggplot(caret_glm_full_preds,aes(x=date,y=RECESSION))+
  geom_col(col="darkgrey") + 
  #scale_x_datetime()+
  geom_line(data = caret_glm_full_preds, aes(y = y), colour = 'red', size = 0.5) +
  ggtitle('GLM')+ylab('Recession Probability')


####################################
#svm visualization and comparisons
caret_svm_full_preds<-rbind.data.frame(
  cbind(TRAIN_PREDS,caret_svm_pr_tr),
  cbind(TEST_PREDS,caret_svm_pr_te)) %>% 
  select(date,RECESSION,svm_predicted,n,y)

ggplot(caret_svm_full_preds,aes(x=date,y=RECESSION))+
  geom_col(col="darkgrey") + 
  #scale_x_datetime()+
  geom_line(data = caret_svm_full_preds, aes(y = y), colour = 'red', size = 0.5) +
  ggtitle('SVM')+ylab('Recession Probability')


####################################
#rf visualization and comparisons
caret_rf_full_preds<-rbind.data.frame(
  cbind(TRAIN_PREDS,caret_rf_pr_tr),
  cbind(TEST_PREDS,caret_rf_pr_te)) %>% 
  select(date,RECESSION,rf_predicted,n,y)

ggplot(caret_rf_full_preds,aes(x=date,y=RECESSION))+
  geom_col(col="grey") + 
  #scale_x_datetime()+
  geom_line(data = caret_rf_full_preds, aes(y = y), 
            colour = 'red', size = 0.5) +
  ggtitle('RF')+
  ylab('Recession Probability')+
  theme(axis.text = element_text(size=9,colour="black"))+
  theme(axis.title.x = element_text(size = rel(1.1)))+
  theme(axis.title.y = element_text(size = rel(1.1)))+
  geom_line(aes(y = 0.5), colour = 'lightgrey', lty=5 , size = 0.5)


####################################
#xgb visualization and comparisons
caret_xgb_full_preds<-rbind.data.frame(
  cbind(TRAIN_PREDS,caret_xgb_pr_tr),
  cbind(TEST_PREDS,caret_xgb_pr_te)) %>% 
  select(date,RECESSION,xgb_predicted,n,y)

ggplot(caret_xgb_full_preds,aes(x=date,y=RECESSION))+
  geom_col(col="darkgrey") + 
  #scale_x_datetime()+
  geom_line(data = caret_xgb_full_preds, aes(y = y), colour = 'red', size = 0.5) +
  ggtitle('XGB')+ylab('Recession Probability')


####################################
#nnet visualization and comparisons
caret_nnet_full_preds<-rbind.data.frame(
  cbind(TRAIN_PREDS,caret_nnet_pr_tr),
  cbind(TEST_PREDS,caret_nnet_pr_te)) %>% 
  select(date,RECESSION,nnet_predicted,n,y)

ggplot(caret_nnet_full_preds,aes(x=date,y=RECESSION))+
  geom_col(col="darkgrey") + 
  #scale_x_datetime()+
  geom_line(data = caret_nnet_full_preds, aes(y = y), colour = 'red', size = 0.5) +
  ggtitle('NNET')+ylab('Recession Probability')

accuracy(caret_glm_full_preds$RECESSION,caret_glm_full_preds$glm_predicted)
accuracy(caret_svm_full_preds$RECESSION,caret_svm_full_preds$svm_predicted)
accuracy(caret_rf_full_preds$RECESSION,caret_rf_full_preds$rf_predicted)
accuracy(caret_xgb_full_preds$RECESSION,caret_xgb_full_preds$xgb_predicted)
accuracy(caret_nnet_full_preds$RECESSION,caret_nnet_full_preds$nnet_predicted)


#############################################
#FULL SAMPLE PERFORMANCE SUMMARY
metrics_summary_full<-function(model_obs, model_preds){
  cbind(
    accuracy(model_obs,model_preds),
    ce(model_obs,model_preds),
    fbeta_score(model_obs,model_preds,beta=1),
    ModelMetrics::sensitivity(model_obs,model_preds),#recall/sensitivity/tpr
    ModelMetrics::specificity(model_obs,model_preds),
    ModelMetrics::auc(model_obs,model_preds),
    ModelMetrics::logLoss(model_obs,model_preds),
    ModelMetrics::precision(model_obs,model_preds),
    ModelMetrics::kappa(model_obs,model_preds),
    ModelMetrics::tnr(model_obs,model_preds),#specificity/tnr
    HMeasure(model_obs,model_preds)$metrics[1],#H-Measure
    HMeasure(model_obs,model_preds)$metrics[5]#KS
  )
}

perf_summary_full<-rbind.data.frame(metrics_summary_full(caret_glm_full_preds$RECESSION,caret_glm_full_preds$glm_predicted),
                                    metrics_summary_full(caret_svm_full_preds$RECESSION,caret_svm_full_preds$svm_predicted),                                    
                                    metrics_summary_full(caret_rf_full_preds$RECESSION,caret_rf_full_preds$rf_predicted),
                                    metrics_summary_full(caret_xgb_full_preds$RECESSION,caret_xgb_full_preds$xgb_predicted),
                                    metrics_summary_full(caret_nnet_full_preds$RECESSION,caret_nnet_full_preds$nnet_predicted)) %>% 
  mutate(model=c("glm","svm","rf","xgb","nnet")) %>% select(model,everything())

#rownames(perf_summary_full)<-c("glm","rf","xgb","nnet")

colnames(perf_summary_full)<-c("model","accuracy","ce","fbeta_score",
                               "sensitivity","specificity","auc","logLoss",
                               "precision","kappa","tnr","H","KS")


write.xlsx(perf_summary_tr, sheetName="perf_summary_tr", file="perf_summary.xlsx", row.names=FALSE)
write.xlsx(perf_summary_te, sheetName="perf_summary_te", file="perf_summary.xlsx", row.names=FALSE, append=TRUE)
write.xlsx(perf_summary_full, sheetName="perf_summary_full", file="perf_summary.xlsx", row.names=FALSE, append=TRUE)

#################################################################
#################################################################


library(gt)
gt(perf_summary_tr) %>% tab_header(title = "perf_summary_train") %>% 
  tab_style(
    style = cell_text(style = "italic",weight="bold"),
    locations = cells_body(columns = vars(model))
  ) %>% 
  tab_style(
    style = cell_text(style = "italic",weight="bold"),
    locations = cells_column_labels(columns = 1:ncol(perf_summary_full))
  ) %>% 
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(columns = 2:ncol(perf_summary_full))
  ) 

#################################################################
gt(perf_summary_te) %>% tab_header(title = "perf_summary_test") %>% 
  tab_style(
    style = cell_text(style = "italic",weight="bold"),
    locations = cells_body(columns = vars(model))
  ) %>% 
  tab_style(
    style = cell_text(style = "italic",weight="bold"),
    locations = cells_column_labels(columns = 1:ncol(perf_summary_full))
  ) %>% 
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(columns = 2:ncol(perf_summary_full))
  ) 

#################################################################
gt(perf_summary_full) %>% 
  tab_header(title = "perf_summary_full") %>% 
  tab_style(
    style = cell_text(style = "italic",weight="bold"),
    locations = cells_body(columns = vars(model))
  ) %>% 
  tab_style(
    style = cell_text(style = "italic",weight="bold"),
    locations = cells_column_labels(columns = 1:ncol(perf_summary_full))
  ) %>% 
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(columns = 2:ncol(perf_summary_full))
  ) 

################################################
###############################################
###############################################
#RF and XGB plots

p1<-vip(caret_rf, bar = T, horizontal = T, 
        num_features=3,
        size = 1, width=0.5,
        fill="steelblue") + ggtitle("RF") +
  theme(axis.text = element_text(size=10,colour="black"))
################################################
p2<-vip(caret_xgb, bar = T, horizontal = T,
        num_features=3, 
        size = 1, width=0.5,
        fill="steelblue") + ggtitle("XGB") +
  theme(axis.text = element_text(size=10,colour="black"))
################################################
p3<-autoplot(caret_rf %>% partial(pred.var = "PAYEMS",
                                  prob=T,
                                  type="classification",
                                  chull=F), 
             ylab="Predicted Probability",
             col="darkblue",size=1)+ 
  #ggtitle("RF PDP") + 
  theme(axis.title.x = element_text(size = rel(0.9)))+
  theme(axis.title.y = element_text(size = rel(0.9)))+
  theme(axis.text = element_text(size=8,colour="black"))

################################################
p4<-autoplot(caret_xgb %>% partial(pred.var = "YC",
                                   prob=T,
                                   type="classification",
                                   chull=F), 
             ylab="Predicted Probability",
             col="darkblue",size=1)+ 
  #ggtitle("XGB PDP") + 
  theme(axis.title.x = element_text(size = rel(0.9)))+
  theme(axis.title.y = element_text(size = rel(0.9)))+
  theme(axis.text = element_text(size=8,colour="black"))


gridExtra::grid.arrange(p1,p2,p3,p4,
                        nrow=2,ncol=2)

################################################
################################################
################################################
#Variable Importance
p5<-vip(caret_rf, bar = T, horizontal = T, 
        num_features=5,
        size = 1, width=0.5,
        fill="steelblue") + 
  ggtitle("Random Forest") +
  theme(axis.text = element_text(size=10,colour="black"))
################################################
#1D-PDP
p6<-autoplot(caret_rf %>% partial(pred.var = "UNEMPLOY",
                                  prob=T,
                                  type="classification",
                                  chull=F), 
             ylab="Predicted Probability",
             col="darkblue",size=1)+ 
  #ggtitle("RF PDP") + 
  theme(axis.title.x = element_text(size = rel(0.9)))+
  theme(axis.title.y = element_text(size = rel(0.9)))+
  theme(axis.text = element_text(size=8,colour="black"))


gridExtra::grid.arrange(p5,p6,
                        nrow=2,ncol=1)

################################################
################################################
################################################
#training ROC for RF vs GLM
sm_tr_glm_rf_rocs<-rbind.data.frame(sm_tr_glm_roc,
                                 sm_tr_rf_roc)



ggplot(data=sm_tr_glm_rf_rocs, aes(1-specificities,sensitivities,group=model)) + 
  geom_line(size=1,aes(linetype=model, color=model)) +
  scale_color_manual(values=c('black','red'))+
  scale_linetype_manual(values=c(1,2))+
  ggtitle("ROC Comparison (Training)")+
  theme(legend.position="right")+
  theme(plot.title = element_text(hjust = 0.5,size=15))+
  xlab("Specificity")+
  ylab("Sensitivity")+
  theme(axis.title.x = element_text(size = rel(1.1)))+
  theme(axis.title.y = element_text(size = rel(1.1)))+
  theme(axis.text = element_text(size=10,colour="black"))+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")



