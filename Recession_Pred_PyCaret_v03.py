#!/usr/bin/env python
# coding: utf-8

# In[1]:


from pycaret.classification import * 


# In[2]:


import matplotlib.pyplot as plt


# In[3]:


import os, csv
import pandas as pd


# In[4]:


os.chdir("as_you_wish!")
os.getcwd()


# In[5]:


get_ipython().run_line_magic('matplotlib', 'inline')


# In[6]:


DV_IV_TRAIN=pd.read_csv('DV_IV_TRAIN.csv')
DV_IV_TEST=pd.read_csv('DV_IV_TEST.csv')
DV_IV_TRANSFORMED=pd.read_csv('DV_IV_TRANSFORMED.csv')


# In[7]:


DV_IV_TRAIN.head
DV_IV_TEST.head
DV_IV_TRANSFORMED.head


# In[8]:


DV_IV_TRAIN.shape
DV_IV_TEST.shape
DV_IV_TRANSFORMED.shape


# In[9]:


DV_IV_TRAIN.columns
#type(DV_IV_TRAIN)


# In[10]:


DV_IV_TRANSFORMED['RECESSION'].value_counts()
DV_IV_TRAIN['RECESSION'].value_counts()
DV_IV_TEST['RECESSION'].value_counts()


# In[11]:


df_tr=DV_IV_TRAIN.drop(['date', 'yy', 'mm'], axis=1)
df_tr.head


# In[12]:


df_te=DV_IV_TEST.drop(['date', 'yy', 'mm'], axis=1)
df_te.head


# In[13]:


#change folding and default metric
model_steup = setup(data = df_tr, target = 'RECESSION', 
                    fix_imbalance=True, fold_shuffle=False, 
                    data_split_shuffle = False, 
                    fold_strategy='timeseries', 
                    log_experiment=True, session_id=12345)


# In[14]:


#best_model = compare_models()
best_model = compare_models(sort='AUC')


# In[15]:


#get_logs()


# In[16]:


models(type='ensemble').index.tolist()


# In[17]:


print(best_model)


# In[18]:


top3 = compare_models(n_select=3,sort='Recall')


# In[19]:


top3


# In[20]:


et = create_model('et')


# In[21]:


print(et)


# In[22]:


tuned_et = tune_model(et)


# In[23]:


plot_model(tuned_et, plot = 'auc')


# In[24]:


plot_model(tuned_et, plot = 'pr')


# In[25]:


plot_model(tuned_et, plot='feature')


# In[26]:


plot_model(tuned_et, plot = 'confusion_matrix')


# In[27]:


plot_model(tuned_et, plot = 'boundary')


# In[28]:


plot_model(tuned_et, plot = 'class_report')


# In[29]:


eval_et = evaluate_model(tuned_et)


# In[30]:


interpret_model(tuned_et)


# In[36]:


interpret_model(tuned_et, plot = 'reason', observation=35)


# In[31]:


interpret_model(tuned_et, plot = 'reason')


# In[32]:


predict_model(tuned_et)


# In[37]:


final_et = finalize_model(tuned_et)


# In[38]:


print(final_et)


# In[39]:


predict_model(final_et)


# In[40]:


from pycaret.utils import check_metric
#Accuracy, AUC, Recall, Precision, F1, Kappa, MCC


# In[41]:


tr_pred = predict_model(final_et, data=df_tr)
tr_pred.head()


# In[42]:


te_pred = predict_model(final_et, data=df_te)
te_pred.head()


# In[43]:


[check_metric(tr_pred['RECESSION'], tr_pred['Label'], metric = 'Accuracy'),
check_metric(tr_pred['RECESSION'], tr_pred['Label'], metric = 'AUC'),
check_metric(tr_pred['RECESSION'], tr_pred['Label'], metric = 'Recall'),
check_metric(tr_pred['RECESSION'], tr_pred['Label'], metric = 'Precision')]


# In[44]:


[check_metric(te_pred['RECESSION'], te_pred['Label'], metric = 'Accuracy'),
check_metric(te_pred['RECESSION'], te_pred['Label'], metric = 'AUC'),
check_metric(te_pred['RECESSION'], te_pred['Label'], metric = 'Recall'),
check_metric(te_pred['RECESSION'], te_pred['Label'], metric = 'Precision')]


# In[45]:


type(te_pred)


# In[46]:


tr_pred["date"] = DV_IV_TRAIN['date']
tr_pred["Pr"] = 1- tr_pred["Score"]
tr_pred.plot(x='date',y='Score')


# In[47]:


te_pred["date"] = DV_IV_TEST['date']
te_pred["Pr"] = 1- te_pred["Score"]
te_pred.plot(x='date',y='Score')


# In[48]:


full_pred=pd.concat([tr_pred,te_pred],ignore_index=True)


# In[49]:


full_pred


# In[50]:


full_pred.plot(x='date',y='Score')


# In[ ]:




