#!/usr/bin/env python
# coding: utf-8

# In[5]:


from selenium import webdriver
import pandas as pd
import time
from datetime import datetime
import numpy as np


# In[6]:


from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup
import re
import os


# In[4]:


urls=[]
for i in range(97,100):  

     temp_url='https://www.whoscored.com/Matches/15460'+str(i)+'/Live/Netherlands-Eredivisie-2019-2020'
     urls.append(temp_url)
for i in range(100,106):  

     temp_url='https://www.whoscored.com/Matches/1546'+str(i)+'/Live/Netherlands-Eredivisie-2019-2020'
     urls.append(temp_url)

# temp_url='https://www.whoscored.com/Matches/1469829/Live/Netherlands-Eredivisie-2019-2020'
# urls.append(temp_url)
# temp_url='https://www.whoscored.com/Matches/1469830/Live/Netherlands-Eredivisie-2019-2020'
# urls.append(temp_url)
# temp_url='https://www.whoscored.com/Matches/1469831/Live/Netherlands-Eredivisie-2019-2020'
# urls.append(temp_url)
# temp_url='https://www.whoscored.com/Matches/1469832/Live/Netherlands-Eredivisie-2019-2020'
# urls.append(temp_url)
# temp_url='https://www.whoscored.com/Matches/1469833/Live/Netherlands-Eredivisie-2019-2020'
# urls.append(temp_url)
# temp_url='https://www.whoscored.com/Matches/1469834/Live/Netherlands-Eredivisie-2019-2020'
# urls.append(temp_url)
# for i in range(715,721):  


#     temp_url='https://www.whoscored.com/Matches/1469'+str(i)+'/Live/Netherlands-Eredivisie-2019-2020'

#     urls.append(temp_url)


for funt in range(len(urls)):
	
    print('processing..........')
    driver = webdriver.Chrome()
    driver.get(urls[funt])
    time.sleep(2.4)
    soup_level1=BeautifulSoup(driver.page_source, 'lxml')
    driver.close()
    print('Fetching details of match...........')
    false='false'
    true='true'
    print(soup_level1)
    krokuch=soup_level1.text.split('{"playerIdNameDictionary":{')[1]
    milakuch=krokuch.split('commonEvents')[1]
    haukuch=milakuch.split('PostGame')
    dddd=(haukuch[0]+'PostGame'+(haukuch[1])+'PostGame"'+'}')
    needed=dddd.split('{"id')[1:]
    # c1_id=[]
    cleaned=[]
    for i in range(len(needed)-1):
        cleaned.append(eval('{"id'+needed[i][:-2]+'}'))
    df=pd.DataFrame(cleaned)
    print('details fetched.............')
    #for column 'type'
    types=list(df['type'])
    types=pd.DataFrame(types)
    typedisplayName='type/'+types.columns[0]
    typevalue='type/'+types.columns[1]
    types.rename(columns = {types.columns[0]:typedisplayName,types.columns[1]:typevalue}, inplace = True) 
    df=df.drop(columns=['type'])
    df=pd.concat([df,types], axis=1)
    #for column 'satisfiedEventsTypes'
    satisfiedEventsTypess=list(df['satisfiedEventsTypes'])
    satisfiedEventsTypess=pd.DataFrame(satisfiedEventsTypess)
    s_temp=[]
    for i in range(len(satisfiedEventsTypess.columns)):
        s_temp.append('satisfiedEventsTypes/'+str(satisfiedEventsTypess.columns[i]))
        satisfiedEventsTypess.rename(columns = {satisfiedEventsTypess.columns[i]:s_temp[-1]}, inplace = True)
    df=df.drop(columns=['satisfiedEventsTypes'])
    df=pd.concat([df,satisfiedEventsTypess], axis=1)
    #for column 'period'
    periods=list(df['period'])
    periods=pd.DataFrame(periods)
    s_temp=[]
    for i in range(len(periods.columns)):
        s_temp.append('period/'+str(periods.columns[i]))
        periods.rename(columns = {periods.columns[i]:s_temp[-1]}, inplace = True)
    df=df.drop(columns=['period'])
    df=pd.concat([df,periods], axis=1)
    #for column 'outcomeType'
    outcomeTypes=list(df['outcomeType'])
    outcomeTypes=pd.DataFrame(outcomeTypes)
    s_temp=[]
    for i in range(len(outcomeTypes.columns)):
        s_temp.append('outcomeType/'+str(outcomeTypes.columns[i]))
        outcomeTypes.rename(columns = {outcomeTypes.columns[i]:s_temp[-1]}, inplace = True)
    df=df.drop(columns=['outcomeType'])
    df=pd.concat([df,outcomeTypes], axis=1)
    # cardTypess=list(df['cardType'])
    # cardTypess=pd.DataFrame(cardTypes)
    print('processing...........')
    #for column 'qualifiers'
    qualifierss=pd.DataFrame(list(df['qualifiers']))
    print(urls[funt])
    for i in range(len(qualifierss.columns)):
        dfdf_1=pd.DataFrame(dict(qualifierss[i])).T
        dfdf_type_1=pd.DataFrame(dict(dfdf_1['type'])).T#needed
        s_temp=[]
        for j in range(len(dfdf_type_1.columns)):
            s_temp.append('qualifiers/'+str(i)+'/type/'+str(dfdf_type_1.columns[j]))
            dfdf_type_1.rename(columns = {dfdf_type_1.columns[j]:s_temp[-1]}, inplace = True)
        df=pd.concat([df,dfdf_type_1], axis=1)
        dfdf_value_1=pd.DataFrame((pd.DataFrame(dict(qualifierss[0])).T)['value'])#needed
        vald='qualifiers/'+str(i)+'/value'
        dfdf_value_1.rename(columns = {'value':vald}, inplace = True)
        df=pd.concat([df,dfdf_value_1], axis=1)
    df=df.drop(columns=['qualifiers'])

    print('converting to csv file.............')
    date_match=soup_level1.find_all('div',{'class':'info-block cleared'})[2].text[-9:]
    first_team=soup_level1.find_all('a',{'class':'team-link'})[0].text[:3]
    first_team_home=soup_level1.find_all('a',{'class':'team-link'})[0].text
    second_team=soup_level1.find_all('a',{'class':'team-link'})[1].text[:3]
    second_team_away=soup_level1.find_all('a',{'class':'team-link'})[1].text
    filename='01-'+date_match+'-'+first_team+second_team+'-event.csv'
    filename=filename.replace(" ",'')
    col=df.columns.tolist()
    try:
        if(col[3]!='cardType'):
            df.insert(3,'cardType',np.nan)
            col=df.columns.tolist()
    except:
        print('Finalizing........')
    try:
        if(col[11]!='isGoal'):
            df.insert(11,'isGoal',np.nan)
            col=df.columns.tolist()
    except:
        print('Please wait........')
    try:
        if(col[1]!='Date'):
            df.insert(1,'Date',date_match)
            col=df.columns.tolist()
    except:
        print('Please wait date........')
    try:
        if(col[11]!='isOwnGoal'):
            df.insert(11,'isOwnGoal',np.nan)
            col=df.columns.tolist()
    except:
        print('Done.............')
    try:
        if(col[11]!='AwayTeam'):
            df.insert(11,'AwayTeam',second_team_away)
            col=df.columns.tolist()
    except:
        print('Home.............')
    try:
        if(col[11]!='HomeTeam'):
            df.insert(11,'HomeTeam',first_team_home)
            col=df.columns.tolist()
    except:
        print('Home.............')      


    df.insert(0,'TeamId',df['teamId'])
    df=df.drop(columns=['teamId'])
    df.insert(1,'PlayerId',df['playerId'])
    df=df.drop(columns=['playerId'])
    
    print('Fetched data for ',filename)

    df.to_csv(filename,encoding='utf-8')


# In[ ]:





# In[ ]:




