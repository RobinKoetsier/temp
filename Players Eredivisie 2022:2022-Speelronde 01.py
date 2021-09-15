#!/usr/bin/env python
# coding: utf-8

# In[1]:


from selenium import webdriver
import pandas as pd
import time
from datetime import datetime


# In[61]:


from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup
import re
import os


# In[351]:




urls=[]
for i in range(97,100):  

     temp_url='https://www.whoscored.com/Matches/15460'+str(i)+'/Live/Netherlands-Eredivisie-2019-2020'
     urls.append(temp_url)
for i in range(100,106):  

     temp_url='https://www.whoscored.com/Matches/1546'+str(i)+'/Live/Netherlands-Eredivisie-2019-2020'
     urls.append(temp_url)

# temp_url='https://www.whoscored.com/Matches/1469826/Live/Netherlands-Eredivisie-2019-2020'
# urls.append(temp_url)
# temp_url='https://www.whoscored.com/Matches/1469827/Live/Netherlands-Eredivisie-2019-2020'
# urls.append(temp_url)
# temp_url='https://www.whoscored.com/Matches/1469828/Live/Netherlands-Eredivisie-2019-2020'
# urls.append(temp_url)
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

    
for fun in range(len(urls)):
    print('processing..........')
    driver = webdriver.Chrome()
    # driver.get(url)
    # driver.implicitly_wait(30)
    driver.get(urls[fun])
    time.sleep(3)
    soup_level1=BeautifulSoup(driver.page_source, 'lxml')
#     table = soup_level1.find_all('script')
    driver.close()
    print('Fetching details of match...........')
#     tempo=table[:]
#     data_for_names=''
#     for i in range(len(tempo)):
#         data_for_names=''+tempo[i].text
    nishu=soup_level1.text.split('{"playerIdNameDictionary":{')[1]
    fir_data=nishu.split(',"periodMinuteLimits')[0][:-1]
    
#     a=table[-13]
#     fir_data=a.text.split(',"periodMinuteLimits')
#     tttt=fir_data[0]
#     tttt=tttt[59:-1]
    # fir_data=a.text[59:1116]
    fir_data=fir_data.replace('"','')
    players=fir_data.split(',')
    ids=[]
    name=[]
    print(urls[fun])
    for i in players:
        id_name=i.split(':')
        ids.append(id_name[0])
        name.append(id_name[1])

    date_match=soup_level1.find_all('div',{'class':'info-block cleared'})[2].text[-9:]
    first_team=soup_level1.find_all('a',{'class':'team-link'})[0].text[:3]
    second_team=soup_level1.find_all('a',{'class':'team-link'})[1].text[:3]
    filename=date_match+'-'+first_team+second_team+'_playerid'+'.csv'
    filename=filename.replace(" ",'')
    print('Fetched data for ',filename)
    fin=pd.DataFrame({'ID':ids,'Name':name})
    fin.to_csv(filename,encoding='utf-8')


# In[ ]:




