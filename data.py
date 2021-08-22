 # Required libraries
import numpy as np
import pandas as pd
import string as st
# Code for defining BALANCE SHEET as DataFrame
df = pd.read_excel("https://github.com/aiasci/firms/blob/main/data/all/ALL_BS_TOTAL.xlsx?raw=true")
 # Code for changing index as Sector Name
df.index =np.repeat(df.iloc[0,0],len(df.index))
 # Enumareting the first column for ease
df.loc[:,df.columns[0]]=list(np.arange(1, len(df.index)+1))
 # Getting rid of th NaN values
df=df.dropna()
 # Renumareting the first column
df.loc[:,df.columns[0]]=list(np.arange(1, len(df.index)+1))
 # Code for defining INCOME STATEMENT as DataFrame
df1 = pd.read_excel("https://github.com/aiasci/firms/blob/main/data/all/ALL_IS_TOTAL.xlsx?raw=true")
 # Code for changing index as Sector Name
df1.index =np.repeat(df1.iloc[0,0],len(df1.index))
 # Enumareting the first column for ease
df1.loc[:,df1.columns[0]]=list(np.arange(1, len(df1.index)+1))
 # Getting rid of th NaN values # Code for defining BALANCE SHEET as DataFrame
df = pd.read_excel("https://github.com/aiasci/firms/blob/main/data/all/ALL_BS_TOTAL.xlsx?raw=true")
df1=df1.dropna()
 # Renumareting the first column
df1.loc[:,df1.columns[0]]=list(np.arange(1, len(df1.index)+1))
 # Row binding the two DataFrames
df=df.append(pd.DataFrame(data=df1),ignore_index=False)
 # Renumareting the first column
df.loc[:,df.columns[0]]=list(np.arange(1, len(df.index)+1))
 # Creating a key for loops
key_file=st.ascii_uppercase
key_file=key_file[0:10]+key_file[11:14]+key_file[15:19]
 # Defining DataFrame for Each Sector's BALANCE SHEET
for i in range(0, len(key_file)):
    vars()["df_"+key_file[i]] = pd.read_excel("https://github.com/aiasci/firms/blob/main/data/balance_sheet/1digit/total/"+key_file[i]+"_BS_TOTAL.xlsx?raw=true")
 # Defining DataFrame for Each Sector's INCOME STATEMENT
for i in range(0, len(key_file)):
    vars()["df_1"+key_file[i]] = pd.read_excel("https://github.com/aiasci/firms/blob/main/data/income_statement/1digit/total/"+key_file[i]+"_IS_TOTAL.xlsx?raw=true")
 # Manipulating Each Sector's DataFrames
for i in range(0, len(key_file)):
    vars()["df_"+key_file[i]].index =np.repeat(vars()["df_"+key_file[i]].iloc[0,0],len(vars()["df_"+key_file[i]]))
    vars()["df_"+key_file[i]].loc[:,vars()["df_"+key_file[i]].columns[0]]=list(np.arange(1, len(vars()["df_"+key_file[i]].index)+1))
    vars()["df_"+key_file[i]]=vars()["df_"+key_file[i]].dropna()
    vars()["df_1"+key_file[i]].index =np.repeat(vars()["df_1"+key_file[i]].iloc[0,0],len(vars()["df_1"+key_file[i]]))
    vars()["df_1"+key_file[i]].loc[:,vars()["df_1"+key_file[i]].columns[0]]=list(np.arange(1, len(vars()["df_1"+key_file[i]].index)+1))
    vars()["df_1"+key_file[i]]=vars()["df_1"+key_file[i]].dropna()
    vars()["df_1"+key_file[i]].loc[:,vars()["df_1"+key_file[i]].columns[0]]=list(np.arange(1, len(vars()["df_1"+key_file[i]].index)+1))
    vars()["df_"+key_file[i]]=vars()["df_"+key_file[i]].append(pd.DataFrame(data=vars()["df_1"+key_file[i]]),ignore_index=False)
    vars()["df_"+key_file[i]].loc[:,vars()["df_"+key_file[i]].columns[0]]=list(np.arange(1, len(vars()["df_"+key_file[i]].index)+1))
 # Merging all DataFrames
for i in range(0, len(key_file)):
    df= df.append(pd.DataFrame(data=vars()["df_"+key_file[i]]),ignore_index=False)
# Renaming columns
df.columns = ["n","code",2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019]
 # Saving it as excel
df.to_excel("df.xlsx", sheet_name = "Sheet1")