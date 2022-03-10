import tkinter as tk
import pandas as pd
import os
import statsmodels.api as sm
from statsmodels.formula.api import ols
import numpy as np

os.chdir('/Users/rcorr/Documents/Belger/Analysis/CodeToUpload/AcuteStressDataAnalysis/Chapter4_StressConnectivity')

# takes network/region key from FindNetworksKey.xlsx (CONN Toolbox network region names) and pasted regions from pop-up
# window and prints the network membership of each pasted region

def submit():
    global value
    global df
    global dfAtlas
    
    value = txt_edit.get("1.0", tk.END)
    
    Key = pd.read_excel('FindNetworksKey.xlsx',sheet_name='KeyAll')
    Key['SplitROI'] = Key['SplitROI'].map(lambda x: x.strip())
    
    value = list(filter(None, value.split('\n'),))
    dfMid = pd.DataFrame(value,columns=['region'])
    dfAtlas = pd.DataFrame(columns=['Vox','SplitROI'])

    dfMid = dfMid["region"].str.split("(",expand=True)
    dfAtlas['Vox'] = dfMid[0].str.split(" ",expand=True,n=4)[0]
    dfAtlas['SplitROI'] = (dfMid[1].str.split(" ",expand=True,n=4)[4]).map(lambda x: x.strip())
    
    df = pd.merge(dfAtlas, Key, how='left', on=['SplitROI'], validate='many_to_one')
    
    df['PrintMe'] = df['SplitROI'].str.replace('atlas.', '') + ": " + df['Network'] + " (" + df['Vox'] + " voxels)"
    df['PrintMe'] = df['PrintMe'].str.replace('r:', 'R:') 
    df['PrintMe'] = df['PrintMe'].str.replace('l:', 'L:') 
    df['PrintMe'] = df['PrintMe'].str.replace('not-labeled: NoLabel', 'Not Labeled') 
    df['PrintMe'] = df['PrintMe'].str.replace('(1 voxels)', '1 voxel') 

    for elem in df['PrintMe'].to_list():
        print(elem)
        
    print("...........................")
    
window = tk.Tk()
window.title("Paste CONN Output")

window.rowconfigure(0, minsize=800, weight=1)
window.columnconfigure(1, minsize=800, weight=1)

txt_edit = tk.Text(window)
fr_buttons = tk.Frame(window)
btn_open = tk.Button(fr_buttons, text="Submit", command=submit)

btn_open.grid(row=0, column=0, sticky="ew", padx=5, pady=5)

fr_buttons.grid(row=0, column=0, sticky="ns")
txt_edit.grid(row=0, column=1, sticky="nsew")

window.mainloop()