import os, csv, glob
import pandas as pd
import numpy as np

files = [f for f in os.listdir("./data/inputdata") if os.path.isfile(f)]
for f in files:
    #Only for txt files
    if f.endswith(".txt"):  
        #Read txt file
        df=pd.read_table(f, encoding="utf-16", sep="\t")
        #Drop all empty columns
        df.dropna(axis=1,how='all',inplace=True)
        #df.drop(df.columns[0], axis=1)
        #Write df as csv without null columns
        df.to_csv(os.path.splitext(f)[0]+ "1.txt", sep="\t")



