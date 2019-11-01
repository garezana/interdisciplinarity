
import os, csv
import pandas as pd
import numpy as np

files = [f for f in os.listdir('.') if os.path.isfile(f)]
for f in files:
    if f.endswith(".txt"):  
        df=pd.read_table(f, encoding='utf-16', sep="\t") 
        year=np.array(df["PD"].values).tolist()  
        print(year[0])
        journame=df["BN"].values
        name=journame[1].replace(" ", ".")

        os.rename(f, name + "." +  str(year[1]) + ".txt")
