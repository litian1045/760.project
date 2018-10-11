import pandas as pd


data_set = pd.read_csv('new.csv', header=None)
data_set.to_csv('new2.csv')
