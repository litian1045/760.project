import numpy as np
import pandas as pd


index = np.loadtxt('test_set_index.csv', dtype=int, delimiter=',')
data = pd.read_csv('main_fill.csv')
test_set = data.iloc[index]
train_set = data.iloc[list(set(range(len(data))) - set(index))]
print(test_set.iloc[0:10,])
print(train_set.iloc[0:10,])