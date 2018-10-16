# random forest test
from sklearn.model_selection import cross_val_score
from sklearn.datasets import make_blobs
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.tree import DecisionTreeClassifier
import pandas as pd
import numpy as np


index = np.loadtxt('test_set_index.csv', dtype=int, delimiter=',')
data = pd.read_csv('main_fill.csv')
test_set = data.iloc[index]
train_set = data.iloc[list(set(range(len(data))) - set(index))]
features_list = list(set(train_set.columns)-set(["TARGET"]))
train_set = train_set[1:10000]

clf = RandomForestClassifier(n_estimators=100, max_depth=6, min_samples_split=20, random_state=0)
scores = cross_val_score(clf, train_set[features_list], train_set["TARGET"])
print(scores.mean())

