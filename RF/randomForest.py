# random forest test
from sklearn.model_selection import cross_val_score
from sklearn.datasets import make_blobs
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.tree import DecisionTreeClassifier
import pandas as pd
from TSSVM import *


main = pd.read_csv('main_fill.csv')
features_list = list(set(main.columns)-set(["TARGET"]))
len(features_list)


clf = RandomForestClassifier(n_estimators=100, max_depth=None, min_samples_split=2, random_state=0)
scores = cross_val_score(clf, main[features_list], main["TARGET"])
print(scores.mean())

