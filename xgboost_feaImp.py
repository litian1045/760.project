import numpy as np   # linear algebra
import pandas as pd  # data processing, CSV file I/O

# Input data files are available in the "./Data/" directory.
import os
import pandas as pd
import numpy as np
import xgboost as xgb
from xgboost.sklearn import XGBClassifier
from sklearn import metrics

# Suppress warnings
import warnings
warnings.filterwarnings('ignore')

# matplotlib and seaborn for plotting
import matplotlib.pyplot as plt
import seaborn as sns

# Load the data
#index = np.loadtxt('test_set_index.csv', dtype=int, delimiter=',')
#data = pd.read_csv('./Data/main_fill.csv')
#test_df = data.iloc[index]
#train_df = data.iloc[list(set(range(len(data))) - set(index))]

train_df = pd.read_csv('main_fill.csv')
#test_df = pd.read_csv('./Data/training1000.csv')
train_label = train_df['TARGET']


def modelfit(alg, dtrain, predictors, useTrainCV=True, cv_folds=10, early_stopping_rounds=100):
    if useTrainCV:
        xgb_param = alg.get_xgb_params()
        xgtrain = xgb.DMatrix(dtrain[predictors].values, label=dtrain['TARGET'].values)
        print("\nSomething")
        cvresult = xgb.cv(xgb_param, xgtrain, num_boost_round=alg.get_params()['n_estimators'], nfold=cv_folds, metrics='auc', early_stopping_rounds=early_stopping_rounds)
        alg.set_params(n_estimators=cvresult.shape[0])

    # Fit the algorithm on the data
    alg.fit(dtrain[predictors], dtrain['TARGET'], eval_metric='auc')

    # Predict training set:
    dtrain_predictions = alg.predict(dtrain[predictors])
    dtrain_predprob = alg.predict_proba(dtrain[predictors])[:, 1]

    # Print model report:
    print("\nModel Report")
    print("Accuracy : %.4g" % metrics.accuracy_score(dtrain['TARGET'].values, dtrain_predictions))
    print("AUC Score (Train): %f" % metrics.roc_auc_score(dtrain['TARGET'], dtrain_predprob))

    feat_imp = pd.Series(alg.get_booster().get_fscore()).sort_values(ascending=False)
    feat_imp.to_csv("importance.csv")
    #feat_imp.plot(kind='bar', title='Feature Importances')
    #plt.ylabel('Feature Importance Score')

predictors = [x for x in train_df.columns if x not in ['SK_ID_CURR', 'TARGET']]

xgb1 = XGBClassifier(
 learning_rate =0.1,
 n_estimators=1000,
 max_depth=5,
 min_child_weight=1,
 gamma=0,
 subsample=0.8,
 colsample_bytree=0.8,
 objective= 'binary:logistic',
 nthread=4,
 scale_pos_weight=1,
 seed=1)


modelfit(xgb1, train_df, predictors)
