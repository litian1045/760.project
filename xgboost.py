import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.metrics import roc_auc_score

features_df = pd.read_csv('main_cor.csv', low_memory=False)
features_df.info()

index = np.loadtxt('test_set_index.csv', dtype=int, delimiter=',')
test_data = features_df.iloc[index]

train_set = features_df.iloc[list(set(range(len(features_df))) - set(index))]


data_df_test = train_set.iloc[:, :]
data_df = data_df_test.sample(frac=1)
data_df.info()

train_data = data_df.iloc[0:247510, :]
train_data.reset_index(drop=True, inplace=True)

dev_data = data_df.iloc[247510:277510, :]
dev_data.reset_index(drop=True, inplace=True)

#test_data = data_df.iloc[277510:307510, :]
#test_data.reset_index(drop=True, inplace=True)

train_data.info()
dev_data.info()
test_data.info()

print(train_data['TARGET'].sum()/ train_data.shape[0], dev_data['TARGET'].sum()/ dev_data.shape[0], test_data['TARGET'].sum()/ test_data.shape[0])

print(train_data.shape[0], dev_data.shape[0], test_data.shape[0])


def model(train_x, train_y, test_x, test_y, param, num_round):
    dtrain = xgb.DMatrix(train_x.values, label=train_y.values, feature_names=train_x.columns, missing=np.nan)
    dtest = xgb.DMatrix(test_x.values, label=test_y.values, feature_names=test_x.columns, missing=np.nan)
    watchlist = [(dtrain, 'dtrain'), (dtest, 'dev')]
    #     bst = xgb.train(param, dtrain, num_round, watchlist, early_stopping_rounds=20)
    bst = xgb.train(param, dtrain, num_round, watchlist)
    # 模型效果
    train_preds = bst.predict(dtrain, ntree_limit=num_round)
    train_labels = dtrain.get_label()
    #     print('train : ', get_ks(train_preds, train_labels))
    print('train auc:', roc_auc_score(train_labels, train_preds))
    test_preds = bst.predict(dtest, ntree_limit=num_round)
    test_labels = dtest.get_label()
    #     print('dev : ', get_ks(test_preds, test_labels))
    print('dev auc:', roc_auc_score(test_labels, test_preds))

    return train_preds, train_labels, test_preds, test_labels, bst

features_list = list(set(train_data.columns)-set(["TARGET"]))
len(features_list)

param = {
    'max_depth':6,
    'eval_metric':'auc',
#     'objective':'binary:logitraw',
    'objective':'binary:logistic',
    'base_score': 0.2,
    'subsample':0.6,
    'tree_method':'approx',
    'lambda':700,
#     'booster':'dart',
#     'sample_type':'weighted',
#     'normalize_type':'tree',
#     'rate_drop':0.5,
#     'skip_drop':0.5,
    'seed':0,
#     'colsample_bytree':0.8,
#     'min_child_weight':700,
#     'gamma':10,
    'eta':0.01
}
train_preds,train_labels,dev_preds,dev_labels,bst = model(train_data.loc[:, features_list],train_data['TARGET'],dev_data.loc[:, features_list],dev_data['TARGET'],param,1500)

dmatrix = xgb.DMatrix(test_data.loc[:, features_list].values,label = test_data['TARGET'].values,feature_names=features_list, missing=np.nan)
dpreds = bst.predict(dmatrix)
dlabels = dmatrix.get_label()
test_data['pd1_score'] = dpreds
# print('ks : ', get_ks(dpreds, dlabels))
print(roc_auc_score(dlabels, dpreds))
bst.save_model('xgb_cor.model')
bst.load_model("test1.model")
feature_score = bst.get_fscore()
feature_score = sorted(feature_score.items(), key=lambda x:x[1],reverse = True)
feature_score