import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.svm import SVC
from sklearn.svm import LinearSVC
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score
from sklearn.metrics import f1_score
from sklearn.externals import joblib


#train_data = pd.read_csv('./Data/training1000.csv')
# train_data = pd.read_csv('trainning0.csv')
# X_train = train_data.drop('TARGET', axis=1)
# y_train = train_data['TARGET']

#test_data = pd.read_csv('./Data/test1000.csv')
# test_data = pd.read_csv('test0.csv')
# X_test = test_data.drop('TARGET', axis=1)
# y_test = test_data['TARGET']

main_fill = pd.read_csv('main\main_fill.csv')
index = np.loadtxt('test_set_index.csv', dtype=int, delimiter=',')
test_set = main_fill.iloc[index]
train_set = main_fill.iloc[list(set(range(len(main_fill))) - set(index))]

X_train = train_set.drop('TARGET', axis=1)
y_train = train_set['TARGET']
X_test = test_set.drop('TARGET', axis=1)
y_test = test_set['TARGET']

svclassifier = LinearSVC(C=1.0, class_weight='balanced', dual=True, fit_intercept=True,  penalty='l2', random_state=0, tol=1e-05)
svclassifier.fit(X_train, y_train)

#SVC(C=1.0, cache_size=200, class_weight=None, coef0=0.0,
#decision_function_shape='ovr', degree=3, gamma='scale', kernel='rbf',
#    max_iter=-1, probability=False, random_state=None, shrinking=True,
 #   tol=0.001, verbose=False)

# svclassifier = SVC(kernel='rbf', class_weight='balanced', C=1.0, degree=3, tol=0.001, verbose=True, probability=True )
# svclassifier.fit(X_train, y_train)

y_pred_train = svclassifier.predict(X_train)
print('train auc:', roc_auc_score(y_train, y_pred_train))
print('train fscore:', f1_score(y_train, y_pred_train))

y_pred_test = svclassifier.predict(X_test)
print('test auc:', roc_auc_score(y_test, y_pred_test))
print('test fscore:', f1_score(y_test, y_pred_test))

# save model and prediction result
joblib.dump(svclassifier, 'svc_rbf.joblib')
pd.DataFrame(y_pred_train, columns=['predictions']).to_csv('prediction_lcsv_training_0.csv')
pd.DataFrame(y_pred_test, columns=['predictions']).to_csv('prediction_lcsv_training_1.csv')