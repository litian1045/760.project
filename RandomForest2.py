import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import roc_auc_score
from sklearn.metrics import f1_score
from sklearn.externals import joblib

plt.style.use('fivethirtyeight')


def plot_feature_importances(df):
    df = df.sort_values('importance', ascending=False).reset_index()
    df['importance_normalized'] = df['importance'] / df['importance'].sum()
    plt.figure(figsize=(10, 6))
    ax = plt.subplot()
    ax.barh(list(reversed(list(df.index[:15]))),
            df['importance_normalized'].head(15),
            align='center', edgecolor='k')
    ax.set_yticks(list(reversed(list(df.index[:15]))))
    ax.set_yticklabels(df['feature'].head(15))
    plt.xlabel('Normalized Importance')
    plt.title('Feature Importances')
    plt.show()
    return df


main_fill = pd.read_csv('main\main_rf.csv')
index = np.loadtxt('test_set_index.csv', dtype=int, delimiter=',')
test_set = main_fill.iloc[index]
train_set = main_fill.iloc[list(set(range(len(main_fill))) - set(index))]

train = train_set.drop(columns=['TARGET'])
features = list(train.columns)
train_labels = train_set['TARGET']

# Make the random forest classifier
random_forest = RandomForestClassifier(n_estimators=5000, random_state=50, verbose=1, n_jobs=-1, max_depth=12,
                                       min_samples_split=200, min_samples_leaf=100, class_weight="balanced",
                                       max_features="auto", oob_score=False, min_weight_fraction_leaf=0.01)
# random_forest = RandomForestClassifier(n_estimators=200,
#                                        min_samples_split=10,
#                                        min_samples_leaf=5,
#                                        n_jobs=-1,
#                                        random_state=42,
#                                        class_weight="balanced",
#                                        verbose=1)

# Train on the training data
random_forest.fit(train, train_labels)

# Extract feature importance
feature_importance_values = random_forest.feature_importances_
feature_importance = pd.DataFrame({'feature': features, 'importance': feature_importance_values})
dpreds_tr = random_forest.predict_proba(train)[:, 1]
dpreds_te = random_forest.predict_proba(test_set.drop(columns=['TARGET']))[:, 1]
print(roc_auc_score(train_labels, dpreds_tr))
print(roc_auc_score(test_set['TARGET'], dpreds_te))

for i in range(80):
    t = (i+1)/100
    y_bin = [1. if y_cont > t else 0. for y_cont in dpreds_te]
    print(t, f1_score(test_set['TARGET'], y_bin))

print(feature_importance)
joblib.dump(random_forest, "rf_rf.sav")
# Show the feature importances for the default features
# feature_importances_sorted = plot_feature_importances(feature_importances)
