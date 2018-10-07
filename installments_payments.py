import pandas as pd


data = pd.read_csv('data/installments_payments.csv')
print(len(data))
data_fix = data[data['DAYS_INSTALMENT'].notna()]
data_fix = data_fix[data_fix['DAYS_ENTRY_PAYMENT'].notna()]
data_fix = data_fix[data_fix['AMT_INSTALMENT'].notna()]
data_fix = data_fix[data_fix['AMT_PAYMENT'].notna()]
print(len(data_fix))

data_fix['flag_pay_on_time'] = data['DAYS_INSTALMENT'] >= data['DAYS_ENTRY_PAYMENT']
data_fix['flag_pay_early'] = data['DAYS_INSTALMENT'] > data['DAYS_ENTRY_PAYMENT']
data_fix['flag_pay_late'] = data['DAYS_INSTALMENT'] < data['DAYS_ENTRY_PAYMENT']

data_fix['days_pay_days'] = data['DAYS_INSTALMENT'] - data['DAYS_ENTRY_PAYMENT']

data_fix['flag_pay_same'] = data['AMT_INSTALMENT'] == data['AMT_PAYMENT']
data_fix['flag_pay_more'] = data['AMT_INSTALMENT'] < data['AMT_PAYMENT']
data_fix['flag_pay_less'] = data['AMT_INSTALMENT'] > data['AMT_PAYMENT']

print(len(data_fix))

features_list = ['SK_ID_CURR', 'DAYS_INSTALMENT', 'DAYS_ENTRY_PAYMENT', 'AMT_INSTALMENT', 'AMT_PAYMENT',
                 'flag_pay_on_time', 'flag_pay_early', 'flag_pay_late', 'days_pay_days', 'flag_pay_same',
                 'flag_pay_more', 'flag_pay_less']
data_group_by = data_fix.loc[:, features_list].groupby('SK_ID_CURR')

feature_min = data_group_by.min()
feature_min['days_pay_days_late'] = feature_min['days_pay_days']
feature_max = data_group_by.max()
feature_max['days_pay_days_early'] = feature_max['days_pay_days']
feature_mean = data_group_by.mean()
feature_mean['days_pay_days_mean'] = feature_mean['days_pay_days']
feature_mean['flag_pay_on_time_mean'] = feature_mean['flag_pay_on_time']
feature_mean['flag_pay_early_mean'] = feature_mean['flag_pay_early']
feature_mean['flag_pay_late_mean'] = feature_mean['flag_pay_late']
feature_mean['flag_pay_same_mean'] = feature_mean['flag_pay_same']
feature_mean['flag_pay_more_mean'] = feature_mean['flag_pay_more']
feature_mean['flag_pay_less_mean'] = feature_mean['flag_pay_less']
feature_sum = data_group_by.sum()
feature_sum['flag_pay_on_time_sum'] = feature_sum['flag_pay_on_time']
feature_sum['flag_pay_early_sum'] = feature_sum['flag_pay_early']
feature_sum['flag_pay_late_sum'] = feature_sum['flag_pay_late']
feature_sum['flag_pay_same_sum'] = feature_sum['flag_pay_same']
feature_sum['flag_pay_more_sum'] = feature_sum['flag_pay_more']
feature_sum['flag_pay_less_sum'] = feature_sum['flag_pay_less']
feature_sum['AMT_INSTALMENT_sum'] = feature_sum['AMT_INSTALMENT']
feature_sum['AMT_PAYMENT_sum'] = feature_sum['AMT_PAYMENT']
feature_count = data_group_by.count()
feature_count['count_payment'] = feature_count['AMT_INSTALMENT']

features = feature_min
group_by_list = [feature_max, feature_mean, feature_sum, feature_count]
for f in group_by_list:
    features = features.merge(f, on='SK_ID_CURR', how='left')

features = features.loc[:, ['flag_pay_on_time_sum', 'flag_pay_early_sum', 'flag_pay_late_sum',
                            'flag_pay_same_sum', 'flag_pay_more_sum', 'flag_pay_less_sum',
                            'AMT_INSTALMENT_sum', 'AMT_PAYMENT_sum', 'days_pay_days_mean',
                            'flag_pay_on_time_mean', 'flag_pay_early_mean', 'flag_pay_late_mean',
                            'flag_pay_same_mean', 'flag_pay_more_mean', 'flag_pay_less_mean',
                            'days_pay_days_early', 'days_pay_days_late', 'count_payment']]
features['AMT_payment_instalment_p'] = features['AMT_PAYMENT_sum'] / features['AMT_INSTALMENT_sum']

print(len(feature_min))
features.to_csv('Features/features_installments_payments.csv')
