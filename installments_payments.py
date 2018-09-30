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
feature_max = data_group_by.max()
feature_count = data_group_by.count()
feature_mean = data_group_by.mean()
feature_sum = data_group_by.sum()
feature_sum['sum_pay_on_time'] = feature_sum['flag_pay_on_time']
feature_median = data_group_by.median()
print(len(feature_min))
feature_sum.to_csv('test.csv')
