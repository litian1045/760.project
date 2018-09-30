import pandas as pd
import numpy as np

data = pd.read_csv('data/POS_CASH_balance.csv')
data_fix = data[data['NAME_CONTRACT_STATUS'].isin(['Active', 'Completed'])][data['CNT_INSTALMENT_FUTURE'].notna()]
print(len(data_fix))
data_fix['flag_contract_status_completed'] = 0
data_fix['flag_contract_status_completed'][data_fix['NAME_CONTRACT_STATUS'] == 'Completed'] = 1

data_groupby = data_fix.loc[:, ['SK_ID_CURR', 'flag_contract_status_completed', 'SK_DPD', 'SK_DPD_DEF']].groupby('SK_ID_CURR')
f1 = data_groupby.min()
f1['SK_DPD_min'] = f1['SK_DPD']
f1['SK_DPD_DEF_min'] = f1['SK_DPD_DEF']
f2 = data_groupby.max()
f2['SK_DPD_max'] = f2['SK_DPD']
f2['SK_DPD_DEF_max'] = f2['SK_DPD_DEF']
f3 = data_groupby.mean()
f3['SK_DPD_mean'] = f3['SK_DPD']
f3['SK_DPD_DEF_mean'] = f3['SK_DPD_DEF']
f4 = data_groupby.median()
f4['SK_DPD_median'] = f4['SK_DPD']
f4['SK_DPD_DEF_median'] = f4['SK_DPD_DEF']
f5 = data_groupby.sum()
f5['SK_DPD_sum'] = f5['SK_DPD']
f5['SK_DPD_DEF_sum'] = f5['SK_DPD_DEF']
f6 = data_groupby.count()
f6['complete_percentage'] = f5['flag_contract_status_completed'] / f6['SK_DPD']

d1 = f1.merge(f2, on='SK_ID_CURR', how='left').merge(f3, on='SK_ID_CURR', how='left').merge(f4, on='SK_ID_CURR', how='left')
d2 = d1.merge(f5, on='SK_ID_CURR', how='left').merge(f6, on='SK_ID_CURR', how='left')
d3 = d2.loc[:, ['SK_ID_CURR', 'SK_DPD_min', 'SK_DPD_DEF_min', 'SK_DPD_max', 'SK_DPD_DEF_max', 'SK_DPD_mean', 'SK_DPD_DEF_mean',
                'SK_DPD_median', 'SK_DPD_DEF_median', 'SK_DPD_sum', 'SK_DPD_DEF_sum', 'complete_percentage']]
d3.to_csv('Features/features_POS_CASH_balance.csv')
