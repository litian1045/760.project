import pandas as pd
import matplotlib.pyplot as plt
import warnings
warnings.filterwarnings('ignore')
plt.style.use('fivethirtyeight')


def count_categorical(df, group_var, df_name):
    categorical = pd.get_dummies(df.select_dtypes('object'))
    categorical[group_var] = df[group_var]
    categorical = categorical.groupby(group_var).agg(['sum', 'mean'])
    column_names = []
    for var in categorical.columns.levels[0]:
        for stat in ['count', 'count_norm']:
            column_names.append('%s_%s_%s' % (df_name, var, stat))
    categorical.columns = column_names
    return categorical


def agg_numeric(df, group_var, df_name):
    for col in df:
        if col != group_var and 'SK_ID' in col:
            df = df.drop(columns=col)
    group_ids = df[group_var]
    numeric_df = df.select_dtypes('number')
    numeric_df[group_var] = group_ids
    agg = numeric_df.groupby(group_var).agg(['count', 'mean', 'max', 'min', 'sum']).reset_index()
    columns = [group_var]
    for var in agg.columns.levels[0]:
        if var != group_var:
            for stat in agg.columns.levels[1][:-1]:
                columns.append('%s_%s_%s' % (df_name, var, stat))
    agg.columns = columns
    return agg


cc_csv = pd.read_csv('data/credit_card_balance.csv').drop(columns=['SK_ID_PREV', 'MONTHS_BALANCE',
                                                                   'NAME_CONTRACT_STATUS', 'SK_DPD', 'SK_DPD_DEF'])
# cc_counts_csv = count_categorical(cc_csv, group_var='SK_ID_CURR', df_name='cc')
cc_agg_csv = agg_numeric(cc_csv, group_var='SK_ID_CURR', df_name='cc')
cc_features_csv = cc_agg_csv
print(len(cc_features_csv))
cc_features_csv.to_csv('Features/cc_features.csv')
