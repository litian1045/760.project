import pandas as pd

main = pd.read_csv('Features/main_features.csv')
bureau = pd.read_csv('Features/bureau_balance.csv')
cc = pd.read_csv('Features/cc_features.csv')
ins = pd.read_csv('Features/features_installments_payments.csv')
POS = pd.read_csv('Features/features_POS_CASH_balance.csv')
per = pd.read_csv('Features/per_features.csv')

df_list = [bureau, cc, ins, POS, per]
print(len(main))
for df in df_list:
    main = main.merge(df, on='SK_ID_CURR', how='left')
    print(len(main))
main.to_csv('main.csv')
