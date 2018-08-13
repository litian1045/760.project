import pandas


apl_tr = pandas.read_csv('Data/application_train.csv')
cols = apl_tr.columns
apl_tr_dict = list()
for col in cols:
    apl_tr_dict += [{
        'col_name': col,
        'type': apl_tr[col].dtypes,
        'length': len(apl_tr[col]),
        'NAs': sum(pandas.isna(apl_tr[col]))
    }]
print(apl_tr_dict)

print(1)

