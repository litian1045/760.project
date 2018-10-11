from fancyimpute import BiScaler, KNN, NuclearNormMinimization, SoftImpute
import pandas as pd
import numpy

n = 10000
data_set = pd.read_csv('main.csv')
data_r = data_set.sample(frac=1)
dl = n
data_array = KNN(k=3).fit_transform(data_r[0:n])
while dl < (len(data_set) - n):
    data_array = numpy.concatenate((data_array,  KNN(k=3).fit_transform(data_r[dl:(dl + n)])), axis=0)
    print(dl)
    dl += n
data_array = numpy.concatenate((data_array,  KNN(k=3).fit_transform(data_r[dl:len(data_set)])), axis=0)

numpy.savetxt('new.csv', data_array, delimiter=',', fmt='%10.5f')
