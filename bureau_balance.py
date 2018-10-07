# First, Import the Pandas and the numpy for data manipulation
import pandas as pd
import numpy as np

# Second, Import the matplotlib and seaborn for plotting
import matplotlib.pyplot as plt
import seaborn as sns

# Third, Suppress warnings from pandas
import warnings
warnings.filterwarnings('ignore')
plt.style.use('fivethirtyeight')

# Fourth, Import the OS to list the files
# import os
# print(os.listdir('E:\BigData'))

###################################################################
##
##
# Define 2 functions count_categorical and agg_numeric()
##
##
###################################################################
def count_categorical(df, group_var, df_name):
    """Computes counts and normalized counts for each observation
    of `group_var` of each unique category in every categorical variable

    Parameters
    --------
    df : dataframe
        The dataframe to calculate the value counts for.

    group_var : string
        The variable by which to group the dataframe. For each unique
        value of this variable, the final dataframe will have one row

    df_name : string
        Variable added to the front of column names to keep track of columns

    Return
    --------
    categorical : dataframe
        A dataframe with counts and normalized counts of each unique category in every categorical variable
        with one row for every unique value of the `group_var`.
        """

    # Select the categorical columns
    categorical = pd.get_dummies(df.select_dtypes('object'))

    # Make sure to put the identifying id on the column
    categorical[group_var] = df[group_var]

    # Groupby the group var and calculate the sum and mean
    categorical = categorical.groupby(group_var).agg(['sum', 'mean'])

    column_names = []

    # Iterate through the columns in level 0
    for var in categorical.columns.levels[0]:
        # Iterate through the stats in level 1
        for stat in ['count', 'count_norm']:
            # Make a new column name
            column_names.append('%s_%s_%s' % (df_name, var, stat))

    categorical.columns = column_names

    return categorical


def agg_numeric(df, group_var, df_name):
    """Aggregates the numeric values in a dataframe. This can
    be used to create features for each instance of the grouping variable.

    Parameters
    --------
        df (dataframe):
            the dataframe to calculate the statistics on
        group_var (string):
            the variable by which to group df
        df_name (string):
            the variable used to rename the columns

    Return
    --------
        agg (dataframe):
            a dataframe with the statistics aggregated for
            all numeric columns. Each instance of the grouping variable will have
            the statistics (mean, min, max, sum; currently supported) calculated.
            The columns are also renamed to keep track of features created.

    """
    # Remove id variables other than grouping variable
    for col in df:
        if col != group_var and 'SK_ID' in col:
            df = df.drop(columns=col)

    group_ids = df[group_var]
    numeric_df = df.select_dtypes('number')
    numeric_df[group_var] = group_ids

    # Group by the specified variable and calculate the statistics
    agg = numeric_df.groupby(group_var).agg(['count', 'mean', 'max', 'min', 'sum']).reset_index()

    # Need to create new column names
    columns = [group_var]

    # Iterate through the variables names
    for var in agg.columns.levels[0]:
        # Skip the grouping variable
        if var != group_var:
            # Iterate through the stat names
            for stat in agg.columns.levels[1][:-1]:
                # Make a new column name for the variable and stat
                columns.append('%s_%s_%s' % (df_name, var, stat))
    agg.columns = columns
    return agg


###################################################################
##
##
# Merge the 3 table together
##
##
###################################################################

# Step 1: Read the 3 table data
train_csv = pd.read_csv('data/application_train.csv')
bureau_csv = pd.read_csv('data/bureau.csv')
bureau_balance_csv = pd.read_csv('data/bureau_balance.csv')
print('train_csv shape: ', train_csv.shape)
print(train_csv.head())
print('bureau_csv shape: ', bureau_csv.shape)
print(bureau_csv.head())
print('bureau_balance_csv shape: ', bureau_balance_csv.shape)
print(bureau_balance_csv.head())

# Step 2:  Use one-hot encoding method to deal with the categorical columns of bureau_csv
bureau_counts_csv = count_categorical(bureau_csv, group_var = 'SK_ID_CURR', df_name = 'bureau')
print('bureau_counts_csv shape: ', bureau_counts_csv.shape)
print(bureau_counts_csv.head())

# Step 3: group by the client ID of the bureau.csv in order to calculate the aggregation statistics
bureau_agg_csv = agg_numeric(bureau_csv.drop(columns = ['SK_ID_BUREAU']), group_var = 'SK_ID_CURR',
                             df_name = 'bureau')
print('bureau_agg_csv shape: ', bureau_agg_csv.shape)
print(bureau_agg_csv.head())

# Step 4:  Use one-hot encoding method to deal with the categorical columns of bureau_balance_csv
bureau_balance_counts_csv = count_categorical(bureau_balance_csv, group_var = 'SK_ID_BUREAU',
                                              df_name = 'bureau_balance_csv')
print('bureau_balance_counts_csv shape: ', bureau_balance_counts_csv.shape)
print(bureau_balance_counts_csv.head())

# Step 5: group by the client ID of the bureau_balance_csv in order to calculate the aggregation statistics
bureau_balance_agg_csv = agg_numeric(bureau_balance_csv, group_var = 'SK_ID_BUREAU', df_name = 'bureau_balance')
print('bureau_balance_agg_csv shape: ', bureau_balance_agg_csv.shape)
print(bureau_balance_agg_csv.head())

# grouped by the loan
bureau_by_loan_csv = bureau_balance_agg_csv.merge(bureau_balance_counts_csv, right_index = True,
                                                  left_on = 'SK_ID_BUREAU',how = 'outer')
print(len(bureau_by_loan_csv))

# Merge to include the SK_ID_CURR
bureau_by_loan_csv = bureau_csv[['SK_ID_BUREAU', 'SK_ID_CURR']].merge(bureau_by_loan_csv,
                                                                      on = 'SK_ID_BUREAU', how = 'left')
print(len(bureau_by_loan_csv))

# Aggregate the stats for each client
bureau_balance_by_client_csv = agg_numeric(bureau_by_loan_csv.drop(columns = ['SK_ID_BUREAU']),
                                       group_var = 'SK_ID_CURR', df_name = 'client')
print(len(bureau_balance_by_client_csv))
bureau_balance_by_client_csv.to_csv('Features/bureau_balance.csv')

# original_features = list(train_csv.columns)
# print('Original Number of Features: ', len(original_features))

# Merge with the value counts of bureau
# train_csv = train_csv.merge(bureau_counts_csv, on = 'SK_ID_CURR', how = 'left')
# print('train_csv shape: ', train_csv.shape)
# print(train_csv.head())

# Merge with the stats of bureau
# train_csv = train_csv.merge(bureau_agg_csv, on = 'SK_ID_CURR', how = 'left')
# print('train_csv shape: ', train_csv.shape)
# print(train_csv.head())

# Merge with the monthly information grouped by client
# train_csv = train_csv.merge(bureau_balance_by_client_csv, on = 'SK_ID_CURR', how = 'left')
# print('train_csv shape: ', train_csv.shape)
# print(train_csv.head())
