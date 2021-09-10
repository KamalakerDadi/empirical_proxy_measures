import os
from os.path import join
import collections
import numpy as np
import pandas as pd

from itertools import chain
from sklearn.model_selection import train_test_split
from sklearn.impute import SimpleImputer, MissingIndicator
from sklearn.pipeline import make_union, Pipeline
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import ShuffleSplit, GridSearchCV
from sklearn.model_selection import learning_curve
from sklearn.metrics import (mean_absolute_error, mean_squared_error,
                             explained_variance_score, r2_score)

from ukbb_variables import (brain_dmri_fa, brain_dmri_icvf,
                            brain_dmri_isovf, brain_dmri_l1,
                            brain_dmri_l2, brain_dmri_l3,
                            brain_dmri_md, brain_dmri_mo,
                            brain_dmri_od, brain_smri_plus, neuroticism,
                            earlylife, fluid_intelligence,
                            education, lifestyle, mental_health,
                            primary_demographics)
path_to_csv = '/storage/store/work/kdadi/rs_study/experiments/UKBB/ukb9543.csv'
path_to_matrices = '/storage/store/derivatives/UKBB/rfMRI_tangent_matrix_dim100/'
path_to_merge_brain = '/storage/store/work/kdadi/rs_study/experiments/UKBB/para/roadmap/ukb_add1_merge_brain.csv'
X_iterate = zip([brain_dmri_fa, brain_dmri_icvf, brain_dmri_isovf, brain_dmri_l1,
                 brain_dmri_l2, brain_dmri_l3, brain_dmri_md, brain_dmri_mo,
                 brain_dmri_od, brain_smri_plus, fluid_intelligence,
                 neuroticism, earlylife, education, lifestyle, mental_health,
                 primary_demographics],
                 ['fa', 'icvf', 'isovf', 'l1', 'l2', 'l3', 'md', 'mo', 'od',
                  'smri', 'Fluid \n intelligence', 'Neuroticism', 'Earlylife',
                  'Education', 'Lifestyle', 'MH', 'Age'])

columns = []
for i in X_iterate:
    columns.extend(i[0].keys())
columns.extend(['eid'])

ukbb = pd.read_csv(path_to_csv, usecols=['20016-2.0', 'eid', '20127-0.0'])
y = ukbb[['eid', '20016-2.0']].dropna()
new_ukbb = pd.DataFrame(ukbb, index=y.index)

new_ukbb = new_ukbb.drop(columns=['20016-2.0'], errors='ignore')

# Random splitting of data to train our model
X_train, X_test, y_train, y_test = train_test_split(
    new_ukbb, y, test_size=0.5, random_state=0)

X_train = X_train.dropna()
X_test = X_test.dropna()

merged_data = pd.read_csv(path_to_merge_brain, usecols=columns)

dmriDict = collections.OrderedDict(chain(brain_dmri_fa.items(),
                                         brain_dmri_icvf.items(),
                                         brain_dmri_isovf.items(),
                                         brain_dmri_l1.items(),
                                         brain_dmri_l2.items(),
                                         brain_dmri_l3.items(),
                                         brain_dmri_md.items(),
                                         brain_dmri_mo.items(),
                                         brain_dmri_od.items()))
dmriDict.update({'eid': 'eid'})
dmri = pd.DataFrame(merged_data, columns=dmriDict.keys())
dmri = dmri.dropna()


def load_combine_data(X_split, merged_data, dmri):
    data_frame = []
    connectomes = []
    eids = []
    for e_id in X_split.eid:
        this_eid_data = merged_data[merged_data['eid'] == e_id]
        this_path = os.path.join(
            path_to_matrices, str(e_id) + '_20227_2_0.txt')
        this_dmri_data = dmri[dmri['eid'] == e_id]
        if not e_id == 3551399:
            if os.path.exists(this_path) and not len(this_dmri_data) == 0:
                eids.append(e_id)
                data_frame.append(this_eid_data)
                connectomes.append(np.loadtxt(this_path))

    X_split = pd.concat(data_frame)
    y_split = pd.DataFrame(X_split, columns=['20127-0.0'])

    connectomes = pd.DataFrame(connectomes, index=X_split.index)
    df = pd.concat([X_split, connectomes], axis=1)
    return df, y_split

df, y_train = load_combine_data(X_train, merged_data, dmri)

df_test, y_test = load_combine_data(X_test, merged_data, dmri)

X_iterate = zip([earlylife, education, lifestyle, mental_health, primary_demographics],
                 ['Earlylife', 'Edu', 'ls', 'mh', 'age'])
new_columns = []
for i in X_iterate:
    new_columns.extend(i[0].keys())
new_columns.extend(['eid', '20127-0.0'])

df = pd.DataFrame(df, columns=new_columns)
df_test = pd.DataFrame(df_test, columns=new_columns)

X_train_post_hoc = df
X_test_post_hoc = df_test

df = df.drop(columns=['eid', '20127-0.0'], axis=1)
df_test = df_test.drop(columns=['eid', '20127-0.0'], axis=1)

# Learning curves: train sizes
train_sizes = [100, 500, 1000, 1500, 2000, 2500, 3000, 3195]

# Model
estimator = RandomForestRegressor(n_estimators=250, criterion='mse',
                                  n_jobs=10, verbose=1, random_state=0)
pipeline = Pipeline([
    ('imputation', make_union(SimpleImputer(strategy="median"),
                              MissingIndicator(error_on_new=False))),
    ('estimator', estimator)])

cv = ShuffleSplit(n_splits=100, test_size=0.1, random_state=0)

param_grid = {'estimator__max_depth': [5, 10, 20, 40, None],
              'estimator__max_features': [1, 5, 'log2', 'sqrt', 'auto', None]}
grid_search = GridSearchCV(pipeline, param_grid=param_grid,
                           cv=5, verbose=2, n_jobs=10)
metrics = []

train_sizes, train_scores, validation_scores = learning_curve(
        estimator=grid_search, X=df, y=y_train,
        train_sizes=train_sizes, cv=cv, scoring='r2')

train_scores = pd.DataFrame(train_scores)
train_scores.to_csv('inputs/train_scores_predict_neuroticism.csv')

validation_scores = pd.DataFrame(validation_scores)
validation_scores.to_csv('inputs/validation_scores_predict_neuroticism.csv')
