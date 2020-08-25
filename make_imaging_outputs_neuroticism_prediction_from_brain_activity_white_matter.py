import os
import collections
from os.path import join
import numpy as np
import pandas as pd

from itertools import chain
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import ShuffleSplit, GridSearchCV
from sklearn.metrics import (mean_absolute_error, mean_squared_error,
                             explained_variance_score, r2_score)

from ukbb_variables import (brain_dmri_fa, brain_dmri_icvf,
                            brain_dmri_isovf, brain_dmri_l1,
                            brain_dmri_l2, brain_dmri_l3,
                            brain_dmri_md, brain_dmri_mo,
                            brain_dmri_od, neuroticism,
                            fluid_intelligence)
path_to_csv = '/scratch/kdadi/ukb9543.csv'
path_to_matrices = '/scratch/kdadi/rfMRI_tangent_matrix_dim100'
path_to_merge_brain = '/scratch/kdadi/ukb_add1_merge_brain.csv'
X_iterate = zip([brain_dmri_fa, brain_dmri_icvf, brain_dmri_isovf, brain_dmri_l1,
                 brain_dmri_l2, brain_dmri_l3, brain_dmri_md, brain_dmri_mo,
                 brain_dmri_od, fluid_intelligence, neuroticism],
                 ['fa', 'icvf', 'isovf', 'l1', 'l2', 'l3', 'md', 'mo', 'od',
                  'Fluid \n intelligence', 'Neuroticism'])

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

    X_split = X_split.drop(columns=['eid', '20016-2.0', '20127-0.0'], axis=1)
    connectomes = pd.DataFrame(connectomes, index=X_split.index)
    df = pd.concat([X_split, connectomes], axis=1)
    return df, y_split

df, y_train = load_combine_data(X_train, merged_data, dmri)

df_test, y_test = load_combine_data(X_test, merged_data, dmri)

# Model
estimator = RandomForestRegressor(n_estimators=250, criterion='mse',
                                  n_jobs=-1, verbose=1, random_state=0)

cv = ShuffleSplit(n_splits=100, test_size=0.1, random_state=0)

param_grid = {'max_depth': [5, 10, 20, 40, None],
              'max_features': [1, 5, 'log2', 'sqrt', 'auto', None]}
grid_search = GridSearchCV(estimator, param_grid=param_grid,
                           cv=5, verbose=2, n_jobs=-1)
metrics = []


def predict_collect_save(data_pred, data_collect, y_true, test_index,
                         split, save_type):
    scores = {}
    pred_ = grid_search.predict(data_pred)
    y_true_ = y_true.iloc[test_index]
    predictions = pd.DataFrame(pred_, columns=['predicted'],
                               index=y_true_.index)
    predictions['true'] = y_true_
    predictions['test_indices'] = pd.DataFrame(test_index,
                                               columns=['test indices'],
                                               index=y_true_.index)
    predictions['fold'] = pd.Series([split] * len(predictions),
                                    index=predictions.index)
    data_collect.append(predictions)
    scores['mae'] = mean_absolute_error(y_true_, pred_)
    scores['mse'] = mean_squared_error(y_true_, pred_)
    scores['ev'] = explained_variance_score(y_true_, pred_)
    scores['r2_score'] = r2_score(y_true_, pred_)
    scores['fold'] = split
    scores['Estimator'] = 'RandomForest'
    scores['Permuted'] = 'no'
    scores['model_testing'] = save_type
    scores['target'] = 'Neuroticism'
    scores['modality'] = 'dMRI, fMRI'
    metrics.append(scores)
    return

data = []
data_generalization = []
for split, (train_index, test_index) in enumerate(cv.split(df, y_train)):
    scores = {}
    grid_search.fit(df.iloc[train_index], y_train.iloc[train_index])
    predict_collect_save(data_pred=df.iloc[test_index], data_collect=data,
                         y_true=y_train, test_index=test_index, split=split,
                         save_type='validation')
    predict_collect_save(data_pred=df_test, data_collect=data_generalization,
                         y_true=y_test,
                         test_index=np.arange(df_test.shape[0], dtype=np.int),
                         split=split, save_type='generalization')

# save outputs
savedir = join('outputs', 'imaging_neuroticism_prediction_from_brain_activity_white_matter')
if not os.path.exists(savedir):
    os.makedirs(savedir)
scores = pd.DataFrame(metrics)
scores.to_csv(join(savedir, 'scores.csv'))
