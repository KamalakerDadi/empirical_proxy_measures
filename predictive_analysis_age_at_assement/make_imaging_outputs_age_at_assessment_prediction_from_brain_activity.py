import os
from os.path import join
import numpy as np
import pandas as pd

from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import ShuffleSplit, GridSearchCV
from sklearn.metrics import (mean_absolute_error, mean_squared_error,
                             explained_variance_score, r2_score)
from sklearn.utils import check_random_state

# Data paths
path_to_csv = '/storage/store/work/kdadi/rs_study/experiments/UKBB/ukb9543.csv'
path_to_matrices = '/storage/store/derivatives/UKBB/rfMRI_tangent_matrix_dim100/'

ukbb = pd.read_csv(path_to_csv, usecols=['20016-2.0', 'eid', '21003-2.0'])
y = ukbb[['eid', '20016-2.0']].dropna()
new_ukbb = pd.DataFrame(ukbb, index=y.index)

new_ukbb = new_ukbb.drop(columns=['20016-2.0'], errors='ignore')

# Random splitting of data to train our model
X_train, X_test, y_train, y_test = train_test_split(
    new_ukbb, y, test_size=0.5, random_state=0)

X_train = X_train.dropna(subset=['21003-2.0'])
X_test = X_test.dropna(subset=['21003-2.0'])


def load_combine_data(X_split):
    data_frame = []
    connectomes = []
    eids = []
    for e_id in X_split.eid:
        this_eid_data = X_split[X_split['eid'] == e_id]
        this_path = os.path.join(
            path_to_matrices, str(e_id) + '_20227_2_0.txt')
        if os.path.exists(this_path):
            eids.append(e_id)
            data_frame.append(this_eid_data)
            connectomes.append(np.loadtxt(this_path))

    X_split = pd.concat(data_frame)
    y_split = pd.DataFrame(X_split, columns=['21003-2.0'])

    X_split = X_split.drop(columns=['eid', '21003-2.0'], axis=1)
    connectomes = pd.DataFrame(connectomes, index=X_split.index)

    return connectomes, y_split


df, y_train = load_combine_data(X_train)

df_test, y_test = load_combine_data(X_test)

# Model
estimator = RandomForestRegressor(n_estimators=250, criterion='mse',
                                  n_jobs=15, verbose=1, random_state=0)

rng = check_random_state(42)

cv = ShuffleSplit(n_splits=100, test_size=0.1, random_state=0)

param_grid = {'max_depth': [5, 10, 20, 40, None],
              'max_features': [1, 5, 'log2', 'sqrt', 'auto', None]}
grid_search = GridSearchCV(estimator, param_grid=param_grid,
                           cv=5, verbose=2, n_jobs=15)
data = []
data_generalization = []
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
    scores['modality'] = 'functional connectivity (fMRI)'
    scores['target'] = 'Age'
    metrics.append(scores)
    return


for split, (train_index, test_index) in enumerate(cv.split(df, y_train)):
    scores = {}
    grid_search.fit(df.iloc[train_index], y_train.iloc[train_index])

    predict_collect_save(data_pred=df.iloc[test_index], data_collect=data,
                         y_true=y_train, test_index=test_index,
                         split=split, save_type='validation')

    predict_collect_save(data_pred=df_test, data_collect=data_generalization,
                         y_true=y_test,
                         test_index=np.arange(df_test.shape[0], dtype=np.int),
                         split=split, save_type='generalization')

# save outputs
savedir = join('outputs', 'imaging_age_at_assessment_prediction_from_brain_activity')
if not os.path.exists(savedir):
    os.makedirs(savedir)
scores = pd.DataFrame(metrics)
scores.to_csv(join(savedir, 'scores.csv'))
