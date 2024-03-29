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
from sklearn.metrics import (mean_absolute_error, mean_squared_error,
                             explained_variance_score, r2_score)

from ukbb_variables import (brain_dmri_fa, brain_dmri_icvf,
                            brain_dmri_isovf, brain_dmri_l1,
                            brain_dmri_l2, brain_dmri_l3,
                            brain_dmri_md, brain_dmri_mo,
                            brain_dmri_od, brain_smri_plus,
                            lifestyle, fluid_intelligence)
path_to_csv = '/storage/store/work/kdadi/rs_study/experiments/UKBB/ukb9543.csv'
path_to_matrices = '/storage/store/derivatives/UKBB/rfMRI_tangent_matrix_dim100/'
path_to_merge_brain = '/storage/store/work/kdadi/rs_study/experiments/UKBB/para/roadmap/ukb_add1_merge_brain.csv'
X_iterate = zip([brain_dmri_fa, brain_dmri_icvf, brain_dmri_isovf, brain_dmri_l1,
                 brain_dmri_l2, brain_dmri_l3, brain_dmri_md, brain_dmri_mo,
                 brain_dmri_od, brain_smri_plus, lifestyle, fluid_intelligence],
                 ['fa', 'icvf', 'isovf', 'l1', 'l2', 'l3', 'md', 'mo', 'od',
                  'smri', 'Lifestyle', 'Fluid \n intelligence'])

columns = []
for i in X_iterate:
    columns.extend(i[0].keys())
columns.extend(['eid', '21003-2.0'])

ukbb = pd.read_csv(path_to_csv, usecols=['20016-2.0', 'eid', '21003-2.0'])
y = ukbb[['eid', '20016-2.0']].dropna()
new_ukbb = pd.DataFrame(ukbb, index=y.index)

new_ukbb = new_ukbb.drop(columns=['20016-2.0'], errors='ignore')

# Random splitting of data to train our model
X_train, X_test, y_train, y_test = train_test_split(
    new_ukbb, y, test_size=0.5, random_state=0)

X_train = X_train.dropna(subset=['21003-2.0'])
X_test = X_test.dropna(subset=['21003-2.0'])


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
    y_split = pd.DataFrame(X_split, columns=['21003-2.0'])

    connectomes = pd.DataFrame(connectomes, index=X_split.index)
    df = pd.concat([X_split, connectomes], axis=1)
    return df, y_split

df, y_train = load_combine_data(X_train, merged_data, dmri)

df_test, y_test = load_combine_data(X_test, merged_data, dmri)


X_iterate = zip([lifestyle], ['ls'])
new_columns = []
for i in X_iterate:
    new_columns.extend(i[0].keys())
new_columns.extend(['eid', '21003-2.0'])

df = pd.DataFrame(df, columns=new_columns)
df_test = pd.DataFrame(df_test, columns=new_columns)

X_train_post_hoc = df
X_test_post_hoc = df_test

df = df.drop(columns=['eid', '21003-2.0'], axis=1)
df_test = df_test.drop(columns=['eid', '21003-2.0'], axis=1)

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
                           cv=5, verbose=2)
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
    scores['imputation_strategy'] = 'median'
    scores['Estimator'] = 'RandomForest'
    scores['Permuted'] = 'no'
    scores['target'] = 'Age'
    scores['variable'] = 'Life style (LS)'
    metrics.append(scores)
    return


data = []
data_generalization = []
for split, (train_index, test_index) in enumerate(cv.split(df, y_train)):
    scores = {}
    grid_search.fit(df.iloc[train_index], y_train.iloc[train_index])

    predict_collect_save(
        data_pred=df.iloc[test_index], data_collect=data,
        y_true=y_train, test_index=test_index, split=split,
        save_type='validation')

    predict_collect_save(
        data_pred=df_test, data_collect=data_generalization,
        y_true=y_test,
        test_index=np.arange(df_test.shape[0], dtype=np.int),
        split=split, save_type='generalization')


# save outputs
savedir = join('outputs', 'non_imaging_age_at_assessment_prediction_from_lifestyle')
if not os.path.exists(savedir):
    os.makedirs(savedir)
scores = pd.DataFrame(metrics)
scores.to_csv(join(savedir, 'scores.csv'))


def collect_ids(model_testing, data, outputs):
    # locate ids
    subjects = data.iloc[outputs['test_indices']][['21003-2.0', 'eid']]
    to_save = pd.concat([outputs, subjects], axis=1)
    to_save['target'] = pd.Series(['Age'] * len(to_save),
                                  index=to_save.index)
    to_save['variable'] = pd.Series(['Lifestyle'] * len(to_save),
                                    index=to_save.index)
    if model_testing == 'validation':
        to_save['evaluation'] = pd.Series(['Validation'] * len(to_save),
                                          index=to_save.index)
    else:
        to_save['evaluation'] = pd.Series(['Generalization'] * len(to_save),
                                          index=to_save.index)
    return to_save


# Outputs for post-hoc associations
validation = pd.concat(data)
post_analysis_v = collect_ids('validation', X_train_post_hoc,
                              validation)
post_analysis_v.to_csv(join(savedir,
                            'post_hoc_data_to_plot_validation.csv'))

generalization = pd.concat(data_generalization)
post_analysis_g = collect_ids('generalization', X_test_post_hoc,
                              generalization)
post_analysis_g.to_csv(join(savedir,
                            'post_hoc_data_to_plot_generalization.csv'))
