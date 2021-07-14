import pandas as pd
import matplotlib.pyplot as plt


def compute_mean_std_r2_scores(scores):
    """
    """
    scores_mean = pd.Series(scores.mean(axis=1))
    scores_std = pd.Series(scores.std(axis=1))

    return scores_mean, scores_std


aliases = {'age': 'Age',
           'age_at_assessment': 'Age at assessment',
           'fluid_intelligence': 'Fluid intelligence',
           'neuroticism': 'Neuroticism'}

train_sizes = {'age': [100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 3700],
               'age_at_assessment': [100, 500, 1000, 1500, 2000, 2500, 3000, 3600],
               'fluid_intelligence': [100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 3700],
               'neuroticism': [100, 500, 1000, 1500, 2000, 2500, 3000, 3195]}

targets = ['age', 'fluid_intelligence', 'neuroticism']

colors = {'train': 'r',
          'validation': 'g'}

df = []

for curve_type in ['train', 'validation']:
    color = colors[curve_type]
    for i, target in enumerate(targets):
        data = {}
        this_sizes = train_sizes[target]
        scores = pd.read_csv('inputs/{}_scores_predict_{}.csv'.format(curve_type,
                                                                        target))
        scores = scores.drop('Unnamed: 0', axis=1)
        scores_mean, scores_std = compute_mean_std_r2_scores(scores)
        data['mean'] = scores_mean
        data['std'] = scores_std
        data['target'] = pd.Series([aliases[target]] * len(scores_mean),
                                   index=scores_mean.index)
        data['learning_type'] = pd.Series([curve_type] * len(scores_mean),
                                          index=scores_mean.index)
        data['train_sizes'] = pd.Series(train_sizes[target])
        df.append(pd.concat(data, axis=1))

df = pd.concat(df)
df.to_csv('learning_curves.csv', index=False)
