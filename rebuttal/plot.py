import pandas as pd
import matplotlib.pyplot as plt


def compute_mean_std_r2_scores(scores):
    """
    """
    scores_mean = scores.mean(axis=1)
    scores_std = scores.std(axis=1)
    
    return scores_mean, scores_std


plt.style.use('seaborn')

fig, axes = plt.subplots(ncols=3, sharey=True)

aliases = {'age': 'Age',
           'fluid_intelligence': 'Fluid intelligence',
           'neuroticism': 'Neuroticism'}

train_sizes = {'age': [100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 3700],
               'fluid_intelligence': [100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 3700],
               'neuroticism': [100, 500, 1000, 1500, 2000, 2500, 3000, 3195]}

targets = ['age', 'fluid_intelligence', 'neuroticism']

colors = {'train': 'r',
          'validation': 'g'}

for curve_type in ['train', 'validation']:
    color = colors[curve_type]
    for i, (ax, target) in enumerate(zip(axes, targets)):
        this_sizes = train_sizes[target]
        scores = pd.read_csv('{}_scores_predicted_{}.csv'.format(curve_type,
                                                                 target))
        scores = scores.drop('Unnamed: 0', axis=1)
        scores_mean, scores_std = compute_mean_std_r2_scores(scores)
        ax.plot(train_sizes[target], scores_mean, 'o-',
                color=color, label = curve_type)
        ax.fill_between(this_sizes, scores_mean - scores_std,
                        scores_mean + scores_std, alpha=0.2,
                        color=color)
        if i == 0:
            ax.set_ylabel('$R^{2}$', fontsize = 18)
        elif i == 1:
            ax.set_xlabel('Training sizes', fontsize = 18)
        ax.set_title(aliases[target], fontsize=18)
plt.legend()
plt.tight_layout()
plt.savefig('learning_curves.png', bbox_inches='tight')
plt.savefig('learning_curves.pdf', bbox_inches='tight')
