"""Plot learning curves on the save results

   Results are R-squared scores meaned/std across 100 CV splits
"""
import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv('learning_curves.csv')
targets = df['target'].unique()
learning_types = df['learning_type'].unique()

# plotting
plt.style.use('seaborn')

colors = {'train': 'r',
          'validation': 'g'}

fig, axes = plt.subplots(figsize=(10, 4), ncols=4, sharey=True)

for learning_type in learning_types:
    this_type_data = df[df['learning_type'] == learning_type]
    for i, (ax, target) in enumerate(zip(axes, targets)):
        this_target = this_type_data[this_type_data['target'] == target]
        ax.plot(this_target['train_sizes'], this_target['mean'],
                'o-', color=colors[learning_type],
                label = learning_type)
        ax.fill_between(this_target['train_sizes'],
                this_target['mean'] - this_target['std'],
                this_target['mean'] + this_target['std'],
                alpha=0.2, color=colors[learning_type])
        if i == 0:
            ax.set_ylabel('$R^{2}$', fontsize = 18)
        elif i == 2:
            ax.set_xlabel('Training sizes', fontsize = 18)
        ax.set_title(target, fontsize=18)
plt.legend()
plt.tight_layout()
plt.show()

