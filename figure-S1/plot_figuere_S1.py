import pandas as pd                                                     
import numpy as np                                                      
import matplotlib as mpl
import matplotlib.pyplot as plt
from nilearn import plotting

from ukbb_variables import observations                                 

mpl.rc(
    'font',
    **{'family': 'sans-serif', 'sans-serif': 'Arial'})

corr = np.load("./corr.npy")
labels = np.load('./inputs/labels.npy', allow_pickle=True)                       

new_labels = []                                                         
for l in labels:
    new_labels.append(observations[l])


fig, ax = plt.subplots(1, 1, figsize=(24, 14))

display = plotting.plot_matrix(corr, reorder=True,
                               labels=new_labels,
                               cmap='RdBu_r',
                               auto_fit=False,
                               axes=ax)

display.axes.set_xticklabels([])
display.axes.set_xticks([])

plt.yticks(rotation=0)
plt.yticks(fontsize=10)

for tt in plt.findobj(fig, mpl.text.Text):
    text = tt.get_text()
    if not text:
        continue 
    elif text.replace('-', '').replace('.', '').isnumeric():
        continue
    tt.set_fontsize(8) 

plt.findobj(fig, mpl.text.Text)

for spine in ax.spines.values():
    spine.set_visible(False)

fig.savefig('figure_S1.pdf', bbox_inches="tight", dpi=300)
fig.savefig('figure_S1.png', bbox_inches="tight", dpi=300)
