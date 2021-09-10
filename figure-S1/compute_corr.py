import numpy as np

data = np.load('./inputs/sociodemographics_correlation_input.npy')                                      
labels = np.load('./inputs/labels.npy', allow_pickle=True)                       
corr = np.corrcoef(data.T)
np.save('corr.npy', corr)
