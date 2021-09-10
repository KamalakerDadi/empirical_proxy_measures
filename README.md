# empirical_proxy_measures

Populationmodeling withmachine learning can enhancemeasures of mental health.

Authors: Kamalaker Dadi, Gaël Varoquaux, Josselin Houenou, Danilo Bzdok,
 Bertrand Thirion, Denis Engemann, 2020, Submitted to journal.

## code and data sharing

The data shared with this repository is aggregated and does *not* reveal individual-specific inputs nor participant ids.

To obtain model predictions needed to compute all results, the code can be run on the input data accessible from the UK Biobank.

For all main figures and most supplementary figures, we shared aggregate results that will allow readers to replot the figures and inspect without having to recomputing all results.

## structure of individual-specific results (model predictions)

Some of the scripts refer to input files with a `"post_predictive*.csv"` naming pattenrn. Thes files contain the model predictions (both left-out fold and held-out generalization dataset) from each fold from the cross-validation procedure performed on the data used for model construction.

The table below gives a summary of the structure and data types of the results.

|Unnamed: 0 |20016-2.0 |20127-0.0 |21022-0.0 |eid     |evaluation |fold    |predicted |target    |test_indices |true    |variable  |Data type |
|:----------|:---------|:---------|:---------|:-------|:----------|:-------|:---------|:---------|:------------|:-------|:---------|:---------|
|integer    |logical   |logical   |integer   |integer |character  |integer |double    |character |integer      |integer |character |character |
