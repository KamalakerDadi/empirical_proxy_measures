# empirical_proxy_measures

Populationmodeling withmachine learning can enhancemeasures of mental health.

Authors: Kamalaker Dadi, Gaël Varoquaux, Josselin Houenou, Danilo Bzdok,
 Bertrand Thirion, Denis Engemann, 2020, Submitted to journal.

## code and data sharing

The data shared with this repository is aggregated and does *not* reveal individual-specific inputs nor participant ids.

To obtain model predictions needed to compute all results, the code can be run on the input data accessible from the UK Biobank.

For all main figures and most supplementary figures, we shared aggregate results that will allow readers to replot the figures and inspect without having to recomputing all results.

## structure of individual-specific results

### model predictions

Some of the scripts refer to input files with a `"post_predictive*.csv"` naming pattern. Thes files contain the model predictions (both left-out fold and held-out generalization dataset) alongside the true values for age, fluid intelligence and neuroticism from each fold from the cross-validation procedure performed on the data used for model construction.

The table below gives a summary of the structure and data types of the results.

|Unnamed: 0 |20016-2.0 |20127-0.0 |21022-0.0 |eid     |evaluation |fold    |predicted |target    |test_indices |true    |variable  |Data type |
|:----------|:---------|:---------|:---------|:-------|:----------|:-------|:---------|:---------|:------------|:-------|:---------|:---------|
|integer    |logical   |logical   |integer   |integer |character  |integer |double    |character |integer      |integer |character |character |


### validation against health data

For some of the analyses, model predictions were validated against external health data.
The tables below show the file structure for the respective inputs and health domains.

`alcohol.csv`

|eid     |1558-0.0 |1568-0.0 |1578-0.0 |1588-0.0 |1598-0.0 |1608-0.0 |1618-0.0 |3731-0.0 |4407-0.0 |4418-0.0 |4429-0.0 |4440-0.0 |4451-0.0 |4462-0.0 |5364-0.0 |20117-0.0 |variable  |
|:-------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:---------|:---------|
|integer |double   |double   |double   |double   |double   |double   |double   |double   |double   |double   |double   |double   |double   |double   |double   |double    |character |

`sleep.csv`

|eid     |1160-0.0 |1170-0.0 |1180-0.0 |1190-0.0 |1200-0.0 |1210-0.0 |1220-0.0 |variable  |
|:-------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:---------|
|integer |double   |double   |double   |double   |double   |double   |double   |character |

`physical_activity.csv`

|eid     |22032-0.0 |22033-0.0 |22034-0.0 |22035-0.0 |22036-0.0 |22037-0.0 |22038-0.0 |22039-0.0 |22040-0.0 |variable  |
|:-------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|
|integer |double    |double    |double    |double    |double    |double    |double    |double    |double    |character |

`smoking.csv`

|eid     |20161-0.0 |20162-0.0 |variable  |
|:-------|:---------|:---------|:---------|
|integer |double    |double    |character |
