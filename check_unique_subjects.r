library(data.table)

DT <-
  fread(
    file.path(
      'figure-2', 
      'inputs',
      paste0('post_predictive_analysis_all_models_combined_imaging_non_',
             'imaging_age_at_assessment.csv')))


eid_val <- unique(DT[evaluation == "Validation"]$eid)

eid_gen <- unique(DT[evaluation == "Generalization"]$eid)

n_overlap <- print(length(intersect(eid_gen, eid_val)))

sprintf(
  "There is an overlap of %d subjects between validation and generalization",
  n_overlap)

#[1] "There is an overlap of 0 subjects between validation and generalization"
