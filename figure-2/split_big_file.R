library(data.table)

fname <- "post_predictive_analysis_all_models_combined_imaging_non_imaging.csv"

dt <- fread(file.path('inputs',fname))
dt_split <- split(dt, by = "target")

for (ii in seq_along(dt_split)){
    part_name <- strsplit(fname, ".csv")[[1]]
    part_name <- paste0(part_name, "_", tolower(names(dt_split)[[ii]]), ".csv") 
    fwrite(dt_split[[ii]], file.path('inputs', part_name))
}

