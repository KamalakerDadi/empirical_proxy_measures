library(data.table)

split_files <- function(fname, by = c("target", "variable"))
{
    dt <- fread(file.path('inputs',fname))
    dt_split <- split(dt, by = by)

    split_names <- gsub(" ", "", names(dt_split))
    split_names <- gsub(".", "_", split_names, fixed = T)

    for (ii in seq_along(dt_split))
    {
        part_name <- strsplit(fname, ".csv")[[1]]
        part_name <- paste0(
            part_name, "_", tolower(split_names[[ii]]), ".csv") 
        fwrite(dt_split[[ii]], file.path('inputs', part_name))
    }
}

split_files("post_predictive_analysis_imaging_non_imaging.csv")
split_files("post_predictive_analysis_non_imaging_fresh.csv")
split_files("post_predictive_analysis_full_mri.csv", by = "target")
