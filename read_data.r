stack_wide2 <- function(DT)
{
  DT_wide <- data.table(eid = unique(DT$eid))

  DT_tmp <- merge(
    DT_wide,
    DT[target == 'Age'][, c("eid")])

  setkey(DT_wide, eid)
  DT_tmp <- DT[target == 'Age'][,
    .(age = true, age_pred = predicted, eid = eid)]

  setkey(DT_tmp, eid)
  DT_wide[DT_tmp, `:=`(age = i.age, age_pred = i.age_pred)]

  DT_tmp <- DT[target == 'Neuroticism'][,
      .(N = true, N_pred = predicted, eid = eid)]
  setkey(DT_tmp, eid)
  DT_wide[DT_tmp, `:=`(N = i.N, N_pred = i.N_pred)]

  DT_tmp <- DT[target == 'Fluid intelligence'][,
      .(FI = true, FI_pred = predicted, eid = eid)]
  setkey(DT_tmp, eid)
  DT_wide[DT_tmp, `:=`(FI = i.FI, FI_pred = i.FI_pred)]
  DT_wide
}

read_file_parts <- function(fname, path1){
  fnames <- list.files(
    path1, paste0(strsplit(fname, ".csv")[[1]], "_"), full.names = T)
  
  # make sure not to use the old age outputs
  fnames <- Filter(function(x) !endsWith(x, 'age_at_assessment.csv'),
                   fnames)  

  rbindlist(lapply(fnames, fread))
}

assemble_inputs <- function(fname, path1)
{
  DT_pred <- read_file_parts(fname, path1 = path1)
  DT_pred$variable <- NULL
  has_var <- "variable" %in% names(DT_pred)

  # kick out unneccesary variables ... and average over folds (shuffle splits..).
  if (has_var) {
    DT_pred <- DT_pred[, .(predicted = mean(predicted), true = true[1]),
                       by = .(eid, variable, target, evaluation)]    
  } else {
    DT_pred <- DT_pred[, .(predicted = mean(predicted), true = true[1]),
                       by = .(eid, target, evaluation)]    
  }

  this_stack_wide <- ifelse(has_var, stack_wide, stack_wide2)

  DT_pred[, eid := factor(eid)]
  #' We need to clean up a bit now. Load test and train sets independently.
  #' Then make sure things look as expected. Then stack the two

  DT_pred_dev <- DT_pred[evaluation == "Validation"]
  DT_pred_dev[,c("evaluation")] <- NULL
  DT_pred_dev_wide <- this_stack_wide(DT_pred_dev)

  DT_pred_gen <- DT_pred[evaluation != "Validation"]
  DT_pred_gen[,c("evaluation")] <- NULL
  DT_pred_gen_wide <- this_stack_wide(DT_pred_gen)

  stopifnot(sum(DT_pred_dev_wide$eid %in%
                DT_pred_gen_wide$eid) == 0)

  DT_pred_dev_wide[, evaluation := "Validation"]
  DT_pred_gen_wide[, evaluation := "Generalization"]
  DT_pred_wide <- rbind(DT_pred_dev_wide, DT_pred_gen_wide)

  DT2 <- get_extras2(extra_path)

  DT_pred_wide2 <- merge(DT_pred_wide, DT2, by = "eid",
                         allow.cartesian = T)

  #' So far we have computed models grouped by predictor models. Hower, this
  #' brings the slight disadvantage of not combining the best models as different
  #' models were most predictive for each target. We can do better. Let's select
  #' subject the best predictions to multiple regression.

  if (has_var) {
    DT_pred_wide3 <- DT_pred_wide2[variable == 'Life style',
      .(eid, age, age_pred, evaluation, value, family, name)]

    DT_tmp <- DT_pred_wide2[variable == 'Mental Health', .(eid, N, N_pred, family)]
    setkey(DT_tmp, eid, family)
    setkey(DT_pred_wide3, eid, family)
    DT_pred_wide3[DT_tmp, `:=`(N_pred = N_pred, N = N)]

    DT_tmp <- DT_pred_wide2[variable == 'Education', .(eid, FI, FI_pred, family)]
    setkey(DT_tmp, eid, family)
    DT_pred_wide3[DT_tmp, `:=`(FI_pred = FI_pred, FI = FI)]
  } else {
    DT_pred_wide3 <- DT_pred_wide2
  }
  DT_pred_wide3
}
