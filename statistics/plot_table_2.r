#'---
#'title: "Score models on test set"
#'author: "Denis A. Engemann, Kamalaker Dadi"
#'date: "7/18/2020"
#'output:
#'    html_document:
#'        code_folding:
#'            hide
#'    md_document:
#'        variant:
#'            markdown_github
#'---

#' First execrise: Load all relevant data:
#' 1. Predictions for all targets from brain
#' 2. ... from sociodemographics
#' 3. ... from sociodemographics + brain

library(data.table)

base_path <- file.path('post_predictive_analysis_class_%s.csv')


get_inputs <- function(fname, input_type)
{
  DT <- fread(fname)
  # kick out unneccesary variables and average over folds (shuffle splits..).
  DT <- DT[,
    .(predicted = mean(predicted), true = true[1]),
    by = .(eid, target, evaluation)]
  DT[, `:=`(eid = factor(eid), input_type = input_type)]
  DT
}

DT <- rbind(
  get_inputs(sprintf(base_path, "non_imaging_fresh"), "social"),
  get_inputs(sprintf(base_path, "imaging_non_imaging"), "social_brain")
)

#' Now let's set a few goals.
#'
#' What?
#' 1. brain vs non-brain models within each group of socio-demographics
#' 2. brain models vs chance.
#'
#' How?
#' - out-of-sample permutation of r2 value for brain models
#' - out-of-sample permutation of difference between 2 models
#' - same for effect-sizes using bootstrap
#'
#' Output?
#' - tables

roc_auc_score <- function(predicted, true) {
  n1 <- sum(!true)
  n2 <- sum(true)
  U  <- sum(rank(predicted)[!true]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}

DT_inf_diff <- DT[evaluation == "Generalization",
  {
    dat1 <- data.frame(
      predicted_soc = .SD[input_type == "social", predicted], 
      true_soc = .SD[input_type == "social", true],
      eid = .SD[input_type == "social", eid]
    )
    dat2 <- data.frame(
      predicted_soc_b = .SD[input_type == "social_brain", predicted], 
      true_soc_b = .SD[input_type == "social_brain", true],
      eid = .SD[input_type == "social_brain", eid]
    )
    dat <- na.omit(merge(dat1, dat2, by = "eid"))

    # # permutations
    t_obs <- `-`(
      roc_auc_score(true = dat$true_soc_b, predicted = dat$predicted_soc_b),
      roc_auc_score(true = dat$true_soc, predicted = dat$predicted_soc)
    )
    perms <- 1:10000
    perm_auc <- sapply(perms, function(perm){
      set.seed(perm)

      cand <- cbind(dat$predicted_soc,
                    dat$predicted_soc_b)
      # flip columns in each row
      permuted <- do.call(rbind,
        lapply(
          1:nrow(cand),
          function(ind){
            sample(cand[ind, c(1, 2)], replace = F)
          })
      )

      auc_soc <- roc_auc_score(true = dat$true_soc,
                               predicted = permuted[,1])
      auc_soc_b <- roc_auc_score(true = dat$true_soc_b,
                                 predicted = permuted[,2])
      auc_soc_b - auc_soc
    })
    t_perm <- mean(perm_auc)
    p_val <- max(mean(abs(t_obs) < abs(perm_auc)), 1 / length(perms))

    # bootstrap
    boot_r2s <- sapply(perms, function(perm){
      set.seed(perm)
      inds <- sample.int(nrow(dat), replace = T)

      auc_soc_b <- roc_auc_score(true = dat$true_soc_b[inds],
                                 predicted = dat$predicted_soc_b[inds])

      auc_soc <- roc_auc_score(true = dat$true_soc[inds],
                               predicted = dat$predicted_soc[inds])
      auc_soc_b - auc_soc

    })

    .(t_obs = t_obs, t_perm  = t_perm, p_val = p_val, 
      CI_low = quantile(boot_r2s, 0.025),
      CI_hi = quantile(boot_r2s, 0.975))
  }, by = .(target)]


setorder(DT_inf_diff, target)
fwrite(DT_inf_diff, "paired_diff_inference_class.csv")
knitr::kable(DT_inf_diff)
tex_out_diff <- knitr::kable(DT_inf_diff[,-c("t_perm")], format = "latex")
writeLines(tex_out_diff, "paired_diff_inference_class.tex")
