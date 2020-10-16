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

DT_brain <- fread(sprintf(base_path, "full_mri"))
DT_brain <- DT_brain[,
  .(predicted = mean(predicted), true = true[1]),
  by = .(eid, target, evaluation)]

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

DT_inf_brain <- DT_brain[evaluation == "Generalization",
  {
    dat <- na.omit(data.frame(predicted = predicted, true = true))
    perms <- 1:10000

    # permutations
    t_obs <- roc_auc_score(true = dat$true, predicted = dat$predicted)
    perm_auc <- sapply(perms, function(perm){
      set.seed(perm)
      roc_auc_score(true = dat$true,
                    predicted = sample(dat$predicted, replace = F))
    })
    t_perm <- mean(perm_auc)
    p_val <- max(mean(t_obs < perm_auc), 1 / length(perms))

    # bootstrap
    boot_r2s <- sapply(perms, function(perm){
      set.seed(perm)
      inds <- sample.int(nrow(dat), replace = T)
      roc_auc_score(true = dat$true[inds] ,
                    predicted = dat$predicted[inds])
    })

    .(t_obs = t_obs, t_perm  = t_perm, p_val = p_val, 
      CI_low = quantile(boot_r2s, 0.025),
      CI_hi = quantile(boot_r2s, 0.975))
  }, by = .(target)]

fwrite(DT_inf_brain, "brain_inference_class.csv")
knitr::kable(DT_inf_brain)
tex_out_brain <- knitr::kable(DT_inf_brain[,-c("t_perm")], format = "latex")
writeLines(tex_out_brain, "brain_inference_class.tex")

DT_aucs <- DT[evaluation == "Generalization",
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
      
      out <- data.frame(
        auc_score = c(
          roc_auc_score(
            true = dat$true_soc_b, predicted = dat$predicted_soc_b),
          roc_auc_score(
            true = dat$true_soc, predicted = dat$predicted_soc)),
        imaging = c("yes", "no")
      )
      out
    },
  .(target)]

fwrite(DT_aucs, "gen_observed_aucs.csv")
