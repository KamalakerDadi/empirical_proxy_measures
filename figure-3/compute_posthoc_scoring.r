#'---
#'title: "Score models on test set"
#'author: "Denis A. Engemann"
#'date: "7/17/2020"
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
library(future.apply)
source("../utils.r")
plan(multiprocess, workers = 4) 

par_sapply <- function(...) future_sapply(..., future.seed = T)

base_path <- 'post_predictive_analysis_%s.csv'

get_inputs <- function(fname, input_type)
{
  DT <- read_file_parts(fname)
  # kick out unneccesary variables and average over folds (shuffle splits..).
  DT <- DT[,
    .(predicted = mean(predicted), true = true[1]),
    by = .(eid, variable, target, evaluation)]
  DT[, `:=`(eid = factor(eid), input_type = input_type)]
  DT
}

DT <- rbind(
  get_inputs(sprintf(base_path, "non_imaging_fresh"), "social"),
  get_inputs(sprintf(base_path, "imaging_non_imaging"), "social_brain")
)

DT_brain <- read_file_parts(sprintf(base_path, "full_mri"))
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

r2_score <- function(true, predicted)
{
  rss <- sum((predicted - true) ^ 2)
  tss <- sum((true - mean(true)) ^ 2)
  R2 <- 1 - rss / tss
  R2
}

DT_inf_brain <- DT_brain[evaluation == "Generalization",
  {
    dat <- na.omit(data.frame(predicted = predicted, true = true))
    perms <- 1:10000

    # permutations
    t_obs <- r2_score(true = dat$true, predicted = dat$predicted)
    perm_r2s <- par_sapply(perms, function(perm){
      set.seed(perm)
      r2_score(true = dat$true,
               predicted = sample(dat$predicted, replace = F))
    })
    t_perm <- mean(perm_r2s)
    p_val <- max(mean(t_obs < perm_r2s), 1 / length(perms))

    # bootstrap
    boot_r2s <- par_sapply(perms, function(perm){
      set.seed(perm)
      inds <- sample.int(nrow(dat), replace = T)
      r2_score(true = dat$true[inds],
               predicted = dat$predicted[inds])
    })

    .(t_obs = t_obs, t_perm  = t_perm, p_val = p_val, 
      CI_low = quantile(boot_r2s, 0.025),
      CI_hi = quantile(boot_r2s, 0.975))
  }, by = .(target)]

fwrite(DT_inf_brain, "brain_inference.csv")
knitr::kable(DT_inf_brain)
tex_out_brain <- knitr::kable(DT_inf_brain[,-c("t_perm")], format = "latex")
writeLines(tex_out_brain, "brain_inference.tex")

# compute separate R2s for ech side.

DT_r2s <- DT[evaluation == "Generalization",
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
        r2_score = c(
          r2_score(true = dat$true_soc_b, predicted = dat$predicted_soc_b),
          r2_score(true = dat$true_soc, predicted = dat$predicted_soc)),
        imaging = c("yes", "no")
      )
      out
    },
  .(target, variable)]

fwrite(DT_r2s, "gen_observed_r2s.csv")

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
      r2_score(true = dat$true_soc_b, predicted = dat$predicted_soc_b),
      r2_score(true = dat$true_soc, predicted = dat$predicted_soc)
    )
    perms <- 1:10000
    perm_r2s <- par_sapply(perms, function(perm){
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
      r2_soc <- r2_score(true = dat$true_soc,
                         predicted = permuted[, 1])
      r2_soc_b <- r2_score(true = dat$true_soc_b,
                           predicted = permuted[, 2])
      r2_soc_b - r2_soc
    })
    t_perm <- mean(perm_r2s)
    p_val <- max(mean(abs(t_obs) < abs(perm_r2s)), 1 / length(perms))

    # bootstrap
    boot_r2s <- par_sapply(perms, function(perm){
      set.seed(perm)
      inds <- sample.int(nrow(dat), replace = T)

      r2_soc_b <- r2_score(true = dat$true_soc_b[inds],
                           predicted = dat$predicted_soc_b[inds])

      r2_soc <- r2_score(true = dat$true_soc[inds],
                         predicted = dat$predicted_soc[inds])
      r2_soc_b - r2_soc

    })

    .(t_obs = t_obs, t_perm  = t_perm, p_val = p_val, 
      CI_low = quantile(boot_r2s, 0.025),
      CI_hi = quantile(boot_r2s, 0.975))
  }, by = .(target, variable)]


setorder(DT_inf_diff, target, variable)
fwrite(DT_inf_diff, "paired_diff_inference.csv")
knitr::kable(DT_inf_diff)
tex_out_diff <- knitr::kable(DT_inf_diff[,-c("t_perm")], format = "latex")
writeLines(tex_out_diff, "paired_diff_inference.tex")
