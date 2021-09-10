
library(data.table)
library(ggplot2)
source('../config.r')
source('../utils.r')
source('../read_data.r')

extra_path <- file.path('inputs')

fname <-
  'post_predictive_analysis_all_models_combined_imaging_non_imaging.csv'

DT_pred <- assemble_inputs(fname, path1 = extra_path)


#' Get point estimates + standard outputs for conditional target model
DT_cond_proxy <- DT_pred[,
  {
    train <- .SD[evaluation == "Validation"]
    test <- .SD[evaluation == "Generalization"]
    deconf <- fit_train_deconfound(
      train = train, test = test, var = "value", confound = "age")
    value_adjusted <- deconf[['pred']]
    # fit association model on test set

    mod <- lm(scale(value) ~           # y
              scale(value_adjusted) +  # confound_train XXX
              scale(age_pred) +       # brain age
              scale(FI_pred) +              # Fluid Intelligence
              scale(N_pred), test)     # Neuroticism

    coefs <- t(coef(summary(mod)))
    out <- coefs[, c('scale(age_pred)', 'scale(FI_pred)', 'scale(N_pred)')]
    out <- t(out)[, c('Estimate', 'Std. Error', 'Pr(>|t|)')]
    names(out) <- c('coef', 'se', 'p.val')
    out <- data.frame(out)
    out$target <- c("Brain Age", "Fluid Intelligence", "Neuroticism")
    out['nobs'] = nobs(mod)
    out['name'] = name[1]
    out
  },
  by = family]

#' get parametric bootstrap for conditional proxy model.
DT_cond_proxy_bs <- DT_pred[,
  {
    train <- .SD[evaluation == "Validation"]
    test <- .SD[evaluation == "Generalization"]
    deconf <- fit_train_deconfound(
      train = train, test = test, var = "value", confound = "age")
    value_adjusted <- deconf[['pred']]

    # fit association model on test set

    mod <- lm(scale(value) ~           # y
              scale(value_adjusted) +  # confound_train XXX
              scale(age_pred) +       # brain age delta
              scale(FI_pred) +              # Fluid Intelligence
              scale(N_pred), test)     # Neuroticism
    n.sim <- 10000
    mod.sim <- arm::sim(mod, n.sim = n.sim)
    out <- data.table(mod.sim@coef)
    data.table(
      name = name[1],
      coef = c(out[['scale(age_pred)']],
                out[['scale(FI_pred)']],
                out[['scale(N_pred)']]),
      target = rep(c("Brain Age", "Fluid Intelligence", "Neuroticism"),
        each = n.sim)
    )
  },
  by = family]


# Target models

#' Get point estimates + standard outputs for conditional target model
DT_cond_target <- DT_pred[,
  {
    train <- .SD[evaluation == "Validation"]
    test <- .SD[evaluation == "Generalization"]

    # fit association model on test set

    mod <- lm(scale(value) ~           # y
              scale(age) +       # age
              scale(FI) +              # Fluid Intelligence
              scale(N), test)     # Neuroticism

    coefs <- t(coef(summary(mod)))
    out <- coefs[, c('scale(age)', 'scale(FI)', 'scale(N)')]
    out <- t(out)[, c('Estimate', 'Std. Error', 'Pr(>|t|)')]
    names(out) <- c('coef', 'se', 'p.val')
    out <- data.frame(out)
    out$target <- c("Brain Age", "Fluid Intelligence", "Neuroticism")
    out['nobs'] <- nobs(mod)
    out['name'] <- name[1]
    out
  },
  by = family]

# get parametric bootstrap for conditional target model
DT_cond_target_bs <- DT_pred[,
  {
    train <- .SD[evaluation == "Validation"]
    test <- .SD[evaluation == "Generalization"]

    # fit association model on test set
    mod <- lm(scale(value) ~           # y
              scale(age) +       # brain age delta
              scale(FI) +              # Fluid Intelligence
              scale(N), test)     # Neuroticism
    n.sim <- 10000
    mod.sim <- arm::sim(mod, n.sim = n.sim)
    out <- data.table(mod.sim@coef)
    data.table(
      name = name[1],
      coef = c(out[['scale(age)']],
               out[['scale(FI)']],
               out[['scale(N)']]),
      target = rep(c("Age", "Fluid Intelligence", "Neuroticism"),
        each = n.sim)
    )
  },
  by = family]


fwrite(DT_cond_proxy, 'mod22_cond_proxy.csv')
fwrite(DT_cond_proxy_bs, 'mod22_cond_proxy_bs.csv')
fwrite(DT_cond_target, 'mod22_cond_target.csv')
fwrite(DT_cond_target_bs, 'mod22_cond_target_bs.csv')
