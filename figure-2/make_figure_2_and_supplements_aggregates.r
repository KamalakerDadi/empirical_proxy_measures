library(data.table)
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
    age_delta <- with(test, age_pred - age)  # ... XXX

    mod <- lm(scale(value) ~           # y
              scale(value_adjusted) +  # confound_train XXX
              scale(age_delta) +       # brain age
              scale(FI_pred) +              # Fluid Intelligence
              scale(N_pred), test)     # Neuroticism

    coefs <- coef(summary(mod))
    out <- data.frame(
      coef = coefs[-c(1, 2), 1],  # kick out intercept + confound term
      se = coefs[-c(1, 2), 2],    # ... and get columns for coef and error
      p.val = coefs[-c(1, 2), 4],
      target = c("Brain Age Delta", "Fluid Intelligence", "Neuroticism"))
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
    age_delta <- with(test, age_pred - age)  # ... XXX

    mod <- lm(scale(value) ~           # y
              scale(value_adjusted) +  # confound_train XXX
              scale(age_delta) +       # brain age delta
              scale(FI_pred) +              # Fluid Intelligence
              scale(N_pred), test)     # Neuroticism
    n.sim <- 10000
    mod.sim <- arm::sim(mod, n.sim = n.sim)
    out <- data.table(mod.sim@coef)
    data.table(
      name = name[1],
      coef = c(out[['scale(age_delta)']],
                out[['scale(FI_pred)']],
                out[['scale(N_pred)']]),
      target = rep(c("Brain Age Delta", "Fluid Intelligence", "Neuroticism"),
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
    age_delta <- with(test, age_pred - age)  # ... XXX

    mod <- lm(scale(value) ~           # y
              scale(age) +       # brain age delta
              scale(FI) +              # Fluid Intelligence
              scale(N), test)     # Neuroticism

    coefs <- coef(summary(mod))
    out <- data.frame(
      coef = coefs[-1, 1],  # kick out intercept 
      se = coefs[-1, 2],    # ... and get columns for coef and error
      p.val = coefs[-1, 4],
      target = c("Age", "Fluid Intelligence", "Neuroticism"))
    out['nobs'] = nobs(mod)
    out['name'] = name[1]
    out
  },
  by = family]

# get parametric bootstrap for conditional target model
DT_cond_target_bs <- DT_pred[,
  {
    train <- .SD[evaluation == "Validation"]
    test <- .SD[evaluation == "Generalization"]

    # fit association model on test set
    age_delta <- with(test, age_pred - age)  # ... XXX

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

###############################################################################
# Marginal models for supplement

DT_margin_proxy <- DT_pred[,
  {
    train <- .SD[evaluation == "Validation"]
    test <- .SD[evaluation == "Generalization"]
    deconf <- fit_train_deconfound(
      train = train, test = test, var = "value", confound = "age")
    value_adjusted <- deconf[['pred']]
 
    # fit association model on test set
    age_delta <- with(test, age_pred - age)  # ... XXX

    mod1 <- lm(scale(value) ~           # y
               scale(value_adjusted) +  # confound_train XXX
               scale(age_delta), test)     # Age delta

    mod2 <- lm(scale(value) ~           # y
               scale(value_adjusted) +  # confound_train XXX
               scale(FI_pred), test)     # Neuroticism

    mod3 <- lm(scale(value) ~           # y
               scale(value_adjusted) +  # confound_train XXX
               scale(N_pred), test)     # Neuroticism

    coefs1 <- coef(summary(mod1))
    coefs2 <- coef(summary(mod2))
    coefs3 <- coef(summary(mod3))
    sel <- -c(1, 2)
    out <- data.frame(
      coef = c(coefs1[sel, 1],
               coefs2[sel, 1],
               coefs3[sel, 1]),
      se = c(coefs1[sel, 2],
             coefs2[sel, 2],
             coefs3[sel, 2]),
      p.val = c(coefs1[sel, 4],
                coefs2[sel, 4],
                coefs3[sel, 4]),
      target = c("Brain Age Delta", "Fluid Intelligence", "Neuroticism"))
    out['nobs'] = nobs(mod1)
    out['name'] = name[1]
    out
  },
  by = family]

# ... for marginal target model
DT_margin_target <- DT_pred[,
  {
    train <- .SD[evaluation == "Validation"]
    test <- .SD[evaluation == "Generalization"]

    # fit association model on test set
    age_delta <- with(test, age_pred - age)  # ... XXX

    mod1 <- lm(scale(value) ~           # y
               scale(age), test)     # Neuroticism

    mod2 <- lm(scale(value) ~           # y
               scale(FI), test)     # Neuroticism

    mod3 <- lm(scale(value) ~           # y
               scale(N), test)     # Neuroticism

    coefs1 <- coef(summary(mod1))
    coefs2 <- coef(summary(mod2))
    coefs3 <- coef(summary(mod3))
    sel <- -1
    out <- data.frame(
      coef = c(coefs1[sel, 1],
               coefs2[sel, 1],
               coefs3[sel, 1]),
      se = c(coefs1[sel, 2],
             coefs2[sel, 2],
             coefs3[sel, 2]),
      p.val = c(coefs1[sel, 4],
                coefs2[sel, 4],
                coefs3[sel, 4]),
      target = c("Age", "Fluid Intelligence", "Neuroticism"))
    out['nobs'] = nobs(mod1)
    out['name'] = name[1]
    out
  },
  by = family]

# get parametric bootstrap for marginal target model
DT_margin_target_bs <- DT_pred[,
  {
    train <- .SD[evaluation == "Validation"]
    test <- .SD[evaluation == "Generalization"]

    mod1 <- lm(scale(value) ~           # y
               scale(age), test)     # Neuroticism

    mod2 <- lm(scale(value) ~           # y
               scale(FI), test)     # Neuroticism

    mod3 <- lm(scale(value) ~           # y
               scale(N), test)     # Neuroticism

    n.sim <- 10000
    # boots
    mod1.sim <- arm::sim(mod1, n.sim = n.sim)
    out1 <- data.table(mod1.sim@coef)
    mod2.sim <- arm::sim(mod2, n.sim = n.sim)
    out2 <- data.table(mod2.sim@coef)
    mod3.sim <- arm::sim(mod3, n.sim = n.sim)
    out3 <- data.table(mod3.sim@coef)

    data.table(
      name = name[1],
      coef = c(out1[['scale(age)']],
               out2[['scale(FI)']],
               out3[['scale(N)']]),
      target = rep(c("Age", "Fluid Intelligence", "Neuroticism"),
        each = n.sim)
    )
  },
  by = family]

# get parametric bootstrap for marginal proxy model
DT_margin_proxy_bs <- DT_pred[,
  {
    train <- .SD[evaluation == "Validation"]
    test <- .SD[evaluation == "Generalization"]
    deconf <- fit_train_deconfound(
      train = train, test = test, var = "value", confound = "age")
    value_adjusted <- deconf[['pred']]

    age_delta <- with(test, age_pred - age)  # ... XXX

    mod1 <- lm(scale(value) ~           # y
               scale(value_adjusted) +  # confound_train XXX
               scale(age_delta), test)     # Neuroticism

    mod2 <- lm(scale(value) ~           # y
               scale(value_adjusted) +  # confound_train XXX
               scale(FI_pred), test)     # Neuroticism

    mod3 <- lm(scale(value) ~           # y
               scale(value_adjusted) +  # confound_train XXX
               scale(N_pred), test)     # Neuroticism

    n.sim <- 10000
    # boots
    mod1.sim <- arm::sim(mod1, n.sim = n.sim)
    out1 <- data.table(mod1.sim@coef)
    mod2.sim <- arm::sim(mod2, n.sim = n.sim)
    out2 <- data.table(mod2.sim@coef)
    mod3.sim <- arm::sim(mod3, n.sim = n.sim)
    out3 <- data.table(mod3.sim@coef)

    data.table(
      name = name[1],
      coef = c(out1[['scale(age_delta)']],
               out2[['scale(FI_pred)']],
               out3[['scale(N_pred)']]),
      target = rep(c("Brain Age Delta", "Fluid Intelligence", "Neuroticism"),
        each = n.sim)
    )
  },
  by = family]

##############################################################################
# writing it out

fwrite(DT_cond_proxy, 'mod_cond_proxy.csv')
fwrite(DT_cond_proxy_bs, 'mod_cond_proxy_bs.csv')
fwrite(DT_cond_target, 'mod_cond_target.csv')
fwrite(DT_cond_target_bs, 'mod_cond_target_bs.csv')

fwrite(DT_margin_proxy, 'mod_margin_proxy.csv')
fwrite(DT_margin_proxy_bs, 'mod_margin_proxy_bs.csv')
fwrite(DT_margin_target, 'mod_margin_target.csv')
fwrite(DT_margin_target_bs, 'mod_margin_target_bs.csv')
