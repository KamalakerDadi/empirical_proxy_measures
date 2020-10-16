#'---
#'title: "Explore post-hoc proxy measure associations"
#'author: "Denis A. Engemann"
#'date: "7/10/2020"
#'output:
#'    html_document:
#'        code_folding:
#'            hide
#'    md_document:
#'        variant:
#'            markdown_github
#'---

library(data.table)
library(ggplot2)
library(arm)
library(ggbeeswarm)
library(patchwork)
source('../visualizations/config.r')
source('../visualizations/utils.r')

#' ## How are predictions linked to extra health-related variables?
#'
#' ### The Setup.
#'
#' 1. We have x-val predictions for each target in the dev-set by all blocks
#' of predictors. 1 prediction per subject as the folds were used for CV.
#'
#' 2. We have k fold times n_subject prediction on the gen-set as we predicted
#' from each fold in dev-set to gen-set.
#'
#' The second aspect brings us uncertainty estimation for free.
#'
#' ### Challenges
#'
#' 1. The predictions may be correlated among another, reducing their indpendence
#'
#' 2. Effects are also confounded by age and other factors, further reducing
#'    specificity of associations
#'
#' 3. The pattenrs may be significant but may make no sense.
#'
#' ### Questions
#'
#' 1. What is the importance of different variance components?
#'
#' 2. What is the degree of correlation between the predictions?
#'
#' 3. What are characteristic loadings for each prediction?
#'
#'

extra_path <- file.path('correlation_variables')

#' Get together all input files.
#' First the extra data files. We'll write a function for that.
#'

get_extras <- function(path){
  # extra files
  extras <-  c(
    "alcohol.csv",
    "physical_activity.csv",
    "sleep.csv",
    "smoking.csv")

  # build lookup tables
  # This allows us to to elegantly (vectortized dict!) add extra info later.
  lookup_names <- fread(file.path(extra_path, "names.csv"))

  lookup_names <- setNames(
    unlist(lookup_names[2,-1]),
    unlist(lookup_names[1,-1]))

  lookup_family <- setNames(
    rep(c("Alcohol", "Sleep", "Smoking", "Activity"), c(16, 7, 2, 9)),
    unname(lookup_names))

  # read first file and do recursive join using DT syntax
  DT <- fread(file.path(extra_path, extras[1]))
  for (ii in 2:4){
    this_DT <- fread(file.path(extra_path, extras[ii]))
    DT <- DT[this_DT, on = "eid"]
  }

  # move to long format
  # kick out variable columns
  DT[,c('variable', 'i.variable', 'i.variable.1', 'i.variable.2')] <- NULL
  DT <- melt(
    DT,
    id.vars = c("eid"), measure.vars = names(DT)[-c(1)],
    variable.factor = F,
    variable.name = "var_code")

  # Add new column inplace using lookup trick. Then order by individual Ids
  DT[, name := lookup_names[var_code]]
  DT[, family := lookup_family[name]]
  setorder(DT, eid)
  DT
}

DT <- get_extras(extra_path)
DT[, eid := factor(eid)]  # inplace modification syntax

#' Second, we read in the predictions for each target and the true scores,
#' 2-fold nesting!-- aggregating over subjects/eid. We'll average over
#' folds for now.

DT_pred <- fread(
  file.path('post_predictive_analysis_imaging_non_imaging.csv'))

# kick out unneccesary variables ... and average over folds (shuffle splits..).
DT_pred <- DT_pred[, .(predicted = mean(predicted), true = true[1]),
                   by = .(eid, variable, target, evaluation)]
DT_pred[, eid := factor(eid)]

#' Some custom not very elegant but fast code to reshape large
#' data tables in a custom way. We need a partially wide format
#' where 1 row has got all proxy measures and true values
#' per subject per predictor model and per extra variable.

stack_wide <- function(DT)
{
  DT_wide <- data.table(expand.grid(
    eid = unique(DT$eid), variable = unique(DT$variable)))

  DT_tmp <- merge(
    DT_wide,
    DT[target == 'Age'][, c("eid", "variable")])

  setkey(DT_wide, eid, variable)
  DT_tmp <- DT[target == 'Age'][,
    .(age = true, age_pred = predicted, eid = eid, variable = variable)]

  setkey(DT_tmp, eid, variable)
  DT_wide[DT_tmp, `:=`(age = i.age, age_pred = i.age_pred)]

  DT_tmp <- DT[variable != 'Age, Sex' & target == 'Neuroticism'][,
      .(N = true, N_pred = predicted, eid = eid, variable = variable)]
  setkey(DT_tmp, eid, variable)
  DT_wide[DT_tmp, `:=`(N = i.N, N_pred = i.N_pred)]

  DT_tmp <- DT[variable != 'Age, Sex' & target == 'Fluid intelligence'][,
      .(FI = true, FI_pred = predicted, eid = eid, variable = variable)]
  setkey(DT_tmp, eid, variable)
  DT_wide[DT_tmp, `:=`(FI = i.FI, FI_pred = i.FI_pred)]

  DT_tmp <- DT[variable == 'Age, Sex' & target == 'Neuroticism'][,
      .(N = true, N_pred = predicted, eid = eid, variable = variable)]
  setkey(DT_tmp, eid, variable)
  DT_wide[DT_tmp, `:=`(N = i.N, N_pred = i.N_pred)]

  DT_tmp <- DT[variable == 'Age, Sex' & target == 'Fluid intelligence'][,
      .(FI = true, FI_pred = predicted, eid = eid, variable = variable)]
  setkey(DT_tmp, eid, variable)
  DT_wide[DT_tmp, `:=`(FI = i.FI, FI_pred = i.FI_pred)]
  DT_wide
}

#' We need to clean up a bit now. Load test and train sets independently.
#' Then make sure things look as expected. Then stack the two

DT_pred_dev <- DT_pred[evaluation == "Validation"]
DT_pred_dev[,c("evaluation")] <- NULL
DT_pred_dev_wide <- stack_wide(DT_pred_dev)

DT_pred_gen <- DT_pred[evaluation != "Validation"]
DT_pred_gen[,c("evaluation")] <- NULL
DT_pred_gen_wide <- stack_wide(DT_pred_gen)

stopifnot(sum(DT_pred_dev_wide$eid %in%
              DT_pred_gen_wide$eid) == 0)

DT_pred_dev_wide[, evaluation := "Validation"]
DT_pred_gen_wide[, evaluation := "Generalization"]
DT_pred_wide <- rbind(DT_pred_dev_wide, DT_pred_gen_wide)

#' Before proceeding it is a good idea to do a few plausibility checks

abbr_lookup <- setNames(
  sapply(strsplit(unique(DT$name), " "), function(x) paste(unname(abbreviate(x
    , 4)), collapse = ' ')),
  unique(DT$name)
)

DT[, name_abr := abbr_lookup[name]]

#+ fig.height = 12, fig.width = 16
ggplot(DT[value >= 0, ], mapping = aes(x = value, fill = family, color = family)) +
  geom_histogram(alpha = 0.8) +
  facet_wrap(name_abr~., scales = "free", labeller = label_wrap_gen(width=28)) +
  theme(strip.text = element_text(size = 8))

DT_pred_wide1 <- merge(DT_pred_wide, DT, by = "eid",
                       allow.cartesian = T)

DT_pred_long <- DT_pred_wide1[
  evaluation == "Generalization",
  {
    out <- rbind(
      data.table(
        value = value,
        eid = eid,
        target = "age_delta",
        pred = scale(age_pred - age)),
      data.table(
        value = value,
        eid = eid,
        target = "FI",
        pred = scale(FI_pred)),
      data.table(
        value = value,
        eid = eid,
        target = "N",
        pred = scale(N_pred))
    )
    out[, `:=`(name = name[1],
               family = family[1])]
    out
  },
  by = .(variable, var_code)
]

#' Are patterns so clear, that we could simply display raw data ?

#+ fig.width = 16, fig.height = 12
ggplot(DT_pred_long[variable == "Life style" ],
       mapping = aes(x = value, y = pred.V1, color = target)) +
  geom_point(size = 0.1, alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "line") +
  facet_wrap(.~var_code, scales = "free")

#' That does not seem to be the case. In the following we shall
#' Aim at statistical summaries.

# Let's write some code analogous to get_extras() just doing this summing

get_extras2 <- function(path)
{
  # extra files
  extras <-  c(
    "alcohol.csv",
    "physical_activity.csv",
    "sleep.csv",
    "smoking.csv")

  summary_vars <- list(
    "Alcohol" = c(
      "Average weekly intake of other alcoholic drinks",
      "Average weekly fortified wine intake",
      "Average weekly spirits intake",
      "Average weekly beer plus cider intake",
      "Average weekly champagne plus white wine intake",
      "Average weekly red wine intake"),
    "Activity" = c("Summed MET minutes per week for all activity"),
    "Sleep" = c("Sleep duration"),
    "Smoking" = c("Pack years of smoking"))

  summary_names <- setNames(
    c("Summed weekly drinks", "Summed MET actvity", "Sleep duration",
      "Pack years"),
    names(summary_vars)
  )
  # build lookup tables
  # This allows us to to elegantly (vectortized dict!) add extra info later.
  lookup_names <- fread(file.path(extra_path, "names.csv"))

  lookup_codes <- setNames(
    unlist(lookup_names[1,-1]),
    unlist(lookup_names[2,-1]))

  lookup_family <- setNames(
    rep(c("Alcohol", "Sleep", "Smoking", "Activity"), c(16, 7, 2, 9)),
    names(lookup_codes))

  # read first file and do recursive join using DT syntax
  DTS <- NULL
  for (ii in seq_along(extras)){
    DT <- fread(file.path(extra_path, extras[ii]))
    family <- names(summary_vars)[[ii]]

    cols <- summary_vars[[family]]
    sel <- unname(lookup_codes[cols])
    DT <- DT[, c("eid", sel), with = F]
    # set  values < 0 to NA
    for(j in seq_along(DT)[-1]){
      set(DT, i = which(DT[[j]] < 0), j = j, value = NA)
    }
    # get absolutely NA rows
    na_index <- rowSums(is.na(DT[, sel, with = F])) == length(sel)
    # compute sums
    DT[, value := rowSums(.SD, na.rm = T), .SDcols = sel]
    # replaces wrong zeros for NAs
    DT$value[na_index] <- NA
    DT[, family := family]  # set family
    DT[, name := summary_names[family]]
    DT <- DT[, -sel, with = F]  # kick out inputs
    DTS[[ii]] <- DT  # append to list
  }
  DTS <- do.call(rbind, DTS)
  DTS[, eid := factor(eid)]
  setorder(DTS, eid)
  DTS
}

DT2 <- get_extras2(extra_path)

#' Let's take a quick glimpse at the results.

#+ fig.width = 10, fig.height = 5
(fig_dt2 <- ggplot(DT2) +
  aes(x = value, color = family, fill = family) +
  geom_histogram() + facet_wrap(.~family, scales = "free"))

#' That looks heavy tailed in most cases. Let's see if a simple sqrt transform
#' can help (variance stabilizing + defined for 0).

fig_dt2 + scale_x_sqrt()

#' That looks pretty acceptable. We will use sqrt transforms in what follows.

DT2[, value := sqrt(value)]

DT_pred_wide2 <- merge(DT_pred_wide, DT2, by = "eid",
                      allow.cartesian = T)

#' When analyzing the association between proxy outputs and external variables
#' the correlations between the targets. Kick in. How much overlap do we find?
corr.table <- DT_pred_wide2[
  family == "Sleep" & variable != "Age, Sex" & evaluation  == "Generalization",
  {
    list(
      age_fi = cor.test(age, FI_pred)$estimate,
      age_n = cor.test(age, N_pred)$estimate,
      age_delta_fi = cor.test(age_pred - age, FI_pred)$estimate,
      age_delta_n = cor.test(age_pred - age, N_pred)$estimate,
      fi_n = cor.test(FI_pred, N_pred)$estimate
    )
  },
  by = "variable"]

knitr::kable(corr.table)
tb1 <- knitr::kable(corr.table, format = "latex")
writeLines(tb1, "targets-corr.tex")
#'There is substantial overlap but after removing computing the BA-delta
#'that overlap is substantially reduced at least when age is implied.
#'Overall, to be sure we extract specific correlations, we shall use multiple
#'regression.

#'Another related topic is brain age bias. Our predictions of age won't
#'be perfect, hence, we're exposed to the risk of seeing essentially age
#'everywhere. Let's look at age performance on test set.
mae.table <- DT_pred_wide2[
  family == "Sleep" & variable != "Age, Sex" & evaluation  == "Generalization",
  .(MAE = mean(abs(age_pred - age))),
  by = "variable"]
knitr::kable(mae.table)

#'Best performance is around 3.3 years MAE ... but we can see that we suffer
#'from substantial brain age bias: in extreme groups erorrs are larger!

ggplot(DT_pred_wide[variable != "Age, Sex"]) +
  aes(y = abs(age_pred - age), x = age) +
  geom_hex() +
  scale_fill_viridis_c() +
  facet_wrap(.~variable) +
  geom_text(
    data = mae.table, x = 50, y = 20, aes(label = sprintf("MAE = %0.1f", MAE)),
    show.legend = F, color = "black")

#' In the next step we shall establish that delta-style deconfounding using the
#' training set as suggested in Chyzhyk et al. 2019 (... in press)

fit_train_deconfound <- function(train, test, var, confound,
                                 default.fml = "%s ~ poly(%s, degree = 2)")
{
  # fit confounding model on train set
  train <- na.omit(train[, c(confound, var), with = F])
  var_s <- paste0(var, "_s")
  conf_s <- paste0(confound, "_s")

  train[[var_s]] <- scale(train[[var]])
  train[[conf_s]] <- scale(train[[confound]])

  fml <- as.formula(
    sprintf(default.fml, var_s, conf_s, env = train))
  mod_confound <- lm(fml, train)

  # apply on test set
  test[[var_s]] <- scale(test[[var]],
                         attr(train[[var_s]], "scaled:center"),
                         attr(train[[var_s]], "scaled:scale"))
  test[[conf_s]] <- scale(test[[confound]],
                          attr(train[[conf_s]], "scaled:center"),
                          attr(train[[conf_s]], "scaled:scale"))
  attributes(test[[var_s]]) <- NULL
  attributes(test[[conf_s]]) <- NULL
  list(pred = predict(mod_confound, test),
       mod = mod_confound)
}

#' Here comes the main operation.
#' We do a split-apply-combine framework using the data.table
#' syntax. In the apply block we'll do:
#' 
#' 1. confound modeling on training data
#' 2. multiple regression to obtain partial correlation estimates
#'    of the proxy measures with regard to the extra variable.
#' 3. xtract quantities of interest.

# Using confound as in Engemann 2020 eqs 6-8
DT_corr1 <- DT_pred_wide2[
  variable != "Age, Sex",  # XXX fill in age in missing cell.
  {
    test <- .SD[evaluation == "Generalization"]

    age_delta <- with(test, age_pred - age)  # ... XXX

    mod <- lm(scale(value) ~           # y
              poly(scale(age), 3) +    # confound_train XXX
              scale(age_delta) +       # brain age delta
              scale(FI_pred) +              # Fluid Intelligence
              scale(N_pred),           # Neuroticism
              test)

    coefs <- coef(summary(mod))
    out <- data.frame(
      coef = coefs[-c(1:4), 1],  # kick out intercept + confound term
      se = coefs[-c(1:4), 2],    # ... and get columns for coef and error
      p.val = coefs[-c(1:4), 4],
      target = c("age_delta", "FI", "N"))
    out['nobs'] = nobs(mod)
    out['name'] = name[1]
    out
  },
  by = .(variable, family)]

# Using train-based confound
DT_corr <- DT_pred_wide2[
  variable != "Age, Sex",  # XXX fill in age in missing cell.
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

    coefs <- coef(summary(mod))
    out <- data.frame(
      coef = coefs[-c(1, 2), 1],  # kick out intercept + confound term
      se = coefs[-c(1, 2), 2],    # ... and get columns for coef and error
      p.val = coefs[-c(1, 2), 4],
      target = c("age_delta", "FI", "N"))
    out['mod_cf'] <- nobs(deconf[['mod']])
    out['nobs'] = nobs(mod)
    out['name'] = name[1]
    out
  },
  by = .(variable, family)]

#' Is deconfounding via train set correctly implemented and consistent with
#' approach from Engemann et al 2020? The answer seems yes:
#' it's at least equally good/bad.

test.res <- cor.test(
  DT_corr1[target == "age_delta"]$coef,
  DT_corr[target == "age_delta"]$coef, method = "spearman")

ggplot(mapping = aes(
  x = DT_corr1[target == "age_delta"]$coef,
  y = DT_corr[target == "age_delta"]$coef)) +
  geom_point() +
  annotate(x = -0.1, y = 0.2, geom = "text",
           label = sprintf("rho = %0.2f", test.res$estimate))

#+ fig.height = 12, fig.width = 16
ggplot(data = DT_corr,
       mapping = aes(y = coef, color = family, x = name)) +
  geom_linerange(
    size = 1.5,
    aes(ymin = coef - se, ymax = coef + se)) +
  geom_point(shape = 21, fill = "white", size = 2.5) +
  facet_grid(variable~target) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme_minimal() + coord_flip() +
  labs(x = "Variable Codes", y = "Correlation")

#' So far we have computed models grouped by predictor models. Hower, this
#' brings the slight disadvantage of not combining the best models as different
#' models were most predictive for each target. We can do better. Let's select
#' subject the best predictions to multiple regression.
DT_pred_wide3 <- DT_pred_wide2[variable == 'Life style',
  .(eid, age, age_pred, evaluation, value, family, name)]

DT_tmp <- DT_pred_wide2[variable == 'Mental Health', .(eid, N, N_pred, family)]
setkey(DT_tmp, eid, family)
setkey(DT_pred_wide3, eid, family)
DT_pred_wide3[DT_tmp, `:=`(N_pred = N_pred, N = N)]

DT_tmp <- DT_pred_wide2[variable == 'Education', .(eid, FI, FI_pred, family)]
setkey(DT_tmp, eid, family)
DT_pred_wide3[DT_tmp, `:=`(FI_pred = FI_pred, FI = FI)]


plot_corr <- function(data, data_mean, data_swarm, title)
{
  set.seed(33)
  data_swarm <- data[,.(coef = sample(coef, 200, replace = F)), 
                     by = .(target, family, name)]

  this_colors <- with(color_cats, c(orange, `blueish green`, blue))
  fig <- ggplot(data = data) +
    aes(y = coef, fill = target, color = target, x = name) +
    geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.8) +
    stat_summary(fun.data = my_quants, geom = 'boxplot', outlier.shape = NA,
                 alpha = 0.3, width = 0.8) +
    stat_summary(fun.data = my_quants, geom = 'errorbar', width = 0.5) +
    geom_text(
      data = data_mean,
      mapping = aes(,
        y = coef,
        label = sprintf("%1.2f", coef)),
      size = 2.6,
      hjust = 0.5,
      vjust = 5.5,
      show.legend = F,
      position = position_dodge(width = -0.6)) +
    geom_beeswarm(
      data = data_swarm,
      size = 0.7, cex = 0.8, alpha = 0.2, priority = "random") +
    facet_grid(.~target) +
    scale_color_manual(values = this_colors) +
    scale_fill_manual(values = this_colors) +
    scale_x_discrete(label = c(
      "# Cigarettes smoked\n(Pack-Years)",
      "Sleep duration (hours)",
      "Metabolic Equivalent Task\n(minutes/week)",
      "# Alcoholic beverages")) +
    my_theme +
    coord_flip(ylim = c(-0.25, 0.25)) +
    guides(
      color = guide_legend(nrow = 1, label.hjust = 1, title = element_blank()),
      fill = guide_legend(nrow = 1, label.hjust = 1, title = element_blank())
    ) +
    theme(
      legend.position = "none",
      legend.justification = "left",
      panel.grid.major.y = element_line(size = 1.5)
    ) +
    labs(
      title = title,
      x = element_blank(),
      y = bquote(
        beta[proxy]%+-%scriptstyle(bootstrap-based~uncertainty~estimates)))
  return(fig)
}

###############################################################################

#' Get point estimates + standard outputs for marginal proxy model

DT_corr3_margin_proxy <- DT_pred_wide3[,
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
               scale(age_delta), test)     # Neuroticism

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
    out['mod_cf'] <- nobs(deconf[['mod']])
    out['nobs'] = nobs(mod1)
    out['name'] = name[1]
    out
  },
  by = family]

# ... for marginal target model
DT_corr3_margin_target <- DT_pred_wide3[,
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
               scale(age), test)     # Neuroticism

    mod2 <- lm(scale(value) ~           # y
               scale(value_adjusted) +  # confound_train XXX
               scale(FI), test)     # Neuroticism

    mod3 <- lm(scale(value) ~           # y
               scale(value_adjusted) +  # confound_train XXX
               scale(N), test)     # Neuroticism

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
      target = c("Age", "Fluid Intelligence", "Neuroticism"))
    out['mod_cf'] <- nobs(deconf[['mod']])
    out['nobs'] = nobs(mod1)
    out['name'] = name[1]
    out
  },
  by = family]

# get parametric bootstrap for marginal target model
DT_corr3b_margin_boot_target <- DT_pred_wide3[,
  {
    train <- .SD[evaluation == "Validation"]
    test <- .SD[evaluation == "Generalization"]
    deconf <- fit_train_deconfound(
      train = train, test = test, var = "value", confound = "age")
    value_adjusted <- deconf[['pred']]


    mod1 <- lm(scale(value) ~           # y
               scale(value_adjusted) +  # confound_train XXX
               scale(age), test)     # Neuroticism

    mod2 <- lm(scale(value) ~           # y
               scale(value_adjusted) +  # confound_train XXX
               scale(FI), test)     # Neuroticism

    mod3 <- lm(scale(value) ~           # y
               scale(value_adjusted) +  # confound_train XXX
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

(
  fig_corr_margin_target <- plot_corr(
    data = DT_corr3b_margin_boot_target,
    data_mean = DT_corr3_margin_target,
    title = 'A (target measure)')
)

# get parametric bootstrap for marginal proxy model
DT_corr3b_margin_boot_proxy <- DT_pred_wide3[,
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


(
  fig_corr_margin_proxy <- plot_corr(
    data = DT_corr3b_margin_boot_proxy,
    data_mean = DT_corr3_margin_proxy,
    title = 'A (proxy measure)')
)

big_fig2 <- (fig_corr_margin_proxy / fig_corr_margin_target) + 
  plot_annotation(
    title = 'Marginal associations of target and proxy measures with health-related habits')

my_ggsave("fig_posthoc_combined_margin",
          plot = big_fig2,
          width = 12, height = 9)

