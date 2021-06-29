#'---
#'title: "Rebuttal on proxy measure correction"
#'author: "Denis A. Engemann"
#'date: "5/21/2021"
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
source('../../config.r')
source('../../utils.r')

extra_path <- file.path('..', '..', 'figure-2', 'inputs')

new_path <- file.path('.', 'inputs')

#' Get together all input files.
#' First the extra data files. We'll write a function for that.
#'

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

read_file_parts <- function(fname, path1, path2){
  fnames <- list.files(
    path1, paste0(strsplit(fname, ".csv")[[1]], "_"), full.names = T)
  
  # make sure not to use the old age outputs
  fnames <- Filter(function(x) {!(endsWith(x, 'age.csv') |
                                  endsWith(x, 'age_at_assessment.csv'))},
                   fnames)  
  
  fnames2 <- list.files(
    path2, paste0(strsplit(fname, ".csv")[[1]], "_"), full.names = T)

  fnames <- c(fnames, fnames2)

  rbindlist(lapply(fnames, fread))
}

assemble_inputs <- function(fname, path1, path2)
{
  DT_pred <- read_file_parts(fname, path1 = path1, path2= path2)
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

fname <-
  'post_predictive_analysis_all_models_combined_imaging_non_imaging.csv'

DT_pred <- assemble_inputs(fname, path1 = extra_path, path2 = new_path)


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


plot_corr <- function(data, data_mean, data_swarm, title, show.xlab = T)
{
  set.seed(33)
  data_swarm <- data[,.(coef = sample(coef, 200, replace = F)), 
                     by = .(target, family, name)]

  this_colors <- with(color_cats, c(orange, `blueish green`, blue))
  fig <- ggplot(data = data) +
    aes(y = coef, fill = target, color = target, x = name) +
    geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.8) +
    stat_summary(fun.data = my_quants, geom = 'boxplot', outlier.shape = NA,
                 alpha = 0.3, width = 0.8, size=0.8) +
    stat_summary(fun.data = my_quants, geom = 'errorbar', width = 0.5) +
    # geom_text(
    #   data = data_mean,
    #   mapping = aes(,
    #     y = coef,
    #     label = sprintf("%1.2f", coef)),
    #   size = 2.6,
    #   hjust = 0.5,
    #   vjust = 4.4,
    #   show.legend = F,
    #   position = position_dodge(width = -0.6)) +
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
    coord_flip(ylim = c(-0.27, 0.27)) +
    guides(
      color = guide_legend(nrow = 1, label.hjust = 1, title = element_blank()),
      fill = guide_legend(nrow = 1, label.hjust = 1, title = element_blank())
    ) +
    theme(
      legend.position = "none",
      legend.justification = "left",
      panel.grid.major.y = element_line(size = 1.5)
      # plot.title = element_text(vjust=-1.5, hjust=1.5)
    ) +
    labs(
      title = title,
      x = element_blank())

  if (show.xlab) {
    fig <- fig + labs(
      y = bquote(
        beta[proxy]%+-%scriptstyle(bootstrap-based~uncertainty~estimates)))
  } else {
    fig <- fig + labs(y = element_blank()) + theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank())
  }
  return(fig)
}

(
  fig_corr_proxy <- plot_corr(
    data = DT_cond_proxy_bs,
    data_mean = DT_cond_proxy,
    show.xlab = F,
    title = element_blank())
)

(
  fig_corr_target <- plot_corr(
    data = DT_cond_target_bs,
    data_mean = DT_cond_target,
    title = element_blank())
)

big_fig <- (fig_corr_proxy / fig_corr_target) + 
  plot_annotation(
    title = 'Specific associations for proxy and target measures with health-related habits')

my_ggsave("figure_2",
          plot = big_fig,
          width = 12, height = 6)

###############################################################################

#' Get point estimates + standard outputs for marginal proxy model

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

(
  fig_corr_margin_proxy <- plot_corr(
    data = DT_margin_proxy_bs,
    data_mean = DT_margin_proxy,
    show.xlab = F,
    title = element_blank())
)
(
  fig_corr_margin_target <- plot_corr(
    data = DT_margin_target_bs,
    data_mean = DT_margin_target,
    title = element_blank())
)

big_fig2 <- (fig_corr_margin_proxy / fig_corr_margin_target) + 
  plot_annotation(
    title = 'Marginal associations of target and proxy measures with health-related habits')

my_ggsave("figure_2_supp_1",
          plot = big_fig2,
          width = 12, height = 6)

