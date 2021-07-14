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
source('./read_data.r')

extra_path <- file.path('..', '..', 'figure-2', 'inputs')

fname <-
  'post_predictive_analysis_all_models_combined_imaging_non_imaging.csv'

DT_pred <- assemble_inputs(fname, path1 = extra_path)


#' Get point estimates + standard outputs for conditional target model
DT_cond_proxy <- DT_pred[,
  {
    train <- .SD[evaluation == "Validation"]
    test <- .SD[evaluation == "Generalization"]
    
    value_adj_age <- fit_train_deconfound(
      train = train, test = test, var = "value",
      confound = "age")[['pred']]

    value_adj_fi <- fit_train_deconfound(
      train = train, test = test, var = "value",
      confound = "FI")[['pred']]

    value_adj_n <- fit_train_deconfound(
      train = train, test = test, var = "value",
      confound = "N")[['pred']]

    # fit association model on test set
    mod <- lm(scale(value) ~           # y
              scale(value_adj_age) +   # confound_train age
              scale(age_pred) +        # brain age
              scale(value_adj_fi) +    # confound_train fI
              scale(FI_pred) +         # Fluid Intelligence
              scale(value_adj_n) +     # confound_train N
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

    value_adj_age <- fit_train_deconfound(
      train = train, test = test, var = "value",
      confound = "age")[['pred']]

    value_adj_fi <- fit_train_deconfound(
      train = train, test = test, var = "value",
      confound = "FI")[['pred']]

    value_adj_n <- fit_train_deconfound(
      train = train, test = test, var = "value",
      confound = "N")[['pred']]

    # fit association model on test set

    mod <- lm(scale(value) ~           # y
              scale(value_adj_age) +   # confound_train age
              scale(age_pred) +        # brain age
              scale(value_adj_fi) +    # confound_train fI
              scale(FI_pred) +         # Fluid Intelligence
              scale(value_adj_n) +     # confound_train N
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
              scale(age) +       # brain age
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

my_ggsave("figure_2_supp_3",
          plot = big_fig,
          width = 12, height = 6)
