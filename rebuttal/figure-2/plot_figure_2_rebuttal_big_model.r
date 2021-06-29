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
library(stargazer)
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


get_family_model <- function(DT_family)
  {
    mod <- lm(scale(value) ~            # y
              scale(age_pred) +         # brain age
              scale(age) +              # age
              scale(FI_pred) +          # Fluid Intelligence
              scale(FI) +               # Fluid Intelligence
              scale(N_pred) +
              scale(N),
              DT_family)                     # Neuroticism
    mod
}

get_coefs <- function(mod, coef_names)
{ 
    coefs <- t(coef(summary(mod)))
    out <- coefs[, coef_names]
    out <- t(out)[, c('Estimate', 'Std. Error', 'Pr(>|t|)')]
    out <- data.frame(out)
    names(out) <- c('coef', 'se', 'p.val')
    out
}

get_bootstrap <- function(mod, coef_names, n.sim = 10000)
{
    mod.sim <- arm::sim(mod, n.sim = n.sim)
    out <- data.table(mod.sim@coef)
    
    has_scale <- all(sapply(coef_names, function(s) startsWith(s, "scale(")))
    if (has_scale)
    {
      out_names <- gsub("scale(", "", coef_names, fixed = T)
      out_names <- gsub(".{1}$", "", out_names)       
    } else {
      out_names <- coef_names
    }

    data.table(
      name = rep(out_names, each = n.sim),
      coef = do.call(c, lapply(coef_names, function(name) out[[name]])))
}

fname <-
  'post_predictive_analysis_all_models_combined_imaging_non_imaging.csv'

DT_pred <- assemble_inputs(fname, path1 = extra_path, path2 = new_path)[
  evaluation == "Generalization"
]

split_dt <- split(DT_pred, DT_pred$family) 
mod_list <- vector(mode = "list", length = length(split_dt)) 
bs_list <- vector(mode = "list", length = length(split_dt))

for (ii in 1:length(split_dt))
{
  mod <- get_family_model(split_dt[[ii]]) 
  mod_list[[ii]] <- mod
  coef_names <- names(coef(mod))[-1]
  mod_bs <- get_bootstrap(mod, coef_names)
  mod_bs$family <- names(split_dt)[[ii]]
  bs_list[[ii]] <- mod_bs
}

dt_bs <- rbindlist(bs_list)

target_map <- setNames(
  c('age', 'age', 'FI', 'FI', 'N', 'N'),
  c('age_pred', 'age', 'FI_pred', 'FI', 'N', 'N_pred'))

dt_bs$target <- as.factor(target_map[dt_bs$name])
dt_bs$type <- "proxy"
dt_bs[!endsWith(name, "_pred"), type := "target"]
dt_bs$type <- factor(dt_bs$type, levels=c("target", "proxy"))

this_colors <- with(color_cats, c(orange, `blueish green`, blue))

family_levels <- rev(c("Smoking", "Sleep", "Activity", "Alcohol"))
family_labels <- rev(c(
  "# Cigarettes \n smoked\n(Pack-Years)",
  "Sleep \nduration \n(hours)",
  "Metabolic \nEquivalent Task \n(minutes/week)",
  "# Alcoholic \nbeverages"
))
dt_bs$family <- factor(
  dt_bs$family, levels = family_levels, labels = family_labels) 

dt_bs$target <- factor(dt_bs$target, levels = c("age", "FI", "N"),
                       labels = c("Age", "Fluid Intelligence", "Neuroticism"))

data <- dt_bs
data_swarm <- data[,.(coef = sample(coef, 200, replace = F)), 
                     by = .(target, family, name, type)]

fig <- ggplot(data = data) +
    aes(y = coef, fill = target, color = target, x = type) +
    geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.8) +
    stat_summary(fun.data = my_quants, geom = 'boxplot', outlier.shape = NA,
                 alpha = 0.3, width = 0.8, size=0.8) +
    stat_summary(fun.data = my_quants, geom = 'errorbar', width = 0.5) +
    geom_beeswarm(
      data = data_swarm,
      size = 0.7, cex = 0.8, alpha = 0.2, priority = "random") +
    scale_color_manual(values = this_colors) +
    scale_fill_manual(values = this_colors) +
    facet_grid(rows = vars(family), cols = vars(target),
               switch='y') +
    coord_flip(ylim = c(-0.27, 0.27)) +
    scale_x_discrete(position = "top") +  #  rotated semantics
    labs(
      title = bquote("Health-related habits jointly modeled from" ~
                     bold("proxy") ~ "and" ~ bold("target") ~ "measures"),
      x = element_blank(),
      y = bquote(
        beta[proxy]%+-%scriptstyle(bootstrap-based~uncertainty~estimates))) +
    my_theme +
    theme(
      strip.text = element_text(size = 13),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 18),
      legend.position = "none",
      legend.justification = "left",
      panel.grid.major.y = element_line(size = 1.5)
    ) 
print(fig)

my_ggsave("fig_2-supp-4",
          plot = fig,
          width = 13, height = 7)

col_names <-  c("predicted Age", "Age",
                "predicted Fluid Intelligence", "Fluid Intelligrence",
                "predicted Neuroticism", "Neuroticism")

# reorder models to be in line with the figure
mod_list_out <- mod_list[c(2, 1, 3, 4)] 
table <- stargazer(mod_list_out, summary = T,
                   dep.var.caption = "Health-related Habits",
                   dep.var.labels.include = F,
                   column.labels = names(mod_list_out),
                   covariate.labels = col_names)  

writeLines(table, "big_models_inference.tex")

vif_results <- do.call(data.frame, lapply(mod_list_out, function(x) car::vif(x)))
rownames(vif_results) <- col_names
writeLines(stargazer(vif_results, summary = F), "big_models_vifs.tex")
