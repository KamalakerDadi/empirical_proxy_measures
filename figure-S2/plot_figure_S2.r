library(data.table)
library(ggplot2)
library(patchwork)

source('../utils.r')
source('../config.r')

old_age_path <- file.path(
  "..", "figure-2", "inputs",
  "post_predictive_analysis_all_models_combined_imaging_non_imaging_age.csv")

df_old <- fread(old_age_path)
df_old <- df_old[evaluation == "Generalization"]


new_age_path <- file.path(
  "inputs",
  "post_predictive_analysis_all_models_combined_imaging_non_imaging_age_at_assessment.csv")

df_new <- fread(new_age_path)
df_new <- df_new[evaluation == "Generalization"]


common_eid <- intersect(unique(df_new$eid), unique(df_old$eid))

sprintf(
  "There are %d subjects in common betwee AGE at 1st visit and AGE at scan time",
  length(common_eid))

df_old <- df_old[df_old$eid %in% common_eid][order(eid)]
df_new <- df_new[df_new$eid %in% common_eid][order(eid)]

stopifnot(all(df_new$eid == df_old$eid))

old_pred <- df_old[,.(predicted = mean(predicted), true = unique(true)), .(eid)]

new_pred <- df_new[,.(predicted = mean(predicted), true = unique(true)), .(eid)]



df_age_comp <- data.frame(old_age_pred = old_pred$predicted,
                          new_age_pred = new_pred$predicted,
                          old_age_true = old_pred$true,
                          new_age_true = new_pred$true)

c1 <- cor.test(old_pred$true, new_pred$true)
c2 <- cor.test(old_pred$predicted, new_pred$predicted)

color <- color_cats$`sky blue`

get_corr_label <- function(c_res)
{
  bquote(r == .(round(c_res$estimate, 3))*
         ",  "~italic(p)~.(format.pval(c_res$p.val)))
}

(
  fig1 <- ggplot(data = df_age_comp,
                mapping = aes(x = (new_age_true - old_age_true))) +
    geom_histogram(binwidth = 0.9, bins = 9, fill = color, color = color) +
    my_theme +
    coord_cartesian(xlim = c(0, 13)) +
    labs(x = "difference in chronological age:\nscan time - at first visit",
         y = "count") +
    annotate('text', x = 4, y = 1400, label = get_corr_label(c1), size = 5)
)

(
  fig2 <- ggplot(data = df_age_comp,
                mapping = aes(x = old_age_pred, y = new_age_pred)) +
    geom_point(size = 0.3, alpha = 0.3, color = color) +
    my_theme +
    labs(x = "predicted age (first visit)", y = "predicted age (scan time)") +
    annotate('text', x = 59, y = 55, label = get_corr_label(c2), size = 5)
)

(fig1 | fig2) + plot_annotation(tag_levels = 'A') -> big_fig

my_ggsave('fig_sup_2', big_fig, width = 10, height = 5)
