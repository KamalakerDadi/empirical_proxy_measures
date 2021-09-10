library(data.table)
library(ggplot2)
library(ggbeeswarm)
library(patchwork)
source('../config.r')
source('../utils.r')

DT_cond_proxy <- fread('mod22_cond_proxy.csv')
DT_cond_proxy_bs <- fread('mod22_cond_proxy_bs.csv')
DT_cond_target <- fread('mod22_cond_target.csv')
DT_cond_target_bs <- fread('mod22_cond_target_bs.csv')


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

my_ggsave("figure_2_supp_2",
          plot = big_fig,
          width = 12, height = 6)
