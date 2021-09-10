library(data.table)
library(ggplot2)
library(ggbeeswarm)
library(patchwork)
source('../config.r')
source('../utils.r')


data <- fread('mod24_cond_big.csv')

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
    coord_flip(ylim = c(-0.3, 0.3)) +
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
          width = 13, height = 6.5)
