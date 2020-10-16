library(ggplot2)
library(ggbeeswarm)
source('config.r')
source('utils.r')

data <- read.csv("./imaging.csv")

data$fold <- factor(data$fold)

data$modality <- factor(
    data$modality,
    levels = c("brain volumes (sMRI)", "white matter (dMRI)", 
               "functional connectivity (fMRI)", "sMRI, dMRI",
               "sMRI, fMRI", "dMRI, fMRI", "sMRI, dMRI, fMRI (full MRI)"),
    labels = c("sMRI", "dMRI", "fMRI", "sMRI, dMRI", "sMRI, fMRI",
               "dMRI, fMRI", "full MRI")
)

data$model_type <- "full MRI"
data$model_type[grepl(",", data$modality)] <- "multi-modal"
data$model_type[data$modality == 'sMRI'] <- "mono-modal"
data$model_type[data$modality == 'dMRI'] <- "mono-modal"
data$model_type[data$modality == 'fMRI'] <- "mono-modal"
data$model_type <- factor(data$model_type,
                          levels = c("full MRI", "multi-modal", "mono-modal"))

dodge <- 0.6

my_fmt <- function(x){
  n <- 2
  format(round(x, n), nsmall = n, scientific = F)
}

dat1 <- subset(data, model_testing == "validation" &
               Permuted == "no")
dat2 <- subset(data, model_testing != "validation" &
               Permuted == "no")

dat3 <- rbind(dat1, dat2)
dat3$model_testing <- factor(dat3$model_testing,
                             levels = c("generalization", "validation"))

this_colors <- with(color_cats, c(orange, `blueish green`, blue))

fig <- ggplot(
    data = dat1,
    mapping = aes(y = r2_score,
                  x = modality,
                  fill = target,
                  color = target)) +
    facet_wrap(.~target, nrow = 1, scale="free_x") +
    geom_violin(width = 1.,
	       	position = position_dodge(width = dodge),
		trim = F, alpha = 0.6, show.legend = T) +
    scale_y_continuous(labels = my_fmt) +
    scale_shape_manual(values=c(21, 22, 23)) +
    stat_summary(
       fun.y = mean, geom = "point", size = 1.5,
       shape = 21,
       fill = 'white',
       show.legend = F,
       position = position_dodge(width = dodge)) +
    # stat_summary(
    #   data = dat2,
    #   fun.y = mean, geom = "point", size = 1.5,
    #   fill = 'white', shape = 24,
    #   position = position_dodge(width = 0.4), show.legend = F) +
    stat_summary(
        geom = 'text',
        mapping = aes(label = sprintf("%1.2f", ..y..)),
        fun.y = mean, size = 2.55,
       	show.legend = FALSE,
        color = 'black',
	hjust = -0.04,
	vjust = 2.6,
        position = position_dodge(width = dodge)) +
    scale_color_manual(values = this_colors) +
    scale_fill_manual(values = this_colors) +
    guides(fill = F, linetype = F, alpha = F, color = F) +
    theme_minimal() +
    theme(legend.position = c(0.1, 1.15)) +
    coord_flip() +
    theme(text = element_text(family = 'Helvetica', size = 12),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          panel.spacing.x = unit(0.03, "npc"),
	        panel.grid.major.y = element_line(size = 1.5),
          strip.text.x = element_text(hjust = 0.5),
          axis.text = element_text(size = 12),
	  axis.title.y = element_blank()) +
    ylab(bquote(R^2%+-%~scriptstyle("CV-based uncertainty estimates"))) +
    ggtitle("Approximation quality based on Brain Imaging") +
    labs(tag = expression("Using" * " " *symbol('\257') * " " * "to predict:")) +
    theme(plot.tag.position = c(0.07, 0.89),
	  plot.tag = element_text(size = 9.5),
	  plot.title = element_text(hjust = 1.0))
    # theme(panel.spacing = unit(5, "lines"))

print(fig)

fname <- 'figure_2_supplement_1'
my_ggsave(fname = fname, plot = fig, height = 3.8, width = 7)
