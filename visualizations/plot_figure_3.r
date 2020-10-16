library(data.table)
library(ggplot2)
library(ggbeeswarm)
source('config.r')
source("utils.r")

data <- read.csv("./classification.csv")
data2 <- read.csv("./classification_non_imaging_fresh.csv")

data <- rbind(
  subset(data, Type %in% c('Imaging', 'Imaging & Non-imaging')),
  data2
)

#data <- subset(data, model_testing == "validation")
this_colors <- with(color_cats, c(orange, `blueish green`, blue))

dodge <- 0.6
    
dat1 <- subset(
  data, model_testing == "validation" &
  Permuted == 'no' & data$group %in% c('low vs high'))

dat1_agg <- aggregate(roc~target*Type + model_testing, data = dat1, FUN = mean)

# get brain results used for stats tables
data_brain_agg <- read.csv("brain_inference_class.csv")[,c("target", "t_obs")]
names(data_brain_agg)[[2]] <- "roc"
data_brain_agg$Type <- "Imaging"

# get joint results used for stats tables
dat2 <- fread("gen_observed_aucs.csv")
setorder(dat2, imaging, target)
setnames(dat2, c("auc_score"), c("roc"))
dat2[, Type := "Imaging & Non-imaging"]
dat2[imaging == "no", Type := "Non-imaging"]
dat2[, "imaging"] <- NULL
dat2 <- rbind(data_brain_agg, dat2)
dat2[, model_testing := "generalization"]

dat3 <- rbind(dat1_agg, dat2)

dico <- setNames(
  c("Brain Imaging", "Sociodemographics", "Brain Imaging &\nSociodemographics"),
  c("Imaging", "Non-imaging", "Imaging & Non-imaging")
)

fig <- ggplot(
  data = dat1,
  mapping = aes(y = roc,
                # linetype = Type,
                x = Type,
                fill = target, color = target)) +
  facet_wrap(.~target, scales = "free_x") +
  geom_violin(width = 0.6,
              size = 1,
              position = position_dodge(width = dodge),
	      show.legend = F,trim = T, alpha = 0.5) +
  geom_point(
    data = dat3,
    mapping = aes(shape = model_testing, size = model_testing),
    fill = 'white',
    position = position_dodge(width = 0.2), show.legend = T) +
  scale_shape_manual(values = c(24, 21)) +
  scale_size_manual(values = c(2.5, 1.5)) +
  scale_x_discrete(labels = dico) +
  stat_summary(
      geom = 'text',
      mapping = aes(label = sprintf("%1.2f",
                    ..y..)),
      fun.y = mean, size = 3,
	show.legend = FALSE,
      color = 'black',
	alpha = 1,
	vjust = -1.2,
	hjust = 1.2,
      position = position_dodge(width = dodge)) +
  theme_minimal() +
  guides(
     size = guide_legend(title = element_blank(), nrow = 2),
     shape = guide_legend(title = element_blank(), nrow = 2),
	   fill = F,
	   color = F) +
  theme(legend.position = c(0.12, 0.65)) +
  coord_flip() +
  scale_linetype_manual(values = c("dotted", "solid", "dashed")) +
  scale_color_manual(values = this_colors) +
  scale_fill_manual(values = this_colors) +
  geom_hline(yintercept = 0.5, alpha = 0.3) +
  theme(title = element_text(family = "Helvetica", size = 14),
        text = element_text(family = 'Helvetica', size = 14),
        strip.text.x = element_text(hjust = 0.5, size = 14),
        panel.spacing.x = unit(0.03, "npc"),
        panel.grid.major.y = element_line(size = 1.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
	axis.title.y = element_blank()) +
  ylab(bquote("ROC-score"%+-%scriptstyle("CV-based uncertainty estimation"))) +
  labs(title =  "Extreme-group classification with proxy measures \nderived from sociodemographics and brain imaging"
       # subtitle = expression("low versus high" * " " * symbol('\257'))
     ) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12))

print(fig)

fname <- 'classification_low_vs_high_validation'
my_ggsave(fname = fname, plot = fig, height = 3.3, width = 10)
