library(data.table)
library(ggplot2)
library(ggbeeswarm)
library(grid)
library(ggpubr)
source('../config.r')
source('../utils.r')

# text grob
text_annotation <- function(label, x, y, size, color)
{
  gtext <- grid.text(label, x = unit(x, "npc"), y = unit(y, "npc"),
		     gp = gpar(fontsize = size, col = color))
  return(gtext)
}

get_data <-  function(imaging = T)
{
  if(imaging == T){
    fname <- "./inputs/imaging_non_imaging.csv"
  }else{
    fname <- "./inputs/non_imaging_fresh.csv"
  }

  data <- read.csv(fname)

  data$variable <- factor(
      data$variable,
      levels = c("Early Life (EL)",
                 "Education (EDU)",
                 "Life style (LS)",
                 "Age, Gender (AG)",
                 "Age, Sex (AS)",
		       "Mental health (MH)",
		       "Mental Health (MH)"),
      labels = c("Early Life",
                 "Education",
                 "Life Style",
                 "Age, Sex",
                 "Age, Sex",
                 "Mood &\n Sentiment",
                 "Mood &\n Sentiment")
  )

  data$fold <- factor(data$fold)

  # count commas for n modalities
  #data$group_type <- sapply((gregexpr(",", data$variable, fixed = T)),
  #                         function(x) sum(x > 0) + 1)
  #data$group_type <- factor(data$group_type)
  data$imaging <- ifelse(imaging, "yes", "no")
  return(data)
}

data <- rbind(get_data(imaging = F), get_data(imaging = T))

data_brain_agg <- read.csv("./brain_inference.csv")[,c("target", "t_obs")]
names(data_brain_agg)[[2]] <- "r2_score"
data_brain_agg <- data_brain_agg[order(data_brain_agg$target),]
data_brain_agg$text <- "Brain Imaging"
data_brain_agg$y <- c(4, 2, 5.01)

social_factors <- c(
  "Early Life",
  "Education",
  "Life Style",
  "Age, Sex",
  "Mood &\n Sentiment")

dodge <- 0.7
this_colors <- with(color_cats, c(orange, `blueish green`, blue))

dat1 <- subset(data, model_testing == "validation" &
               Permuted == "no" & variable %in% social_factors)

dat2 <- fread("./gen_observed_r2s.csv")
setorder(dat2, imaging, target, variable)
dat2[, variable := factor(
  dat2$variable,
  levels = c("Age, Sex", "Early Life", "Education", "Life style", "Mental Health"),
  labels = c("Age, Sex", "Early Life", "Education", "Life Style", "Mood &\n Sentiment"))]

make_figure <- function(dat1, dat2, data_brain_agg, title, fname)
{
  fig <- ggplot(
      data = dat1,
      mapping = aes(y = r2_score, x = variable, color = target,
		    fill = target, linetype = imaging)) +
    geom_hline(yintercept = 0, alpha = 0.3) +
    geom_violin(width = 1.5,
                size = 1,
            		position = position_dodge(width = dodge),
            		show.legend = T,
            		trim = T, mapping = aes(alpha = imaging)) +
    scale_alpha_manual(values = c(0.1, 0.6)) +
    stat_summary(
      data = dat1,
      shape = 21,
      fun = mean, geom = "point", size = 1.5,
      fill = "white", show.legend = F,
      position = position_dodge(width = 0.7)) +
    geom_point(
      data = dat2,
      shape = 24,
      size = 2,
      fill = "white", show.legend = F,
      position = position_dodge(width = 0.3)) +
    stat_summary(
      geom = 'text',
      mapping = aes(label = sprintf("%1.2f", ..y..)),
      fun = mean, size = 2.8,
      color = 'black',
      hjust = 1.1,
      vjust = -1.2,
      show.legend = F,
      position = position_dodge(width = dodge)) +
    coord_flip() +
    scale_linetype_manual(values = c("dotted", "solid")) +
    scale_color_manual(values = this_colors) +
    scale_fill_manual(values = this_colors) +
    facet_wrap(.~target, scales = "free_x") +
    theme_minimal()
    # best brain-only model
  
  if (!is.null(data_brain_agg)){
    fig <- fig + geom_hline(
      data = data_brain_agg, mapping = aes(yintercept = r2_score),
      linetype = 'dashed', size = 0.6, alpha = 0.3) +
      geom_text(data = data_brain_agg,
                angle = 90,
                vjust = 1.4,
                inherit.aes = F,
                size = 2.9,
                mapping = aes(
                  y = r2_score, x = y, label = text))
  }
  
  fig <- fig + guides(
      shape = guide_legend(title = element_blank, nrow =1),
      linetype = guide_legend(title = "Brain Imaging",
                              nrow = 1,
			                      title.position = 'left'),
      fill = F,
      alpha = guide_legend(title = "Brain Imaging",
                           nrow = 1,
                           title.position = "left"),
      color = F) +
    theme(
      # legend.position = "bottom",
      legend.position = c(0.1, -.1),
      legend.direction = "horizontal") +
    theme(title = element_text(family = "Helvetica", size = 14),
          text = element_text(family = 'Helvetica', size = 14),
          strip.text.x = element_text(hjust = 0.5, size = 14),
	        panel.spacing.x = unit(0.01, "npc"),
          panel.grid.major.y = element_line(size = 1.5), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          axis.text = element_text(size = 14),
	  axis.title.y = element_blank()) +
    ylab(bquote(R^2%+-%~scriptstyle("CV-based uncertainty estimates"))) +
    labs(tag = expression("Using" * " " *symbol('\257') * " " * "to predict:")) +
    theme(
      plot.tag.position = c(0.078, 0.904),
	  plot.tag = element_text(size = 12),
	  plot.title = element_text(hjust = 1.0)) +
    ggtitle(title)

  new_fig <- ggarrange(fig)

  mhealth = "Frequency of \ntenseness, low mood, ..."
  new_fig <- new_fig + annotation_custom(text_annotation(label = mhealth,
						       x = 0.07, y = 0.675,
  						       size = 8.5, color = 'black'))
  ls = "Status of current \n employment, smoking, ..."
  new_fig <- new_fig + annotation_custom(text_annotation(label = ls,
						       x = 0.07, y = 0.42,
  						       size = 8.5, color = 'black'))
  education = "Qualifications, ..."
  new_fig <- new_fig + annotation_custom(text_annotation(label = education,
						       x = 0.07, y = 0.305,
  						       size = 8.5, color = 'black'))

  el = "Country of birth, \n adopted as a child, ..."
  new_fig <- new_fig + annotation_custom(text_annotation(label = el,
						       x = 0.07, y = 0.152,
  						       size = 8.5, color = 'black'))

  # new_fig <- new_fig + annotation_custom(text_annotation(label = "+",
		# 				       x = 0.41, y = 0.455,
  # 						       size = 20, color = 'red'))
  # if(fname != 'figure_summary_both-1'){
  #   new_fig <- new_fig + annotation_custom(text_annotation(label = "+",
  #      						   x = 0.71, y = 0.358,
  # 						       size = 20, color = 'red'))

  # }
  # if((fname != 'figure_summary_both-1') & (fname != 'figure_summary_both-2')){
  #   new_fig <- new_fig + annotation_custom(text_annotation(label = "+",
  #      						       x = 0.99, y = 0.79,
  # 						       size = 20, color = 'red'))

  # }

  my_ggsave(fname, new_fig, width = 10, height = 5)

  print(new_fig)
  return(new_fig)
}

make_figure(
  dat1, dat2,
  data_brain_agg = data_brain_agg,
  title = 'Approximation quality of proxy measures derived from \nsociodemographics with and without brain imaging',
  fname = 'figure_3')
