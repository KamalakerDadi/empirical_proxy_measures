library(data.table)
library(ggplot2)
source('../config.r')
source('../utils.r')
source('../read_data.r')

extra_path <- file.path('..', 'figure-2', 'inputs')

#' Get together all input files.
#' First the extra data files. We'll write a function for that.
#'

fname <- "../figure-3/inputs/post_predictive_analysis_imaging_non_imaging_age_earlylife.csv"

fname2 <- '../figure-3/inputs/post_predictive_analysis_imaging_non_imaging_fluidintelligence_education.csv'

fname3 <- '../figure-3/inputs/post_predictive_analysis_imaging_non_imaging_neuroticism_mentalhealth.csv'


DT_age <- fread(fname)[evaluation == 'Generalization']
DT_fi <- fread(fname2)[evaluation == 'Generalization']
DT_n <- fread(fname3)[evaluation == 'Generalization']

age_dt <- rbind(
  DT_age[,.(age = mean(true), measure='original'), .(eid)],
  DT_age[,.(age = mean(predicted), measure='proxy'), .(eid)])
 
FI_dt <- rbind(
  DT_fi[evaluation=='Generalization',
        .(FI = mean(true), measure='original'), .(eid)],
  DT_fi[evaluation=='Generalization',
        .(FI = mean(predicted), measure='proxy'), .(eid)])

N_dt <- rbind(
  DT_n[evaluation=='Generalization',
          .(N = mean(true), measure='original'), .(eid)],
  DT_n[evaluation=='Generalization',
          .(N = mean(predicted), measure='proxy'), .(eid)])


my_theme <- theme_bw() + theme(
  text = element_text(family = 'Helvetica', size = 20),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 16),
  axis.text = element_text(size = 20),
  axis.title.x = element_text(size = 28),
  axis.line=element_blank(),
  axis.title.y=element_blank(),
  legend.position="none",
  panel.background=element_blank(),
  panel.border=element_blank(),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank()
)
theme_set(my_theme)

fig <- ggplot(age_dt, mapping = aes(x=age, linetype = measure)) +
  geom_density(size=2,
               adjust = 2,
               show.legend = F,
               color=color_cats[['orange']],
               fill=color_cats[['orange']], alpha=0.25) +
  facet_grid(measure~.) + my_theme +
  theme(
    axis.title.x = element_text(hjust=0.03),
    plot.title = element_text(hjust = 1),
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.background = element_rect(
      fill = alpha(color_cats[['orange']], 0.2),
      colour = alpha(color_cats[['orange']], 0.2),
      size = 0.5, linetype = "solid")) +
  scale_y_continuous(breaks=NULL) +
  labs(x='Age (physical)')

my_ggsave(fname = './fig_1_elements_age', plot = fig, height=3.7, width=5)


fig <- ggplot(FI_dt, mapping = aes(x = FI, linetype = measure)) +
  geom_density(size=2,
               adjust = 2,
               show.legend = F,
               color=color_cats[['blueish green']],
               fill=color_cats[['blueish green']], alpha=0.25) +
  facet_grid(measure~., scales='free_y') + my_theme +
  theme(
    axis.title.x = element_text(hjust=0.03),
    plot.title = element_text(hjust = 1),
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.background = element_rect(
      fill = alpha(color_cats[['blueish green']], 0.2),
      colour = alpha(color_cats[['blueish green']], 0.2),
      size = 0.5, linetype = "solid")) +
  scale_y_continuous(breaks=NULL) +
  labs(x='Fluid Intelligence (test)')

my_ggsave(fname = './fig_1_elements_FI', plot = fig, height=3.7, width=5)


fig <- ggplot(N_dt, mapping = aes(x = N, linetype = measure)) +
  geom_density(size=2,
               adjust = 2,
               show.legend = F,
               color=color_cats[['blue']],
               fill=color_cats[['blue']], alpha=0.25) +
  facet_grid(measure~., scales='free_y') + my_theme +
  theme(
    axis.title.x = element_text(hjust=0.03),
    plot.title = element_text(hjust = 1),
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.background = element_rect(
      fill = alpha(color_cats[['blue']], 0.2),
      colour = alpha(color_cats[['blue']], 0.2),
      size = 0.5, linetype = "solid")) +
  scale_y_continuous(breaks=NULL) +
  labs(x='Neuroticism (questionnaire)')

my_ggsave(fname = './fig_1_elements_N', plot = fig, height=3.7, width=5)
