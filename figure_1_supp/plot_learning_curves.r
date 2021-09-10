library(ggplot2)
library(data.table)
source('../utils.r')
source('../config.r')

df <- fread('learning_curves.csv')
df[,learning_type := factor(learning_type, levels = c("train", "validation"))] 
df[learning_type=="train", learning_type := "Training"]
df[learning_type=="validation", learning_type := "Testing"]
df <- df[learning_type=='Testing']

fig <- ggplot(
  data = df,
  mapping = aes(x = train_sizes, y = mean, color = learning_type,
                fill = learning_type)) +
  facet_wrap(.~target) +
  geom_line(size = 1.4, show.legend = F) +
  geom_ribbon(mapping = aes(ymin = mean -std, ymax = mean + std),
              alpha = 0.2, size = 0.3, show.legend = F) +
  scale_color_manual(values = with(color_cats, c(`sky blue`))) +
  scale_fill_manual(values = with(color_cats, c(`sky blue`))) +
  my_theme +
  coord_cartesian(xlim = c(0, 3100)) +
  scale_x_continuous(limits = c(0, 3000)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  labs(x = "# training samples", y = expression("out-of-sample" ~ R^2 ~ "score")) +
  theme(legend.title = element_blank(), legend.position="bottom")

my_ggsave("fig_1-supp-1", fig, width = 11, height = 5)
