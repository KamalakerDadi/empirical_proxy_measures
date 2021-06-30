library(ggplot2)
library(data.table)
source('../../utils.r')
source('../../config.r')

df <- fread('learning_curves.csv')
df[,learning_type := factor(learning_type, levels = c("train", "validation"))] 
df[target=="Age at assessment", target := "Age"]
df[learning_type=="train", learning_type := "Validation"]
df[learning_type=="validation", learning_type := "Generalization"]

fig <- ggplot(
  data = df,
  mapping = aes(x = train_sizes, y = mean, color = learning_type,
                fill = learning_type)) +
  facet_wrap(.~target) +
  geom_line(size = 1.3) +
  geom_ribbon(mapping = aes(ymin = mean -std, ymax = mean + std),
            alpha = 0.2, size = 0.2) +
  scale_color_manual(values = with(color_cats, c(vermillon, `sky blue`))) +
  scale_fill_manual(values = with(color_cats, c(vermillon, `sky blue`))) +
  my_theme +
  labs(x = "# training samples", y = expression(R^2 ~ "score")) +
  theme(legend.title = element_blank(), legend.position="bottom")

my_ggsave("fig_1-supp-1", fig, width = 10, height = 5)
