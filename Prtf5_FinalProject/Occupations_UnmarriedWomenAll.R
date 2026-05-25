# Finding the most common occupations among all unmarried women 1860 Aarhus


library(tidyverse)
ugifte_kvinder <- read_csv("data/kvinder_ugifte_1860.csv")


# Top 10 most common descriptions in the "stilling" column
ugifte_kvinder %>%
  count(stilling, sort = TRUE) %>%
  slice_head(n = 10)


# Making a vector over descriptions that needs to be excluded
# Not occupations
ikke_erhverv <- c("datter", "plejedatter", "barn", "søn", "kone", "søster", "fattiglem", "huusfader", "indsidder",
                  "forsørges af familie", "barnebarn", "steddatter", "tjenestekarl", "familiemedlem", "tjener forældrene",
                  "dagleier", "gaardmand", "huusmand")


# Making a top 10 with only occupations
# Keep adding to the vector above until the top 10 is only occupations
top_erhverv <- ugifte_kvinder %>% 
  filter(!stilling %in% ikke_erhverv) %>%
  count(stilling, sort = TRUE) %>%
  slice_head(n = 10)


# Making a plot
# Showing the amount in numbers
plot_toperhverv <- top_erhverv %>% 
  ggplot(aes(x = reorder(stilling,n), y=n))+
  geom_col(fill = "salmon") +
  geom_text(aes(label = n),
            hjust = -0.3,
            size = 4) +
  coord_flip() +labs(
    title = "Top 10 occupations for all unmarried women 1860 Aarhus",
    x = "Occupation",
    y = "Count"
  ) +
  theme_bw()+
  theme(text=element_text(size=16))

plot_toperhverv

ggsave("figures/toperhverv.png", plot_toperhverv, width = 13, height = 8, dpi = 150)


# Mutate to get a column showing percentage as well
percentage <- ugifte_kvinder %>%
  count(stilling, sort = TRUE) %>% 
  mutate(percentage = n/sum(n)*100) %>% 
  filter(!stilling %in% ikke_erhverv) %>%
  slice_head(n = 10)


# Making a plot
# Showing the amount in percentage
percentage_toperhverv <- percentage %>% 
  ggplot(aes(x = reorder(stilling, percentage), y=percentage))+
  geom_col(fill = "palegreen") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")),
            hjust = 0.5,
            size = 4) +
  coord_flip() +labs(
    title = "Top 10 occupations for all unmarried women 1860 Aarhus",
    x = "Occupation",
    y = "Percentage"
  ) +
  theme_bw()+
  theme(text=element_text(size=16))

percentage_toperhverv

ggsave("figures/percentage_toperhverv.png", percentage_toperhverv, width = 13, height = 8, dpi = 150)
