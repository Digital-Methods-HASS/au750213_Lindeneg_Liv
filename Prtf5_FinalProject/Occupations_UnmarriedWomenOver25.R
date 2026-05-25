# Finding the most common occupations among unmarried women over 25 1860 Aarhus


library(tidyverse)
ugifte_kvinder <- read_csv("data/kvinder_ugifte_1860.csv")


# Making a vector over descriptions that needs to be excluded
# Not occupations
ikke_erhverv <- c("datter", "plejedatter", "barn", "søn", "kone", "søster", "fattiglem", "huusfader", "indsidder",
                  "forsørges af familie", "barnebarn", "steddatter", "tjenestekarl", "familiemedlem", "tjener forældrene",
                  "dagleier", "gaardmand", "huusmand")


# Filter to get only the women that are older than 25
stilling_over25 <- ugifte_kvinder %>% 
  filter(alder > 25) %>% 
  count(stilling, sort = TRUE)


# Making a top 10 for occupations for women over 25
# Excluding non occupation descriptions 
top_over25 <- stilling_over25 %>% 
  filter(!stilling %in% ikke_erhverv) %>%
  slice_head(n = 10)


# Making a plot
# Showing the amount in numbers
plot_over25 <- top_over25 %>% 
  ggplot(aes(x = reorder(stilling,n), y=n))+
  geom_col(fill = "lightskyblue1") +
  geom_text(aes(label = n),
            hjust = -0.3,
            size = 4) +
  coord_flip() +labs(
    title = "Top 10 occupations for unmarried women over 25 1860 Aarhus",
    x = "Occupation",
    y = "Count"
  ) +
  theme_bw()+
  theme(text=element_text(size=16))

plot_over25

ggsave("figures/top_over25.png", plot_over25, width = 13, height = 8, dpi = 150)


# Adding a column with percentage
# Over 25 but percentage instead
percentage_over25 <- stilling_over25 %>% 
  mutate(percentage = n/sum(n)*100) %>% 
  filter(!stilling %in% ikke_erhverv) %>%
  slice_head(n = 10)


# Making a plot
# Showing the amount in percentage
percentage_over25top <- percentage_over25 %>% 
  ggplot(aes(x = reorder(stilling, percentage), y=percentage))+
  geom_col(fill = "plum2") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")),
            hjust = 0.5,
            size = 4) +
  coord_flip() +labs(
    title = "Top 10 occupations for unmarried women over 25 1860 Aarhus",
    x = "Occupation",
    y = "Percentage"
  ) +
  theme_bw()+
  theme(text=element_text(size=16))

percentage_over25top

ggsave("figures/percentage_over25top.png", percentage_over25top, width = 13, height = 8, dpi = 150)