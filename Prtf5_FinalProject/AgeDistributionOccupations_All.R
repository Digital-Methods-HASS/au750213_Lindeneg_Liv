# Age distribution for the most common occupations for all unmarried women 1860 Aarhus


library(tidyverse)
ugifte_kvinder <- read_csv("data/kvinder_ugifte_1860.csv")


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

# Making a vector of the most common occupation
# The vector is needed for filtering
top_vector <- top_erhverv %>% 
  pull(stilling) # to make a vector in order to filter

# Filtering all the occupations and corresponding ages for the most common occupations
alder_stilling <- ugifte_kvinder %>% 
  select(alder, stilling) %>% 
  filter(stilling %in% top_vector)


# Making a plot
# Multiple histograms using facets_wrap()
plot_agedistri <- alder_stilling %>% 
  ggplot(aes(x=alder, fill=stilling))+
  geom_histogram(bins=20, color="white")+
  facet_wrap(~stilling, scales="free_y")+
  labs(title="Age distribution for the top 10 occupations among unmarried women 1860 Aarhus",
       x="Age",
       y="Count")+
  theme_bw()+
  theme(text=element_text(size=16))+
  theme(legend.position = "none")

plot_agedistri

ggsave("figures/agedistribution.png", plot_agedistri, width = 13, height = 8, dpi = 150)