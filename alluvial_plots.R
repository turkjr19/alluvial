library(tidyverse)
library(ggalluvial)
library(RColorBrewer)

# Load in previously cleaned data from OHL scrape
my_data <- read_csv("~/Documents/hockey/OHL_OIR/my_data.csv")

# create data frame based on team and goal scorer
who_contributed <- my_data %>%
  filter(scoring_team == "FLNT",
         goal.scorer == "Ty Dellandrea") %>%
  select(goal.scorer, assist1, assist2) %>% 
  group_by(goal.scorer, assist1, assist2) %>% 
  summarise(count = n()) %>% 
  arrange(goal.scorer, assist1, assist2)

# set text size
geom.text.size = 3
theme.size = (14/5) * geom.text.size

# plot
ggplot(who_contributed, aes(y = count,
                axis1 = goal.scorer,
                axis2 = assist1,
                axis3 = assist2)) +
  geom_alluvium(aes(fill = assist1),
                width = 0) +
  guides(fill = FALSE) +
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = "BuPu") +
  geom_stratum(width = 5/8, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE, size = geom.text.size) +
  scale_x_continuous(breaks = 1:3, labels = c("Goal Scorer", "Primary Assist",
                                              "Secondary Assist")) +
  scale_color_brewer(type = "qual", palette = "BuPu") +
  labs(y = "")


