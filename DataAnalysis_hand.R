# Tests for defining the Average values of all personality traits between left-handed and right-handed over the age

source("DataPreparation.R")
if (!require("compare")) install.packages("compare")
library("compare")

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

df = getDataSetWithBig5(data, FALSE)
df$ageCat = as.factor(df$ageCat)

cols = c("Extraversion", "Neuroticism", "Agreeableness", 
         "Conscientiousness", "Openess", "ageCat")

# take only left and right-handed, 
# remove unchecked and those who are able to write with both hands
# also remove 8 & 9 age categories, since they have too few observations

df = df[(df$hand==1 | df$hand==2) & (df$ageCat!=9 & df$ageCat!=8), ] 

# Averages Big5 Right-handed
right = df %>% 
  filter(hand == 1) %>%
  select(cols) %>%
  group_by(ageCat) %>% 
  summarise_all(mean)

# Averages Big5 Left-handed
left = df %>% 
  filter(hand == 2) %>%
  select(cols) %>%
  group_by(ageCat) %>% 
  summarise_all(mean)

# All averages
all = df %>% 
  select(cols) %>%
  group_by(ageCat) %>% 
  summarise_all(mean)

# Change density plot line colors by groups
# Extraversion
ggplot(df, aes(x=Extraversion, color=hand)) +
  geom_density() + theme_light() +
  xlim(-3, 3) +
  labs(title="Extraversion density curve", x="Extraversion", y="Density")

ggplot(data = right, aes(x = ageCat, y=Extraversion)) +
  geom_line(data = right, aes(x = ageCat, y=Extraversion, group = 1, color='Right-handed')) +
  geom_line(data = left, aes(x = ageCat, y=Extraversion, group = 1, color='Left-handed')) + 
  geom_line(data = all, aes(x = ageCat, y=Extraversion, group = 1, color='All-handed')) +
  labs(title="Average values of Extraversion between left and right-handed over the age",
       x="Age Category", y="Values of Traits") +
  theme_light()

# Openness
ggplot(df, aes(x=Openess, color=hand)) +
  geom_density() + theme_light() +
  xlim(-4, 3) +
  labs(title="Openness density curve", x="Openness", y="Density")

# Conscientiousness
ggplot(df, aes(x=Conscientiousness, color=hand)) +
  geom_density() + theme_light() +
  xlim(-3.5, 3) +
  labs(title="Conscientiousness density curve", x="Conscientiousness", y="Density")

# Agreeableness
ggplot(df, aes(x=Agreeableness, color=hand)) +
  geom_density() + theme_light() +
  xlim(-5, 3) +
  labs(title="Agreeableness density curve", x="Agreeableness", y="Density")

# Neuroticism
ggplot(df, aes(x=Neuroticism, color=hand)) +
  geom_density() + theme_light() +
  xlim(-3.5, 3.5) +
  labs(title="Neuroticism density curve", x="Neuroticism", y="Density")

