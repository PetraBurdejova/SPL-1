# Tests for defining the Average values of all personality traits between left-handed and right-handed over the age

source("DataPreparation.R")
if (!require("compare")) install.packages("compare")
library("compare")

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

df = getDataSetWithBig5(data, FALSE, FALSE)
df$ageCat = as.factor(df$ageCat)

cols = c("Intro", "Neuro", "Agree", 
         "Conscient", "Openess", "ageCat")

# take only left and right-handed, 
# remove unchecked and those who are able to write with both hands
# also remove 8 & 9 age categories, since they have too few observations

df = df[(df$hand==1 | df$hand==2) & (df$ageCat!=9 & df$ageCat!=8), ] 

# Averages Big5 Right-handed
right = df %>% 
  filter(hand == 1) %>%
  dplyr::select(cols) %>%
  group_by(ageCat) %>% 
  summarise_all(mean)

# Averages Big5 Left-handed
left = df %>% 
  filter(hand == 2) %>%
  dplyr::select(cols) %>%
  group_by(ageCat) %>% 
  summarise_all(mean)

# All averages
all = df %>% 
  dplyr::select(cols) %>%
  group_by(ageCat) %>% 
  summarise_all(mean)

# Change density plot line colors by groups
# Extraversion
ggplot(df, aes(x=Intro, color=hand)) +
  geom_density() + theme_light() +
  xlim(0, 60) +
  labs(title="Extraversion density curve", x="Intro", y="Density")

ggplot(data = right, aes(x = ageCat, y=Intro)) +
  geom_line(data = right, aes(x = ageCat, y=Intro, group = 1, color='Right-handed')) +
  geom_line(data = left, aes(x = ageCat, y=Intro, group = 1, color='Left-handed')) + 
  geom_line(data = all, aes(x = ageCat, y=Intro, group = 1, color='All-handed')) +
  labs(title="Average values of Extraversion between left and right-handed over the age",
       x="Age Category", y="Values of Traits") +
  theme_light()

# Openness
ggplot(df, aes(x=Openess, color=hand)) +
  geom_density() + theme_light() +
  xlim(0, 60) +
  labs(title="Openness density curve", x="Openness", y="Density")

ggplot(data = right, aes(x = ageCat, y=Openess)) +
  geom_line(data = right, aes(x = ageCat, y=Openess, group = 1, color='Right-handed')) +
  geom_line(data = left, aes(x = ageCat, y=Openess, group = 1, color='Left-handed')) + 
  geom_line(data = all, aes(x = ageCat, y=Openess, group = 1, color='All-handed')) +
  labs(title="Average values of Openness between left and right-handed over the age",
       x="Age Category", y="Values of Traits") +
  theme_light()

# Conscientiousness
ggplot(df, aes(x=Conscient, color=hand)) +
  geom_density() + theme_light() +
  xlim(0, 60) +
  labs(title="Conscientiousness density curve", x="Conscientiousness", y="Density")

ggplot(data = right, aes(x = ageCat, y=Conscient)) +
  geom_line(data = right, aes(x = ageCat, y=Conscient, group = 1, color='Right-handed')) +
  geom_line(data = left, aes(x = ageCat, y=Conscient, group = 1, color='Left-handed')) + 
  geom_line(data = all, aes(x = ageCat, y=Conscient, group = 1, color='All-handed')) +
  labs(title="Average values of Conscientiousness between left and right-handed over the age",
       x="Age Category", y="Values of Traits") +
  theme_light()

# Agreeableness
ggplot(df, aes(x=Agree, color=hand)) +
  geom_density() + theme_light() +
  xlim(0, 60) +
  labs(title="Agreeableness density curve", x="Agreeableness", y="Density")

ggplot(data = right, aes(x = ageCat, y=Agree)) +
  geom_line(data = right, aes(x = ageCat, y=Agree, group = 1, color='Right-handed')) +
  geom_line(data = left, aes(x = ageCat, y=Agree, group = 1, color='Left-handed')) + 
  geom_line(data = all, aes(x = ageCat, y=Agree, group = 1, color='All-handed')) +
  labs(title="Average values of Agreeableness between left and right-handed over the age",
       x="Age Category", y="Values of Traits") +
  theme_light()

# Neuroticism
ggplot(df, aes(x=Neuro, color=hand)) +
  geom_density() + theme_light() +
  xlim(0, 60) +
  labs(title="Neuroticism density curve", x="Neuroticism", y="Density")

ggplot(data = right, aes(x = ageCat, y=Neuro)) +
  geom_line(data = right, aes(x = ageCat, y=Neuro, group = 1, color='Right-handed')) +
  geom_line(data = left, aes(x = ageCat, y=Neuro, group = 1, color='Left-handed')) + 
  geom_line(data = all, aes(x = ageCat, y=Neuro, group = 1, color='All-handed')) +
  labs(title="Average values of Neuroticism between left and right-handed over the age",
       x="Age Category", y="Values of Traits") +
  theme_light()