library(tidyverse)
library(lubridate)

# ---- Load and clean data ----
articles <- read_csv("data/Primates as Relatives Final_03_28_Data cleaned.csv")

articles <- articles %>% 
  mutate(JGender = ifelse(JGender == "na", NA, JGender)) %>%
  mutate(Date = dmy(Date))

# ---- Explore trends over time ----
# How does each relatedness code/category change over time?
articles %>% 
  # Summarise frequencies of each Hamiltonian category in each year
  count(Year, Hamilton) %>% 
  
  ggplot(aes(x = Year, y = n, colour = factor(Hamilton), group = factor(Hamilton))) +
  geom_point() +
  geom_line() +
  
  labs(x = NULL, y = "Number of articles", colour = "Relatedness") +
  theme_classic() +
  theme(legend.position = "bottom")


# How does relatedness vary with time and political leaning?
articles %>% 
  ggplot(aes(x = Date, y = Hamilton, colour = Politics, fill = Politics)) +
  geom_point() +
  geom_smooth() +
  
  labs(x = NULL, y = "Implied Hamiltonian relatedness") +
  theme_classic() +
  theme(legend.position = "bottom")
  

articles %>% 
  filter(!is.na(JGender)) %>% 
  
  ggplot(aes(x = Date, y = Hamilton, colour = JGender)) +
  geom_point() +
  geom_smooth()

# ---- Hypothesis 1 ----
# - Left wing newspapers and broadsheets will allow a “closer” familial relationship to the other apes - 
articles %>% 
  ggplot(aes(x = Politics, y = Hamilton)) +
  geom_boxplot(draw_quantiles = 0.5) +
  
  labs(x = "Political leaning", y = "Implied Hamiltonian relatedness") +
  theme_classic()

summary(lm(Hamilton ~ Politics, data = articles))

# - ... similarly for female journalists vs male journalists -
articles %>% 
  filter(!is.na(JGender)) %>% 
  
  ggplot(aes(x = JGender, y = Hamilton)) +
  geom_boxplot(draw_quantiles = 0.5) +
  
  labs(x = "Journalist gender", y = "Implied Hamiltonian relatedness") +
  theme_classic()

summary(lm(Hamilton ~ JGender, data = articles))
