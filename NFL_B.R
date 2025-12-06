
#-------------------------------------------------------------------------
# Effects of defensive stats on wins 
#-------------------------------------------------------------------------

# Testing with Football Analytics with R

library(tidyverse)
library(dplyr)
library(nflverse)
library(ggrepel)
library(GGally)
library(stats)
library(broom)
library(kableExtra)



data = load_pbp(2024)

demo_data_r = tibble(down = c("first", "second"),
                     ydstogo = c(10, 5))
model.matrix(~ ydstogo + down -1 ,
             data = demo_data_r)

pbp_run_r = data %>% 
  filter(play_type == "run" & !is.na(rusher_id) & 
           !is.na(down) & !is.na(run_location)) %>% 
  mutate(rushing_yards = ifelse(is.na(rushing_yards), 0, rushing_yards))


pbp_run_r =
  pbp_run_r %>% 
  mutate(down = as.character(down))

ggplot(pbp_run_r, aes(x = rushing_yards)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(down), ncol=2,
             labeller = label_both) +
  theme_bw() +
  theme(strip.background = element_blank())

pbp_run_r %>% 
  filter(ydstogo == 10) %>% 
  ggplot(aes(x = down, y = rushing_yards)) +
  geom_boxplot() +
  theme_bw()

ggplot(pbp_run_r, aes(x = yardline_100, y = rushing_yards)) +
  geom_point(alpha = 0.25) +
  stat_smooth(method = "lm")+
  theme_bw()

pbp_run_r %>% 
  group_by(yardline_100) %>% 
  summarize(rushing_yards_mean = mean(rushing_yards)) %>% 
  ggplot(aes(x = yardline_100, y = rushing_yards_mean)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()

ggplot(pbp_run_r, aes(run_location, rushing_yards)) +
  geom_boxplot() +
  theme_bw()

pbp_run_r %>% 
  group_by(score_differential) %>% 
  summarize(rushing_yards_mean = mean(rushing_yards)) %>% 
  ggplot(aes(score_differential, rushing_yards_mean)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()

# Multiple Regression Model

pbp_run_r = 
  pbp_run_r %>% 
  mutate(down = as.character(down))

expected_yards_r = 
  lm(rushing_yards ~ 1 + down + ydstogo + down:ydstogo + yardline_100
  + run_location + score_differential, data = pbp_run_r)

pbp_run_r =
  pbp_run_r %>% 
  mutate(ryoe = resid(expected_yards_r))

summary(expected_yards_r)
