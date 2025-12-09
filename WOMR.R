
library(tidyverse)
library(dplyr)
library(nflverse)
library(GGally)
library(stats)
library(ggrepel)
library(broom)
library(kableExtra)
library(pscl)


pbp = load_pbp(2018:2022) #load the data
