
#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("C:/Apps/projects/DrillBit")


#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory
library(tidyverse)

source("./code/tools.R")

#--------------------------------------------------------
# Paths
#-------------------------------------------------------- 
d1_path = "./data/20well_40 bit runs_csv/ALLEN-55-2-44-E1H.csv"
d2_path = "./data/20well_40 bit runs_csv/University20-2004H.csv"
b_path = "Z:/project/Adhoc/data/b.csv"

#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
d <- read.csv(b_path, header=TRUE, na.strings="N/A")

# correct names
colnames(d) <- gsub("\\.+", ".", colnames(d))
colnames(d) <- gsub("\\.$", "", colnames(d))

# select vars and filter obs
d <- d %>% 
  select(varA:varB) %>%
  filter(varC=="Y")



#--------------------------------------------------------
# EDA
#-------------------------------------------------------- 
# summary stats
summary(dat)

# missing values
sapply(dat, function(x) sum(is.na(x)))  # count
sapply(dat, function(x) sum(is.na(x))/nrow(dat))  # proportion

## Numerical vars
# hist
plot_HistDensity(dat$v1, "xxx")

# boxplot
plot_Box(dat, x="v1", y="v2", title="vvv")


#--------------------------------------------------------
# Modelling
#-------------------------------------------------------- 
Contact GitHub API Training Shop Blog About
© 2017 GitHub, Inc. Terms Privacy Security Status Help