## Marble races
## Load packages ####
library(tidyverse)

## Read in the data ####
marbles <- read.csv("../data/202006_marbles.csv",
                    header = T,
                    stringsAsFactors = F)
