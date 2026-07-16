library(tidyverse)
library(readr)
library(stringr)

#Read in all the datasets
pinus_est <- read_csv(
  "pinus_establishment_year_for_figshare.csv",
  skip = 10
)

pinus_size <- read_csv(
  "pinus_size_for_figshare.csv",
  skip = 12
)

tree_est <- read_csv(
  "trees_establishment_year_for_figshare.csv",
  skip = 10
)

tree_presence <- read_csv(
  "trees_presence_for_figshare.csv",
  skip = 12
)

# Fix the column names in tree_presence
names(tree_presence) <- tree_presence[1, ]
tree_presence <- tree_presence[-1, ]