library(tidyverse)
library(readr)
library(stringr)

# Read in all the datasets
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
  skip = 13
)

# One row per plot
pinus_size_summary <-
  pinus_size %>%
  group_by(plot_num) %>%
  summarize(
    AvgHeight = mean(height, na.rm = TRUE),
    AvgBD = mean(basal_diameter, na.rm = TRUE),
    Trees2015 = n(),
    .groups = "drop"
  )

# Delete 2013 and 2014
tree2015 <-
  tree_presence %>%
  filter(year_cont == '3') %>%
  select(
    plot_num,
    tree_presence,
    pinus_presence,
    rich_colherbs
  )

# Rename variables
pinus_est <-
  pinus_est %>%
  rename(
    Plot = plot_num,
    Replicate = blk,
    Nitrogen = nutrients,
    Pesticide = consumers,
    Duration = year_est
  )

# Join each dataset
plots <-
  pinus_est %>%
  left_join(pinus_size_summary,
            by = c("Plot" = "plot_num")) %>%
  left_join(tree2015,
            by = c("Plot" = "plot_num"))

# Create species column
plots <- plots %>%
  mutate(
    Species = str_extract(community_comp, "^[A-Z]{4}")
  )

# Changing some variables to binary
plots <- plots %>% 
  mutate(
    Pesticide = as.integer(str_detect(Pesticide, "Sprayed")),
    Nitrogen = as.integer(str_detect(Nitrogen, "Fertilized")),
  )

# Delete Richness Column
plots <- plots %>%
  select(-c(richness))

# Realized not all plots are present, create a new master plot for all
master_plots <- tree_presence %>%
  distinct(
    plot_num,
    blk,
    consumers,
    nutrients,
    community_comp
  ) %>%
  rename(
    Plot = plot_num,
    Replicate = blk,
    Nitrogen = nutrients,
    Pesticide = consumers
  )

plots <- master_plots %>%
  left_join(pinus_est, by = "Plot") %>%
  left_join(pinus_size_summary, by = c("Plot" = "plot_num")) %>%
  left_join(tree2015, by = c("Plot" = "plot_num"))

# Get rid of duplicate columns during join
plots <- plots %>%
  select(-c(ends_with(".y")))

# Get rid of '.x'
# Create 0/1 Species Columns (Outcomes)
plots <- plots %>%
  mutate(
    ANVI = as.integer(str_detect(community_comp, "ANVI")),
    PAAN = as.integer(str_detect(community_comp, "PAAN")),
    SCIN = as.integer(str_detect(community_comp, "SCIN")),
    SEPA = as.integer(str_detect(community_comp, "SEPA")),
    SOPI = as.integer(str_detect(community_comp, "SOPI")),
    TRFL = as.integer(str_detect(community_comp, "TRFL"))
  )
