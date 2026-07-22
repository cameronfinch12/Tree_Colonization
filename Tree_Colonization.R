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
plots <- plots %>%
  rename_with(~str_remove(., "\\.x$"))

# Create 0/1 Species Columns (Outcomes)
plots <- plots %>%
  mutate(
    ANVI = case_when(
      community_comp == "ANVI_Monoculture" ~ 1,
      str_detect(community_comp, "Polyculture") &
        !str_detect(community_comp, "ANVI") ~ 1,
      TRUE ~ 0
    ),
    
    PAAN = case_when(
      community_comp == "PAAN_Monoculture" ~ 1,
      str_detect(community_comp, "Polyculture") &
        !str_detect(community_comp, "PAAN") ~ 1,
      TRUE ~ 0
    ),
    
    SCIN = case_when(
      community_comp == "SCIN_Monoculture" ~ 1,
      str_detect(community_comp, "Polyculture") &
        !str_detect(community_comp, "SCIN") ~ 1,
      TRUE ~ 0
    ),
    
    SEPA = case_when(
      community_comp == "SEPA_Monoculture" ~ 1,
      str_detect(community_comp, "Polyculture") &
        !str_detect(community_comp, "SEPA") ~ 1,
      TRUE ~ 0
    ),
    
    SOPI = case_when(
      community_comp == "SOPI_Monoculture" ~ 1,
      str_detect(community_comp, "Polyculture") &
        !str_detect(community_comp, "SOPI") ~ 1,
      TRUE ~ 0
    ),
    
    TRFL = case_when(
      community_comp == "TRFL_Monoculture" ~ 1,
      str_detect(community_comp, "Polyculture") &
        !str_detect(community_comp, "TRFL") ~ 1,
      TRUE ~ 0
    )
  )

# Replace NAs with 0 when necessary
plots <- plots %>%
  mutate(
    across(
      where(is.numeric),
      ~if_else(is.na(.x), 0, .x)
    )
  )

# Fixing empty community_comp entries
plots <- plots %>%
  mutate(
    richness = if_else(
      str_detect(community_comp, "Polyculture"), "Polyculture", "Monoculture"
    )
  )

# Grouping desirability columns for easy access
desirability <- c('AvgHeight', 'Duration', 'AvgBD', 'Trees2015', 'rich_colherbs')

# Moving desirability columns, deleting some unnecessary
plots <- plots %>%
  relocate(
    all_of(desirability), .after = "TRFL"
    ) %>%
  select(
    -c("tree_presence", "pinus_presence")
  ) 

plots <- plots %>% 
  relocate(
    "community_comp", .after = "Replicate"
  )

# Adding .initial to environmental variables, deleting community_comp
plots <- plots %>%
  rename_with(~ paste0(.x, ".initial"), all_of(desirability)) %>%
  select(
    -c("community_comp")
  )

#Add .outcome to each species column
plots <- plots %>%
  rename_with(~ paste0(.x, ".outcome"), c("ANVI", "PAAN", "SCIN", "SEPA", "SOPI", "TRFL"))

# Writing Final CSV
write_csv(plots, "data_Tree_Colonization.csv")

