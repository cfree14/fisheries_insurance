


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data"
pacfindir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/wc_landings_data/data/pacfin/processed"

# Get PACFIN data
pacfin_orig <- readRDS(file.path(pacfindir, "PACFIN_ALL001_1980_2023_all_species_landings.Rds"))


# Format data
################################################################################

# Species do
species_do <- c("Dungeness crab",
                "Market squid",
                "Pacific whiting",
                "Dover sole",
                "Sablefish",
                "Longspine thornyhead",
                "Shortspine thornyhead",
                "Thornyheads (mixed)")

# Format data
data <- pacfin_orig %>%
  # Remove at-sea
  # filter(state!="At-Sea") %>%
  # Assume "nominal" species are corrects
  mutate(comm_name_simple=gsub("Nom. ", "", comm_name)) %>%
  # Reduce to species of interest
  filter(comm_name_simple %in% species_do) %>%
  # Summarize landings/value
  group_by(comm_name_simple, state, year) %>%
  summarize(landings_mt=sum(landings_mt, na.rm=T),
            landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Derive price
  mutate(price_usd_lb=value_usd/landings_lb) %>%
  # Rename
  rename(comm_name=comm_name_simple) %>%
  # Add fishery
  mutate(fishery=ifelse(comm_name %in% c("Dover sole", "Longspine thornyhead",
                                         "Shortspine thornyhead", "Thornyheads (mixed)", "Sablefish"),
                        "DTS", comm_name)) %>%
  # Arrange
  select(fishery, everything()) %>%
  arrange(fishery, comm_name, state, year) %>%
  # Remove 2023
  filter(year!="2023")

# Confirm species
unique(data$comm_name) %>% sort()

# Export data
saveRDS(data, file=file.path(datadir, "souha_value_data.Rds"))

