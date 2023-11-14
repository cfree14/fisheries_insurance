


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data"

# Read SST data
sst_orig <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/environmental/cobe/processed/COBE_1981_2020_by_ram_stocks.csv")


# Format RAM data
################################################################################

# Format
data <- sst_orig %>%
  # Stocks of interest
  filter(stockid %in% c("SABLEFPCOAST", "SSTHORNHPCOAST", "LSTHORNHPCOAST", "DSOLEPCOAST", "PHAKEPCOAST",
                        "CABEZNCAL", "BNKROCKCAL", "PGEODWA", "CABEZORECOAST")) %>%
  # Recode
  mutate(fishery=recode(stockid,
                        "SABLEFPCOAST"="West Coast Sablefish",
                        "SSTHORNHPCOAST"="West Coast Shortspine thornyhead",
                        "LSTHORNHPCOAST"="West Coast Longspine thornyhead",
                        "DSOLEPCOAST"="West Coast Dover sole",
                        "PHAKEPCOAST"="West Coast Pacific hake",
                        "CABEZNCAL"="California Dungeness crab",
                        "BNKROCKCAL"="California Market squid",
                        "PGEODWA"="Washington Dungeness crab",
                        "CABEZORECOAST"="Oregon Dungeness crab"),
         comm_name=gsub("West Coast |California |Oregon |Washington |", "", fishery)) %>%
  select(fishery, comm_name, year, sst_c_avg) %>%
  mutate(fishery=recode(fishery,
                       "West Coast Sablefish"="West Coast DTS",
                       "West Coast Shortspine thornyhead"="West Coast DTS",
                       "West Coast Longspine thornyhead"="West Coast DTS",
                       "West Coast Dover sole"="West Coast DTS"))

# Export data
saveRDS(data, file=file.path(datadir, "souha_sst_data.Rds"))




