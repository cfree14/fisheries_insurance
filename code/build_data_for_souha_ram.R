


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data"

# Read RAM data
load("/Users/cfree/Dropbox/Chris/UCSB/data/ramldb/RAMLDB v4.495/R Data/DBdata[asmt][v4.495].RData")

# Read SST data
sst_orig <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/environmental/cobe/processed/COBE_1981_2020_by_ram_stocks.csv")


# Format RAM data
################################################################################

# Stock key
stock_key <- stock %>%
  # West Coast
  filter(region %in% c("US West Coast", "US Alaska")) %>%
  # Add area
  left_join(area %>% select(areaid, areaname), by="areaid") %>%
  # Simplify
  select(stockid, scientificname, commonname, stocklong, areaname) %>%
  # Rename
  rename(comm_name=commonname, sci_name=scientificname, area=areaname)

# Stock assessment data
data <- timeseries_values_views %>%
  # West Coast
  filter(stockid %in% stock_key$stockid) %>%
  # Simplify
  select(stockid, year, TB, SSB, TN) %>%
  # Deciding to use TB
  select(stockid, year, TB) %>%
  na.omit() %>%
  # Rename
  rename(biomass=TB) %>%
  # Add species info
  left_join(stock_key %>% select(stockid, stocklong, comm_name, sci_name, area), by="stockid") %>%
  # Add SST data
  left_join(sst_orig, by=c("stockid", "year")) %>%
  # Arrange
  select(stockid, stocklong, comm_name, sci_name, area, year, biomass, everything()) %>%
  # Filter
  filter(comm_name %in% c("Pacific hake", "Longspine thornyhead", "Shortspine thornyhead", "Sablefish", "Dover sole", "Pacific cod"))

# Inspect
freeR::complete(data)
sort(unique(data$comm_name))
sort(unique(data$stockid))


# Export data
saveRDS(data, file=file.path(datadir, "souha_ram_data.Rds"))




