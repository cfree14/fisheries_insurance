


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data"

# Get PACFIN data
pacfin_orig <- wcfish::pacfin_all1

# Read RAM data
ram_orig <- readRDS(file.path(datadir, "WC_RAM_data_processed.Rds"))


# Format PACFIN data
################################################################################

# Summarize
pacfin <- pacfin_orig %>%
  # Reduce to species
  filter(!is.na(sci_name)) %>%
  # Summarize by species-year
  group_by(comm_name, sci_name, year) %>%
  summarize(value_usd=sum(value_usd, na.rm = T),
            landings_lb=sum(landings_lb, na.rm=T)) %>%
  ungroup() %>%
  # Calculate ex-vessel price
  mutate(price_usd_lb=value_usd/landings_lb)

# Check names
freeR::check_names(pacfin$sci_name)


# Format RAM data
################################################################################

# Format
ram <- ram_orig %>%
  # Record number of stocks
  group_by(comm_name, sci_name) %>%
  mutate(nstocks=n_distinct(stockid)) %>%
  ungroup() %>%
  # Summarize
  group_by(comm_name, sci_name, year, nstocks) %>%
  summarize(nstocks_w_data=n_distinct(stockid),
            biomass=sum(biomass),
            sst_c_avg=mean(sst_c_avg)) %>%
  ungroup() %>%
  # Reduce to years with biomass estimates for all stocks
  filter(nstocks==nstocks_w_data) %>%
  select(-nstocks_w_data)

# Check names
freeR::check_names(ram$sci_name)


# Merge data
################################################################################

# Merge data
data <- ram %>%
  # Add PACFIN revenues and price
  left_join(pacfin %>% select(sci_name, year, landings_lb, value_usd, price_usd_lb), by=c("sci_name", "year")) %>%
  # Reduce to years with all data
  na.omit() %>%
  # Arrange
  arrange(comm_name, year)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(datadir, "west_coast_fish_stock_data.Rds"))
write.csv(data, file=file.path(datadir, "west_coast_fish_stock_data.csv"), row.names=F)



