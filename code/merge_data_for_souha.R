


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

# Read data
landings_orig <- readRDS(file.path(datadir, "souha_value_data.Rds"))
biomass_orig <- readRDS(file.path(datadir, "souha_ram_data.Rds"))
sst_orig <- readRDS(file=file.path(datadir, "souha_sst_data.Rds"))



# Format landings data
################################################################################

# Summarize landings
landings_coast <- landings_orig %>%
  # Summarize coastwide
  group_by(fishery, comm_name, year) %>%
  summarize(landings_mt=sum(landings_mt, na.rm = T),
            landings_lb=sum(landings_lb, na.rm = T),
            value_usd=sum(value_usd, na.rm = T)) %>%
  ungroup() %>%
  # Remove state fisheries
  filter(!comm_name %in% c("Dungeness crab", "Market squid", "Thornyheads (mixed)")) %>%
  mutate(fishery=paste("West Coast", fishery))

# Isolate state landings
landings_state <- landings_orig %>%
  filter(comm_name %in% c("Dungeness crab", "Market squid")) %>%
  mutate(fishery=paste(state, comm_name)) %>%
  filter(fishery=="California Market squid" | grepl("Dungeness crab", fishery)) %>%
  select(-c(state, price_usd_lb))

# Merge
landings <- bind_rows(landings_coast, landings_state) %>%
  mutate(price_usd_lb=value_usd / landings_lb) %>%
  mutate(comm_name=recode(comm_name,
                        "Pacific whiting"="Pacific hake"),
         fishery=recode(fishery,
                        "West Coast Pacific whiting"="West Coast Pacific hake"))


# Format biomass data
################################################################################

# Biomass
biomass <- biomass_orig %>%
  # Stocks of interest
  filter(stocklong %in% c("Dover sole Pacific Coast",
                          "Longspine thornyhead Pacific Coast",
                          "Pacific hake Pacific Coast",
                          "Sablefish Pacific Coast",
                          "Shortspine thornyhead Pacific Coast")) %>%
  # Simplify
  select(stocklong, comm_name, year, biomass) %>%
  # Rename
  rename(fishery=stocklong,
         biomass_mt=biomass) %>%
  # Recode fishery
  mutate(fishery=recode(fishery,
                        "Dover sole Pacific Coast"="West Coast DTS",
                        "Longspine thornyhead Pacific Coast"="West Coast DTS",
                        "Shortspine thornyhead Pacific Coast"="West Coast DTS",
                        "Pacific hake Pacific Coast"="West Coast Pacific hake",
                        "Sablefish Pacific Coast"="West Coast DTS"))


# Merge data
################################################################################

# Merge data
data <- sst_orig %>%
  # Add landings
  left_join(landings, by=c("fishery", "comm_name", "year")) %>%
  # Add biomass
  left_join(biomass, by=c("fishery", "comm_name", "year")) %>%
  # Arrange
  select(fishery, comm_name, year, sst_c_avg, biomass_mt, everything()) %>%
  arrange(fishery, comm_name, year)

# Export
write.csv(data, file=file.path(datadir, "data_for_souha.csv"), row.names = F)
