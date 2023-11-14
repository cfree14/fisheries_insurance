


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/landings_receipts/processed/1980_2022_landings_receipts.Rds")

# Read port key
port_key <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/public/cdfw_keys/processed/CDFW_port_key.Rds")


# Build data
################################################################################

# Complexes
complexes <- c("San Diego", "Los Angeles", "Santa Barbara", "Morro Bay", "Monterey", "San Francisco", "Bodega Bay", "Fort Bragg", "Eureka") %>% rev()

# Build time series
data <- data_orig %>%
  # Reduce
  filter(comm_name=="Market squid") %>%
  # Add port
  left_join(port_key %>% select(port_code, port_complex), by=c("port_id"="port_code")) %>%
  # Summarize
  group_by(year, port_complex) %>%
  summarize(nvessels=n_distinct(vessel_id),
            landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Convert landings
  mutate(landings_mt=measurements::conv_unit(landings_lbs, "lbs", "kg") /1000 ) %>%
  # Remove conifidential data
  filter(nvessels>=3) %>%
  # Order port complexes
  mutate(port_complex=factor(port_complex, levels=complexes))


# Time series plot
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data, aes(x=year, y=nvessels, fill=port_complex)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Year", y="Number of vessels", tag="A") +
  # Legend
  scale_fill_ordinal(name="Port complex") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Plot data
g2 <- ggplot(data, aes(x=year, y=landings_mt/1000, fill=port_complex)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Year", y="Landings (1000s mt)", tag="B") +
  # Legend
  scale_fill_ordinal(name="Port complex") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Plot data
g3 <- ggplot(data, aes(x=year, y=value_usd/1e6, fill=port_complex)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", tag="C") +
  # Legend
  scale_fill_ordinal(name="Port complex") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8),
        legend.key.size = unit(0.3, "cm"))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
g



# Time series plot
################################################################################


# Build data
stats <- data_orig %>%
  # Reduce
  filter(comm_name=="Market squid" & vessel_id!=-1) %>%
  # Add port
  left_join(port_key %>% select(port_code, port_complex), by=c("port_id"="port_code")) %>%
  # Summarize by vessel
  group_by(vessel_id, port_complex) %>%
  summarize(nreceipts=n_distinct(receipt_id),
            landings_lbs=sum(landings_lbs, na.rm=T)) %>%
  ungroup() %>%
  # Calculate props and rank
  arrange(vessel_id, desc(nreceipts)) %>%
  group_by(vessel_id) %>%
  mutate(preceipts=nreceipts/sum(nreceipts),
         rank=1:n(),
         landings_lbs_tot=sum(landings_lbs)) %>%
  # Reduce to primary port
  slice(1) %>%
  ungroup() %>%
  # Reduce to more major vessels
  filter(landings_lbs_tot>0.5*1e6)

hist(x=stats$landings_lbs_tot/1e6, breaks=seq(0,150,0.5), xlim=c(0,10))

# Plot data
ggplot(stats, aes(x="", y=preceipts)) +
  geom_boxplot() +
  # Labels
  labs(x="", y="Proportion of receipts") +
  # Theme
  theme_bw()





