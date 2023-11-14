


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
data <- readRDS(file=file.path(datadir, "west_coast_fish_stock_data.Rds")) %>%
  filter(is.finite(price_usd_lb)) %>%
  filter(!sci_name %in% c("Raja rhina"))

# Fix me
# Why are there infinite prices?
# Work out less lazy inclusion criteria

# Analyze data
################################################################################

# Species
spp <- sort(unique(data$comm_name))

# Loop through and regress
x <- spp[10]
out <- purrr::map_df(spp, function(x){

  # Filter data
  print(x)
  sdata <- data %>%
    filter(comm_name==x)

  # Regress data
  lmfit <- lm(value_usd ~ biomass + sst_c_avg + price_usd_lb, sdata)

  # Extract data
  summary(lmfit)
  cis <- confint(lmfit)
  coefs <- coef(lmfit)
  pvals <- summary(lmfit)$coefficients[,4]
  covs <- rownames(summary(lmfit)$coefficients)

  # Record data
  df <- tibble(comm_name=x,
               variable=covs,
               est=coefs,
               est_lo=cis[,1],
               est_hi=cis[,2],
               pval=pvals)

})

# Format data
data_plot2 <- out %>%
  filter(variable!="(Intercept)") %>%
  mutate(variable=recode(variable,
                         "biomass"="Biomass",
                         "price_usd_lb"="Price",
                         "sst_c_avg"="SST")) %>%
  # Clasiffy signif and direction
  mutate(sig=ifelse(pval<=0.05, "significant", "non-significant"),
         type=ifelse(sig=="non-significant", "none",
                     ifelse(est>0, "positive", "negative"))) %>%
  mutate(type=factor(type, levels=c("positive", "none", "negative")))

# Plot data
################################################################################


# Plot data
g <- ggplot(data_plot2, aes(x=variable, y=comm_name, fill=type)) +
  geom_raster() +
  # Labels
  labs(x="Variable", y="") +
  # Legend
  scale_fill_manual(name="Influence significance", values=c("blue", "grey80", "red")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_var_inf_on_revenues.png"),
       width=4.5, height=4.5, units="in", dpi=600)



# Plot data
################################################################################

# Data to plot
data_plot <- out %>%
  # Reduce
  filter(variable=="sst_c_avg") %>%
  # Arrange
  arrange(desc(est)) %>%
  mutate(comm_name=factor(comm_name, levels=comm_name)) %>%
  # Clasiffy signif and direction
  mutate(sig=ifelse(pval<=0.05, "significant", "non-significant"),
         type=ifelse(sig=="non-significant", "none",
                     ifelse(est>0, "positive", "negative"))) %>%
  mutate(type=factor(type, levels=c("positive", "none", "negative")))

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.margin = margin(0,0,-5,0),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data_plot, aes(x=est, y=comm_name, color=type)) +
  # Reference line
  geom_vline(xintercept=0, linetype="dotted") +
  # Data
  geom_errorbar(mapping=aes(y=comm_name, xmin=est_lo, xmax=est_hi, color=type), width=0, alpha=0.5) +
  geom_point() +
  # Labels
  labs(x="Influence of SST on revenues", y="") +
  # Legend
  scale_color_manual(name="Influence significance", values=c("blue", "black", "red")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_sst_inf_on_revenues.png"),
       width=4.5, height=4.5, units="in", dpi=600)





