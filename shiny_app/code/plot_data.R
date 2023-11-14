
# Plot data
# spp <- "Arrowtooth flounder"
#
plot_data <- function(data, spp){

  # Subset data
  sdata <- data %>%
    # Filter
    filter(comm_name==spp) %>%
    # Scale profits
    mutate(value_usd_scaled=scale(value_usd) %>% as.numeric())

  # Derive landings trigger
  lmfit1 <- lm(landings_lb ~ value_usd_scaled, sdata)
  landings_lb_trig <- predict(lmfit1, newdata=tibble(value_usd_scaled=0))
  lmfit1a <- lm(value_usd_scaled ~landings_lb, sdata)
  landings_dir <- ifelse(coef(lmfit1a)[2]>0, "positive", "negative")
  payout_x <- ifelse(landings_dir=="positive", min(sdata$landings_lb/1000), max(sdata$landings_lb/1000))
  payout_hjust <- ifelse(landings_dir=="positive", 0, 1)

  # Derive SST trigger
  lmfit2 <- lm(sst_c_avg ~ value_usd_scaled, sdata)
  sst_c_trig <- predict(lmfit2, newdata=tibble(value_usd_scaled=0))
  lmfit2a <- lm(value_usd_scaled ~ sst_c_avg, sdata)
  sst_dir <- ifelse(coef(lmfit2a)[2]>0, "positive", "negative")
  payout_y <- ifelse(sst_dir=="positive", min(sdata$sst_c_avg), max(sdata$sst_c_avg))

  # Classify data


  # Theme
  my_theme <-  theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=13),
                     legend.text=element_text(size=12),
                     legend.title=element_text(size=13),
                     plot.title=element_blank(),
                     plot.tag = element_text(size=14, face="bold"),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))

  # Plot biomass
  g1 <- ggplot(sdata, aes(x=year, y=biomass/1000)) +
    geom_line() +
    # Labels
    labs(x="Year", y="Biomass\n(1000s mt)", tag="A") +
    lims(y=c(0,NA)) +
    # Theme
    theme_bw() +  my_theme
  g1

  # Plot catch
  g2 <- ggplot(sdata, aes(x=year, y=landings_lb/1000)) +
    geom_line() +
    # Labels
    labs(x="Year", y="Landings\n(1000s lbs)", tag="B") +
    lims(y=c(0,NA)) +
    # Theme
    theme_bw() +   my_theme
  g2

  # Plot price
  g3 <- ggplot(sdata, aes(x=year, y=price_usd_lb)) +
    geom_line() +
    # Labels
    labs(x="Year", y="Ex-vessel price\n(USD / lb)", tag="C") +
    lims(y=c(0,NA)) +
    # Theme
    theme_bw() +   my_theme
  g3

  # Plot revenue
  g4 <- ggplot(sdata, aes(x=year, y=value_usd / 1e6)) +
    geom_line() +
    # Labels
    labs(x="Year", y="Value\n(USD millions)", tag="D") +
    lims(y=c(0,NA)) +
    # Theme
    theme_bw() +   my_theme
  g4

  # Temperature
  g5 <- ggplot(sdata, aes(x=year, y=sst_c_avg)) +
    geom_line() +
    # Labels
    labs(x="Year", y=" \nTemperature (°C)", tag="E") +
    # Theme
    theme_bw() +   my_theme
  g5

  # Value ~ landings
  g6 <- ggplot(sdata, aes(x=landings_lb/1000, y=value_usd_scaled)) +
    geom_smooth(method="lm", fill="grey80", color="grey30") +
    geom_vline(xintercept=landings_lb_trig/1000, linetype="dashed") +
    geom_point(size=3) +
    # Labels
    labs(y="Value (scaled)", x="Landings (1000s lbs)", tag="F") +
    # Theme
    theme_bw() + my_theme
  g6

  # Value ~ SST
  g7 <- ggplot(sdata, aes(x=sst_c_avg, y=value_usd_scaled)) +
    geom_smooth(method="lm", fill="grey80", color="grey30") +
    geom_vline(xintercept=sst_c_trig, linetype="dashed") +
    geom_point(size=3) +
    # Labels
    labs(y="Value (scaled)", x="Temperature (°C)", tag="G") +
    # Theme
    theme_bw() + my_theme
  g7

  # Dual trigger
  g8 <- ggplot(sdata, aes(x=landings_lb/1000, y=sst_c_avg, fill=value_usd_scaled)) +
    # Reference lines
    geom_vline(xintercept = landings_lb_trig/1000, linetype="dashed") +
    geom_hline(yintercept = sst_c_trig, linetype="dashed") +
    # Data
    geom_point(pch=21, size=3) +
    # Label payout quadrant
    annotate(geom="text", label="Payout", x=payout_x, y=payout_y, hjust=payout_hjust,
             fontface="bold", size=5) +
    # Labels
    labs(x="Landings\n(1000s lbs)", y="Temperature (°C)", tag="H") +
    # Legend
    scale_fill_gradient2(name="Value (scaled)",
                         low="darkred", mid="white", high="navy") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = c(0.8, 0.8),
          legend.key.size = unit(0.5, "cm"))
  g8

  # Merge left
  g_left <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, ncol=1)
  g_left

  # Merge right
  layout_matrix <- matrix(data=c(1,2,
                                 3,3,
                                 4,4), ncol=2, byrow=T)
  g_right <- gridExtra::grid.arrange(g6, g7, g8,
                                     layout_matrix=layout_matrix,
                                     heights=c(0.25, 0.5, 0.25))

  # Merge more
  g <- gridExtra::grid.arrange(g_left, g_right,
                               widths=c(0.5, 0.5))
  g

}




