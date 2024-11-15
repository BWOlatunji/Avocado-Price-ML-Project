
# Display the plot with a specific size

grid.arrange(p, top = textGrob("Distribution Price", gp = gpar(fontsize = 20)), widths = unit(12, "cm"), heights = unit(5, "cm"))

# Create the factor plot
ggplot(data_filtered, aes(x = AveragePrice, y = region, color = as.factor(year))) +
  geom_boxplot(size = 3) +
  facet_wrap(~ year, ncol = 1) +  # Split by year
  scale_color_viridis_d(option = "magma") +  # Use 'magma' palette from viridis package
  theme_minimal() +
  theme(legend.position = "right") +
  labs(color = "Year")

ggplot(data_organic, aes(x = AveragePrice, y = region, fill = as.factor(year))) +
  geom_boxplot() +
  facet_wrap(~ year, ncol = 1) +  # Facet by year
  scale_fill_viridis_d(option = "magma") +  # Use 'magma' palette from viridis package
  theme_minimal() +
  theme(legend.position = "right") +
  labs(fill = "Year", title = "Average Price Distribution by Region and Year")

# Summarize the data to calculate mean and range for each region and year
data_summary <- data_filtered %>%
  group_by(region, year) %>%
  summarise(mean_price = mean(AveragePrice, na.rm = TRUE),
            lower = mean_price - sd(AveragePrice, na.rm = TRUE),
            upper = mean_price + sd(AveragePrice, na.rm = TRUE)) 

# Create the plot
ggplot(data_summary, aes(x = mean_price, y = region, color = as.factor(year))) +
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 1) +
  scale_color_viridis_d(option = "magma") +  # Use 'magma' palette from viridis package
  theme_minimal() +
  theme(legend.position = "right") +
  labs(color = "Year", title = "Average Price with Range by Region and Year", x = "Average Price")
