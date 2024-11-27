# Filter out rows where new_condition is NA
summary_data <- summary_data %>%
  filter(!is.na(new_condition)) %>% # Remove NA conditions
  mutate(
    variable = recode(variable, 
                      greencar = "Car",
                      greendishwasher = "Dishwasher",
                      greensoap = "Soap",
                      total = "Total")
  )
# Create the combined plot
combined_plot <- ggplot(summary_data, aes(x = mean)) +
  geom_histogram(binwidth = 0.25, fill = "steelblue", color = "black") + # Set binwidth to 0.5
  facet_grid(variable ~ new_condition, scales = "free_y") + # Facet by variable and condition
  labs(
    title = "Distribution of Institution Means by Variable and Condition",
    x = "Mean Value",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 16), # Adjust facet label font size
    plot.title = element_text(size = 20, hjust = 0.5), # Center the title
    axis.text = element_text(size = 16), # Adjust axis text size
    axis.title = element_text(size = 16), # Adjust axis title size
    panel.background = element_rect(fill = "white", color = NA), # White background
    plot.background = element_rect(fill = "white", color = NA), # White plot background
    panel.grid.major = element_line(color = "gray90"), # Light gray gridlines
    panel.grid.minor = element_line(color = "gray95") # Even lighter gray for minor gridlines
  )

# Save the plot
ggsave(
  filename = "figures/combined_histograms_clean.png",
  plot = combined_plot,
  units = "cm",
  height = 25,
  width = 30,
  dpi = 300
)
