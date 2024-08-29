# Random effects meta-analysis with moderators

## List packages
packages = c("metafor", "effsize", "dplyr", "purrr")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## Import data
data <- read.csv("processed_data.csv")

# Recode the "setting" variable
data <- data %>%
  mutate(
    setting = case_when(
      setting %in% c("lab - individual", "lab - groups") ~ "Lab",
      TRUE ~ setting
    )
  )

# Change "US_or_other" to "location"
data <- data %>%
  rename(location = US_or_other)

## # Extract the year from the "project" variable
data <- data %>%
  mutate(year = as.numeric(substr(project, 2, 3)) + 2000)

## Create a variable that holds the mean participant age for each site
data <- data %>%
  group_by(project_code) %>%
  mutate(mean_age = mean(age, na.rm = TRUE)) %>%
  ungroup()

## Recode gender to 0 (female) and 1 (male)
data <- data %>%
  mutate(gender_code = ifelse(gender == "Male", 1, ifelse(gender == "Female", 0, NA)))

## Create a variable that holds the mean gender code (proportion Male) for each site
data <- data %>%
  group_by(project_code) %>%
  mutate(proportion_male = mean(gender_code, na.rm = TRUE)) %>%
  ungroup()

## Do the same thing for "republican/democrat" and "liborcon"
## Recode dem/rep to 0 and 1
data <- data %>%
  mutate(RorD_code = ifelse(RorD == "Republican", 1, ifelse(RorD == "Democrat", 0, NA)))
data <- data %>%
  group_by(project_code) %>%
  mutate(proportion_republican = mean(RorD_code, na.rm = TRUE)) %>%
  ungroup()

## Recode liborcon to 0 and 1
data <- data %>%
  mutate(LiborCon_code = ifelse(LiborCon == "Conservative", 1, ifelse(LiborCon == "Liberal", 0, NA)))
data <- data %>%
  group_by(project_code) %>%
  mutate(proportion_conservative = mean(LiborCon_code, na.rm = TRUE)) %>%
  ungroup()

# Function to calculate effect size and its variance for each project_code, ignoring NA values
calculate_effect_size <- function(df) {
  Control <- df %>% filter(new_condition == "Control" & !is.na(total)) %>% pull(total)
  Status <- df %>% filter(new_condition == "Status" & !is.na(total)) %>% pull(total)
  n_control <- length(Control)
  n_status <- length(Status)
  
  # Check if there are enough data points in both groups
  if (n_control > 1 & n_status > 1) {
    # Calculate Cohen's d effect size and its variance
    effect_size <- cohen.d(Status, Control)
    d_value <- effect_size$estimate
    variance <- ((n_control + n_status) / (n_control * n_status)) + (d_value^2 / (2 * (n_control + n_status)))
  } else {
    d_value <- NA  # Not enough data points to calculate effect size
    variance <- NA
  }
  
  return(list(d_value = d_value, variance = variance, n_control = n_control, n_status = n_status))
}
  

# Calculate effect sizes and their variances for each project_code and gather additional variables
effect_sizes <- data %>%
  group_by(project_code) %>%
  summarize(
    es_result = list(calculate_effect_size(cur_data())),
    location = first(location),
    setting = first(setting),
    year = first(year),
    mean_age = first(mean_age),
    proportion_male = first(proportion_male),
    proportion_republican = first(proportion_republican),
    proportion_conservative = first(proportion_conservative)
  ) %>%
  mutate(
    total_effect_d = map_dbl(es_result, ~ .x$d_value),
    variance = map_dbl(es_result, ~ .x$variance),
    n_control = map_dbl(es_result, ~ .x$n_control),
    n_status = map_dbl(es_result, ~ .x$n_status)
  ) %>%
  filter(!is.na(total_effect_d) & !is.na(variance))  # Remove rows with NA effect sizes

effect_sizes <- effect_sizes[, !(names(effect_sizes) %in% c("es_result"))]

# Check for missing values in effect_sizes
missing_summary_effect_sizes <- effect_sizes %>%
  summarize(across(everything(), ~ sum(is.na(.))))

print(missing_summary_effect_sizes)

# Create the vi column with the sampling variance
effect_sizes <- effect_sizes %>%
  mutate(vi = variance)


# Display the effect sizes
print(effect_sizes)

# Conduct meta-analysis using metafor with moderators
meta_analysis <- rma(yi = total_effect_d, vi = vi, 
                     mods = ~ setting + year + location, 
                     data = effect_sizes,
                     method = "REML")

# Display the meta-analysis results
print(meta_analysis)

effect_sizes <- effect_sizes %>% arrange(year, project_code)


# Save the effect sizes and meta-analysis results to a file
write.csv(effect_sizes, "effect_sizes.csv", row.names = FALSE)

# Create a forest plot for the mixed-effects meta-analysis
forest(meta_analysis, 
       xlab = "Effect Size (Cohen's d)", 
       slab = paste(effect_sizes$project_code),
       ilab = cbind(effect_sizes$location, effect_sizes$setting, effect_sizes$year,
                    effect_sizes$n_control, effect_sizes$n_status, effect_sizes$mean_age,
                    effect_sizes$proportion_conservative, effect_sizes$proportion_male),
       ilab.xpos = c(-4, -3.5, -3, -2.5, -2, -1.5, -1, -.5, 0),
       cex = 0.75)

# Add labels for the columns
# Add labels for the columns
op <- par(xpd = NA)
text(x = c(-2.7, -2.2, -1.8, -1.5, -1.2), y = max(meta_analysis$yi) + 28,
     labels = c("Location", "Setting", "Year", "n\nControl", "n\nStatus"),
     cex = 0.75, font = 2)
par(op)



