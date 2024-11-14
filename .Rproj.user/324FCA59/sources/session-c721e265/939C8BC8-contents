
# Load required packages
library(tidyverse)
library(tidymodels)
library(janitor)
library(lubridate)
library(corrplot)
library(gridExtra)

# First, let's read the data without column specifications to see what we're working with
soul_bikes <- read_csv("https://www4.stat.ncsu.edu/~online/datasets/SeoulBikeData.csv",
                        locale = locale(encoding = "latin1"))

# Now let's see what the actual column names are
names(soul_bikes)

#Transform:
#- dates to datetime formats
#- snake_case vars
# Clean and transform the data
soul_bikes_clean <- soul_bikes |>
  clean_names() |>
  mutate(
    date = dmy(date),
    seasons = factor(seasons),
    holiday = factor(holiday),
    functioning_day = factor(`functioning_day`)
  )

# Preview the cleaned data
glimpse(soul_bikes_clean)

# If you want to see the mapping of old to new names:
tibble(
  original_names = names(soul_bikes),
  cleaned_names = names(soul_bikes_clean)
)

#Basic EDA
#Missingness
missing_summ <- soul_bikes |>
  summarize(across(everything(), \(x) sum(is.na(x)))) |>
  pivot_longer(everything(),
                names_to = "variable",
                values_to = "missing_count")
missing_summ
#Result: No missing values


#By variable summary stats for numerics
soul_bikes_functioning <- soul_bikes_clean[soul_bikes$functioning_day == "Yes", ]
bike_summ_nums <- soul_bikes_functioning |>
  select(where(is.numeric)) |>
  summarize(across(everything(),
                   list(
                     mean = \(x) mean(x, na.rm = TRUE),
                     sd = \(x) sd(x, na.rm = TRUE),
                     median = \(x) median(x, na.rm = TRUE),
                     iqr = \(x) IQR(x, na.rm = TRUE),
                     min = \(x) min(x, na.rm = TRUE),
                     max = \(x) max(x, na.rm = TRUE),
                     n_missing = \(x) sum(is.na(x))
                   ))) |>
              pivot_longer(
                everything(),
                names_to = c("column", "stat"),
                names_pattern = "(.*)_(.*)",
                values_to = "values"
              )
bike_summ_nums

#By variable summary stats for character
bike_summ_cats <- soul_bikes_clean |>
  select(where(is.character), where(is.factor)) |>
  summarize(across(everything(),
    list(
      n_unique = \(x) n_distinct(x),
      n_missing = \(x) sum(is.na(x))
    )
  )) |>
  pivot_longer(
    everything(),
    names_to = c("column", "stat"),
    names_pattern = "(.*)_(.*)",
    values_to = "value"
  )
bike_summ_cats

factor_levels <- soul_bikes_clean |>
  select(where(is.factor), where(is.character)) |>
  summarize(across(everything(), \(x) list(sort(unique(x))))) |>
  pivot_longer(
    everything(),
    names_to = "column",
    values_to = "levels"
  ) |>
  mutate(
    n_levels = map_int(levels, length),
    levels = map_chr(levels, toString)
  )
factor_levels




#7. Summarize across hours
daily_summ <- soul_bikes_functioning |>
  group_by(date, seasons, holiday) |>
  summarize(
    #totals
    total_bikes = sum(rented_bike_count),
    total_rainfall = sum(rainfall_mm),
    total_snowfall = sum(snowfall_cm),

    #weather means
    avg_temp = mean(temperature_c),
    avg_humidity = mean(humidity_percent),
    avg_wind_speed = mean(wind_speed_m_s),
    avg_visibility = mean(visibility_10m),
    avg_dew_point = mean(dew_point_temperature_c),
    avg_solar_rad = mean(solar_radiation_mj_m2),
    .groups = "drop"  # Drop grouping after summary
  )
daily_summ

#Summaries of numerics
bike_summ_nums <- daily_summ |>
  select(where(is.numeric)) |>
  summarize(across(everything(),
                   list(
                     mean = \(x) mean(x, na.rm = TRUE),
                     sd = \(x) sd(x, na.rm = TRUE),
                     median = \(x) median(x, na.rm = TRUE),
                     iqr = \(x) IQR(x, na.rm = TRUE),
                     min = \(x) min(x, na.rm = TRUE),
                     max = \(x) max(x, na.rm = TRUE),
                     n_missing = \(x) sum(is.na(x))
                   ))) |>
  pivot_longer(
    everything(),
    names_to = c("column", "stat"),
    names_pattern = "(.*)_(.*)",
    values_to = "values"
  )
bike_summ_nums

#8 Plots

# Calculate correlation matrix
cor_matrix <- daily_summ |>
  select(total_bikes, avg_temp, avg_humidity, avg_wind_speed,
         avg_visibility, avg_solar_rad, total_rainfall, total_snowfall) |>
  cor(use = "complete.obs")
cor_matrix
# Create correlation plot
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE)

# Individual scatter plots with trend lines
temp_v_rentals <- ggplot(daily_summ, aes(x = avg_temp, y = total_bikes)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Temperature vs Bike Rentals",
       x = "Average Temperature (°C)",
       y = "Total Daily Rentals") +
  theme_minimal()

humid_v_rentals <- ggplot(daily_summ, aes(x = avg_humidity, y = total_bikes)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Humidity vs Bike Rentals",
       x = "Average Humidity (%)",
       y = "Total Daily Rentals") +
  theme_minimal()

sun_v_rentals <- ggplot(daily_summ, aes(x = avg_solar_rad, y = total_bikes)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "orange") +
  labs(title = "Solar Radiation vs Bike Rentals",
       x = "Average Solar Radiation (MJ/m2)",
       y = "Total Daily Rentals") +
  theme_minimal()

vis_v_rentals <- ggplot(daily_summ, aes(x = avg_visibility, y = total_bikes)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "green4") +
  labs(title = "Visibility vs Bike Rentals",
       x = "Average Visibility (10m)",
       y = "Total Daily Rentals") +
  theme_minimal()

# Arrange plots in a grid

grid.arrange(temp_v_rentals, humid_v_rentals, sun_v_rentals, vis_v_rentals, ncol = 2)


#Split the data
set.seed(123)
bike_split <- initial_split(daily_summ,
                            prop = 0.75,
                            strata = seasons)

trn <- training(bike_split)
tst <- testing(bike_split)
folds <- vfold_cv(trn, v = 10, strata = seasons)


#Fitting MLR Models
#Recipe 1
recipe_1 <- recipe(total_bikes ~ ., data = trn) |>
  update_role(date, new_role = "ID") |>
  # Extract day of week and create weekend factor
  step_date(date, features = "dow") |>
  step_mutate(
    weekend = factor(
      if_else(date_dow %in% c("Sat", "Sun"), "weekend", "weekday"),
      levels = c("weekday", "weekend")  # explicitly set levels
    )
  ) |>
  # Remove the intermediate date_dow variable
  step_rm(date_dow) |>
  # Standardize numeric variables
  step_normalize(all_numeric_predictors()) |>
  # Create dummy variables
  step_dummy(all_nominal_predictors())


recipe_1 |>
  prep() |>
  bake(new_data = NULL) |>
  glimpse()


#Recipe 2
recipe_2 <- recipe(total_bikes ~ ., data = trn) |>
  update_role(date, new_role = "ID") |>
  step_date(date, features = "dow") |>
  step_mutate(
    weekend = factor(
      if_else(date_dow %in% c("Sat", "Sun"), "weekend", "weekday"),
      levels = c("weekday", "weekend")
    )
  ) |>
  step_rm(date_dow) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  # Changed interaction syntax to use explicit formulas
  step_interact(terms = ~ starts_with("seasons_"):starts_with("holiday_")) |>
  step_interact(terms = ~ starts_with("seasons_"):avg_temp) |>
  step_interact(terms = ~ total_rainfall:avg_temp)

recipe_2 |>
  prep() |>
  bake(new_data = NULL) |>
  glimpse()

#Recipe 3

recipe_3 <- recipe(total_bikes ~ ., data = trn) |>
  update_role(date, new_role = "ID") |>
  step_date(date, features = "dow") |>
  step_mutate(
    weekend = factor(
      if_else(date_dow %in% c("Sat", "Sun"), "weekend", "weekday"),
      levels = c("weekday", "weekend")
    )
  ) |>
  step_rm(date_dow) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  # Modify interaction specification to use separate terms=
  step_interact(terms = ~ starts_with("seasons_") * starts_with("holiday_") +
                  starts_with("seasons_") * avg_temp +
                  total_rainfall * avg_temp) |>
  step_poly(total_rainfall, avg_temp, avg_humidity, avg_wind_speed,
            avg_visibility, avg_dew_point, avg_solar_rad,
            degree = 2)
# Let's verify it works
recipe_3 |>
  prep() |>
  bake(new_data = NULL) |>
  glimpse()


#setup LM engine
# Set up linear model specification
lm_spec <- linear_reg() |>
  set_engine("lm")

# Create workflows for each recipe
workflow_1 <- workflow() |>
  add_recipe(recipe_1) |>
  add_model(lm_spec)

workflow_2 <- workflow() |>
  add_recipe(recipe_2) |>
  add_model(lm_spec)

workflow_3 <- workflow() |>
  add_recipe(recipe_3) |>
  add_model(lm_spec)

# Fit models using 10-fold CV
cv_fit_1 <- workflow_1 |>
  fit_resamples(
    resamples = folds,
    metrics = metric_set(rmse)
  )

cv_fit_2 <- workflow_2 |>
  fit_resamples(
    resamples = folds,
    metrics = metric_set(rmse)
  )

cv_fit_3 <- workflow_3 |>
  fit_resamples(
    resamples = folds,
    metrics = metric_set(rmse)
  )

# Compare CV results
cv_results <- bind_rows(
  collect_metrics(cv_fit_1) |> mutate(model = "Model 1"),
  collect_metrics(cv_fit_2) |> mutate(model = "Model 2"),
  collect_metrics(cv_fit_3) |> mutate(model = "Model 3")
) |>
  arrange(mean)

print(cv_results)


#Model 3 is best fit
# Use last_fit with the best model (Model 3)
final_fit <- workflow_3 |>
  last_fit(bike_split)

# Get test set metrics
collect_metrics(final_fit)

# Get coefficients
final_fit |>
  extract_fit_parsnip() |>
  tidy()
