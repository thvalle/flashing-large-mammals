# From  https://www.tidymodels.org/start/recipes/

library(tidymodels)      # for the recipes package, along with the rest of tidymodels
# Helper packages
library(nycflights13)    # for flight data

set.seed(123)

flight_data <- 
  flights %>% 
  mutate(
    # Convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    # We will use the date (not date-time) in the recipe below
    date = as.Date(time_hour)
  ) %>% 
  # Include the weather data
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  # Only retain the specific columns we will use
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)

flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))


# Fix the random numbers by setting the seed 
set.seed(555)
# Put 3/4 of the data into the training set 
data_split <- initial_split(flight_data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# Recipe
summary(flights_rec)

flight_data %>% 
  distinct(date) %>% 
  mutate(numeric_date = as.numeric(date)) 

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>% 
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%  
  step_zv(all_predictors()) 

test_data %>%  
  distinct(dest) %>%  
  anti_join(train_data) 

# Fit a model with a recipe

lr_mod <-  # let's use logistic regression
  logistic_reg() %>% 
  set_engine("glm")
# pair a model and recipe together, using workflow()
flights_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(flights_rec)
# prepare recipe and train the model from the resulting predictors:
flights_fit <- 
  flights_wflow %>% 
  fit(data = train_data)


reprex::reprex()
