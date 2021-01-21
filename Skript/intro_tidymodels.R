# Intro to tidymodels

library(tidymodels)  # for the parsnip package, along with the rest of tidymodels
# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results

# Build a model ------------------------------------------------

urchins <-
  # Data were assembled for a tutorial 
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))
urchins
ggplot(urchins,
       aes(x = initial_volume, 
           y = width, 
           group = food_regime, 
           col = food_regime)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = "plasma", end = .7)

width ~ initial_volume * food_regime
?linear_reg()
linear_reg() %>% 
  set_engine("lm")
show_engines("linear_reg") # different engines one can set

lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")
# From here, the model can be estimated or trained using the fit() function:
lm_fit <-
  lm_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)
lm_fit
# This regression output can be shown graphically using the dotwhisker package:
tidy(lm_fit) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))

new_points <- expand.grid(initial_volume = 20, 
                          food_regime = c("Initial", "Low", "High"))
new_points

mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred

conf_int_pred <- predict(lm_fit, 
                         new_data = new_points, 
                         type = "conf_int")
conf_int_pred

# Now combine: 
plot_data <- 
  new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

# and plot:
ggplot(plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "urchin size")

# Trying a bayesian model: we need to generate PRIOR distributions
# The stan_glm can do this, which has been implemented in parsnip as the
# stan engine!

# set the prior distribution
      # prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)

# make the parsnip model
bayes_mod <-   
  linear_reg() %>% 
  set_engine("stan", 
             prior_intercept = prior_dist, 
             prior = prior_dist) 

# train the model
bayes_fit <- 
  bayes_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)
#
print(bayes_fit, digits = 5)
#
tidy(bayes_fit, conf.int = TRUE)
#
bayes_plot_data <- 
  new_points %>% 
  bind_cols(predict(bayes_fit, new_data = new_points)) %>% 
  bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))

ggplot(bayes_plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = .2) + 
  labs(y = "urchin size") + 
  ggtitle("Bayesian model with t(1) prior distribution")


# rstan installation attempts:
# 1
remove.packages("rstan") # no such package exists ~or something in those lines
if (file.exists(".RData")) file.remove(".RData")

Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1) # only necessary for Linux without the nodejs library / headers
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)  # 1st run: try removing rstan00LOCK - Done -> rerun -> freeze



# 2 Preprocess your data with recipes --------------------------------------------
library(tidymodels)      # for the recipes package, along with the rest of tidymodels

# Helper packages
library(nycflights13)    # for flight data
library(skimr)           # for variable summaries


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

glimpse(flight_data)

flight_data %>%
  skimr::skim(dest, carrier) 

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
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
  mutate(numeric_date = as.numeric(date))  # to give model a linear reference to date

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>% #DayOfWeek + month
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>% # and holidays, for explanatory power
  step_rm(date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%  # Em Inglés: Create dummy vars
#  for all of the factor or character columns unless they are outcomes.
  step_zv(all_predictors()) # removes LEX  which only contains 0, ie. no information

test_data %>%  # LEX needed to be removed  
  distinct(dest) %>%  # and was so, using the step_zv() function
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
flights_wflow
# prepare recipe and train the model from the resulting predictors:
flights_fit <- 
  flights_wflow %>% 
  fit(data = train_data)

