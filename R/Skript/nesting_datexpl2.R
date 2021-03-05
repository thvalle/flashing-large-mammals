# Nesting data by species -------------------------------------
by_sp <- time.dep %>% 
  mutate(species = validated_species) %>% 
  filter(species %in% c("raadyr", "rev", "hjort", "grevling", "elg", "gaupe")) %>% 
  group_by(species) %>% nest() #nest
# making a model function with glmer-model used on fox in Neris example above
sp_model <- function(df) {
  glmer(n.obs ~ time.deploy + as.factor(flash) + (1 | loc) + (1 | month), 
        data=df, family = poisson) # random effect arguments for loc and month
}
# applying model function on species data
#models <- map(by_sp$data, sp_model) #map is an interation function

# same thing, but adding each model into my tibble
by_sp <- by_sp %>% 
  mutate(model = map(data, sp_model))
by_sp

# by_sp %>%    #attempt to retrieve report from the nested models | failed
#   mutate(glance = map(model, report::report)) %>% 
#   unnest(glance)

glance_sp <- by_sp %>%
  mutate(glance = map(model, broom.mixed::glance)) %>% 
  unnest(glance) %>%   select(-data, -model) # as .drop in unnest is deprecated
glance_sp %>% 
  arrange(logLik)
tidy_sp <- by_sp %>% 
  mutate(tidy = map(model, broom.mixed::tidy)) %>% 
  unnest(tidy) %>%   select(-data, -model) # as .drop in unnest is deprecated






# Vignettes in package ‘lme4’:
#   
# Theory                                Computational Methods (source, pdf)
# lmer                                  Fitting Linear Mixed-Effects Models using lme4 (source, pdf)
# lmerperf                              lmer Performance Tips (source, html)
# PLSvGLS                               PLS vs GLS for LMMs (source, pdf)
vignette("Theory")
# *****        ********       *********        ********        **************
# broom vignettes
# adding-tidiers                        Adding tidiers to broom (source, html)
# available-methods                     Available methods (source, html)
# broom_and_dplyr                       broom and dplyr (source, html)
# broom                                 Introduction to broom (source, html)
# kmeans                                kmeans with dplyr and broom (source, html)
# bootstrapping                         Tidy bootstrapping (source, html)
vignette("broom")



vignette("introduction_partial_residuals",package = "ggeffects")

# chisq test with tidymodels ----------------------------
#r chisq
library(tidymodels)
Control <- filter(obs, period == "Control")
obs %>% 
  #  filter(!species %in% c("null", "nothing","ukjent")) %>%
  filter(species %in% c("raadyr","rev","elg", "grevling", "hjort","gaupe")) %>% 
  na.omit() %>% 
  chisq_test(species ~ period)

obs_chisq <- obs %>%   filter(species %in% c("raadyr","rev","elg", "grevling", "hjort","gaupe")) %>% 
  select(species, flash) 
# calculate observed statistic
observed_indep_statistic <- obs_chisq %>%
  specify(species ~ flash) %>%
  calculate(stat = "Chisq")
# generate the null distribution using randomization
null_distribution_simulated <- obs_chisq %>%
  specify(species ~ flash) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5000, type = "permute") %>%
  calculate(stat = "Chisq")
# generate the null distribution by theoretical approximation
null_distribution_theoretical <- obs_chisq %>%
  specify(species ~ flash) %>%
  hypothesize(null = "independence") %>%
  # note that we skip the generation step here!
  calculate(stat = "Chisq")
# visualize the null distribution and test statistic!
null_distribution_simulated %>%
  visualize() + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")
# visualize the theoretical null distribution and test statistic!
obs_chisq %>%
  specify(species ~ flash) %>%
  hypothesize(null = "independence") %>%
  visualize(method = "theoretical") + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")
# visualize both null distributions and the test statistic!
null_distribution_simulated %>%
  visualize(method = "both") + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")
# calculate the p value from the observed statistic and null distribution
p_value_independence <- null_distribution_simulated %>%
  get_p_value(obs_stat = observed_indep_statistic,
              direction = "greater")

p_value_independence

#Please be cautious in reporting a p-value of 0. This result is an approximation based on the number of `reps` chosen in the `generate()` step. See `?get_p_value()` for more information.

