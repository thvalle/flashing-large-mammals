GLMM per art
================
Torgeir
01 mars, 2021

-   [Purpose](#purpose)
-   [About the model](#about-the-model)
    -   [Why GLMM?](#why-glmm)
    -   [Formula](#formula)
-   [Modelling](#modelling)
    -   [Roe deer](#roe-deer)
        -   [Diagnostics](#diagnostics)
        -   [Model interpretation](#model-interpretation)
        -   [Some parameter-plots from see and
            effectsize](#some-parameter-plots-from-see-and-effectsize)
-   [Skrivestopp](#skrivestopp)
    -   [Red Fox](#red-fox)
        -   [Interpret](#interpret)
    -   [Badger](#badger)
        -   [Interpret](#interpret-1)
    -   [Moose](#moose)
        -   [Interpret](#interpret-2)
    -   [Red deer](#red-deer)
        -   [Interpret](#interpret-3)
    -   [Lynx](#lynx)
        -   [Interpret](#interpret-4)
-   [Hare, deer and squirrelywhere (and pine
    marten)](#hare-deer-and-squirrelywhere-and-pine-marten)
    -   [Diagnostics](#diagnostics-1)
    -   [Interpret](#interpret-5)

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.6     v dplyr   1.0.4
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

``` r
library(ggeffects) # Estimated Marginal Means and Marginal Effects from Regression Models
# more at: https://strengejacke.github.io/ggeffects/
library(performance) # diagnostic-plots to check assumptions
library(report) # Result-summaries in text-format
```

    ## report is in alpha - help us improve by reporting bugs on github.com/easystats/report/issues

``` r
# Data drom Data_exploration2_nesting.R
time.dep <- readRDS("timedep.rds")
```

## Purpose

This notebook is meant to set up the final GLMModels going into my
thesis. Upset is identical to the “glmm\_in\_process”-file, but
structure is different as I’m going to make one model for each species.
I will write my thoughts about each model as I go along, which will
guide me in the writing of method, result and discussion.

# About the model

### Why GLMM?

Response from a [stackexchange
question](https://stats.stackexchange.com/questions/226946/r-lmer-vs-glmer)
about the differences of `lmer` and `glmer`-functions:

> lmer is used to fit linear mixed-effect models, so it assumes that the
> residual error has a Gaussian distribution. If your dependent variable
> A is a binary outcome (e.g. a yes/no response), then the error
> distribution is binomial and not Gaussian. In this case you have to
> use glmer, which allow to fit a generalized linear mixed-effects
> model: these models include a link function that allows to predict
> response variables with non-Gaussian distributions. One example of
> link function that could work in your case is the logistic function,
> which takes an input with any value from negative to positive infinity
> and return an output that always takes values between zero and one,
> which is interpretable as the probability of the binary outcome
> (e.g. the probability of the subject responding ‘yes’).

### Formula

The model formula I will use is $n  time.deploy \* flash $ for each
species, and my *α* = 0.05.

``` r
sp <- c("raadyr", "rev", "hjort", "grevling", "elg", "gaupe")
ctrl <- c("Control_1", "Control_2", "Control_3","Control_4")
time.dep2 <- time.dep %>% 
  rename(species = validated_species) %>%  #shortening name
  filter(species %in% sp) %>% #filtering out species
  # including Control as part of the flash-column, since it differs from flash=0
  mutate(flash = factor(
        ifelse(period %in% ctrl, "Control", flash)),
         week = lubridate::isoweek(date))
# time.dep2 <- time.dep2c %>% 
#   filter(!period %in% ctrl) %>%  # but removing it for now, because it is set up in a longer timeframe
#   mutate(flash = factor(flash, levels = c(0,1)) )
# class(time.dep2$flash)
```

Not all periods have identical length. Hence, I need to set a maximum
length for my period durations. As proposed by Neri, I will calculate
the median for white LED-periods and IR-periods, and use the smallest
median to shorten all periods overextending that value.

First I’ll filter out any periods shorter than 4 days ( *as of
18.02.2021, only 1 period* ). Then I’ll cut the duration of all periods
overextending the smallest median.

``` r
# filter out shortest periods, and find median period length
cut <- .3 # setting the minimum length of a period
# find lengths
time.period <- time.dep2 %>% group_by(loc, period, flash) %>% 
  summarise(period_length = max(time.deploy))
# checking which periods will be removed
time.period %>% filter(period_length <= cut) %>%
  arrange(period_length) #%>% kableExtra::kable("html")
```

    ## # A tibble: 1 x 4
    ## # Groups:   loc, period [1]
    ##   loc   period flash period_length
    ##   <fct> <chr>  <fct>         <dbl>
    ## 1 829   1_1    1                 0

``` r
# then merge lengths and filter based on that
time.dep3 <- time.dep2 %>% left_join(time.period) %>%
  filter(period_length >= cut)
# find median length after filtering short lengths out
time.period %>% filter(period_length >= cut) %>% filter(flash == 1) %>%  
  summary() # median period length 85 days, mean: 84
```

    ##       loc        period              flash    period_length   
    ##  15     : 2   Length:67          0      : 0   Min.   : 0.800  
    ##  127    : 2   Class :character   1      :67   1st Qu.: 6.550  
    ##  193    : 2   Mode  :character   Control: 0   Median : 8.500  
    ##  231    : 2                                   Mean   : 8.409  
    ##  257    : 2                                   3rd Qu.:11.000  
    ##  455    : 2                                   Max.   :13.200  
    ##  (Other):55

``` r
time.period %>% filter(period_length >= cut) %>% filter(flash == 0) %>%  
  summary() # median period length 79 days, mean: 89
```

    ##       loc        period              flash    period_length   
    ##  15     : 2   Length:69          0      :69   Min.   : 1.200  
    ##  127    : 2   Class :character   1      : 0   1st Qu.: 6.600  
    ##  193    : 2   Mode  :character   Control: 0   Median : 8.400  
    ##  231    : 2                                   Mean   : 9.423  
    ##  257    : 2                                   3rd Qu.:12.500  
    ##  455    : 2                                   Max.   :19.600  
    ##  (Other):57

``` r
# extract lengths of each unique period
h <- time.dep3 %>% group_by(loc, period, period_length, flash)%>% nest() %>% 
  select(!data) 
#extracting median and multiplying by 10, to use in the correctly scaled plot
hh <-       h$period_length[h$flash == 1] %>%  median()       # median white LED
hh <- c(hh, h$period_length[h$flash == 0] %>%  median()) * 10 # + median IR
# smallest median 
h <- min(hh)
```

79 days is the median period length of IR-periods, and it is shorter
than the median of white LED flash. The summary also tells us that

**Update:** With the updated dataset, the IR median has shifted to 84
days, white LED to 85. 84 is the new trimming value.

Periods below 5 days has now been removed, which as of 18.02.2021
(before updated data from Neri) were 1 period. With new data and Control
group, there are 3 periods filtered out, two of which were
Control-periods:

| loc | period | flash | period\_length |
|-----|--------|-------|:--------------:|
| 829 | 1\_1   | 1     |  0.0 = 0 days  |
| 258 | Ctrl1  | Ctrl  |  0.4 = 4 days  |
| 460 | Ctrl1  | Ctrl  |  0.5 = 5 days  |

``` r
# plot periods with median as intercept
p_td <- time.dep3 %>% filter(!period %in% ctrl) %>% 
  ggplot(aes(loc, 10*time.deploy, colour = period, ))  +
  geom_line(aes(linetype = flash),position = position_dodge(width = 1), lineend = "square") +
  coord_flip() + 
  labs(title = "Period lengths per camera",
       x = "Location", y = "Time since deployment",
       caption = "Vertical lines reprecent median period length for IR and white LED.\n Data superceding that is trimmed away for the GLMM-modelling.") +
  ggpubr::theme_classic2() #+ theme(legend.position = "right") find way to set legend inside
p_td + geom_hline(aes(yintercept = h), linetype = "dashed",  alpha =.5) +
  geom_hline(aes(yintercept = max(hh)), linetype = "dashed",  alpha =.5) +
  #annotate(geom = "text",x=4, y=h+8.6, label = "- median", size = 3, alpha =.7) +
  scale_y_continuous(breaks = sort(c(0, 50, h, 100, 150))) +
  scale_color_brewer(palette = "Spectral")
```

![](glmm_sp_files/figure-gfm/period-length-1.png)<!-- -->

``` r
# failed attempts that could inspire a better plot later
# p_td + geom_hline(aes(yintercept = h))+ # using median days as intercept 
#        scale_y_continuous(breaks = sort(c(ggplot_build(p_td)$layout$panel_ranges[[1]]$y.major_source, h)))
# geom_text(aes(25, h, label = "median", vjust = -1), nudge_y = 10, show.legend = F)
```

There was an overweight of IR-periods extending past the median line.

``` r
# remake plot with Control-group data, faceted
p_td2 <- time.dep3 %>% 
  ggplot(aes(loc, 10*time.deploy, colour = period))  +
  geom_line(aes(linetype = flash),position = position_dodge(width = 1), lineend = "square") +
  coord_flip() +  
  geom_hline(aes(yintercept = h), linetype = "dashed",  alpha =.5) +
  scale_y_continuous(breaks = sort(c(0, 50, h, 100, 150))) + facet_grid(rows = "flash") +
  labs(#title = "Period lengths per camera",
       x = "Location", y = "Time since deployment",
       caption = "Vertical line reprecents median period length for IR and white LED.\n Data superceding that is trimmed away for the GLMM-modelling.")
  

p_td2 + ggpubr::theme_classic2() +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        scale_colour_brewer(palette="Spectral")
```

![](glmm_sp_files/figure-gfm/period-length-wControl-1.png)<!-- -->

*Lastly, performing the filter:*

``` r
# filtering out periods longer than (shortest) median length.
time.dep4 <- time.dep3 %>% filter(time.deploy < h/10) # h is normal scale, must be rescaled by /10
```

# Modelling

## Roe deer

### Diagnostics

``` r
time.dep4$loc %>% unique() %>% is.na() %>% any()
```

    ## [1] FALSE

``` r
summary(time.dep4)
```

    ##       loc             date              species              flash      
    ##  488    : 1986   Min.   :2019-01-15   Length:90258       0      :30672  
    ##  494    : 1962   1st Qu.:2019-04-18   Class :character   1      :29268  
    ##  864    : 1962   Median :2019-07-28   Mode  :character   Control:30318  
    ##  861    : 1956   Mean   :2019-07-20                                     
    ##  863    : 1956   3rd Qu.:2019-10-16                                     
    ##  15     : 1938   Max.   :2020-02-26                                     
    ##  (Other):78498                                                          
    ##     period           time.deploy        n.obs            month      
    ##  Length:90258       Min.   :0.000   Min.   :0.0000   08     : 9156  
    ##  Class :character   1st Qu.:1.800   1st Qu.:0.0000   09     : 8892  
    ##  Mode  :character   Median :3.700   Median :0.0000   03     : 8556  
    ##                     Mean   :3.848   Mean   :0.0378   11     : 8124  
    ##                     3rd Qu.:5.800   3rd Qu.:0.0000   06     : 8106  
    ##                     Max.   :8.300   Max.   :6.0000   12     : 7872  
    ##                                                      (Other):39552  
    ##       week       period_length   
    ##  Min.   : 1.00   Min.   : 0.800  
    ##  1st Qu.:14.00   1st Qu.: 7.500  
    ##  Median :29.00   Median : 9.800  
    ##  Mean   :27.63   Mean   : 9.535  
    ##  3rd Qu.:40.00   3rd Qu.:11.800  
    ##  Max.   :52.00   Max.   :19.600  
    ## 

``` r
# filter species
sp = "raadyr"
time_sp <- filter(time.dep4, species %in% sp, !period %in% ctrl) #.dep4 = trimmed data
# Model
m_sp  <- lme4::glmer(n.obs ~ time.deploy * flash + # fixed effects
            (1 | loc) + (1 | week), # random effects
            data   = time_sp,
            family = poisson) # poisson family of distributions

# ggpredict is similar to expand.grid
p_sp <- ggeffects::ggpredict(m_sp, terms = c("time.deploy", "flash"))
# Diagnostics
plot(p_sp, add.data = TRUE) + labs(subtitle = "add.data = TRUE")
```

![](glmm_sp_files/figure-gfm/raadyr-1.png)<!-- -->

``` r
plot(p_sp, residuals = TRUE) + labs(subtitle = "residuals")
```

![](glmm_sp_files/figure-gfm/raadyr-2.png)<!-- -->

``` r
performance::check_model(m_sp) # check assumptions
```

    ## Loading required namespace: qqplotr

    ## `geom_smooth()` using formula 'y ~ x'

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 9846 rows containing missing values (geom_text_repel).

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: ggrepel: 139 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](glmm_sp_files/figure-gfm/raadyr-3.png)<!-- -->

The response variable is a summary of number of events per day. Most
days had no roe deer. In the performance-test for model assumptions *it
is clear that some assumptions aren’t met.*

#### Negative

In the non-normality of resiudals-plot, the residuals skew off from the
line when moving towards positive quantiles. There is *not* a
homogeneity of variance. *Maybe this could be fixed by centering the
n.obs-column?*

There appeares to be five influential observations in the Cook’s
distance-plot, maybe more, as the warning from ggrepel refers to 93
unlabeled data points due to overlaps.

#### Positive

Although the model has an interaction term between flash and time since
deployment, the multicollinearity between them is low! My random effects
follows a normal distribution.

#### Concluding

I am not sure about how to make up for breaking these assumptions. For
now, I will go on completing models for the rest of the species.

``` r
# Summary, report, model
summary(m_sp)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: n.obs ~ time.deploy * flash + (1 | loc) + (1 | week)
    ##    Data: time_sp
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   5044.5   5087.8  -2516.3   5032.5     9984 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.1278 -0.2871 -0.1761 -0.0897 13.2825 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  week   (Intercept) 0.3011   0.5487  
    ##  loc    (Intercept) 2.0987   1.4487  
    ## Number of obs: 9990, groups:  week, 52; loc, 36
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -3.35669    0.28430 -11.807   <2e-16 ***
    ## time.deploy        -0.02660    0.02560  -1.039    0.299    
    ## flash1              0.10871    0.12880   0.844    0.399    
    ## time.deploy:flash1 -0.01120    0.02998  -0.374    0.709    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash1
    ## time.deploy -0.328              
    ## flash1      -0.247  0.562       
    ## tm.dply:fl1  0.219 -0.681 -0.832

``` r
# report::report(m_sp) # text-summary of my model, to include in a report
plot(p_sp)
```

![](glmm_sp_files/figure-gfm/raadyr-report-1.png)<!-- -->

### Model interpretation

The intercept-value is considered significantly negative, which is to
say that there were a low chance of detecting any roe deer at an
IR-camera the same day I visited the camera.

> I saw a roe deer about to walk by a CT when I came to inspect it. The
> roe deer saw me and fled, right before it was detected by the camera.
> Chances are I’ve scared animals other times as well, but haven’t
> noticed it.

The effect of *Time since deployment* is non-significant, and
*β* = 0.008. That means there is no difference on the baseline detection
rate for an IR camera over time (after controlling for seasonal
changes).

For white LED flash *β* = 0.170, meaning that the intercept is slightly
higher than for IR, but the difference is non-significant (*p* = 0.18).
However, the detection rate is slightly decreasing the longer the white
LED stays, which differs from the IR, * but the effect is
non-significant* (*p* = 0.23).

#### Hypothesising

If there truly is an effect of the white LED for long periods on the
detection rate of roe deer, this effect could in turn account for the
different intercept values of IR and flash, as the IR periods often
start after a flash period.

Remembering my study design, 20 cameras start with white LED, 20 with
IR. *Intercept should be equal* (1st period). 2nd period; white LED
moved, new LED CTs (same intercept), new IR CTs (hypothetical lower
intercept due to flash effect). 3rd period; white LED moved, new LED CTs
(IR intercept), new IR CTs (hypothetical lower intercept). 4th period; -
- \|\| - - , new LED CTs (- - \| \| - - ), new IR CTs ( - - \| \| - - ).

Which sums up to 3 IR periods where detection rates could start lower
than that of white LED. And over time the lack of white LED flash in the
new IR sites would account for an *increase* in detection rates.

The effect would of course vary because some locations experienced
*gaps* due to full SD cards or empty batteries.
<!--_Maybe, after my thesis, I could try to pry out these differences-->

------------------------------------------------------------------------

#### Update!

After uptaded data from Neri; the effect of flash is now deemed
significant! Still the same pattern with higher intercept and a negative
slope along the time axis, which further supports my hypothesising
above. The interaction with time since deployment is, however, still
non-significant. *Now I want to look at a model including the
Control-group.*

``` r
# filter species
sp = "raadyr"
time_sp <- filter(time.dep4, species %in% sp) #.dep4 = trimmed data

# Model
m_raa  <- glmer(n.obs ~ time.deploy * flash + # fixed effects
              (1 | loc) + (1 | week), # random effects
            data   = time_sp,
            family = poisson) # poisson family of distributions
# --------------------------------------------------------------------
# ggpredict is similar to expand.grid
p_sp <- ggeffects::ggpredict(m_raa, terms = c("time.deploy", "flash"))
# Diagnostics
plot(p_sp, add.data = TRUE) + labs(subtitle  = "Raw data")
```

![](glmm_sp_files/figure-gfm/raadyr-C-1.png)<!-- -->

``` r
plot(p_sp, residuals = TRUE) + labs(subtitle  = "residuals")
```

![](glmm_sp_files/figure-gfm/raadyr-C-2.png)<!-- -->

``` r
performance::check_model(m_raa) # check assumptions
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 14746 rows containing missing values (geom_text_repel).

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: ggrepel: 295 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](glmm_sp_files/figure-gfm/raadyr-C-3.png)<!-- -->

There are a couple of extreme counts in the Control-group, even after
having filtered away all observations with less than a 15 min interval
for each species (ie. setting 15 min margin as definition of an event).
These extreme counts are skewed to the left which probably will affect
the intercept of control quite a bit, but probably won’t deem the
intercept-value as significant (as these extreme values are outliers).

The homogeneity of variance is still off. It deems on me more and more
that this is due to these extreme count-values that is close to
time.deploy = 0. *Will it disappear if I divide by the standard
deviation?*

The multicollinearity is moderate for the interaction term of
time.deploy and flash when the control group is included. Still, the
change is small (in my layman eyes), with a increase in bar height from
\~ 4 to \~ 6.

``` r
# Summary, report, model
summary(m_raa)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: n.obs ~ time.deploy * flash + (1 | loc) + (1 | week)
    ##    Data: time_sp
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7669.3   7730.2  -3826.6   7653.3    15035 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.1836 -0.2832 -0.1641 -0.0763 14.1898 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 2.6166   1.6176  
    ##  week   (Intercept) 0.3232   0.5685  
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              -3.39302    0.30628 -11.078   <2e-16 ***
    ## time.deploy              -0.02443    0.02343  -1.043    0.297    
    ## flash1                    0.10899    0.12577   0.867    0.386    
    ## flashControl             -0.21111    0.51213  -0.412    0.680    
    ## time.deploy:flash1       -0.01654    0.02927  -0.565    0.572    
    ## time.deploy:flashControl  0.01316    0.03205   0.411    0.681    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash1 flshCn tm.d:1
    ## time.deploy -0.276                            
    ## flash1      -0.223  0.584                     
    ## flashContrl -0.544  0.153  0.130              
    ## tm.dply:fl1  0.193 -0.706 -0.829 -0.106       
    ## tm.dply:flC  0.191 -0.698 -0.404 -0.229  0.481

``` r
plot(p_sp)
```

![](glmm_sp_files/figure-gfm/raadyr-C-report-1.png)<!-- -->

The control intercept is almost on top of the IR-intercept, but it has a
more negative trend than the IR-group. After I got updated data and
performed the event-filter, the slope of IR changed to a negative trend
over time, as well, “removing” any significance from the flash terms.
The important thing to note, then, is how small the p-values was, and
how easily they turned non-significant. Looking at the plot, the
confidence interval tells us all we need to know, as they almost
completely overlap. The groups are almost identical. The slope of the
control group is actually steeper than that of white LED (although only
minisculy). Further, the trend over time with the control-group
**should** be due to chance, *as time.deploy = 0 is seldom the actual
day I visited the cameras*. They were visited less often in general, and
the breaks leading to time.deploy = 0 are set manually by me, to make
period lengths that are similar to those of the IR and white LED group.

### Some parameter-plots from see and effectsize

The CI-plots of effectsize will be included in some form in my thesis.
For the equivalence-plot I’m not to sure if I’ll use it, but the
interpretation is neat with that method. From the bayesian statistics,
if a predictor is completely covered by the ROPE-area you can accept the
null hypothesis. This is not possible in a frequentist perspective (as
my models are), but one could claim *practical equivalence* if the
effect is non-significant AND completely inside the ROPE-interval.
Explanations from [Equivalence
vignette](https://easystats.github.io/parameters/reference/equivalence_test.lm.html):

> “classic” - The TOST rule (Lakens 2017) This rule follows the “TOST
> rule”, i.e. a two one-sided test procedure (Lakens 2017). Following
> this rule, practical equivalence of an effect (i.e. H0) is rejected,
> when the coefficient is statistically significant and the narrow
> confidence intervals (i.e. 1-2\*alpha) include or exceed the ROPE
> boundaries. Practical equivalence is assumed (i.e. H0 accepted) when
> the narrow confidence intervals are completely inside the ROPE, no
> matter if the effect is statistically significant or not. Else, the
> decision whether to accept or reject H0 is undecided. “cet” -
> Conditional Equivalence Testing (Campbell/Gustafson 2018) The
> Conditional Equivalence Testing as described by Campbell and Gustafson
> 2018. According to this rule, practical equivalence is rejected when
> the coefficient is statistically significant. When the effect is not
> significant and the narrow confidence intervals are completely inside
> the ROPE, we accept H0, else it is undecided

``` r
library(parameters)
library(effectsize)
library(see)
result <- model_parameters(m_raa)
plot(result, size_text = 3)
```

![](glmm_sp_files/figure-gfm/parameters-1.png)<!-- -->

``` r
result_stnd <- model_parameters(m_raa, standardize = "refit")
plot(result_stnd, show_intercept = TRUE) + labs(subtitle = ' standardize = "refit" ',
                                           caption = "Values are less centered on zero")
```

![](glmm_sp_files/figure-gfm/parameters-2.png)<!-- -->

``` r
# default rules, like in bayestestR::equivalence_test()
result <- equivalence_test(m_raa)
plot(result, size_text = 3)
```

![](glmm_sp_files/figure-gfm/parameters-3.png)<!-- -->

``` r
result
```

    ## # TOST-test for Practical Equivalence
    ## 
    ##   ROPE: [-0.10 0.10]
    ## 
    ##                      Parameter        H0 inside ROPE        90% CI
    ##                    (Intercept)  Rejected      0.00 % [-3.90 -2.89]
    ##                    time.deploy  Accepted    100.00 % [-0.06  0.01]
    ##                      flash [1] Undecided     47.83 % [-0.10  0.32]
    ##                flash [Control] Undecided     11.87 % [-1.05  0.63]
    ##        time.deploy * flash [1]  Accepted    100.00 % [-0.06  0.03]
    ##  time.deploy * flash [Control]  Accepted    100.00 % [-0.04  0.07]

``` r
r_raa <- report(m_raa) # text-summary of my model, to include in a report
summary(r_raa)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is substantial (conditional R2 = 0.45) and the part related to the fixed effects alone (marginal R2) is of 1.88e-03. The model's intercept is at -3.39 (95% CI [-3.99, -2.79]). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly negative (beta = -0.02, 95% CI [-0.07, 0.02], p = 0.297, Std. beta = -0.06)
    ##   - The effect of flash [1] is non-significantly positive (beta = 0.11, 95% CI [-0.14, 0.36], p = 0.386, Std. beta = 0.05)
    ##   - The effect of flash [Control] is non-significantly negative (beta = -0.21, 95% CI [-1.21, 0.79], p = 0.680, Std. beta = -0.16)
    ##   - The interaction effect of flash [1] on time.deploy is non-significantly negative (beta = -0.02, 95% CI [-0.07, 0.04], p = 0.572, Std. beta = -0.04)
    ##   - The interaction effect of flash [Control] on time.deploy is non-significantly positive (beta = 0.01, 95% CI [-0.05, 0.08], p = 0.681, Std. beta = 0.03)

``` r
as.report_table(r_raa)
```

    ## Parameter                     | Coefficient |         95% CI |      z |  df |      p | Std. Coef. | Std. Coef. 95% CI |      Fit
    ## --------------------------------------------------------------------------------------------------------------------------------
    ## (Intercept)                   |       -3.39 | [-3.99, -2.79] | -11.08 | Inf | < .001 |      -3.49 |    [-4.06, -2.91] |         
    ## time.deploy                   |       -0.02 | [-0.07,  0.02] |  -1.04 | Inf | 0.297  |      -0.06 |    [-0.17,  0.05] |         
    ## flash [1]                     |        0.11 | [-0.14,  0.36] |   0.87 | Inf | 0.386  |       0.05 |    [-0.09,  0.18] |         
    ## flash [Control]               |       -0.21 | [-1.21,  0.79] |  -0.41 | Inf | 0.680  |      -0.16 |    [-1.14,  0.82] |         
    ## time.deploy * flash [1]       |       -0.02 | [-0.07,  0.04] |  -0.57 | Inf | 0.572  |      -0.04 |    [-0.17,  0.10] |         
    ## time.deploy * flash [Control] |        0.01 | [-0.05,  0.08] |   0.41 | Inf | 0.681  |       0.03 |    [-0.12,  0.18] |         
    ##                               |             |                |        |     |        |            |                   |         
    ## AIC                           |             |                |        |     |        |            |                   |  7669.27
    ## BIC                           |             |                |        |     |        |            |                   |  7730.22
    ## R2 (conditional)              |             |                |        |     |        |            |                   |     0.45
    ## R2 (marginal)                 |             |                |        |     |        |            |                   | 1.88e-03
    ## Sigma                         |             |                |        |     |        |            |                   |     1.00

# Skrivestopp

## Red Fox

``` r
# filter species
sp = "rev"
time_sp <- filter(time.dep4, species %in% sp) #.dep4 = trimmed data
# Model
m_rev  <- glmer(n.obs ~ time.deploy * flash + # fixed effects
              (1 | loc) + (1 | week), # random effects
            data   = time_sp,
            family = poisson) # poisson family of distributions

# ggpredict is similar to expand.grid
p_sp <- ggeffects::ggpredict(m_rev, terms = c("time.deploy", "flash"))
# Diagnostics
plot(p_sp, add.data = TRUE) + labs(subtitle = "add.data = TRUE")
```

![](glmm_sp_files/figure-gfm/rev-1.png)<!-- -->

``` r
plot(p_sp, residuals = TRUE) + labs(subtitle = "residuals")
```

![](glmm_sp_files/figure-gfm/rev-2.png)<!-- -->

``` r
performance::check_model(m_rev) # check assumptions
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 14853 rows containing missing values (geom_text_repel).

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: ggrepel: 186 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](glmm_sp_files/figure-gfm/rev-3.png)<!-- -->

### Interpret

``` r
# Summary, report, model
summary(m_rev)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: n.obs ~ time.deploy * flash + (1 | loc) + (1 | week)
    ##    Data: time_sp
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   5619.9   5680.8  -2801.9   5603.9    15035 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.6303 -0.2405 -0.1838 -0.1304 13.6640 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 0.74695  0.8643  
    ##  week   (Intercept) 0.06125  0.2475  
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              -3.4114602  0.1980593 -17.224   <2e-16 ***
    ## time.deploy              -0.0007902  0.0285064  -0.028    0.978    
    ## flash1                    0.1531968  0.1658902   0.923    0.356    
    ## flashControl             -0.0426496  0.3186060  -0.134    0.894    
    ## time.deploy:flash1       -0.0087114  0.0372372  -0.234    0.815    
    ## time.deploy:flashControl -0.0005255  0.0404039  -0.013    0.990    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash1 flshCn tm.d:1
    ## time.deploy -0.541                            
    ## flash1      -0.440  0.587                     
    ## flashContrl -0.592  0.329  0.271              
    ## tm.dply:fl1  0.378 -0.695 -0.848 -0.231       
    ## tm.dply:flC  0.379 -0.692 -0.409 -0.488  0.483

``` r
plot(p_sp)
```

![](glmm_sp_files/figure-gfm/rev-report-1.png)<!-- -->

``` r
r_rev <- report(m_rev) # text-summary of my model, to include in a report
as.report_text(r_rev, summary=T)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is moderate (conditional R2 = 0.19) and the part related to the fixed effects alone (marginal R2) is of 1.16e-03. The model's intercept is at -3.41 (95% CI [-3.80, -3.02]). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly negative (beta = -7.90e-04, 95% CI [-0.06, 0.06], p = 0.978, Std. beta = -1.87e-03)
    ##   - The effect of flash [1] is non-significantly positive (beta = 0.15, 95% CI [-0.17, 0.48], p = 0.356, Std. beta = 0.12)
    ##   - The effect of flash [Control] is non-significantly negative (beta = -0.04, 95% CI [-0.67, 0.58], p = 0.894, Std. beta = -0.04)
    ##   - The interaction effect of flash [1] on time.deploy is non-significantly negative (beta = -8.71e-03, 95% CI [-0.08, 0.06], p = 0.815, Std. beta = -0.02)
    ##   - The interaction effect of flash [Control] on time.deploy is non-significantly negative (beta = -5.25e-04, 95% CI [-0.08, 0.08], p = 0.990, Std. beta = -1.21e-03)

## Badger

``` r
# filter species
sp = "grevling"
time_sp <- filter(time.dep4, species %in% sp) #.dep4 = trimmed data
# Model
m_grvl  <- glmer(n.obs ~ time.deploy * flash + # fixed effects
              (1 | loc) + (1 | week), # random effects
            data   = time_sp,
            family = poisson) # poisson family of distributions

# ggpredict is similar to expand.grid
p_sp <- ggeffects::ggpredict(m_grvl, terms = c("time.deploy", "flash"))
# Diagnostics
plot(p_sp, add.data = TRUE) + labs(subtitle  = "add.data = TRUE")
```

![](glmm_sp_files/figure-gfm/grevling-1.png)<!-- -->

``` r
plot(p_sp, residuals = TRUE) + labs(subtitle  = "residuals")
```

![](glmm_sp_files/figure-gfm/grevling-2.png)<!-- -->

``` r
performance::check_model(m_grvl) # check assumptions
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 14752 rows containing missing values (geom_text_repel).

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: ggrepel: 288 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](glmm_sp_files/figure-gfm/grevling-3.png)<!-- -->

### Interpret

``` r
# Summary, report, model
summary(m_grvl)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: n.obs ~ time.deploy * flash + (1 | loc) + (1 | week)
    ##    Data: time_sp
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4673.2   4734.2  -2328.6   4657.2    15035 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.0953 -0.1976 -0.1167 -0.0628 29.5365 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 1.461    1.209   
    ##  week   (Intercept) 1.619    1.272   
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              -4.491946   0.319319 -14.067   <2e-16 ***
    ## time.deploy               0.060290   0.032319   1.865   0.0621 .  
    ## flash1                    0.061151   0.187005   0.327   0.7437    
    ## flashControl             -0.159825   0.432726  -0.369   0.7119    
    ## time.deploy:flash1       -0.001114   0.039462  -0.028   0.9775    
    ## time.deploy:flashControl -0.048149   0.048780  -0.987   0.3236    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash1 flshCn tm.d:1
    ## time.deploy -0.414                            
    ## flash1      -0.322  0.592                     
    ## flashContrl -0.485  0.333  0.237              
    ## tm.dply:fl1  0.293 -0.687 -0.874 -0.208       
    ## tm.dply:flC  0.303 -0.726 -0.391 -0.452  0.446

``` r
plot(p_sp)
```

![](glmm_sp_files/figure-gfm/grevling-report-1.png)<!-- -->

``` r
r_grvl <- report(m_grvl) # text-summary of my model, to include in a report
as.report_text(r_grvl, summary=T)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is substantial (conditional R2 = 0.42) and the part related to the fixed effects alone (marginal R2) is of 5.93e-03. The model's intercept is at -4.49 (95% CI [-5.12, -3.87]). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly positive (beta = 0.06, 95% CI [-3.05e-03, 0.12], p = 0.062, Std. beta = 0.14)
    ##   - The effect of flash [1] is non-significantly positive (beta = 0.06, 95% CI [-0.31, 0.43], p = 0.744, Std. beta = 0.06)
    ##   - The effect of flash [Control] is non-significantly negative (beta = -0.16, 95% CI [-1.01, 0.69], p = 0.712, Std. beta = -0.35)
    ##   - The interaction effect of flash [1] on time.deploy is non-significantly negative (beta = -1.11e-03, 95% CI [-0.08, 0.08], p = 0.977, Std. beta = -2.65e-03)
    ##   - The interaction effect of flash [Control] on time.deploy is non-significantly negative (beta = -0.05, 95% CI [-0.14, 0.05], p = 0.324, Std. beta = -0.11)

## Moose

``` r
# filter species
sp = "elg"
time_sp <- filter(time.dep4, species %in% sp) #.dep4 = trimmed data
# Model
m_elg  <- glmer(n.obs ~ time.deploy * flash + # fixed effects
              (1 | loc) + (1 | week), # random effects
            data   = time_sp,
            family = poisson) # poisson family of distributions
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.102245 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
# ggpredict is similar to expand.grid
p_sp <- ggeffects::ggpredict(m_elg, terms = c("time.deploy", "flash"))
# Diagnostics
plot(p_sp, add.data = TRUE) + labs(subtitle  = "add.data = TRUE")
```

![](glmm_sp_files/figure-gfm/elg-1.png)<!-- -->

``` r
plot(p_sp, residuals = TRUE) + labs(subtitle  = "residuals")
```

![](glmm_sp_files/figure-gfm/elg-2.png)<!-- -->

``` r
performance::check_model(m_elg) # check assumptions
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 14787 rows containing missing values (geom_text_repel).

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: ggrepel: 252 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](glmm_sp_files/figure-gfm/elg-3.png)<!-- -->

### Interpret

``` r
# Summary, report, model
summary(m_elg)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: n.obs ~ time.deploy * flash + (1 | loc) + (1 | week)
    ##    Data: time_sp
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2981.9   3042.9  -1483.0   2965.9    15035 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.4540 -0.1618 -0.1047 -0.0663 20.6438 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 1.456    1.207   
    ##  week   (Intercept) 0.558    0.747   
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                            Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)              -4.7988876  0.0006913 -6941.69   <2e-16 ***
    ## time.deploy               0.0475324  0.0006917    68.71   <2e-16 ***
    ## flash1                    0.3852109  0.0006911   557.37   <2e-16 ***
    ## flashControl             -0.0855770  0.0006912  -123.81   <2e-16 ***
    ## time.deploy:flash1       -0.0617140  0.0006911   -89.30   <2e-16 ***
    ## time.deploy:flashControl -0.0317791  0.0006913   -45.97   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash1 flshCn tm.d:1
    ## time.deploy 0.000                             
    ## flash1      0.000  0.000                      
    ## flashContrl 0.000  0.000  0.000               
    ## tm.dply:fl1 0.000  0.000  0.000  0.000        
    ## tm.dply:flC 0.000  0.000  0.000  0.000  0.000 
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.102245 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
plot(p_sp)
```

![](glmm_sp_files/figure-gfm/elg-report-1.png)<!-- -->

``` r
r_elg <- report(m_elg) # text-summary of my model, to include in a report
as.report_text(r_elg, summary=T)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is substantial (conditional R2 = 0.31) and the part related to the fixed effects alone (marginal R2) is of 3.91e-03. The model's intercept is at -4.80 (95% CI [-4.80, -4.80]). Within this model:
    ## 
    ##   - The effect of time.deploy is significantly positive (beta = 0.05, 95% CI [0.05, 0.05], p < .001, Std. beta = 0.11)
    ##   - The effect of flash [1] is significantly positive (beta = 0.39, 95% CI [0.38, 0.39], p < .001, Std. beta = 0.15)
    ##   - The effect of flash [Control] is significantly negative (beta = -0.09, 95% CI [-0.09, -0.08], p < .001, Std. beta = -0.19)
    ##   - The interaction effect of flash [1] on time.deploy is significantly negative (beta = -0.06, 95% CI [-0.06, -0.06], p < .001, Std. beta = -0.14)
    ##   - The interaction effect of flash [Control] on time.deploy is significantly negative (beta = -0.03, 95% CI [-0.03, -0.03], p < .001, Std. beta = -0.06)

## Red deer

``` r
# filter species
sp = "hjort"
time_sp <- filter(time.dep4, species %in% sp) #.dep4 = trimmed data
# Model
m_hjort  <- glmer(n.obs ~ time.deploy * flash + # fixed effects
              (1 | loc) + (1 | week), # random effects
            data   = time_sp,
            family = poisson) # poisson family of distributions
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0057325 (tol = 0.002, component 1)

``` r
# ggpredict is similar to expand.grid
p_sp <- ggeffects::ggpredict(m_hjort, terms = c("time.deploy", "flash"))
# Diagnostics
plot(p_sp, add.data = TRUE) + labs(subtitle  = "add.data = TRUE")
```

![](glmm_sp_files/figure-gfm/hjort-1.png)<!-- -->

``` r
plot(p_sp, residuals = TRUE) + labs(subtitle  = "residuals")
```

![](glmm_sp_files/figure-gfm/hjort-2.png)<!-- -->

``` r
performance::check_model(m_hjort) # check assumptions
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 14656 rows containing missing values (geom_text_repel).

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: ggrepel: 386 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](glmm_sp_files/figure-gfm/hjort-3.png)<!-- -->

### Interpret

``` r
# Summary, report, model
summary(m_hjort)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: n.obs ~ time.deploy * flash + (1 | loc) + (1 | week)
    ##    Data: time_sp
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     1788     1849     -886     1772    15035 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.5198 -0.1101 -0.0394 -0.0256 24.8132 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 4.5183   2.1256  
    ##  week   (Intercept) 0.2999   0.5476  
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              -5.95762    0.54739 -10.884   <2e-16 ***
    ## time.deploy              -0.02601    0.05877  -0.442   0.6581    
    ## flash1                   -0.63586    0.36729  -1.731   0.0834 .  
    ## flashControl             -0.17339    0.81502  -0.213   0.8315    
    ## time.deploy:flash1        0.15672    0.07884   1.988   0.0468 *  
    ## time.deploy:flashControl -0.03850    0.08456  -0.455   0.6489    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash1 flshCn tm.d:1
    ## time.deploy -0.396                            
    ## flash1      -0.288  0.519                     
    ## flashContrl -0.495  0.248  0.178              
    ## tm.dply:fl1  0.283 -0.684 -0.867 -0.170       
    ## tm.dply:flC  0.266 -0.664 -0.343 -0.381  0.453
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.0057325 (tol = 0.002, component 1)

``` r
# report::report(m_hjort) # text-summary of my model, to include in a report
plot(p_sp)
```

![](glmm_sp_files/figure-gfm/hjort-report-1.png)<!-- -->

``` r
r_hjort <- report(m_hjort) # text-summary of my model, to include in a report
as.report_text(r_hjort, summary=T)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is substantial (conditional R2 = 0.44) and the part related to the fixed effects alone (marginal R2) is of 5.52e-03. The model's intercept is at -5.96 (95% CI [-7.03, -4.88]). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly negative (beta = -0.03, 95% CI [-0.14, 0.09], p = 0.658, Std. beta = -0.06)
    ##   - The effect of flash [1] is non-significantly negative (beta = -0.64, 95% CI [-1.36, 0.08], p = 0.083, Std. beta = -0.03)
    ##   - The effect of flash [Control] is non-significantly negative (beta = -0.17, 95% CI [-1.77, 1.42], p = 0.832, Std. beta = -0.32)
    ##   - The interaction effect of flash [1] on time.deploy is significantly positive (beta = 0.16, 95% CI [2.19e-03, 0.31], p < .05, Std. beta = 0.37)
    ##   - The interaction effect of flash [Control] on time.deploy is non-significantly negative (beta = -0.04, 95% CI [-0.20, 0.13], p = 0.649, Std. beta = -0.09)

## Lynx

``` r
# filter species
sp = "gaupe"
time_sp <- filter(time.dep4, species %in% sp) #.dep4 = trimmed data
# Model
m_gaup  <- glmer(n.obs ~ time.deploy * flash + # fixed effects
              (1 | loc) + (1 | week), # random effects
            data   = time_sp,
            family = poisson) # poisson family of distributions
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0532481 (tol = 0.002, component 1)

``` r
# ggpredict is similar to expand.grid
p_sp <- ggeffects::ggpredict(m_gaup, terms = c("time.deploy", "flash"))
# Diagnostics
plot(p_sp, add.data = TRUE) + labs(subtitle  = "add.data = TRUE")
```

![](glmm_sp_files/figure-gfm/gaupe-1.png)<!-- -->

``` r
plot(p_sp, residuals = TRUE) + labs(subtitle  = "residuals")
```

![](glmm_sp_files/figure-gfm/gaupe-2.png)<!-- -->

``` r
performance::check_model(m_gaup) # check assumptions
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 14787 rows containing missing values (geom_text_repel).

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: ggrepel: 255 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](glmm_sp_files/figure-gfm/gaupe-3.png)<!-- -->

### Interpret

``` r
# Summary, report, model
summary(m_gaup)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: n.obs ~ time.deploy * flash + (1 | loc) + (1 | week)
    ##    Data: time_sp
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    747.7    808.6   -365.8    731.7    15035 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.3296 -0.0556 -0.0319 -0.0254 23.5615 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 2.2755   1.5085  
    ##  week   (Intercept) 0.3021   0.5496  
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              -6.88857    0.59799 -11.519   <2e-16 ***
    ## time.deploy               0.03578    0.08985   0.398    0.691    
    ## flash1                    0.36661    0.56014   0.654    0.513    
    ## flashControl              0.07134    0.86849   0.082    0.935    
    ## time.deploy:flash1        0.00500    0.12072   0.041    0.967    
    ## time.deploy:flashControl -0.10125    0.15784  -0.641    0.521    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash1 flshCn tm.d:1
    ## time.deploy -0.604                            
    ## flash1      -0.526  0.612                     
    ## flashContrl -0.537  0.400  0.336              
    ## tm.dply:fl1  0.423 -0.715 -0.852 -0.278       
    ## tm.dply:flC  0.337 -0.556 -0.334 -0.669  0.393
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.0532481 (tol = 0.002, component 1)

``` r
plot(p_sp)
```

![](glmm_sp_files/figure-gfm/gaupe-report-1.png)<!-- -->

``` r
r_gaup <- report(m_gaup) # text-summary of my model, to include in a report
as.report_text(r_gaup, summary=T)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is substantial (conditional R2 = 0.28) and the part related to the fixed effects alone (marginal R2) is of 0.01. The model's intercept is at -6.89 (95% CI [-8.06, -5.72]). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly positive (beta = 0.04, 95% CI [-0.14, 0.21], p = 0.691, Std. beta = 0.08)
    ##   - The effect of flash [1] is non-significantly positive (beta = 0.37, 95% CI [-0.73, 1.46], p = 0.513, Std. beta = 0.39)
    ##   - The effect of flash [Control] is non-significantly positive (beta = 0.07, 95% CI [-1.63, 1.77], p = 0.935, Std. beta = -0.33)
    ##   - The interaction effect of flash [1] on time.deploy is non-significantly positive (beta = 5.00e-03, 95% CI [-0.23, 0.24], p = 0.967, Std. beta = 0.02)
    ##   - The interaction effect of flash [Control] on time.deploy is non-significantly negative (beta = -0.10, 95% CI [-0.41, 0.21], p = 0.521, Std. beta = -0.23)

------------------------------------------------------------------------

``` r
# library(rstanarm)
# #result <- #compare_parameters(m_raa, m_rev, m_grvl, m_elg, m_hjort, m_gaup) #can't find function :-/
# plot(result)
# 
# rmarkdown::render("glmm_sp.Rmd", output_format = "github_document")
```

``` r
as.report_table(r_raa  , summary = T)
```

    ## Parameter                     | Coefficient |         95% CI |      z |  df |      p | Std. Coef. |      Fit
    ## ------------------------------------------------------------------------------------------------------------
    ## (Intercept)                   |       -3.39 | [-3.99, -2.79] | -11.08 | Inf | < .001 |      -3.49 |         
    ## time.deploy                   |       -0.02 | [-0.07,  0.02] |  -1.04 | Inf | 0.297  |      -0.06 |         
    ## flash [1]                     |        0.11 | [-0.14,  0.36] |   0.87 | Inf | 0.386  |       0.05 |         
    ## flash [Control]               |       -0.21 | [-1.21,  0.79] |  -0.41 | Inf | 0.680  |      -0.16 |         
    ## time.deploy * flash [1]       |       -0.02 | [-0.07,  0.04] |  -0.57 | Inf | 0.572  |      -0.04 |         
    ## time.deploy * flash [Control] |        0.01 | [-0.05,  0.08] |   0.41 | Inf | 0.681  |       0.03 |         
    ##                               |             |                |        |     |        |            |         
    ## R2 (conditional)              |             |                |        |     |        |            |     0.45
    ## R2 (marginal)                 |             |                |        |     |        |            | 1.88e-03
    ## Sigma                         |             |                |        |     |        |            |     1.00

``` r
as.report_table(r_rev  , summary = T)
```

    ## Parameter                     | Coefficient |         95% CI |      z |  df |      p | Std. Coef. |      Fit
    ## ------------------------------------------------------------------------------------------------------------
    ## (Intercept)                   |       -3.41 | [-3.80, -3.02] | -17.22 | Inf | < .001 |      -3.41 |         
    ## time.deploy                   |   -7.90e-04 | [-0.06,  0.06] |  -0.03 | Inf | 0.978  |  -1.87e-03 |         
    ## flash [1]                     |        0.15 | [-0.17,  0.48] |   0.92 | Inf | 0.356  |       0.12 |         
    ## flash [Control]               |       -0.04 | [-0.67,  0.58] |  -0.13 | Inf | 0.894  |      -0.04 |         
    ## time.deploy * flash [1]       |   -8.71e-03 | [-0.08,  0.06] |  -0.23 | Inf | 0.815  |      -0.02 |         
    ## time.deploy * flash [Control] |   -5.25e-04 | [-0.08,  0.08] |  -0.01 | Inf | 0.990  |  -1.21e-03 |         
    ##                               |             |                |        |     |        |            |         
    ## R2 (conditional)              |             |                |        |     |        |            |     0.19
    ## R2 (marginal)                 |             |                |        |     |        |            | 1.16e-03
    ## Sigma                         |             |                |        |     |        |            |     1.00

``` r
as.report_table(r_grvl , summary = T)
```

    ## Parameter                     | Coefficient |         95% CI |      z |  df |      p | Std. Coef. |      Fit
    ## ------------------------------------------------------------------------------------------------------------
    ## (Intercept)                   |       -4.49 | [-5.12, -3.87] | -14.07 | Inf | < .001 |      -4.26 |         
    ## time.deploy                   |        0.06 | [ 0.00,  0.12] |   1.87 | Inf | 0.062  |       0.14 |         
    ## flash [1]                     |        0.06 | [-0.31,  0.43] |   0.33 | Inf | 0.744  |       0.06 |         
    ## flash [Control]               |       -0.16 | [-1.01,  0.69] |  -0.37 | Inf | 0.712  |      -0.35 |         
    ## time.deploy * flash [1]       |   -1.11e-03 | [-0.08,  0.08] |  -0.03 | Inf | 0.977  |  -2.65e-03 |         
    ## time.deploy * flash [Control] |       -0.05 | [-0.14,  0.05] |  -0.99 | Inf | 0.324  |      -0.11 |         
    ##                               |             |                |        |     |        |            |         
    ## R2 (conditional)              |             |                |        |     |        |            |     0.42
    ## R2 (marginal)                 |             |                |        |     |        |            | 5.93e-03
    ## Sigma                         |             |                |        |     |        |            |     1.00

``` r
as.report_table(r_elg  , summary = T)
```

    ## Parameter                     | Coefficient |         95% CI |        z |  df |      p | Std. Coef. |      Fit
    ## --------------------------------------------------------------------------------------------------------------
    ## (Intercept)                   |       -4.80 | [-4.80, -4.80] | -6941.69 | Inf | < .001 |      -4.62 |         
    ## time.deploy                   |        0.05 | [ 0.05,  0.05] |    68.71 | Inf | < .001 |       0.11 |         
    ## flash [1]                     |        0.39 | [ 0.38,  0.39] |   557.37 | Inf | < .001 |       0.15 |         
    ## flash [Control]               |       -0.09 | [-0.09, -0.08] |  -123.81 | Inf | < .001 |      -0.19 |         
    ## time.deploy * flash [1]       |       -0.06 | [-0.06, -0.06] |   -89.30 | Inf | < .001 |      -0.14 |         
    ## time.deploy * flash [Control] |       -0.03 | [-0.03, -0.03] |   -45.97 | Inf | < .001 |      -0.06 |         
    ##                               |             |                |          |     |        |            |         
    ## R2 (conditional)              |             |                |          |     |        |            |     0.31
    ## R2 (marginal)                 |             |                |          |     |        |            | 3.91e-03
    ## Sigma                         |             |                |          |     |        |            |     1.00

``` r
as.report_table(r_hjort, summary = T)
```

    ## Parameter                     | Coefficient |         95% CI |      z |  df |      p | Std. Coef. |      Fit
    ## ------------------------------------------------------------------------------------------------------------
    ## (Intercept)                   |       -5.96 | [-7.03, -4.88] | -10.88 | Inf | < .001 |      -6.06 |         
    ## time.deploy                   |       -0.03 | [-0.14,  0.09] |  -0.44 | Inf | 0.658  |      -0.06 |         
    ## flash [1]                     |       -0.64 | [-1.36,  0.08] |  -1.73 | Inf | 0.083  |      -0.03 |         
    ## flash [Control]               |       -0.17 | [-1.77,  1.42] |  -0.21 | Inf | 0.832  |      -0.32 |         
    ## time.deploy * flash [1]       |        0.16 | [ 0.00,  0.31] |   1.99 | Inf | 0.047  |       0.37 |         
    ## time.deploy * flash [Control] |       -0.04 | [-0.20,  0.13] |  -0.46 | Inf | 0.649  |      -0.09 |         
    ##                               |             |                |        |     |        |            |         
    ## R2 (conditional)              |             |                |        |     |        |            |     0.44
    ## R2 (marginal)                 |             |                |        |     |        |            | 5.52e-03
    ## Sigma                         |             |                |        |     |        |            |     1.00

``` r
as.report_table(r_gaup , summary = T)
```

    ## Parameter                     | Coefficient |         95% CI |      z |  df |      p | Std. Coef. |  Fit
    ## --------------------------------------------------------------------------------------------------------
    ## (Intercept)                   |       -6.89 | [-8.06, -5.72] | -11.52 | Inf | < .001 |      -6.75 |     
    ## time.deploy                   |        0.04 | [-0.14,  0.21] |   0.40 | Inf | 0.691  |       0.08 |     
    ## flash [1]                     |        0.37 | [-0.73,  1.46] |   0.65 | Inf | 0.513  |       0.39 |     
    ## flash [Control]               |        0.07 | [-1.63,  1.77] |   0.08 | Inf | 0.935  |      -0.33 |     
    ## time.deploy * flash [1]       |    5.00e-03 | [-0.23,  0.24] |   0.04 | Inf | 0.967  |       0.02 |     
    ## time.deploy * flash [Control] |       -0.10 | [-0.41,  0.21] |  -0.64 | Inf | 0.521  |      -0.23 |     
    ##                               |             |                |        |     |        |            |     
    ## R2 (conditional)              |             |                |        |     |        |            | 0.28
    ## R2 (marginal)                 |             |                |        |     |        |            | 0.01
    ## Sigma                         |             |                |        |     |        |            | 1.00

# Hare, deer and squirrelywhere (and pine marten)

Blueprint for other species in chunk below:

### Diagnostics

``` r
# filter species
sp = "xx"
time_sp <- filter(time.dep4, species %in% sp) #.dep4 = trimmed data
# Model
m_sp  <- glmer(n.obs ~ time.deploy * flash + # fixed effects
              (1 | loc) + (1 | week), # random effects
            data   = time_sp,
            family = poisson) # poisson family of distributions

# ggpredict is similar to expand.grid
p_sp <- ggeffects::ggpredict(m_sp, terms = c("time.deploy", "flash"))
# Diagnostics
plot(p_sp, add.data = TRUE) + labs(subtitle  = "add.data = TRUE")
plot(p_sp, residuals = TRUE) + labs(subtitle  = "residuals")
performance::check_model(m_sp) # check assumptions
```

### Interpret

``` r
# Summary, report, model
summary(m_sp)
report::report(m_sp) # text-summary of my model, to include in a report
plot(p_sp)
```

``` r
report_parameters(sessionInfo())
```

    ##   - effectsize (version 0.4.3.1; Ben-Shachar M et al., 2020)
    ##   - Matrix (version 1.2.18; Douglas Bates and Martin Maechler, 2019)
    ##   - lme4 (version 1.1.26; Douglas Bates et al., 2015)
    ##   - ggplot2 (version 3.3.3; Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.)
    ##   - stringr (version 1.4.0; Hadley Wickham, 2019)
    ##   - forcats (version 0.5.0; Hadley Wickham, 2020)
    ##   - tidyr (version 1.1.2; Hadley Wickham, 2020)
    ##   - readr (version 1.4.0; Hadley Wickham and Jim Hester, 2020)
    ##   - dplyr (version 1.0.4; Hadley Wickham et al., 2021)
    ##   - tibble (version 3.0.6; Kirill Müller and Hadley Wickham, 2021)
    ##   - purrr (version 0.3.4; Lionel Henry and Hadley Wickham, 2020)
    ##   - ggeffects (version 1.0.1; Lüdecke D, 2018)
    ##   - parameters (version 0.12.0.1; Lüdecke D et al., 2020)
    ##   - see (version 0.6.2.1; Lüdecke et al., 2020)
    ##   - performance (version 0.7.0.1; Lüdecke et al., 2020)
    ##   - report (version 0.2.0; Makowski et al., 2020)
    ##   - R (version 4.0.3; R Core Team, 2020)
    ##   - tidyverse (version 1.3.0; Wickham et al., 2019)
