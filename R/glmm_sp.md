GLMM per art
================
Torgeir
03 mars, 2021

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
library(performance) # diagnostic-plots to check assumptions
library(report)      # Result-summaries in text-format
```

    ## report is in alpha - help us improve by reporting bugs on github.com/easystats/report/issues

``` r
library(ggeffects)   # Estimated Marginal Means and Marginal Effects from Regression Models
                          # more at: https://strengejacke.github.io/ggeffects/
library(parameters)  # extract model-parameters etc. from (most) models
library(sjPlot)      # parameters + sjPlot probably does a similar and better job than ggeffects
```

    ## Learn more about sjPlot with 'browseVignettes("sjPlot")'.

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
time.dep4 <- time.dep3 %>% filter(time.deploy < h/10) %>%  # h is normal scale, must be rescaled by /10
                 mutate(flash = fct_relevel(flash, "Control","0","1")) # relevel to make Control the model intercept
```

# Modelling

## Roe deer

### Diagnostics

``` r
time.dep4$loc %>% unique() %>% is.na() %>% any()
summary(time.dep4)

# filter species
sp = "raadyr"
time_sp <- filter(time.dep4, species %in% sp, !period %in% ctrl) #.dep4 = trimmed data
# Model
m_sp  <- lme4::glmer(n.obs ~ time.deploy * flash + # fixed effects
            (1 | loc) + (1 | week), # random effects
            data   = time_sp,
            family = poisson) # poisson family of distributions

# ggpredict is similar to expand.grid
p_sp <- ggeffects::ggpredict(m_sp, terms = c("time.deploy [all]", "flash"))
# Diagnostics
plot(p_sp, add.data = TRUE) + labs(title = "add.data = TRUE")
plot(p_sp, residuals = TRUE) + labs(title = "residuals")
performance::check_model(m_sp) # check assumptions
```

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
# report::report(m_sp) # text-summary of my model, to include in a report
plot(p_sp)
```

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
p_raa <- ggeffects::ggpredict(m_raa, terms = c("time.deploy [all]", "flash"))
# Diagnostics
plot(p_raa, add.data = TRUE) + labs(title  = "Raw data")
```

![](glmm_sp_files/figure-gfm/raadyr-C-1.png)<!-- -->

``` r
plot(p_raa, residuals = TRUE) + labs(title  = "Residuals")
```

![](glmm_sp_files/figure-gfm/raadyr-C-2.png)<!-- -->

``` r
performance::check_model(m_raa) # check assumptions
```

    ## Loading required namespace: qqplotr

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
    ## -1.1836 -0.2832 -0.1641 -0.0763 14.1894 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 2.6164   1.6175  
    ##  week   (Intercept) 0.3232   0.5685  
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -3.60407    0.43060  -8.370   <2e-16 ***
    ## time.deploy        -0.01127    0.02298  -0.490    0.624    
    ## flash0              0.21104    0.51227   0.412    0.680    
    ## flash1              0.32001    0.51138   0.626    0.531    
    ## time.deploy:flash0 -0.01317    0.03205  -0.411    0.681    
    ## time.deploy:flash1 -0.02971    0.03132  -0.948    0.343    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash0 flash1 tm.d:0
    ## time.deploy -0.203                            
    ## flash0      -0.802  0.163                     
    ## flash1      -0.805  0.171  0.970              
    ## tm.dply:fl0  0.136 -0.683 -0.229 -0.130       
    ## tm.dply:fl1  0.149 -0.744 -0.134 -0.224  0.573

``` r
plot(p_raa) + labs(title = "Predicted counts of roe deer")
```

![](glmm_sp_files/figure-gfm/raadyr-C-report-1.png)<!-- -->

``` r
model_parameters(m_raa,   standardize = "refit") %>% plot()
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00228998 (tol = 0.002, component 1)

![](glmm_sp_files/figure-gfm/raadyr-C-report-2.png)<!-- -->

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
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00228998 (tol = 0.002, component 1)

``` r
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
    ##                Parameter        H0 inside ROPE        90% CI
    ##              (Intercept)  Rejected      0.00 % [-4.31 -2.90]
    ##              time.deploy  Accepted    100.00 % [-0.05  0.03]
    ##                flash [0] Undecided     11.87 % [-0.63  1.05]
    ##                flash [1] Undecided     11.89 % [-0.52  1.16]
    ##  time.deploy * flash [0]  Accepted    100.00 % [-0.07  0.04]
    ##  time.deploy * flash [1]  Accepted    100.00 % [-0.08  0.02]

``` r
r_raa <- report(m_raa) # text-summary of my model, to include in a report
summary(r_raa)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is substantial (conditional R2 = 0.45) and the part related to the fixed effects alone (marginal R2) is of 1.88e-03. The model's intercept is at -3.60 (95% CI [-4.45, -2.76]). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly negative (beta = -0.01, 95% CI [-0.06, 0.03], p = 0.624, Std. beta = -0.03)
    ##   - The effect of flash [0] is non-significantly positive (beta = 0.21, 95% CI [-0.79, 1.22], p = 0.680, Std. beta = 0.16)
    ##   - The effect of flash [1] is non-significantly positive (beta = 0.32, 95% CI [-0.68, 1.32], p = 0.531, Std. beta = 0.21)
    ##   - The interaction effect of flash [0] on time.deploy is non-significantly negative (beta = -0.01, 95% CI [-0.08, 0.05], p = 0.681, Std. beta = -0.03)
    ##   - The interaction effect of flash [1] on time.deploy is non-significantly negative (beta = -0.03, 95% CI [-0.09, 0.03], p = 0.343, Std. beta = -0.07)

``` r
as.report_table(r_raa)
```

    ## Parameter               | Coefficient |         95% CI |     z |  df |      p | Std. Coef. | Std. Coef. 95% CI |      Fit
    ## -------------------------------------------------------------------------------------------------------------------------
    ## (Intercept)             |       -3.60 | [-4.45, -2.76] | -8.37 | Inf | < .001 |      -3.65 |    [-4.47, -2.82] |         
    ## time.deploy             |       -0.01 | [-0.06,  0.03] | -0.49 | Inf | 0.624  |      -0.03 |    [-0.13,  0.08] |         
    ## flash [0]               |        0.21 | [-0.79,  1.22] |  0.41 | Inf | 0.680  |       0.16 |    [-0.82,  1.14] |         
    ## flash [1]               |        0.32 | [-0.68,  1.32] |  0.63 | Inf | 0.531  |       0.21 |    [-0.77,  1.18] |         
    ## time.deploy * flash [0] |       -0.01 | [-0.08,  0.05] | -0.41 | Inf | 0.681  |      -0.03 |    [-0.18,  0.12] |         
    ## time.deploy * flash [1] |       -0.03 | [-0.09,  0.03] | -0.95 | Inf | 0.343  |      -0.07 |    [-0.21,  0.07] |         
    ##                         |             |                |       |     |        |            |                   |         
    ## AIC                     |             |                |       |     |        |            |                   |  7669.27
    ## BIC                     |             |                |       |     |        |            |                   |  7730.22
    ## R2 (conditional)        |             |                |       |     |        |            |                   |     0.45
    ## R2 (marginal)           |             |                |       |     |        |            |                   | 1.88e-03
    ## Sigma                   |             |                |       |     |        |            |                   |     1.00

``` r
as.report_info(r_raa)
```

    ## We fitted a poisson mixed model (estimated using ML and Nelder-Mead optimizer) to predict n.obs with time.deploy and flash (formula: n.obs ~ time.deploy * flash). The model included loc and week as random effects (formula: list(~1 | loc, ~1 | week)). The model's total explanatory power is substantial (conditional R2 = 0.45) and the part related to the fixed effects alone (marginal R2) is of 1.88e-03. The model's intercept, corresponding to time.deploy = 0 and flash = Control, is at -3.60 (95% CI [-4.45, -2.76], p < .001). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly negative (beta = -0.01, 95% CI [-0.06, 0.03], p = 0.624; Std. beta = -0.03, 95% CI [-0.13, 0.08])
    ##   - The effect of flash [0] is non-significantly positive (beta = 0.21, 95% CI [-0.79, 1.22], p = 0.680; Std. beta = 0.16, 95% CI [-0.82, 1.14])
    ##   - The effect of flash [1] is non-significantly positive (beta = 0.32, 95% CI [-0.68, 1.32], p = 0.531; Std. beta = 0.21, 95% CI [-0.77, 1.18])
    ##   - The interaction effect of flash [0] on time.deploy is non-significantly negative (beta = -0.01, 95% CI [-0.08, 0.05], p = 0.681; Std. beta = -0.03, 95% CI [-0.18, 0.12])
    ##   - The interaction effect of flash [1] on time.deploy is non-significantly negative (beta = -0.03, 95% CI [-0.09, 0.03], p = 0.343; Std. beta = -0.07, 95% CI [-0.21, 0.07])
    ## 
    ## Standardized parameters were obtained by fitting the model on a standardized version of the dataset. 95% Confidence Intervals (CIs) and p-values were computed using the Wald approximation.

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
p_rev <- ggeffects::ggpredict(m_rev, terms = c("time.deploy [all]", "flash"))
# Diagnostics
plot(p_rev, add.data = TRUE) + labs(title = "add.data = TRUE")
```

![](glmm_sp_files/figure-gfm/rev-1.png)<!-- -->

``` r
plot(p_rev, residuals = TRUE) + labs(title = "Residuals")
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
    ## -0.6303 -0.2405 -0.1838 -0.1304 13.6641 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 0.74693  0.8643  
    ##  week   (Intercept) 0.06125  0.2475  
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -3.4540659  0.2570878 -13.435   <2e-16 ***
    ## time.deploy        -0.0013144  0.0291773  -0.045    0.964    
    ## flash0              0.0426336  0.3187784   0.134    0.894    
    ## flash1              0.1958055  0.3169724   0.618    0.537    
    ## time.deploy:flash0  0.0005195  0.0404086   0.013    0.990    
    ## time.deploy:flash1 -0.0081886  0.0395639  -0.207    0.836    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash0 flash1 tm.d:0
    ## time.deploy -0.443                            
    ## flash0      -0.784  0.355                     
    ## flash1      -0.790  0.360  0.864              
    ## tm.dply:fl0  0.313 -0.709 -0.488 -0.277       
    ## tm.dply:fl1  0.324 -0.733 -0.281 -0.481  0.567

``` r
plot(p_rev)+ labs(title = "Predicted counts of red fox")
```

![](glmm_sp_files/figure-gfm/rev-report-1.png)<!-- -->

``` r
r_rev <- report(m_rev) # text-summary of my model, to include in a report
as.report_text(r_rev, summary=T)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is moderate (conditional R2 = 0.19) and the part related to the fixed effects alone (marginal R2) is of 1.16e-03. The model's intercept is at -3.45 (95% CI [-3.96, -2.95]). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly negative (beta = -1.31e-03, 95% CI [-0.06, 0.06], p = 0.964, Std. beta = -3.03e-03)
    ##   - The effect of flash [0] is non-significantly positive (beta = 0.04, 95% CI [-0.58, 0.67], p = 0.894, Std. beta = 0.04)
    ##   - The effect of flash [1] is non-significantly positive (beta = 0.20, 95% CI [-0.43, 0.82], p = 0.537, Std. beta = 0.16)
    ##   - The interaction effect of flash [0] on time.deploy is non-significantly positive (beta = 5.20e-04, 95% CI [-0.08, 0.08], p = 0.990, Std. beta = 1.12e-03)
    ##   - The interaction effect of flash [1] on time.deploy is non-significantly negative (beta = -8.19e-03, 95% CI [-0.09, 0.07], p = 0.836, Std. beta = -0.02)

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
p_grvl <- ggeffects::ggpredict(m_grvl, terms = c("time.deploy [all]", "flash"))
# Diagnostics
plot(p_grvl, add.data = TRUE) + labs(title  = "add.data = TRUE")
```

![](glmm_sp_files/figure-gfm/grevling-1.png)<!-- -->

``` r
plot(p_grvl, residuals = TRUE) + labs(title  = "Residuals")
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
    ## -1.0953 -0.1976 -0.1167 -0.0628 29.5366 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 1.461    1.209   
    ##  week   (Intercept) 1.619    1.272   
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -4.65186    0.39382 -11.812   <2e-16 ***
    ## time.deploy         0.01214    0.03369   0.360    0.719    
    ## flash0              0.15984    0.43265   0.369    0.712    
    ## flash1              0.22107    0.42873   0.516    0.606    
    ## time.deploy:flash0  0.04815    0.04878   0.987    0.324    
    ## time.deploy:flash1  0.04702    0.04710   0.998    0.318    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash0 flash1 tm.d:0
    ## time.deploy -0.333                            
    ## flash0      -0.705  0.334                     
    ## flash1      -0.712  0.338  0.906              
    ## tm.dply:fl0  0.250 -0.751 -0.451 -0.285       
    ## tm.dply:fl1  0.267 -0.789 -0.293 -0.439  0.662

``` r
plot(p_grvl)+ labs(title = "Predicted counts of badger")
```

![](glmm_sp_files/figure-gfm/grevling-report-1.png)<!-- -->

``` r
r_grvl <- report(m_grvl) # text-summary of my model, to include in a report
as.report_text(r_grvl, summary=T)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is substantial (conditional R2 = 0.42) and the part related to the fixed effects alone (marginal R2) is of 5.93e-03. The model's intercept is at -4.65 (95% CI [-5.42, -3.88]). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly positive (beta = 0.01, 95% CI [-0.05, 0.08], p = 0.719, Std. beta = 0.03)
    ##   - The effect of flash [0] is non-significantly positive (beta = 0.16, 95% CI [-0.69, 1.01], p = 0.712, Std. beta = 0.35)
    ##   - The effect of flash [1] is non-significantly positive (beta = 0.22, 95% CI [-0.62, 1.06], p = 0.606, Std. beta = 0.40)
    ##   - The interaction effect of flash [0] on time.deploy is non-significantly positive (beta = 0.05, 95% CI [-0.05, 0.14], p = 0.324, Std. beta = 0.11)
    ##   - The interaction effect of flash [1] on time.deploy is non-significantly positive (beta = 0.05, 95% CI [-0.05, 0.14], p = 0.318, Std. beta = 0.11)

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
    ## Model failed to converge with max|grad| = 0.00315998 (tol = 0.002, component 1)

``` r
# ggpredict is similar to expand.grid
p_elg <- ggeffects::ggpredict(m_elg, terms = c("time.deploy [all]", "flash"))
# Diagnostics
plot(p_elg, add.data = TRUE) + labs(title  = "add.data = TRUE")
```

![](glmm_sp_files/figure-gfm/elg-1.png)<!-- -->

``` r
plot(p_elg, residuals = TRUE) + labs(title  = "Residuals")
```

![](glmm_sp_files/figure-gfm/elg-2.png)<!-- -->

``` r
performance::check_model(m_elg) # check assumptions
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 14807 rows containing missing values (geom_text_repel).

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: ggrepel: 234 unlabeled data points (too many overlaps). Consider
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
    ##   2981.9   3042.8  -1482.9   2965.9    15035 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.4528 -0.1618 -0.1053 -0.0675 20.3353 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 1.3638   1.1678  
    ##  week   (Intercept) 0.5544   0.7446  
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -4.84060    0.39269 -12.327   <2e-16 ***
    ## time.deploy         0.01557    0.04624   0.337    0.736    
    ## flash0              0.07402    0.46979   0.158    0.875    
    ## flash1              0.45778    0.46087   0.993    0.321    
    ## time.deploy:flash0  0.03184    0.06479   0.492    0.623    
    ## time.deploy:flash1 -0.02959    0.06171  -0.480    0.632    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash0 flash1 tm.d:0
    ## time.deploy -0.485                            
    ## flash0      -0.754  0.426                     
    ## flash1      -0.769  0.429  0.865              
    ## tm.dply:fl0  0.333 -0.716 -0.552 -0.341       
    ## tm.dply:fl1  0.366 -0.760 -0.364 -0.537  0.617
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.00315998 (tol = 0.002, component 1)

``` r
plot(p_elg)+ labs(title = "Predicted counts of moose")
```

![](glmm_sp_files/figure-gfm/elg-report-1.png)<!-- -->

``` r
r_elg <- report(m_elg) # text-summary of my model, to include in a report
as.report_text(r_elg, summary=T)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is substantial (conditional R2 = 0.30) and the part related to the fixed effects alone (marginal R2) is of 3.75e-03. The model's intercept is at -4.84 (95% CI [-5.61, -4.07]). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly positive (beta = 0.02, 95% CI [-0.08, 0.11], p = 0.736, Std. beta = 0.04)
    ##   - The effect of flash [0] is non-significantly positive (beta = 0.07, 95% CI [-0.85, 0.99], p = 0.875, Std. beta = 0.20)
    ##   - The effect of flash [1] is non-significantly positive (beta = 0.46, 95% CI [-0.45, 1.36], p = 0.321, Std. beta = 0.34)
    ##   - The interaction effect of flash [0] on time.deploy is non-significantly positive (beta = 0.03, 95% CI [-0.10, 0.16], p = 0.623, Std. beta = 0.07)
    ##   - The interaction effect of flash [1] on time.deploy is non-significantly negative (beta = -0.03, 95% CI [-0.15, 0.09], p = 0.632, Std. beta = -0.07)

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

# ggpredict is similar to expand.grid
p_hjort <- ggeffects::ggpredict(m_hjort, terms = c("time.deploy [all]", "flash"))
# Diagnostics
plot(p_hjort, add.data = TRUE) + labs(title  = "add.data = TRUE")
```

![](glmm_sp_files/figure-gfm/hjort-1.png)<!-- -->

``` r
plot(p_hjort, residuals = TRUE) + labs(title  = "Residuals")
```

![](glmm_sp_files/figure-gfm/hjort-2.png)<!-- -->

``` r
performance::check_model(m_hjort) # check assumptions
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 14660 rows containing missing values (geom_text_repel).

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: ggrepel: 382 unlabeled data points (too many overlaps). Consider
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
    ## -0.5198 -0.1101 -0.0394 -0.0256 24.8152 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 4.5261   2.1275  
    ##  week   (Intercept) 0.2999   0.5476  
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -6.13225    0.72326  -8.479   <2e-16 ***
    ## time.deploy        -0.06455    0.06330  -1.020   0.3078    
    ## flash0              0.17311    0.81582   0.212   0.8320    
    ## flash1             -0.46259    0.83274  -0.555   0.5786    
    ## time.deploy:flash0  0.03858    0.08456   0.456   0.6482    
    ## time.deploy:flash1  0.19528    0.08558   2.282   0.0225 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash0 flash1 tm.d:0
    ## time.deploy -0.323                            
    ## flash0      -0.753  0.278                     
    ## flash1      -0.745  0.283  0.901              
    ## tm.dply:fl0  0.228 -0.720 -0.380 -0.221       
    ## tm.dply:fl1  0.245 -0.738 -0.219 -0.418  0.571

``` r
# report::report(m_hjort) # text-summary of my model, to include in a report
plot(p_hjort)+ labs(title = "Predicted counts of red deer")
```

![](glmm_sp_files/figure-gfm/hjort-report-1.png)<!-- -->

``` r
r_hjort <- report(m_hjort) # text-summary of my model, to include in a report
as.report_text(r_hjort, summary=T)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is substantial (conditional R2 = 0.44) and the part related to the fixed effects alone (marginal R2) is of 5.52e-03. The model's intercept is at -6.13 (95% CI [-7.55, -4.71]). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly negative (beta = -0.06, 95% CI [-0.19, 0.06], p = 0.308, Std. beta = -0.15)
    ##   - The effect of flash [0] is non-significantly positive (beta = 0.17, 95% CI [-1.43, 1.77], p = 0.832, Std. beta = 0.33)
    ##   - The effect of flash [1] is non-significantly negative (beta = -0.46, 95% CI [-2.09, 1.17], p = 0.579, Std. beta = 0.29)
    ##   - The interaction effect of flash [0] on time.deploy is non-significantly positive (beta = 0.04, 95% CI [-0.13, 0.20], p = 0.648, Std. beta = 0.09)
    ##   - The interaction effect of flash [1] on time.deploy is significantly positive (beta = 0.20, 95% CI [0.03, 0.36], p < .05, Std. beta = 0.46)

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

# ggpredict is similar to expand.grid
p_gaup <- ggeffects::ggpredict(m_gaup, terms = c("time.deploy [all]", "flash"))
# Diagnostics
plot(p_gaup, add.data = TRUE) + labs(title  = "add.data = TRUE")
```

![](glmm_sp_files/figure-gfm/gaupe-1.png)<!-- -->

``` r
plot(p_gaup, residuals = TRUE) + labs(title  = "Residuals")
```

![](glmm_sp_files/figure-gfm/gaupe-2.png)<!-- -->

``` r
performance::check_model(m_gaup) # check assumptions
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 14788 rows containing missing values (geom_text_repel).

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: ggrepel: 252 unlabeled data points (too many overlaps). Consider
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
    ## -0.3291 -0.0556 -0.0319 -0.0254 23.5992 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  loc    (Intercept) 2.2787   1.5095  
    ##  week   (Intercept) 0.3044   0.5517  
    ## Number of obs: 15043, groups:  loc, 53; week, 52
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -6.83448    0.74649  -9.155   <2e-16 ***
    ## time.deploy        -0.06326    0.13124  -0.482    0.630    
    ## flash0             -0.04830    0.86926  -0.056    0.956    
    ## flash1              0.31196    0.86197   0.362    0.717    
    ## time.deploy:flash0  0.09730    0.15789   0.616    0.538    
    ## time.deploy:flash1  0.10416    0.15664   0.665    0.506    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) tm.dpl flash0 flash1 tm.d:0
    ## time.deploy -0.627                            
    ## flash0      -0.736  0.532                     
    ## flash1      -0.762  0.548  0.791              
    ## tm.dply:fl0  0.510 -0.822 -0.670 -0.459       
    ## tm.dply:fl1  0.526 -0.842 -0.461 -0.673  0.705

``` r
plot(p_gaup)+ labs(title = "Predicted counts of lynx")
```

![](glmm_sp_files/figure-gfm/gaupe-report-1.png)<!-- -->

``` r
r_gaup <- report(m_gaup) # text-summary of my model, to include in a report
as.report_text(r_gaup, summary=T)
```

    ## We fitted a poisson mixed model to predict n.obs with time.deploy and flash. The model included loc and week as random effects. The model's total explanatory power is substantial (conditional R2 = 0.29) and the part related to the fixed effects alone (marginal R2) is of 0.01. The model's intercept is at -6.83 (95% CI [-8.30, -5.37]). Within this model:
    ## 
    ##   - The effect of time.deploy is non-significantly negative (beta = -0.06, 95% CI [-0.32, 0.19], p = 0.630, Std. beta = -0.15)
    ##   - The effect of flash [0] is non-significantly negative (beta = -0.05, 95% CI [-1.75, 1.66], p = 0.956, Std. beta = 0.33)
    ##   - The effect of flash [1] is non-significantly positive (beta = 0.31, 95% CI [-1.38, 2.00], p = 0.717, Std. beta = 0.71)
    ##   - The interaction effect of flash [0] on time.deploy is non-significantly positive (beta = 0.10, 95% CI [-0.21, 0.41], p = 0.538, Std. beta = 0.23)
    ##   - The interaction effect of flash [1] on time.deploy is non-significantly positive (beta = 0.10, 95% CI [-0.20, 0.41], p = 0.506, Std. beta = 0.24)

------------------------------------------------------------------------

``` r
# library(rstanarm)
# #result <- #compare_parameters(m_raa, m_rev, m_grvl, m_elg, m_hjort, m_gaup) #can't find function :-/
# plot(result)
# 
# rmarkdown::render("glmm_sp.Rmd", output_format = "github_document")
```

``` r
library(xtable)
```

    ## 
    ## Attaching package: 'xtable'

    ## The following object is masked from 'package:parameters':
    ## 
    ##     display

    ## The following object is masked from 'package:performance':
    ## 
    ##     display

``` r
para_raa  <- model_parameters(m_raa,   standardize = "refit")  %>% add_row(Parameter = "Roe deer", .before = 1)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00228998 (tol = 0.002, component 1)

``` r
para_rev  <- model_parameters(m_rev,   standardize = "refit")  %>% add_row(Parameter = "Red fox",  .before = 1)
para_grvl <- model_parameters(m_grvl,  standardize = "refit")  %>% add_row(Parameter = "Badger",   .before = 1)
para_elg  <- model_parameters(m_elg,   standardize = "refit")  %>% add_row(Parameter = "Moose",    .before = 1)
para_hjort<- model_parameters(m_hjort, standardize = "refit")  %>% add_row(Parameter = "Red deer", .before = 1)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00660776 (tol = 0.002, component 1)

``` r
para_gaup <- model_parameters(m_gaup,  standardize = "refit")  %>% add_row(Parameter = "Lynx",     .before = 1)
para_all <- bind_rows(para_raa, para_rev, para_grvl,para_elg,para_hjort,para_gaup) %>%
  insight::format_table(ci_brackets = c("(", ")")) %>% 
  select(!df)

print(xtable(para_all, type = "latex"), file = "../Thesis/tex/tab/parameters.tex")
```

``` r
# # example for multiple imputed datasets
# sp <- c("raadyr","rev","grevling","elg","hjort","gaupe")
#   predictions <- lapply(1:5, function(sp) {
#     m <- glmer(n.obs ~ time.deploy * flash + species +# fixed effects
#             (1 | loc) + (1 | week), # random effects
#             data   = filter(time.dep4, species %in% sp),
#             family = poisson)
#     ggpredict(m, "species")
#   })
# predictions %>% pool_predictions()
# pool_parameters()
# #pool_predictions(list(p_raa, p_rev,p_grvl,p_elg,p_hjort,p_gaup))
#plot(list(p_raa, p_rev,p_grvl,p_elg,p_hjort,p_gaup))

sjPlot::plot_models(m_raa, m_rev, m_grvl, spacing = 0.4, legend.title = "Species",
                    m.labels = c("Roe deer","Red fox","Badger"))
```

![](glmm_sp_files/figure-gfm/mod-plot-1.png)<!-- -->

``` r
sjPlot::plot_models(m_elg, m_hjort, m_gaup, spacing = 0.4, legend.title = "Species",
                    m.labels = c("Moose","Red deer","Lynx"))
```

![](glmm_sp_files/figure-gfm/mod-plot-2.png)<!-- -->

``` r
plot_model(m_raa)
```

![](glmm_sp_files/figure-gfm/mod-plot-3.png)<!-- -->

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
p_sp <- ggeffects::ggpredict(m_sp, terms = c("time.deploy [all]", "flash"))
# Diagnostics
plot(p_sp, add.data = TRUE) + labs(title  = "add.data = TRUE")
plot(p_sp, residuals = TRUE) + labs(title  = "Residuals")
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
    ##   - xtable (version 1.8.4; David Dahl et al., 2019)
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
    ##   - sjPlot (version 2.8.7; Lüdecke D, 2021)
    ##   - parameters (version 0.12.0.1; Lüdecke D et al., 2020)
    ##   - see (version 0.6.2.1; Lüdecke et al., 2020)
    ##   - performance (version 0.7.0.1; Lüdecke et al., 2020)
    ##   - report (version 0.2.0; Makowski et al., 2020)
    ##   - R (version 4.0.3; R Core Team, 2020)
    ##   - tidyverse (version 1.3.0; Wickham et al., 2019)
