
<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview of package
-------------------

This package allows you to simulate time series of environmental health data and perform simulation-based power analyses and other measures of model performance. The package includes four main parts:

1.  Generation of exposure data;
2.  Generation of outcome data;
3.  Fitting models to simulated data; and
4.  Evaluating model performance on simulated data.

The user has the option to customize different aspects of the simulation at each of these steps.

The package creates time series that are relevant for environmental epidemiology studies of ambient exposures (e.g., studies of acute mortality risks associated with daily air pollution concentration, daily temperature, or occurance of a community-wide extreme event like a heat wave).

Basic example of using the package
----------------------------------

The main function of this package is the `eesim` function. You can use the `eesim` function to conduct all four steps of the simulation process at once (simulate exposure data, simulate outcome data, fit models to simulated data, and evaluate model performance).

The `eesim` function requires \[required inputs\]. The function returns a list with two elements. The first element gives simulation-specific results: the estimated effect, standard error, t- and p-values, and upper and lower 95% confidence bounds when a model was applied to each set of simulated data. The second element gives some measures of model assessment, assessed over all simulations, including the mean beta and relative risk estimates across simulations.

For example, you can use the following call to (1) generate 10 observations of a continuous exposure with mean 100 and standard deviation 10, with a seasonal trend in expected value; (2) generate 10 associated outcome values, where the outcome has an average value of 20 and a relative risk of 1.10 per one-unit increase in the exposure; (3) fit a generalized linear model that controls for long-term and seasonal trends with a natural cubic spline with 1 degree of freedom per year; and (4) evaluate the performace of that model on the simulated data:

\[To do-- increase `n` and `n_reps`, but wait until we've edited the vignette, because that will increase time to render. We could change `df` then, too (and maybe change to case-crossover? That may be easier for a first example)\]

``` r
ex_sim <- eesim(n_reps = 100, n = 10, central = 100, sd = 10,
                exposure_type = "continuous", 
                average_outcome = 20, rr = 1.10,
                model = "spline", df_year = 1)
head(ex_sim[[1]])
#>     Estimate    Std.Error   t.value      p.value   lower_ci   upper_ci
#> 1 0.09538419 1.183634e-04  805.8591 1.196594e-18 0.09515221 0.09561618
#> 2 0.09529127 7.454107e-05 1278.3728 4.733330e-20 0.09514517 0.09543737
#> 3 0.09527069 5.934210e-05 1605.4486 9.607198e-21 0.09515438 0.09538700
#> 4 0.09540133 4.692537e-05 2033.0436 1.839696e-21 0.09530936 0.09549330
#> 5 0.09518362 6.996906e-05 1360.3674 3.063208e-20 0.09504649 0.09532076
#> 6 0.09525668 8.699193e-05 1095.0060 1.399110e-19 0.09508618 0.09542718
ex_sim[[2]]
#>     beta_hat   rr_hat var_across_betas mean_beta_var percent_bias coverage
#> 1 0.09530329 1.099992     7.061093e-09  7.493767e-09 0.0006885681      0.9
#>   power
#> 1     1
```

The first element of the returned object can be used to explore the behavior of individual simulations. For example, to look at the relative risk point estimate and 95% confidence interval from each of the 100 simulations, as well as which 95% confidence intervals include the true relative risk using the `coverage_plot` function that comes with the package:

``` r
coverage_plot(ex_sim[[1]], true_param = 1.10)
```

<img src="README-unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

The second element of the returned object gives the following overall summaries of model performance across all simulations:

| Variable           | Description                                                                                   |
|:-------------------|:----------------------------------------------------------------------------------------------|
| `beta_hat`         | The mean estimated effect (log relative risk) over all simulations                            |
| `rr_hat`           | The mean relative risk over all simulations                                                   |
| `var_across_betas` | Variance of the point estimates of the estimated effect over all simulations                  |
| `mean_beta_var`    | The mean across simulations of the variances of each estimated effect                         |
| `percent_bias`     | Relative bias of the mean of the estimated coefficients                                       |
| `coverage`         | Percent of simulations for which the 95% confidence interval estimate includes the true value |
| `power`            | NA                                                                                            |

Piece-by-piece breakdown of package utility
-------------------------------------------

To demonstrate how the `eesim` function works, here is a breakdown of each of the four main parts: generating exposure data, generating outcome data, fitting models, and evaluating models. The helper functions used for each step are described in detail in this section.

### Generating exposure data

The first task of the package is generating exposure data. This can be dones with the `sim_exposure` function. In this function, the user can specify whether they would like the exposure data to be binary or continuous (`exposure_type`). For continuous exposure data, the user can specify the mean (`central`) and standard deviation (`sd`) of the exposure data:

``` r
x_cont <- sim_exposure(n = 1000, central = 50, sd = 5,
                      exposure_type = "continuous") 
x_cont %>% slice(1:5)
#>         date        x
#> 1 2001-01-01 55.09400
#> 2 2001-01-02 45.61076
#> 3 2001-01-03 52.44021
#> 4 2001-01-04 49.34614
#> 5 2001-01-05 48.38926
```

``` r
ggplot(x_cont, aes(x = date, y = x)) + geom_point(alpha = 0.2)
```

<img src="README-unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

You can plot a calendar plot of this simulated exposure time series using the `calendar_plot` function that comes with the package:

``` r
calendar_plot(x_cont, type = "continuous")
```

<img src="README-unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

For binary exposure data, the `central` argument gives the probability of exposure on a study day:

``` r
x_bin <- sim_exposure(n = 1000, central = 0.1,
                      exposure_type = "binary")
x_bin %>% slice(1:5)
#>         date x
#> 1 2001-01-01 0
#> 2 2001-01-02 0
#> 3 2001-01-03 0
#> 4 2001-01-04 0
#> 5 2001-01-05 0
```

``` r
calendar_plot(x_bin, type = "discrete", labels = c("Not exposed", "Exposed"))
```

<img src="README-unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

For environmental epidemiology applications, exposure data often has a seasonal trend and / or long-term trend. The default for `sim_exposure` is to simulate the exposure data without a time trend (`trend = "no trend"`). However, we have also built in several time trends from which a user can to choose to simulate exposure data with a time trend, either seasonal or long-term or both. These trend patterns differ slightly depending on whether the user is simulating binary or continuous data. \[add reference for Schwartz paper where we got some of the trend equations\] Below are plots of the built-in trends for continuous exposure data from which the user may choose.

![](README-unnamed-chunk-11-1.png)

The trend options are similar for binary exposure, but exclude "curvilinear" and "cos1linear" and include a "monthly" trend, for which the user enters a vector of 12 probabilities of exposure, one for each month.

Here is an example of generating continuous exposure data with a "cos1linear"" trend:

``` r
testexp <- sim_exposure(n=1000, central = 50, sd = 5, trend = "cos1linear",
                        amp = .6, exposure_type = "continuous")
head(testexp)
#>         date        x
#> 1 2001-01-01 60.14935
#> 2 2001-01-02 62.20198
#> 3 2001-01-03 53.13609
#> 4 2001-01-04 61.56852
#> 5 2001-01-05 59.14720
#> 6 2001-01-06 60.00853
qplot(testexp$date, testexp$x)+geom_point()+coord_cartesian(ylim = c(0,110)) + 
  labs(title = "Exposure with a cos1linear trend", x = "Date", y="Exposure")+theme_bw()
```

<img src="README-unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

``` r
calendar_plot(testexp, type = "continuous")
```

<img src="README-unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

The default amplitude for the trend is .6, but we can adjust the "amp" parameter for a smaller or larger trend amplitude:

``` r
smallamp <- sim_exposure(n=1000, central = 50, sd = 5, trend = "cos1linear",
                        amp = .2, exposure_type = "continuous")
qplot(smallamp$date, smallamp$x) +
  geom_point() + coord_cartesian(ylim = c(0,110)) +
  labs(title = "Cos1linear exposure with smaller amplitude", x="Date", y="Exposure") + theme_bw()
```

<img src="README-unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

``` r
calendar_plot(smallamp, type = "continuous")
```

<img src="README-unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

Here is an example of generating binary exposure data with a "monthly" trend starting from June 1, 2002. The probabilities for the central parameter are for each month of the year, starting in January.

``` r
testbin <- sim_exposure(n=1000, central = c(.1,.1,.2,.3,.4,.4,.5,.6,.5,.3,.2,.1),
                        trend = "monthly", exposure_type = "binary", 
                        start.date = "2002-06-01")
head(testbin)
#>         date x
#> 1 2002-06-01 0
#> 2 2002-06-02 0
#> 3 2002-06-03 0
#> 4 2002-06-04 0
#> 5 2002-06-05 1
#> 6 2002-06-06 0
qplot(testbin$date, testbin$x)+geom_point()+labs(title = "Binary exposure data with monthly trend", x="Date", y="Exposure")+theme_bw()
```

<img src="README-unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

``` r
calendar_plot(testbin, type = "discrete", labels = c("Not exposed", "Exposed"))
```

<img src="README-unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

The following figure shows the steps taken in simulating exposure ...

![](README-unnamed-chunk-18-1.png)

### Generating outcome data

Next, the eesim package uses the exposure data to generate health outcome data with or without seasonal trends. eesim creates "baseline" values for outcome based on the user-specified trend and average outcome. Next it uses the following function to relate exposure and relative risk to outcomes:
*λ* = *e*<sup>log(*b**a**s**e**l**i**n**e*) + log(*r**e**l**a**t**i**v**e**r**i**s**k*) \* *e**x**p**o**s**u**r**e*</sup>
 The values of lambda are then used to randomize the outcome values around the baseline with a Poisson($$) distribution.

Here is an example of generating health outcome data with an upward linear trend using exposure data with a "cos1" trend:

``` r
testexp2 <- sim_exposure(n=1000, central = 100, sd = 10, trend = "cos1", exposure_type = "continuous")
testout <- sim_outcome(exposure = testexp2, average_outcome = 22, trend = "linear", rr = 1.01)
qplot(testout$date, testout$outcome) + geom_point() +
  labs(title = "Health outcomes with a linear trend", x="Date", y="Outcome") + theme_bw()
```

<img src="README-unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

``` r
calendar_plot(testout %>% select(date, outcome), type = "continuous")
```

<img src="README-unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

### Fitting models

eesim next uses many generated data sets to fit statistical models to relate exposure to outcome and estimate relative risk. The built-in model choices are a spline model and a case-crossover model. The fit\_mods function outputs a data frame with estimates of the log relative risk, p-values, and upper and lower 95% confidence bounds for each simulated data set.

Here is an example of fitting the spline model with 7 degrees of freedom:

``` r
sims <- create_sims(n_reps = 10, n = 100, central = 100, sd = 10,
             exposure_type="continuous", exposure_trend = "cos1",
             exposure_amp = .6, average_outcome = 22,
             outcome_trend = "no trend", outcome_amp = .6, rr = 1.01)
fits <- fit_mods(outcome = sims, model = "spline", df_year = 7)
fits
#>       Estimate   Std.Error  t.value      p.value    lower_ci   upper_ci
#> 1  0.011280449 0.001277840 8.827746 4.896215e-14 0.008775928 0.01378497
#> 2  0.008780129 0.001196216 7.339921 6.892369e-11 0.006435589 0.01112467
#> 3  0.009564164 0.001385761 6.901739 5.514337e-10 0.006848121 0.01228021
#> 4  0.008372466 0.001272358 6.580274 2.474207e-09 0.005878689 0.01086624
#> 5  0.010340983 0.001344296 7.692490 1.264016e-11 0.007706211 0.01297575
#> 6  0.009449141 0.001350507 6.996737 3.523826e-10 0.006802196 0.01209609
#> 7  0.011291006 0.001333248 8.468799 2.866004e-13 0.008677889 0.01390412
#> 8  0.011843253 0.001380390 8.579644 1.662040e-13 0.009137739 0.01454877
#> 9  0.009686359 0.001237789 7.825532 6.635552e-12 0.007260337 0.01211238
#> 10 0.011151598 0.001289402 8.648660 1.183412e-13 0.008624417 0.01367878
```

### Evaluating the models

Lastly, eesim evaluates model performance with several different measures. The check\_sims function takes the true relative risk as input and returns mean beta and relative risk estimates across all simulated data sets, variance of the estimates of beta, the mean of the variances of each beta hat, the relative bias of the mean of the beta hats, the percent coverage of the true beta, and the power of the test at the 5% significance level.

Here is an example of the use of the check\_sims function:

``` r
check_sims(fits, true_rr = 1.01)
#>     beta_hat   rr_hat var_across_betas mean_beta_var percent_bias coverage
#> 1 0.01017595 1.010229     1.396156e-06  1.711174e-06  -0.02262777        1
#>   power
#> 1     1
```

Using custom functions
----------------------

An important feature of eesim is that the user may input custom functions for any part of the simulation process. For example, the user may wish to generate exposure data with a custom trend, then automate the process of generating outcomes, fitting models, and evaluating performance using the built-in features of eesim. Functions the user has the option to customize within the eesim framework are exposure trend, outcome baseline, and outcome lambda. A custom model may also be used as long as returns the necessary output if the model assessmen tools in eesim are to be used.

To use custom functions within eesim, the user must input the name of the custom function as well as a list of all arguments for the custom function and their values. Many inputs for the eesim functions may no longer be necessary when a custom function is used, in which case they can simply be left out.

### Custom exposure trend

Here is an example of using a custom function to specify an exposure trend that is not built in to eesim:

``` r
sintrend <- function(n, mu, y){
  day <- c(1:n)
  base <- mu + y * sin(2 * pi * (day / 365))
  rnorm(n, mean = base, sd = 1)
}
customexp <- sim_exposure(n=1000, exposure_type = "continuous", cust_exp_func = sintrend, cust_exp_args = list(n=1000, mu = 75, y=3))
head(customexp)
#>         date        x
#> 1 2001-01-01 76.81591
#> 2 2001-01-02 75.75008
#> 3 2001-01-03 77.36643
#> 4 2001-01-04 74.80841
#> 5 2001-01-05 76.35757
#> 6 2001-01-06 74.77990
qplot(customexp$date, customexp$x)+geom_point()+labs(title = "Exposure values with a custom trend", x="Date", y="Exposure")+theme_bw()
```

<img src="README-unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

``` r
calendar_plot(customexp, type = "continuous")
```

<img src="README-unnamed-chunk-24-1.png" style="display: block; margin: auto;" />

This custom exposure function can be called with eesim:

``` r
ex_sim2 <- eesim(n_reps = 3, n = 10, central = 100, sd = 10,
                exposure_type = "continuous", exposure_trend = "custom", 
                exposure_amp = .6, cust_exp_func = sintrend,
                cust_exp_args = list(n=10, mu=75,y=3),
                average_outcome = 22, rr = 1.01, 
                model = "spline", df_year = 1)
#> Error in do.call(custom_func, arguments): 'what' must be a function or character string
ex_sim2
#> Error in eval(expr, envir, enclos): object 'ex_sim2' not found
```

### Custom outcomes

There are two ways to customize the simulated outcome data: creating a custom baseline for outcome values or customizing the relationship between outcome and exposure.

The outcome baseline is comprised of the values the user expects the outcomes to take on without exposure factored in. The user may write a function to specify the trend of the baseline, then use it as an input in sim\_outcome or eesim. Here is an example of creating a custom baseline function and using it in the eesim function:

``` r
custombase <- function(n, slope, intercept){
  day <- c(1:n)
  baseline <- day*slope + intercept
  return(baseline)
}

#Example:
custombase(n=5, slope = .3, intercept = 55)
#> [1] 55.3 55.6 55.9 56.2 56.5

ex_sim3 <- eesim(n_reps = 3, n = 10, central = 100, sd = 10,
                exposure_type = "continuous", exposure_trend = "cos1",
                exposure_amp = .6, average_outcome = 22, rr = 1.01, 
                cust_base_func = custombase, cust_base_args = list(n=10, slope = .5,                        intercept = 12), model = "spline", df_year = 1)
ex_sim3
#> [[1]]
#>      Estimate   Std.Error  t.value    p.value     lower_ci   upper_ci
#> 1 0.004867892 0.003692153 1.318443 0.22885465 -0.002368594 0.01210438
#> 2 0.016790622 0.007714032 2.176634 0.06596776  0.001671397 0.03190985
#> 3 0.019943860 0.006317172 3.157087 0.01599315  0.007562430 0.03232529
#> 
#> [[2]]
#>     beta_hat   rr_hat var_across_betas mean_beta_var percent_bias coverage
#> 1 0.01386746 1.013985     6.322986e-05  3.768165e-05   -0.3945942        1
#>       power
#> 1 0.6666667
```

Here is an example of creating a custom lambda, meaning a custom function relating relative risk and exposure to outcomes, and using it in eesim with the custom baseline function created above:

``` r
customlambda <- function(exposure, rr, constant, baseline){
  log_lambda <- log(baseline) + log(rr) * exposure + constant
  lambda <- exp(log_lambda)
  return(lambda)
}
```
