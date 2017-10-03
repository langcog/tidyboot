
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyboot
========

`tidyboot` let's you compute arbitrary non-parametric bootstrap statistics on data in tidy data frames.

Installation
------------

You can install tidyboot from CRAN with:

``` r
install.packages("tidyboot")
```

You can install tidyboot from github with:

``` r
# install.packages("devtools")
devtools::install_github("langcog/tidyboot")
```

Examples
--------

For the simplest use case of bootstrapping the mean and getting the mean and confidence interval of that estimate, use the convenience function `tidyboot_mean()`, specifying which column has the relevant values to compute the mean over:

``` r
library(dplyr)
library(tidyboot)

gauss1 <- data_frame(value = rnorm(500, mean = 0, sd = 1), condition = 1)
gauss2 <- data_frame(value = rnorm(500, mean = 2, sd = 3), condition = 2)
df <- bind_rows(gauss1, gauss2)

df %>%
  group_by(condition) %>%
  tidyboot_mean(column = value)
#> # A tibble: 2 x 6
#>   condition     n empirical_mean    ci_lower       mean   ci_upper
#>       <dbl> <int>          <dbl>       <dbl>      <dbl>      <dbl>
#> 1         1   500      0.0415903 -0.04795741 0.04006775 0.04006775
#> 2         2   500      2.0494461  1.80454457 2.05265278 2.05265278
```

For bootstrapping any statistic and any properties of its sampling distribution, use `tidyboot()`.

You can provide the statistic to be estimated either as a function and a column to compute it over, or as function that takes the whole dataframe and computes the relevant value.

Similarly, you can provide the properties of the sampling distribution to be computed either as a named list of functions and a column to compute them over, or a function that takes the whole dataframe and returns the relevant values.

``` r
df %>%
  group_by(condition) %>%
  tidyboot(column = value, summary_function = median,
           statistics_functions = list("mean" = mean, "sd" = sd))
#> # A tibble: 2 x 5
#>   condition     n empirical_median       mean         sd
#>       <dbl> <int>            <dbl>      <dbl>      <dbl>
#> 1         1   500       0.05551952 0.05217279 0.05283488
#> 2         2   500       2.07596528 2.06370324 0.19673194
```

``` r
df %>%
  group_by(condition) %>%
  tidyboot(summary_function = function(x) x %>% summarise(median = median(value)),
           statistics_functions = function(x) x %>% summarise_at(vars(median), funs(mean, sd)))
#> # A tibble: 2 x 5
#>   condition     n empirical_median       mean         sd
#>       <dbl> <int>            <dbl>      <dbl>      <dbl>
#> 1         1   500       0.05551952 0.04991507 0.05351953
#> 2         2   500       2.07596528 2.06533995 0.19299904
```