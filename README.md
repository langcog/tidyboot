# tidyboot

`tidyboot` let's you compute arbitrary non-parametric bootstrap statistics on data in tidy data frames.

## Installation
```
# install.packages("devtools")
devtools::install_github("langcog/tidyboot")
```

## Usage

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
#>   condition     n empirical_stat   ci_lower        mean   ci_upper
#>       <dbl> <int>          <dbl>      <dbl>       <dbl>      <dbl>
#> 1         1   500   0.0008064668 -0.0892684 0.002753401 0.09427569
#> 2         2   500   2.0106763605  1.7437848 2.010110389 2.30370024
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
#> 1         1   500       0.04617069 0.05895638 0.06423416
#> 2         2   500       1.98783298 1.97081271 0.18546355
```

``` r
df %>%
  group_by(condition) %>%
  tidyboot(summary_function = function(x) x %>% summarise(median = median(value)),
           statistics_functions = function(x) x %>% summarise_at(vars(median), funs(mean, sd)))
#> # A tibble: 2 x 5
#>   condition     n empirical_median         mean         sd
#>       <dbl> <int>            <dbl>        <dbl>      <dbl>
#> 1         1   500    -0.0004748318 0.0009849159 0.06044211
#> 2         2   500     2.0958642583 2.0368482033 0.23270858
```
