#' @importFrom dplyr "%>%"
NULL

#' Non-parametric bootstrap for numeric vector data
#'
#' Computes arbitrary bootstrap statistics on univariate data.
#'
#' @param data A numeric vector of data to bootstrap over.
#' @param summary_function A string that is the name of a function to be
#'   computed over each set of samples. This function needs to take a vector and
#'   return a single number (defaults to \code{"mean"}).
#' @param statistics_functions A vector of strings that are names of functions to be
#'   computed over the set of summary values from all samples.
#' @param nboot The number of bootstrap samples to take (defaults to \code{1000}).
#' @param size The fraction of items to sample (defaults to 1).
#' @param replace Logical indicating whether to sample with replacement (defaults to \code{TRUE}).
#' @param ... Other arguments passed from generic.
#'
#' @examples
#' ## Mean and 95% confidence interval for 1000 samples from a normal distribution
#' x <- rnorm(1000, mean = 0, sd = 1)
#' ci_lower <- function(x) {quantile(x, 0.025)}
#' ci_upper <- function(x) {quantile(x, 0.975)}
#' multi_boot(x, statistics_functions = c("ci_lower", "mean", "ci_upper"))
#' @export
multi_boot.numeric <- function(data,
                               summary_function = "mean",
                               statistics_functions,
                               nboot = 1000,
                               size = 1,
                               replace = TRUE, ...) {

  formulas <- sapply(statistics_functions,
                     function(x) lazyeval::interp(~fun, fun = x))

  one_sample <- function() {
    do.call(summary_function, list(sample(data, size = size * length(data),
                                          replace = replace)))
  }

  all_samples <- data.frame(sample = replicate(nboot, one_sample())) %>%
    dplyr::summarise_at(dplyr::vars(sample), dplyr::funs_(formulas))

  if (length(statistics_functions) == 1) {
    all_samples <- all_samples %>%
      dplyr::rename_(.dots = stats::setNames("sample", statistics_functions))
  }

  return(all_samples)

}

#' Non-parametric bootstrap for logical vector data
#'
#' Computes arbitrary bootstrap statistics on univariate data.
#'
#' @param data A logical vector of data to bootstrap over.
#' @inheritParams multi_boot.numeric
#'
#' @examples
#' ## Mean and 95% confidence interval for 1000 samples from a binomial distribution
#' x <- as.logical(rbinom(1000, 1, 0.5))
#' ci_lower <- function(x) {quantile(x, 0.025)}
#' ci_upper <- function(x) {quantile(x, 0.975)}
#' multi_boot(x, statistics_functions = c("ci_lower", "mean", "ci_upper"))
#' @export
multi_boot.logical <- function(data,
                               summary_function = "mean",
                               statistics_functions,
                               nboot = 1000,
                               size = 1,
                               replace = TRUE, ...) {
  multi_boot(as.numeric(data), summary_function, statistics_functions,
             size, nboot, replace, ...)
}

#' Non-parametric bootstrap for data frames
#'
#' Computes arbitrary bootstrap statistics on univariate data.
#'
#' @param data A data frame.
#' @param summary_function A function to be computed over each set of samples as a data frame, or a
#'   string that is the name of the function to be computed over each set of samples as a single column of
#'   a data frame indicated by \code{column} (defaults to \code{mean}).
#' @param column A string indicating the column of \code{data} to bootstrap over (only necessary if
#'   \code{summary_function} is a string).
#' @param summary_groups A vector of strings that are column names of \code{data} indicating how it should
#'   be grouped before applying \code{summary_function}.
#' @param statistics_functions A function to be computed over the data frame of summary values from all
#'   samples, or a vector of strings that are names of functions to be computed over the vector of
#'   summary values from all samples.
#' @param statistics_groups A vector of strings that are column names of \code{data} indicating how it should
#'   be grouped before applying \code{statistics_functions} (defaults to \code{summary_groups}).
#' @param nboot The number of bootstrap samples to take (defaults to \code{1000}).
#' @param size The fraction of rows to sample (defaults to 1).
#' @param replace Logical indicating whether to sample with replacement (defaults to \code{TRUE}).
#' @param ... Other arguments passed from generic.
#'
#' @examples
#' ## Mean and 95% confidence interval for 1000 samples from two different normal distributions
#' require(dplyr)
#' gauss1 <- data.frame(value = rnorm(1000, mean = 0, sd = 1), condition = 1)
#' gauss2 <- data.frame(value = rnorm(1000, mean = 2, sd = 3), condition = 2)
#' ci_lower <- function(x) {quantile(x, 0.025)}
#' ci_upper <- function(x) {quantile(x, 0.975)}
#' multi_boot(data = bind_rows(gauss1, gauss2),
#'            summary_function = "mean", column = "value", summary_groups = "condition",
#'            statistics_functions = c("ci_lower", "mean", "ci_upper"))
#' multi_boot(data = bind_rows(gauss1, gauss2),
#'            summary_function = function(df) summarise(df, mean = mean(value)),
#'            summary_groups = c("condition"),
#'            statistics_functions = function(df) summarise_at(df,
#'                                                             vars(mean),
#'                                                             funs("ci_upper", "mean", "ci_lower"))
#'            statistics_groups = c("condition"),
#'            nboot = 100, replace = TRUE)
#' @export
multi_boot.data.frame <- function(data,
                                  summary_function = "mean",
                                  column = NULL,
                                  summary_groups = NULL,
                                  statistics_functions,
                                  statistics_groups = summary_groups,
                                  nboot = 1000,
                                  size = 1,
                                  replace = TRUE, ...) {

  fun_types <- c("closure", "character")
  assertthat::assert_that(typeof(summary_function) %in% fun_types)
  assertthat::assert_that(typeof(statistics_functions) %in% fun_types)
  assertthat::assert_that(all(statistics_groups %in% summary_groups))

  original_groups <- dplyr::groups(data)

  if (typeof(summary_function) == "closure") {
    call_summary_function <- summary_function
  } else {
    assertthat::assert_that(!is.null(column))
    summary_dots <- list(lazyeval::interp(~fun(arg),
                                          fun = as.name(summary_function),
                                          arg = as.name(column)))
    call_summary_function <- function(df) {
      dplyr::summarise_(df, .dots = stats::setNames(summary_dots, "summary"))
    }
  }

  if (typeof(statistics_functions) == "closure") {
    call_statistics_functions <- statistics_functions
  } else {
    statistics_formulas <- sapply(statistics_functions,
                                  function(x) lazyeval::interp(~fun, fun = x))
    call_statistics_functions <- function(df) {
      df %>% dplyr::summarise_at(dplyr::vars(summary),
                                 dplyr::funs_(statistics_formulas))
    }
  }

  one_sample <- function(df, call_summary_function, summary_groups, replace) {
    function(k) {
      if (!is.null(summary_groups)) {
        df <- df %>%
          dplyr::group_by_(.dots = summary_groups)
      }
      df %>%
        dplyr::sample_frac(size = size, replace = replace) %>%
        call_summary_function() %>%
        dplyr::mutate(sample = k)
    }
  }

  all_samples <- sapply(1:nboot, one_sample(data, call_summary_function,
                                            summary_groups, replace),
                        simplify = FALSE) %>%
    dplyr::bind_rows()

  if (is.null(summary_groups) & !is.null(original_groups))
    all_samples <- dplyr::group_by_(all_samples,.dots = original_groups)

  if (!is.null(statistics_groups)) {
    all_samples <- dplyr::group_by_(all_samples, .dots = statistics_groups)
  }

  booted_vals <- call_statistics_functions(all_samples)

  if (typeof(statistics_functions) == "character" &
      length(statistics_functions) == 1) {
    booted_vals <- dplyr::rename_(booted_vals,
                                  .dots = stats::setNames("summary",
                                                          statistics_functions))
  }

  return(booted_vals)
}


#' Nonparemetric Bootstrap and Empirical central tendency for data frames
#' Designed to make standard use of \code{multi_boot.data.frame} easier
#'
#' Computes arbitrary bootstrap statistics on univariate data.
#' NOTE: Both empirical functions and bootstrapping functions will be computed
#'  over the grouping variables currently specified for the data frame.
#'
#' @param data A data frame.
#' @param column A string indicating the column of \code{data} to bootstrap
#' @param na.rm A logical indicating whether NAs should be dropped before
#'  bootstrapping (defaults to \code{NULL})
#' @param empirical_function a string indicating the function to compute and
#'  bootstrap over (defaults to \code{mean})
#' @param statistics_functions A vector of strings that are names of functions
#'  to be bootstrapped (defaults to \code{c("ci_lower", "ci_upper")})
#' @param nboot The number of bootstrap samples to take (defaults to \code{1000}).
#'
#' @examples
#' ## Mean and 95% confidence interval for 1000 samples from two different normal distributions
#' require(dplyr)
#' gauss1 <- data.frame(value = rnorm(1000, mean = 0, sd = 1), condition = 1)
#' gauss2 <- data.frame(value = rnorm(1000, mean = 2, sd = 3), condition = 2)
#' ci_lower <- function(x) {quantile(x, 0.025)}
#' ci_upper <- function(x) {quantile(x, 0.975)}
#' df <- bind_rows(gauss1, gauss2) %>%
#'  group_by(condition)
#'  multi_boot_standard(data = df, column = "value")
#' @export
multi_boot_standard <- function(data, column, na.rm = NULL,
                                empirical_function = "mean",
                                statistics_functions = c("ci_lower",
                                                         "ci_upper"),
                                nboot = 1000) {

  assertthat::assert_that(typeof(empirical_function) == "character")

  if (!is.null(na.rm)) {
    empirical_dots <- list(lazyeval::interp(~fun(arg, na.rm = na.rm),
                                            fun = as.name(empirical_function),
                                            arg = as.name(column)))

    statistics_funs <- sapply(
      statistics_functions,
      function(x) lazyeval::interp(~fun(., na.rm = na.rm), fun = as.name(x))
    )

    statistics_formulas <- function(df)
      df %>% dplyr::summarise_at(dplyr::vars(summary),
                                 dplyr::funs_(statistics_funs))


  } else {
    empirical_dots <- list(lazyeval::interp(~fun(arg),
                                            fun = as.name(empirical_function),
                                            arg = as.name(column)))

    statistics_formulas <- statistics_functions
  }

  call_empirical_function <- function(df) {
    dplyr::summarise_(df, .dots = stats::setNames(empirical_dots, "summary"))
  }

  booted_data <- multi_boot(data, summary_function = call_empirical_function,
                            column, statistics_functions = statistics_formulas,
                            nboot = nboot)

  call_empirical_function(data) %>%
    dplyr::left_join(booted_data) %>%
    dplyr::rename_(.dots = stats::setNames("summary", empirical_function))
}

#' Non-parametric bootstrap with multiple sample statistics
#'
#' \code{multi_boot} is a generic function for bootstrapping on various data
#' structures. The function invokes particular methods which depend on the class
#' of the first argument.
#'
#' @param data A data structure containg the data to bootstrap.
#' @param ... Additional arguments passed to particular methods.
#'
#' @examples
#' ## List of available methods
#' methods(multi_boot)
#' @export
multi_boot <- function(data, ...) UseMethod("multi_boot")
