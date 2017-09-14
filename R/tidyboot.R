#' @importFrom dplyr "%>%"
NULL

#' Non-parametric bootstrap for numeric vector data
#'
#' Computes arbitrary bootstrap statistics on univariate data.
#'
#' @param data A numeric vector of data to bootstrap over.
#' @param summary_function A function to be computed over each set of samples.
#'   This function needs to take a vector and return a single number (defaults
#'   to \code{mean}).
#' @param statistics_functions A named list of functions to be computed over the
#'   set of summary values from all samples.
#' @param nboot The number of bootstrap samples to take (defaults to
#'   \code{1000}).
#' @param size The fraction of items to sample (defaults to 1).
#' @param replace Logical indicating whether to sample with replacement
#'   (defaults to \code{TRUE}).
#' @param ... Other arguments passed from generic.
#'
#' @examples
#' ## Mean and 95% confidence interval for 1000 samples from a normal distribution
#' x <- rnorm(1000, mean = 0, sd = 1)
#' tidyboot(x, statistics_functions = list("ci_lower" = ci_lower,
#'                                           "mean" = mean,
#'                                           "ci_upper" = ci_upper))
#' @export
tidyboot.numeric <- function(data,
                             summary_function = mean,
                             statistics_functions,
                             nboot = 1000,
                             size = 1,
                             replace = TRUE, ...) {

  one_sample <- function() {
    do.call(summary_function, list(sample(data, size = size * length(data),
                                          replace = replace)))
  }

  all_samples <- dplyr::data_frame(sample = replicate(nboot, one_sample())) %>%
    dplyr::summarise_at(dplyr::vars(sample),
                        dplyr::funs(!!!statistics_functions))

  return(all_samples)

}

#' Non-parametric bootstrap for logical vector data
#'
#' Computes arbitrary bootstrap statistics on univariate data.
#'
#' @param data A logical vector of data to bootstrap over.
#' @inheritParams tidyboot.numeric
#'
#' @examples
#' ## Mean and 95% confidence interval for 1000 samples from a binomial distribution
#' x <- as.logical(rbinom(1000, 1, 0.5))
#' tidyboot(x, statistics_functions = c(ci_lower, mean, ci_upper))
#' @export
tidyboot.logical <- function(data,
                             summary_function = mean,
                             statistics_functions,
                             nboot = 1000,
                             size = 1,
                             replace = TRUE, ...) {
  tidyboot(as.numeric(data), summary_function, statistics_functions, size,
           nboot, replace, ...)
}

#' Non-parametric bootstrap for data frames
#'
#' Computes arbitrary bootstrap statistics on univariate data.
#'
#' @param data A data frame.
#' @param summary_function A function to be computed over each set of samples as
#'   a data frame, or a string that is the name of the function to be computed
#'   over each set of samples as a single column of a data frame indicated by
#'   \code{column} (defaults to \code{mean}).
#' @param column A column of \code{data} to bootstrap over (if not supplied,
#'   summary_function and statistic_function must operate over the appropriate
#'   data frame).
#' @param summary_groups A vector of strings that are column names of
#'   \code{data} indicating how it should be grouped before applying
#'   \code{summary_function}.
#' @param statistics_functions A named list of functions to be computed over the
#'   data frame of summary values from all samples.
#' @param statistics_groups A vector of strings that are column names of
#'   \code{data} indicating how it should be grouped before applying
#'   \code{statistics_functions} (defaults to \code{summary_groups}).
#' @param nboot The number of bootstrap samples to take (defaults to
#'   \code{1000}).
#' @param ... Other arguments passed from generic.
#'
#' @examples
#' ## Mean and 95% confidence interval for 1000 samples from two different normal distributions
#' require(dplyr)
#' gauss1 <- data_frame(value = rnorm(1000, mean = 0, sd = 1), condition = 1)
#' gauss2 <- data_frame(value = rnorm(1000, mean = 2, sd = 3), condition = 2)
#' tidyboot(data = bind_rows(gauss1, gauss2),
#'          summary_function = mean, column = value, summary_groups = "condition",
#'          statistics_functions = list("ci_lower" = ci_lower, "mean" = mean, "ci_upper" = ci_upper))
#' tidyboot(data = bind_rows(gauss1, gauss2),
#'          summary_function = function(df) df %>% summarise(mean = mean(value)),
#'          summary_groups = "condition",
#'          statistics_functions = function(df) df %>%
#'            summarise_at(vars(mean), funs(ci_upper, mean, ci_lower)),
#'          statistics_groups = "condition",
#'          nboot = 100)
#' @export
tidyboot.data.frame <- function(data,
                                summary_function = mean,
                                column = NULL,
                                summary_groups = NULL,
                                statistics_functions,
                                statistics_groups = summary_groups,
                                nboot = 1000, ...) {

  assertthat::assert_that(all(statistics_groups %in% summary_groups))

  # TODO: do something smart if no groups
  original_groups <- dplyr::groups(data)

  column <- rlang::enquo(column)

  if (rlang::quo_is_null(column)) {
    call_summary_function <- summary_function
  } else {
    summary_function <- rlang::enquo(summary_function)
    call_summary_function <- function(df) {
      df %>% dplyr::summarise_at(dplyr::vars(!!column),
                                 dplyr::funs(!!summary_function))
    }
  }

  if (rlang::quo_is_null(column)) {
    call_statistics_functions <- statistics_functions
  } else {
    if (length(statistics_functions) == 1 &
        !("list" %in% class(statistics_functions))) {
      statistics_functions <- list(statistics_functions)
    }
    call_statistics_functions <- function(df) {
      df %>% dplyr::summarise_at(dplyr::vars(!!column),
                                 dplyr::funs(!!!statistics_functions))
    }
  }

  summary_groups <- chr_to_quo(summary_groups)

  if (!is.null(summary_groups)) {
    data <- data %>% dplyr::group_by(!!!summary_groups)
  }

  samples <- data %>% modelr::bootstrap(n = nboot)

  summarise_sample <- function(df) {
    if (!is.null(summary_groups)) {
      df <- df %>% dplyr::group_by(!!!summary_groups)
    }
    df %>% call_summary_function()
  }

  sample_vals <- samples$strap %>%
    purrr::map_df(~.x$data %>% summarise_sample)

  if (is.null(statistics_groups) & !is.null(original_groups))
    sample_vals <- sample_vals %>% dplyr::group_by(!!!original_groups)

  if (!is.null(statistics_groups)) {
    statistics_groups <- chr_to_quo(statistics_groups)
    sample_vals <- sample_vals %>% dplyr::group_by(!!!statistics_groups)
  }

  booted_vals <- call_statistics_functions(sample_vals)

  return(booted_vals)
}


#' Nonparemetric Bootstrap and Empirical central tendency for data frames
#' Designed to make standard use of \code{tidyboot.data.frame} easier
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
#' df <- bind_rows(gauss1, gauss2) %>%
#'  group_by(condition)
#'  tidyboot_default(data = df, column = value)
#' @export
tidyboot_default <- function(data, column, na.rm = NULL,
                             empirical_function = mean,
                             statistics_functions = list("ci_lower" = ci_lower,
                                                         "ci_upper" = ci_upper),
                             nboot = 1000) {

  #assertthat::assert_that(typeof(empirical_function) == "character")
  column <- rlang::enquo(column)

  # if (!is.null(na.rm)) {
  #   empirical_dots <- list(lazyeval::interp(~fun(arg, na.rm = na.rm),
  #                                           fun = as.name(empirical_function),
  #                                           arg = as.name(column)))
  #
  #   statistics_funs <- sapply(
  #     statistics_functions,
  #     function(x) lazyeval::interp(~fun(., na.rm = na.rm), fun = as.name(x))
  #   )
  #
  #   statistics_formulas <- function(df)
  #     # df %>% dplyr::summarise_at(dplyr::vars(summary),
  #     #                            dplyr::funs_(statistics_funs))
  #     df %>% dplyr::summarise_at(dplyr::vars(!!column),
  #                                dplyr::funs(!!!statistics_funs))
  #
  #
  # } else {
  #   empirical_dots <- list(lazyeval::interp(~fun(arg),
  #                                           fun = as.name(empirical_function),
  #                                           arg = as.name(column)))
  #
  #   statistics_formulas <- statistics_functions
  # }

  # call_empirical_function <- function(df) {
  #   dplyr::summarise_(df, .dots = stats::setNames(empirical_dots, "summary"))
  # }

  booted_data <- tidyboot(data, summary_function = empirical_function,
                          column = column,
                          statistics_functions = statistics_functions,
                          nboot = nboot)

  # call_empirical_function(data) %>%
  #   dplyr::left_join(booted_data) %>%
  #   dplyr::rename_(.dots = stats::setNames("summary", empirical_function))
}

#' Non-parametric bootstrap with multiple sample statistics
#'
#' \code{tidyboot} is a generic function for bootstrapping on various data
#' structures. The function invokes particular methods which depend on the class
#' of the first argument.
#'
#' @param data A data structure containg the data to bootstrap.
#' @param ... Additional arguments passed to particular methods.
#'
#' @examples
#' ## List of available methods
#' methods(tidyboot)
#' @export
tidyboot <- function(data, ...) UseMethod("tidyboot")
