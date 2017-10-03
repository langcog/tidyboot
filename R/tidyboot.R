if (getRversion() >= "2.15.1") utils::globalVariables(c("stat", ".id", "strap"))

#' @importFrom dplyr "%>%"
#' @importFrom dplyr n
#' @importFrom rlang ":="
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
#' ## Mean and 95% confidence interval for 500 samples from a normal distribution
#' x <- rnorm(500, mean = 0, sd = 1)
#' tidyboot(x, statistics_functions = list("ci_lower" = ci_lower,
#'                                         "mean" = mean,
#'                                         "ci_upper" = ci_upper))
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
#' ## Mean and 95% confidence interval for 500 samples from a binomial distribution
#' x <- as.logical(rbinom(500, 1, 0.5))
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
#' @param column A column of \code{data} to bootstrap over (if not supplied,
#'   summary_function and statistic_function must operate over the appropriate
#'   data frame).
#' @param summary_function A function to be computed over each set of samples as
#'   a data frame, or a function to be computed over each set of samples as a
#'   single column of a data frame indicated by \code{column} (defaults to
#'   \code{mean}).
#' @param statistics_functions A function to be computed over each set of
#'   samples as a data frame, or a named list of functions to be computed over
#'   each set of samples as a single column of a data frame indicated by
#'   \code{column}.
#' @param nboot The number of bootstrap samples to take (defaults to
#'   \code{1000}).
#' @param ... Other arguments passed from generic.
#'
#' @examples
#' ## Mean and 95% confidence interval for 500 samples from two different normal distributions
#' require(dplyr)
#' gauss1 <- data_frame(value = rnorm(500, mean = 0, sd = 1), condition = 1)
#' gauss2 <- data_frame(value = rnorm(500, mean = 2, sd = 3), condition = 2)
#' df <- bind_rows(gauss1, gauss2)
#' df %>% group_by(condition) %>%
#'   tidyboot(summary_function = function(x) x %>% summarise(mean = mean(value)),
#'            statistics_functions = function(x) x %>%
#'            summarise_at(vars(mean), funs(ci_upper, mean, ci_lower)))
#' @export
tidyboot.data.frame <- function(data,
                                column = NULL,
                                summary_function = mean,
                                statistics_functions,
                                nboot = 1000, ...) {

  data_groups <- dplyr::groups(data)

  column <- rlang::enquo(column)

  if (rlang::quo_is_null(column)) {
    call_summary_function <- summary_function
    call_statistics_functions <- statistics_functions

  } else {

    summary_function <- rlang::enquo(summary_function)
    summary_function_name <- rlang::quo_name(summary_function)
    call_summary_function <- function(df) {
      df %>% dplyr::summarise_at(dplyr::vars(!!column),
                                 dplyr::funs(!!summary_function)) %>%
        dplyr::rename(!!summary_function_name := !!column)
    }

    if (length(statistics_functions) == 1 &
        !("list" %in% class(statistics_functions))) {
      statistics_functions <- list(statistics_functions)
    }
    call_statistics_functions <- function(df) {
      df %>% dplyr::summarise_at(dplyr::vars(!!summary_function_name),
                                 dplyr::funs(!!!statistics_functions))
    }

  }

  empirical_summary <- data %>%
    call_summary_function()

  names(empirical_summary) <- names(empirical_summary) %>%
    purrr::modify_if(~!(.x %in% groups(data)), ~paste0("empirical_", .x)) %>%
    unlist()

  n_summary <- data %>%
    dplyr::summarise(n = n())

  samples <- data %>%
    modelr::bootstrap(n = nboot) %>%
    dplyr::mutate(strap = purrr::map(strap, dplyr::as_data_frame)) %>%
    tidyr::unnest()

  if (!rlang::is_null(data_groups)) {
    samples <- samples %>% dplyr::group_by(!!!data_groups, .id)
  } else {
    samples <- samples %>% dplyr::group_by(.id)
  }

  sample_vals <- samples %>%
    call_summary_function()

  if (!is.null(data_groups)) {
    sample_vals <- sample_vals %>% dplyr::group_by(!!!data_groups)
  }

  booted_vals <- call_statistics_functions(sample_vals)

  if (nrow(empirical_summary) > 1) {
    n_summary %>%
      dplyr::left_join(empirical_summary,
                       by = as.character(dplyr::groups(data))) %>%
      dplyr::left_join(booted_vals,
                       by = as.character(dplyr::groups(data)))
  } else {
    n_summary %>%
      dplyr::bind_cols(empirical_summary) %>%
      dplyr::bind_cols(booted_vals)
  }

}


#' Non-parametric bootstrap and empirical central tendency for data frames
#' Designed to make standard use of \code{tidyboot.data.frame} easier
#'
#' Computes arbitrary bootstrap statistics on univariate data. NOTE: Both
#' empirical functions and bootstrapping functions will be computed over the
#' grouping variables currently specified for the data frame.
#'
#' @param data A data frame.
#' @param column A column of \code{data} to bootstrap over.
#' @param nboot The number of bootstrap samples to take (defaults to
#'   \code{1000}).
#' @param na.rm A logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @examples
#' ## Mean and 95% confidence interval for 500 samples from two different normal distributions
#' require(dplyr)
#' gauss1 <- data_frame(value = rnorm(500, mean = 0, sd = 1), condition = 1)
#' gauss2 <- data_frame(value = rnorm(500, mean = 2, sd = 3), condition = 2)
#' df <- bind_rows(gauss1, gauss2)
#' df %>%
#'  group_by(condition) %>%
#'  tidyboot_mean(column = value)
#' @export
tidyboot_mean <- function(data, column, nboot = 1000, na.rm = FALSE) {

  column <- rlang::enquo(column)

  summary_function <- function(df) {
    df %>% dplyr::summarise(mean = mean(!!column, na.rm = na.rm))
  }

  statistics_functions <- function(df) {
    df %>% dplyr::summarise(ci_lower = ci_lower(mean),
                            mean = mean(mean),
                            ci_upper = ci_upper(mean))
  }

  tidyboot(data,
           summary_function = summary_function,
           statistics_functions = statistics_functions,
           nboot = nboot)

}


#' Non-parametric bootstrap with multiple sample statistics
#'
#' \code{tidyboot} is a generic function for bootstrapping on various data
#' structures. The function invokes particular methods which depend on the class
#' of the first argument.
#'
#' @param data A data structure containing the data to bootstrap.
#' @param ... Additional arguments passed to particular methods.
#'
#' @examples
#' ## List of available methods
#' methods(tidyboot)
#' @export
tidyboot <- function(data, ...) UseMethod("tidyboot")
