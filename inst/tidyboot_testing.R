gauss1 <- data_frame(value = rnorm(500, mean = 0, sd = 1), condition = 1)
gauss2 <- data_frame(value = rnorm(500, mean = 2, sd = 3), condition = 2)
df <- bind_rows(gauss1, gauss2) %>% group_by(condition)

system.time(
  df %>% tidyboot(summary_function = function(df) df %>%
                    summarise(stat = mean(value, na.rm = FALSE)),
                  statistics_functions = function(df) df %>%
                    summarise(ci_upper = ci_upper(stat),
                              mean = mean(stat),
                              ci_lower = ci_lower(stat)))
)

modelr_boot <- function(df, nboot = 1000) {
  booted <- df %>%
    modelr::bootstrap(nboot)

  booted$strap %>%
    map_df(~.x %>% as_data_frame() %>%
             group_by(condition) %>%
             summarise(stat = mean(value, na.rm = FALSE))) %>%
    group_by(condition) %>%
    summarise(ci_lower = ci_lower(stat),
              mean = mean(stat),
              ci_upper = ci_upper(stat)) %>%
    left_join(df %>% summarise(value = mean(value)))
}

system.time(
  df %>% modelr_boot()
)

modelr_boot <- function(df, nboot = 1000) {
  booted <- df %>%
    modelr::bootstrap(nboot) %>%
    mutate(strap = map(strap, as_data_frame)) %>%
    unnest()

  booted %>%
    group_by(.id, condition) %>%
    summarise(stat = mean(value)) %>%
    group_by(condition) %>%
    summarise(ci_lower = ci_lower(stat),
              mean = mean(stat),
              ci_upper = ci_upper(stat)) %>%
    left_join(df %>% summarise(value = mean(value)))
}

system.time(modelr_boot(df))


microbenchmark::microbenchmark(df %>% tidyboot_mean(column = value))
microbenchmark::microbenchmark(df %>% tidyboot_mean_na.rm(column = value,
                                                          na.rm = TRUE))

modelr_boot_na.rm <- function(df, nboot = 1000, na.rm = FALSE) {
  booted <- df %>%
    modelr::bootstrap(nboot)

  booted$strap %>%
    map_df(~.x %>% as_data_frame() %>%
             group_by(condition) %>%
             summarise(stat = mean(value, na.rm = na.rm))) %>%
    group_by(condition) %>%
    summarise(ci_lower = ci_lower(stat),
              mean = mean(stat),
              ci_upper = ci_upper(stat)) %>%
    left_join(df %>% summarise(value = mean(value)))
}

system.time(
  df %>% modelr_boot_na.rm()
)

hmisc_boot <- function(df) {
  df %>%
    group_by(condition) %>%
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$value))))
}

system.time(hmisc_boot(df))
