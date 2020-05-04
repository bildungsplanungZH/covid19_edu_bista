# visualize BISTA LENA data for monitoring covid19
#
# Authors: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 1.05.2020
###############################################################################

library(TTR)
library(forecast)

# load functions
save_data <- FALSE
source('prepareLENA.R')

# load data, replace NA values by mean of passed school year
data <- getData() %>%
    mutate('sj' := biplaR::cutDatesToSchoolYear(as.Date(.data$date))) %>%
    group_by(.data$variable_short, .data$sj) %>%
    mutate('value_corr' := .data$value %>% replace_na(mean(.data$value, na.rm = T))) %>%
    ungroup()

base_data <- data %>% filter(.data$variable_short %in% 'lehrstellen_offen')
fc_start <- '2017-08-01'
hw_alpha <- NULL 
hw_beta <- NULL
hw_gamma <- NULL

# function to get a single forecast point
getSingleFC <- function(base_data, hw_alpha = NULL, hw_beta = NULL, hw_gamma = NULL)
{
    # parse argument
    assert_that(is(base_data, 'data.frame'))
    
    assert_that(has_name(base_data, 'variable_short'))
    assert_that(is(base_data$variable_short, 'character'))
    assert_that(length(unique(base_data$variable_short)) == 1)
    
    assert_that(has_name(base_data, 'date'))
    assert_that(is(base_data$date, 'POSIXct'))
    
    assert_that(has_name(base_data, 'value_corr'))
    assert_that(is(base_data$value_corr, 'numeric'))
    assert_that(noNA(base_data$value_corr))
    
    # get infos
    variable_short <- as.character(unique(base_data$variable_short))
    ts_start <- c(year(head(base_data$date, 1)), head(month(base_data$date, 1)))

    # generate timeseries, decompose, forcast
    ts_base <- ts(base_data$value_corr, frequency = 12, start = ts_start)
    ts_decomp <- decompose(ts_base, type = 'add')
    ts_fcm <- HoltWinters(x = ts_base, alpha = hw_alpha, beta = hw_beta, gamma = hw_gamma, seasonal = 'add')
    ts_fc <- forecast(ts_fcm, h = 12)
    
    # extract infos
    fc_dates <- matrix(c(start(ts_fc$mean), end(ts_fc$mean)), ncol = 2, byrow = TRUE) %>% 
        as.data.frame() %>%
        arrange(.data$V1, .data$V2) %>%
        unique() %>%
        mutate('V3' := 1,
               'date' := as.POSIXct(str_c(.data$V1, .data$V2, .data$V3, sep = '-')))
    
    tbl_fc <- tibble('last_value_date' := max(base_data$date),
                     'date' := seq(fc_dates$date[1], fc_dates$date[2], by = 'month'),
                     'variable_short' := variable_short,
                     'fc_lower_95' := ts_fc$lower[, '95%'],
                     'fc_lower_80' := ts_fc$lower[, '80%'],
                     'fc_mean' := as.numeric(ts_fc$mean),
                     'fc_upper_80' := ts_fc$upper[, '80%'],
                     'fc_upper_95' := ts_fc$upper[, '95%']) %>%
        mutate_if(rlang::is_named, unname)
    
    # format, return
    return(list('tbl' = tbl_fc, 'fc' = ts_fc))
}

getSeriesFC <- function(base_data, fc_start, hw_alpha = NULL, hw_beta = NULL, hw_gamma = NULL)
{
    # parse arguments
    assert_that(is(base_data, 'data.frame'))
    
    assert_that(has_name(base_data, 'variable_short'))
    assert_that(is(base_data$variable_short, 'character'))
    assert_that(length(unique(base_data$variable_short)) == 1)
    
    assert_that(has_name(base_data, 'date'))
    assert_that(is(base_data$date, 'POSIXct'))
    
    fc_start_res <- purrr::safely(as.POSIXct)(fc_start)
    assert_that(rlang::is_null(fc_start_res$error))
    fc_start <- fc_start_res$result
    assert_that(is.POSIXct(fc_start))
    assert_that(length(fc_start) == 1)
    
    # get possible last value dates
    lv_dates <- base_data %>% filter(.data$date >= fc_start) %>% pull(.data$date)
    
    # get single forcasts
    ls_fc <- purrr::map(lv_dates, ~getSingleFC(base_data = base_data %>% filter(.data$date <= .x), hw_alpha = hw_alpha, hw_beta = hw_beta, hw_gamma = hw_gamma))
    
    # combine single forcasts
    tbl_fc_all <- purrr::map_dfr(ls_fc, ~.x$tbl)
    idx <- tbl_fc_all %>%
        group_by(.data$date, .data$variable_short) %>%
        mutate('idx' := .data$last_value_date %in% max(.data$last_value_date)) %>%
        ungroup() %>% pull(.data$idx) %>% which()
    assert_that(all.equal(unique(idx - lag(idx)), c(NA_integer_, 12, 1)))
    
    tbl_fc <- tbl_fc_all %>% slice(idx) %>%
        left_join(base_data %>% select(.data$date, .data$value), by = 'date') %>%
        mutate_at(vars(matches('^fc_')), ~replace(.x, which(month(.data$date) == 8), NA))
    
    mean(purrr::map_dbl(ls_fc, ~mean(.x$fc$residuals ^ 2, na.rm = TRUE)))
    
    stats_fc <- list('method' = ls_fc[[1]]$fc$method,
                     'model' = ls_fc[[1]]$fc$model,
                     'var_bw' = mean(purrr::map_dbl(ls_fc, ~mean(.x$fc$residuals ^ 2, na.rm = TRUE))),
                     'var_fc' = mean((tbl_fc$value - tbl_fc$fc_mean) ^ 2, na.rm = TRUE))
    
    # format, return
    return(list('tbl' = tbl_fc, 'fc' = stats_fc))
}


test_1 <- getSingleFC(base_data = base_data %>% filter(.data$date <= fc_start))
test_2 <- getSeriesFC(base_data = base_data, fc_start = fc_start, hw_beta = FALSE)
test_2_plot <- ggplot(test_2$tbl, aes(x = date)) +
    geom_ribbon(aes(ymin = fc_lower_95, ymax = fc_upper_95), fill = biplaR::getColorZH(1, 'zhpastel'), na.rm = T) +
    geom_line(aes(y = value), size = 1, na.rm = T) +
    scale_y_continuous(limits= )

# test_3_ls <- purrr::map(seq(0, 1, by = .1), ~getSeriesFC(base_data = base_data, fc_start = fc_start, hw_gamma = .x))
# test_3_data <- tibble('variable_short' := test_3_ls[[1]]$tbl$variable_short[1],
#                       'hw_gamma' := purrr::map_dbl(test_3_ls, ~.x$fc$model$gamma),
#                       'mean_residuals_bw' := purrr::map_dbl(test_3_ls, ~.x$fc$mean_residuals_bw),
#                       'mean_variance_fc' := purrr::map_dbl(test_3_ls, ~.x$fc$mean_variance_fc)) %>%
#     gather('cat', 'var', -.data$variable_short, -.data$hw_gamma)
# 
# test_3_plot <- ggplot(test_3_data, aes(x = hw_gamma, y = var, colour = cat)) +
#     geom_line()
