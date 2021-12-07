# visualize BISTA LENA data for monitoring covid19
#
# Authors: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 7.12.2021
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
    
    predict(ts_fcm, n.ahead = 12, prediction.interval = TRUE)
    
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
    return(list('tbl' = tbl_fc, 'fc' = ts_fcm))
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
    lv_dates <- base_data %>% 
        filter(.data$date >= fc_start) %>% 
        filter(month(.data$date) %in% 8)
        pull(.data$date)
    
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

getSingleFC_2 <- function(base_data, sj_s, hw_alpha, hw_beta, hw_gamma)
{
    # parse arguments
    assert_that(is(base_data, 'data.frame'))
    
    assert_that(has_name(base_data, 'variable_short'))
    assert_that(is(base_data$variable_short, 'character'))
    assert_that(length(unique(base_data$variable_short)) == 1)
    
    assert_that(has_name(base_data, 'date'))
    assert_that(is(base_data$date, 'POSIXct'))
    assert_that(has_name(base_data, 'sj'))
    assert_that(is.ordered(base_data$sj))
    
    assert_that(is.string(sj_s))
    assert_that(sj_s %in% levels(base_data$sj))
    assert_that(is.number(hw_alpha))
    assert_that(is.number(hw_beta))
    assert_that(is.number(hw_gamma))
    
    # define dates
    tbl_dates <- base_data %>%
        count(.data$sj, .data$date) %>%
        select(-.data$n) %>%
        filter(.data$sj < sj_s) %>%
        mutate('start' := lubridate::month(.data$date) %in% 9,
               'end' := lubridate::month(.data$date) %in% 7,
               'first' := seq_along(.data$date) %in% (which(.data$start) %>% head(1)),
               'last' := seq_along(.data$date) %in% (which(.data$end) %>% tail(1)))
    ts_range <- c(tbl_dates$date[tbl_dates$first], tbl_dates$date[tbl_dates$last])
    ts_start <- c(lubridate::year(ts_range[1]), lubridate::month(ts_range[1]))
    
    # filter data
    base_data_s <- base_data %>%
        filter(.data$date %>% between(ts_range[1], ts_range[2]))
    
    # get ts, model and forecast
    ts_base <- ts(base_data_s$value_corr, frequency = 12, start = ts_start)
    ts_decomp <- decompose(ts_base, type = 'add')
    ts_fcm <- HoltWinters(x = ts_base, alpha = hw_alpha, beta = hw_beta, gamma = hw_gamma, seasonal = 'add')
    ts_fc <- predict(ts_fcm, n.ahead = 12, prediction.interval = TRUE)
    ts_fc2 <- forecast(ts_fcm, h = 12)
    
    # format, return
    tbl_sj <- data.frame('raw' = time(ts_fc)) %>%
        mutate('year' := floor(.data$raw), 'month' := round((.data$raw - .data$year) * 12 + 1),
               'date' := lubridate::ymd(str_c(.data$year, round(.data$month), '1', sep = '-')),
               'sj' := biplaR::cutDatesToSchoolYear(.data$date) %>% as.character,
               'variable_short' := unique(base_data$variable_short),
               'last_value_date' := max(base_data_s$date))
    
    tbl_fc <- data.frame(as.matrix(ts_fc)) %>%
        bind_cols(tbl_sj) %>%
        select(all_of(c('date', 'sj', 'variable_short', 'fc_lower_95' = 'lwr', 'fc_upper_95' = 'upr', 'last_value_date')))
        
    return(list('tbl' = tbl_fc, 'fc' = ts_fcm))
}

getSeriesFC_2 <- function(base_data, sj_s = c('2019/20', '2020/21'), hw_alpha = .8, hw_beta = 0, hw_gamma = .1)
{
    tbl_def <- expand.grid('sj_s' = sj_s, 
                           'variable_short' = c('lehrstellen_total', 'lehrstellen_offen', 'lehrstellen_besetzt'),
                           stringsAsFactors = F)
    
    results <- purrr::pmap(tbl_def, ~getSingleFC_2(base_data = base_data %>% filter(.data$variable_short %in% ..2), sj_s = ..1, 
                                               hw_alpha = hw_alpha, hw_beta = hw_beta, hw_gamma = hw_gamma))
    results_table <- purrr::map_dfr(results, ~.x$tbl) %>%
        mutate_at('sj', ~ordered(.x, levels = sort(union(levels(base_data$sj), .x))))
    
    nm_join <- c('date', 'sj', 'variable_short')
    assert_that(identical(nrow(base_data), base_data %>% select(all_of(nm_join)) %>% nrow()))
    assert_that(identical(nrow(results_table), results_table %>% select(all_of(nm_join)) %>% nrow()))
    data_tbl <- base_data %>%
        mutate_at('sj', ~ordered(as.character(.x), levels = levels(results_table$sj))) %>%
        full_join(results_table, by = nm_join) %>%
        mutate_at(vars(matches('^fc_')), ~replace(.x, which(month(.data$date) == 8), NA))
    
    # stats_fc <- list('method' = str(results[[1]]$fc)
                     # 'model' = ls_fc[[1]]$fc$model,
                     # 'var_bw' = mean(purrr::map_dbl(ls_fc, ~mean(.x$fc$residuals ^ 2, na.rm = TRUE))),
                     # 'var_fc' = mean((tbl_fc$value - tbl_fc$fc_mean) ^ 2, na.rm = TRUE))
    
    # format, return
    return(list('tbl' = data_tbl, 'fc' = purrr::map(results, ~.x$fc) %>% purrr::set_names(str_c(tbl_def$variable_short, tbl_def$sj_s, sep = ', '))))
    # return(list('tbl' = tbl_fc, 'fc' = stats_fc))
}

plotFC <- function()
{
    hw_alpha <- .8
    hw_beta <- 0
    hw_gamma <- .1
    
    if( FALSE ) {
    fc_start <- '2017-09-01'
    fc_interval <- interval('2019-08-01', max(data$date))
    
    fc_total <- getSeriesFC(base_data = data %>% filter(.data$variable_short %in% 'lehrstellen_total'), fc_start = fc_start, hw_alpha = hw_alpha, hw_beta = hw_beta, hw_gamma = hw_gamma)
    fc_offen <- getSeriesFC(base_data = data %>% filter(.data$variable_short %in% 'lehrstellen_offen'), fc_start = fc_start, hw_alpha = hw_alpha, hw_beta = hw_beta, hw_gamma = hw_gamma)
    fc_besetzt <- getSeriesFC(base_data = data %>% filter(.data$variable_short %in% 'lehrstellen_besetzt'), fc_start = fc_start, hw_alpha = hw_alpha, hw_beta = hw_beta, hw_gamma = hw_gamma)
    
    test_2_plotdata <- fc_total$tbl %>% filter(.data$date %within% fc_interval) %>%
        bind_rows(fc_offen$tbl %>% filter(.data$date %within% fc_interval)) %>%
        bind_rows(fc_besetzt$tbl %>% filter(.data$date %within% fc_interval)) %>%
        filter(month(.data$date) != 8) %>%
        mutate_at('variable_short', ~fct_inorder(.x) %>% fct_relabel(~str_replace(.x, 'lehrstellen_', ''))) %>%
        mutate('sj' := biplaR::cutDatesToSchoolYear(as.Date(.data$date)),
               'month' := fct_inorder(format(.data$date, '%b')))
    
    test_2_plot <- ggplot(test_2_plotdata, aes(x = as.integer(month))) +
        geom_ribbon(aes(ymin = fc_lower_95, ymax = fc_upper_95, fill = variable_short), na.rm = T, alpha = .3) +
        geom_line(aes(y = value, colour = variable_short), size = biplaR::geom_args$line$size, na.rm = T) +
        geom_point(aes(y = value, colour = variable_short), size = biplaR::geom_args$point$size, na.rm = T) +
        facet_wrap('sj', nrow = 2) +
        coord_cartesian(ylim = c(0, NA)) +
        scale_x_continuous(breaks = 1:nlevels(test_2_plotdata$month), minor_breaks = c(), labels = levels(test_2_plotdata$month)) +
        scale_y_continuous(breaks = seq(0, 12000, by = 3000), minor_breaks = seq(0, 12000, by = 1000)) +
        scale_fill_manual('statistisch erwartbare Werte', values = biplaR::getColorZH(3, 'zhlight'), guide = guide_legend(nrow = 1, order = 2)) +
        scale_colour_manual('tatsächliche Werte', values = biplaR::getColorZH(3, 'zh'), guide = guide_legend(nrow = 1, order = 1)) +
        labs('title' = 'Lehrstellensituation im Kanton Zürich', 'subtitle' = 'gemäss kantonalem Lehrstellen-Nachweis', 'caption' = 'Daten: Bildungsstatistik Kanton Zürich/Gesellschaftsmonitoring Covid-19 STAT') +
        biplaR::getTheme(c('no_axis_title')) +
        theme(legend.margin = margin(3.2, 25.6, 0, 25.6, 'pt'),
              plot.title = element_text(size = rel(1)), 
              plot.subtitle = element_text(size = rel(.8)), 
              plot.caption = element_text(size = rel(.6)))

    plot_height <- ifelse(nlevels(test_2_plotdata$sj) > 1, biplaR::plot_dims$height * 1.6, biplaR::plot_dims$height)
    plot <- biplaR::savePlot(test_2_plot, tmpdir = 'img', height = plot_height)
    }
    
    sj_s <- c('2019/20', '2020/21', '2021/22')
    result_2 <- getSeriesFC_2(base_data = data, sj_s = sj_s,
                              hw_alpha = hw_alpha, hw_beta = hw_beta, hw_gamma = hw_gamma)
    plot_data_2 <- result_2$tbl %>%
        mutate_at('variable_short', ~fct_inorder(.x) %>% fct_relabel(~str_replace(.x, 'lehrstellen_', ''))) %>%
        filter(.data$sj %in% sj_s) %>%
        drop_na(.data$fc_lower_95) %>%
        droplevels()
    
    plot_args <- list('colour' = biplaR::getColorZH(n = nlevels(plot_data_2$variable_short), name = 'zh', with_zh_blue = F),
                      'guide' = guide_legend(nrow = 1),
                      'legend_caption' = c('tatsächliche Werte', 'statistisch erwartbare Werte'),
                      'title' = 'Lehrstellensituation im Kanton Zürich',
                      'subtitle' = 'gemäss kantonalem Lehrstellen-Nachweis', 
                      'caption' = 'Daten: Bildungsstatistik Kanton Zürich/Gesellschaftsmonitoring Covid-19 STAT',
                      'path' = 'img',
                      'height' = ifelse(nlevels(plot_data_2$sj) > 1, biplaR::plot_dims$height * 2.2, biplaR::plot_dims$height))
    
    plot_2 <- ggplot(plot_data_2, aes_string(x = 'date', y = 'value')) +
        geom_ribbon(aes_string(ymin = 'fc_lower_95', ymax = 'fc_upper_95', fill = 'variable_short'), na.rm = T, colour = NA, alpha = .3) +
        geom_line(aes_string(colour = 'variable_short'), size = biplaR::geom_args$line$size, na.rm = T) +
        geom_point(aes_string(colour = 'variable_short'), size = biplaR::geom_args$point$size, na.rm = T) +
        facet_wrap('sj', ncol = 1, scales = 'free_x') +
        coord_cartesian(ylim = c(0, NA)) +
        scale_colour_manual(plot_args$legend_caption[1], values = plot_args$colour, guide = plot_args$guide) +
        scale_fill_manual(plot_args$legend_caption[2], values = plot_args$colour, guide = plot_args$guide) +
        labs(title = plot_args$title, subtitle = plot_args$subtitle, caption = plot_args$caption) +
        biplaR::getTheme(c('no_axis_title'))
    plot_ref <- biplaR::savePlot(plot_2, tmpdir = plot_args$path, height = plot_args$height)
    
    
    return(list('plot' = plot_2, 'plot_ref' = plot_ref, 'fc' = result_2$fc))
}

res <- plotFC()