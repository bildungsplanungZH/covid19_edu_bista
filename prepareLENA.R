# prepare BISTA LENA data for monitoring covid19
#
# Authors: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 1.05.2020
###############################################################################

readMeta <- function(file = 'data/lena_meta.json')
{
    meta <- jsonlite::read_json(file) %>%
        purrr::imap_dfc(~tibble('tmp' := 1:2, !!.y := as.character(.x)) %>% select(-.data$tmp))
}

getData <- function(file = 'data/lena.csv')
{
    data_raw <- read.csv2(file, check.names = TRUE) %>%
        rename_all(str_to_lower)
    assert_that(ncol(data_raw) == 5)
    assert_that(identical(data_raw$lehrstellen_offen+data_raw$lehrstellen_besetzt,
                          data_raw$lehrstellen_total))
    
    data_raster <- tibble('jahr' := seq(min(data_raw$jahr), length.out = 12), 
                          'monat' := 1:12,
                          'variable_short' := fct_inorder(head(rep(str_subset(names(data_raw), '^lehrstellen'), 10), 12))) %>%
        complete(.data$jahr, .data$monat, .data$variable_short) %>%
        arrange(.data$variable_short, .data$jahr, .data$monat)
    
    data_range <- range(as.POSIXct(str_c(data_raw$jahr, data_raw$monat, '1', sep = '-')))
    
    data_t <- data_raw %>%
        gather('variable_short', 'value', -.data$jahr, -.data$monat) %>%
        mutate_at('variable_short', fct_inorder) %>%
        mutate_at('value', as.integer) %>%
        drop_na() %>%
        right_join(data_raster, by = c('jahr', 'monat', 'variable_short')) %>%
        mutate('date' := as.POSIXct(str_c(.data$jahr, .data$monat, '1', sep = '-'))) %>%
        filter(.data$date >= data_range[1], .data$date <= data_range[2]) %>%
        arrange(.data$variable_short, .data$date)
        
    assert_that(identical(data_t %>% 
                              spread(.data$variable_short, .data$value) %>%
                              mutate('tmp_total' := .data$lehrstellen_offen + .data$lehrstellen_besetzt) %>%
                              pull(.data$tmp_total),
                          data_t %>%
                              filter(.data$variable_short %in% 'lehrstellen_total') %>%
                              pull(.data$value)))
    
    meta <- readMeta()
    
    df_spec <- readRDS(url('https://github.com/bildungsmonitoringZH/covid19_edu_mindsteps/raw/master/df_spec.rds'))
    
    data_out <- data_t %>%
        mutate_if(is.factor, as.character) %>%
        left_join(meta, by = 'variable_short') %>%
        select(df_spec$name)
}

testTable <- function(df)
{
    df_spec <- readRDS(url('https://github.com/bildungsmonitoringZH/covid19_edu_mindsteps/raw/master/df_spec.rds'))
    
    assert_that(is(df, 'data.frame'))
    assert_that(identical(names(df), df_spec$name))
    
    purrr::pwalk(as.list(df_spec), ~assert_that(is(get(.x, df), .y), msg = sprintf('df$%s is not of class %s.', .x, .y)))
    
    df_t <- df %>% select_if(is.character) %>%
        mutate_all(fct_inorder)
    
    meta_count <- readMeta() %>% as.list() %>% purrr::map_int(~length(unique(.x)))
    
    assert_that(identical(names(df_t), names(meta_count)))
    assert_that(identical(meta_count,
                          purrr::map_int(as.list(df_t), ~nlevels(.x))))
    
    return(invisible(NULL))
}

saveData <- function(flag)
{
    assert_that(is.flag(save_data))
    assert_that(noNA(save_data))

    if( flag) 
        {
        data_prep <- getData()
        test <- testTable(data_prep)
        save_data <- TRUE
        write.table(data_prep %>% filter(.data$date >= '2018-09-01', !is.na(.data$topic)), "./Bildung_Lehrstellen.csv", sep=",", fileEncoding="UTF-8", row.names = F)
    }
}

# main
if( !exists('save_data') ) save_data <- TRUE
saveData(save_data)
