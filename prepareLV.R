# prepare BISTA LV data for monitoring covid19
#
# Authors: Flavian Imlig <flavian.imlig@bi.zh.ch>
# Date: 8.11.2022
###############################################################################

readMeta <- function(file = 'data/lv_meta.json')
{
    meta <- jsonlite::read_json(file)
}

getData <- function(file = 'data/lv.csv')
{
    data_raw <- read.csv(file, check.names = TRUE)
    assert_that(ncol(data_raw) == 13)
    
    data_t <- data_raw %>%
        rename_all(~c('jahr', 1:12)) %>%
        pivot_longer(all_of(as.character(1:12)), names_to = 'monat') %>%
        rename('value_raw' := 'value') %>%
        mutate_all(as.integer) %>%
        arrange(.data$jahr, .data$monat) %>% 
        drop_na() %>%
        mutate('date' := as.POSIXct(str_c(.data$jahr, .data$monat, '1', sep = '-')),
               'value' := case_when(.data$monat == 1 ~ .data$value_raw,
                                    TRUE ~ .data$value_raw - lag(.data$value_raw, 1)))
    
    data_chk <- data_t %>%
        group_by(.data$jahr) %>%
        summarize('n' := n(),
                  'a' := tail(na.omit(.data$value_raw, 1), 1),
                  'b' := sum(.data$value, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate('chk' := identical(.data$a, .data$b)) %>%
        filter(.data$n %in% 12, rlang::is_false(.data$chk))
    
    assert_that(nrow(data_chk) == 0, msg = sprintf('Data check failed for year(s) %s', str_c(data_chk$jahr, sep = ', ')))
    
    # ggplot(data_t, aes(x = date, y = value)) + geom_line()
    
    meta <- readMeta() %>%
        purrr::imap_dfc(~tibble(!!.y := .x[[1]]))
    
    df_spec <- readRDS(url('https://github.com/bildungsmonitoringZH/covid19_edu_mindsteps/raw/master/df_spec.rds'))
    
    data_out <- data_t %>%
        bind_cols(meta %>% slice(rep(1, nrow(data_t)))) %>%
        filter(.data$jahr >= 2019) %>%
        select(df_spec$name)
}

testTable <- function(df)
{
    df_spec <- readRDS(url('https://github.com/bildungsmonitoringZH/covid19_edu_mindsteps/raw/master/df_spec.rds'))
    
    assert_that(is(df, 'data.frame'))
    assert_that(identical(names(df), df_spec$name))
    
    purrr::pwalk(as.list(df_spec), ~assert_that(is(get(.x, df), .y)))
    
    v <- df %>% filter(.data$unit %in% 'Anteil') %>% pull(.data$value)
    assert_that(all(v <= 1))
    
    return(invisible(NULL))
}

# main
dat_prep <- getData()
test <- testTable(dat_prep)
write.table(dat_prep, "./Bildung_Lehrvertraege.csv", sep=",", fileEncoding="UTF-8", row.names = F)

if( FALSE ) {
    ggplot(dat_prep, aes(x = date, y = value, group = variable_short)) +
               geom_line()
}
