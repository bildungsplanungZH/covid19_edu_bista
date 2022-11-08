# load LENA data from csv
# 
# author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# date: 8.11.2022
###############################################################################

combineData <- function(...)
{
    base_data <- getBaseData() %>%
        bind_rows(getUpdateData())
    
    purrr::walk(c('date', 'mode'), ~assert_that(noNA(base_data[[.x]])))

    tibble('date' = seq(min(base_data$date), max(base_data$date[which(is.finite(base_data$Lehrstellen_Total))]), by = '1 month'))
    
    combined_data <- tibble('date' = seq(min(base_data$date), max(base_data$date[which(is.finite(base_data$Lehrstellen_Total))]), by = '1 month')) %>%
        left_join(base_data, by = 'date') %>%
        mutate('mode' := factor(.data$mode),
               'Jahr' := lubridate::year(.data$date),
               'Monat' := lubridate::month(.data$date)) %>%
        arrange(.data$date, desc(.data$mode))
    
    filtered_data <- combined_data %>%
        filter(!duplicated(.data$date))
    assert_that(identical(nrow(filtered_data), length(unique(filtered_data$date))))
    all(is.na(filtered_data %>% filter(.data$Monat %in% 8L) %>% pull(.data$Lehrstellen_Total)))
    all(is.finite(filtered_data %>% filter(!.data$Monat %in% 8L) %>% pull(.data$Lehrstellen_Total)))

    data_out <- filtered_data %>%
        select(all_of(c('Jahr', 'Monat')), matches('^Lehrstellen'))
    return(data_out)
}

getBaseData <- function(...)
{
    files <- list('total' = file.path('data-raw', 'uebersicht-total-lehrstellen.csv'),
                  'offen' = file.path('data-raw', 'uebersicht-offene-lehrstellen.csv')) %>%
        purrr::keep(file.exists)
    assert_that(is.string(files$total))
    assert_that(is.string(files$offen))
    
    data <- left_join(
        readCSV(file = files$total) %>%
            rename('Lehrstellen_Total' := 'value'),
        readCSV(file = files$offen) %>%
            rename('Lehrstellen_Offen' := 'value'),
        by = c('date', 'Jahr', 'Monat')) %>%
        mutate('Lehrstellen_Besetzt' := (.data[['Lehrstellen_Total']] - .data[['Lehrstellen_Offen']]),
               'mode' := 'base')
}

readCSV <- function(file, ...)
{
    data_raw <- read.csv(file) %>%
        rename_all(str_extract, pattern = '\\w{2,}')
    
    nm_cols <- names(data_raw)[-1]
    
    data_t <- data_raw %>%
        pivot_longer(all_of(nm_cols), names_to = 'Monat') %>%
        mutate_at('Monat', ~as.integer(fct_inorder(.x))) %>%
        mutate('date' := lubridate::ymd(str_c(.data$Jahr, .data$Monat, '1', sep = '.'))) %>%
        arrange(.data$date) %>%
        filter(.data$date > '2013-08-01')
    
    return(data_t)
}

getUpdateData <- function()
{
    files <- tibble('path' := list.files('data-raw', pattern = 'offene-und-besetzte-lehrstellen-nach-monaten_\\d{6}\\.csv', full.names = TRUE)) %>%
        mutate('data_date' := lubridate::ymd(str_extract(.data$path, '\\d{6}'))) %>%
        arrange(desc(.data$data_date))
    
    data_raw <- purrr::map_dfr(seq_along(files$path),
                               ~read.table(file = files$path[.x], sep = ',', header = TRUE, as.is = TRUE) %>%
                                   rename_all(~str_remove(.x, pattern = '^.\\W+')) %>%
                                   mutate('date' := lubridate::ymd(str_c(as.character(format(.data$Jahr.Monat, nsmall = 2)), '.1'))) %>%
                                   mutate('data_date' := files$data_date[.x]))
    
    data_filtered <- data_raw %>%
        arrange(.data$date, desc(.data$data_date)) %>%
        filter(!duplicated(.data$date))
    assert_that(identical(nrow(data_filtered), length(unique(data_filtered$date))))

    data_formatted <- data_filtered %>%
        mutate('Total' := .data$Offen + .data$Besetzt) %>%
        rename_at(c('Total', 'Offen', 'Besetzt'), ~str_c('Lehrstellen', .x, sep = '_')) %>%
        mutate('mode' := 'update')
    
    return(data_formatted)
}

# main
if( TRUE ) {
    write.table(combineData(), file.path('data', 'lena.csv'), sep = ';', dec = '.', row.names = FALSE)
}
