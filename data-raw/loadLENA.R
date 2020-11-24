# load LENA data from csv
# 
# author: Flavian Imlig <flavian.imlig@bi.zh.ch>
# date: 24.11.2020
###############################################################################

combineData <- function(...)
{
    files <- list('total' = file.path('data-raw', 'uebersicht-total-lehrstellen.csv'),
                  'offen' = file.path('data-raw', 'uebersicht-offene-lehrstellen.csv')) %>%
        purrr::keep(file.exists)
    assert_that(is.string(files$total))
    assert_that(is.string(files$offen))
    
    data <- left_join(
        readCSV(file = files$total) %>%
            rename('Lehrstellen_Total' := .data$n),
        readCSV(file = files$offen) %>%
            rename('Lehrstellen_Offen' := .data$n),
        by = c('Jahr', 'Monat')) %>%
        mutate('Lehrstellen_Besetzt' := (.data[['Lehrstellen_Total']] - .data[['Lehrstellen_Offen']]))
}

readCSV <- function(file, ...)
{
    data_raw <- read.csv(file) %>%
        rename_all(str_extract, pattern = '\\w{2,}')
    
    data_t <- gather(data_raw, key = 'Monat', value = 'n', -.data$Jahr) %>%
        mutate_at('Monat', ~as.integer(fct_inorder(.x))) %>%
        arrange(.data$Jahr, .data$Monat) %>%
        filter(.data$Jahr > 2013 | .data$Monat > 8) %>%
        filter(.data$Monat == 8 | is.finite(.data$n))
    
    return(data_t)
}

# main
if( TRUE ) {
    write.table(combineData(), file.path('data', 'lena.csv'), sep = ';', dec = '.', row.names = FALSE)
}
