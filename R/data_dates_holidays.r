#' Dates of Norwegian holidays
#'
#' @format
#' \describe{
#' \item{date}{Date.}
#' \item{is_holiday}{Is it a holiday?}
#' }
"norway_dates_holidays"

# Creates the norway_locations data.table
gen_norway_dates_holidays <- function() {

  # variables used by data.table
  is_current <- NULL
  year_end <- NULL
  #

  info <- readxl::read_excel(
    system.file("extdata", "dates_holidays.xlsx", package = "fhidata")
  )
  info$date <- as.Date(info$date)
  setDT(info)

  year_min <- lubridate::year(min(info$date))
  year_max <- lubridate::year(max(info$date))

  date_min <- as.Date(glue::glue("{year_min}-01-01"))
  date_max <- as.Date(glue::glue("{year_max}-12-31"))

  norway_dates_holidays <- data.table(date = seq.Date(date_min, date_max, by = 1))
  norway_dates_holidays[, is_holiday := FALSE]
  norway_dates_holidays[info, on = "date", is_holiday := TRUE]

  return(norway_dates_holidays)
}
