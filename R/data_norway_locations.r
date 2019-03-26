#' Names of areas in Norway that currently exist.
#'
#' @format
#' \describe{
#' \item{municip_code}{Municipality code.}
#' \item{municip_name}{Municipality name.}
#' \item{county_code}{County code.}
#' \item{county_name}{County name.}
#' }
#' @source \url{https://snl.no/kommunenummer}
"norway_locations_current"

#' Names of areas in Norway that previously existed.
#'
#' @format
#' \describe{
#' \item{municip_code}{Municipality code.}
#' \item{municip_name}{Municipality name.}
#' \item{county_code}{County code.}
#' \item{county_name}{County name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_locations_original"

# Creates the norway_locations data.table
gen_norway_locations <- function(is_current_municips = TRUE) {

  # variables used by data.table
  is_current <- NULL
  year_end <- NULL
  #

  norway_locations <- readxl::read_excel(
    system.file("extdata", "norway_locations.xlsx", package = "fhidata")
  )
  setDT(norway_locations)

  norway_locations[, is_current := is.na(year_end)]

  norway_locations <- unique(norway_locations
  [
    ,
    c("is_current", "municip_code", "municip_name", "county_code", "county_name")
  ])

  if (is_current_municips) {
    return(norway_locations[is_current == TRUE, -"is_current"])
  } else {
    return(norway_locations[, -"is_current"])
  }
}
