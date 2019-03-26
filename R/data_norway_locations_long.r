#' Names of areas in Norway that currently exist.
#'
#' @format
#' \describe{
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' }
#' @source \url{https://snl.no/kommunenummer}
"norway_locations_long_current"

#' Names of areas in Norway that previously existed.
#'
#' @format
#' \describe{
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_locations_long_original"

# Creates the norway_locations data.table
gen_norway_locations_long <- function(is_current_municips = TRUE) {
  if (is_current_municips) {
    final_name <- "norway_locations_long_current"
    mid_name <- "norway_locations_current"
  } else {
    final_name <- "norway_locations_long_original"
    mid_name <- "norway_locations_original"
  }

  a1 <- data.table(location_code = "norway", location_name = "Norway")
  a2 <- data.table(location_code = "norge", location_name = "Norge")
  b <- gen_norway_locations(is_current_municips = is_current_municips)[, c("municip_code", "municip_name")]
  c <- gen_norway_locations(is_current_municips = is_current_municips)[, c("county_code", "county_name")]
  setnames(b, c("location_code", "location_name"))
  setnames(c, c("location_code", "location_name"))

  retval <- unique(rbind(a1, a2, b, c))

  return(retval)
}
