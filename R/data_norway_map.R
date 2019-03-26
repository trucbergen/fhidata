#' Maps of Norwegian Counties.
#'
#' We conveniently package map datasets for Norwegian counties
#' (taken from Geonorge) that can be used in ggplot2 without needing any geo
#' libraries. This data is licensed under Creative Commons 0.
#'
#' @format
#' \describe{
#' \item{long}{Location code.}
#' \item{lat}{Location name.}
#' \item{order}{The order that this line should be plotted in.}
#' \item{hole}{Geodata. Not used.}
#' \item{piece}{Geodata. Not used.}
#' \item{group}{Needs to be used as 'group' aesthetic in ggplot2.}
#' \item{id}{Non-informative id code.}
#' \item{location_code}{Location code (county code).}
#' }
#' @source \url{https://kartkatalog.geonorge.no/metadata/uuid/cb02ab77-d3e6-4500-8a92-ea67367e7734}
#' @examples
#' library(ggplot2)
#' q <- ggplot(mapping = aes(x = long, y = lat, group = group, fill = location_code))
#' q <- q + geom_polygon(data = fhidata::norway_map_counties, color = "black")
#' q <- q + theme_void()
#' q <- q + coord_quickmap()
#' q
"norway_map_counties"

#' Maps of Norwegian Municipalities
#'
#' We conveniently package map datasets for Norwegian municipalities
#' (taken from Geonorge) that can be used in ggplot2 without needing any geo
#' libraries. This data is licensed under Creative Commons 0.
#'
#' @format
#' \describe{
#' \item{long}{Location code.}
#' \item{lat}{Location name.}
#' \item{order}{The order that this line should be plotted in.}
#' \item{hole}{Geodata. Not used.}
#' \item{piece}{Geodata. Not used.}
#' \item{group}{Needs to be used as 'group' aesthetic in ggplot2.}
#' \item{id}{Non-informative id code.}
#' \item{location_code}{Location code (municipality code).}
#' }
#' @source \url{https://kartkatalog.geonorge.no/metadata/uuid/cb02ab77-d3e6-4500-8a92-ea67367e7734}
#' @examples
#' library(ggplot2)
#' q <- ggplot(mapping = aes(x = long, y = lat, group = group))
#' q <- q + geom_polygon(data = fhidata::norway_map_municips, color = "black")
#' q <- q + theme_void()
#' q <- q + coord_quickmap()
#' q
"norway_map_municips"

gen_norway_map_counties <- function() {
  id <- NULL
  location_code <- NULL

  require_namespace(c("geojsonio", "broom", "rmapshaper"))

  spdf <- geojsonio::geojson_read(
    system.file("extdata", "Fylker19.geojson", package = "fhidata"),
    what = "sp"
  )

  spdf_simple <- rmapshaper::ms_simplify(spdf, keep = 0.1)

  spdf_fortified <- broom::tidy(spdf_simple, region = "fylkesnummer")

  setDT(spdf_fortified)
  spdf_fortified[, location_code := sprintf("county%s", id)]

  return(invisible(spdf_fortified))
}

gen_norway_map_municips <- function() {
  id <- NULL
  location_code <- NULL

  require_namespace(c("geojsonio", "broom", "rmapshaper"))

  spdf <- geojsonio::geojson_read(
    system.file("extdata", "Kommuner19.geojson", package = "fhidata"),
    what = "sp"
  )

  spdf_simple <- rmapshaper::ms_simplify(rgeos::gBuffer(spdf, byid = TRUE, width = 0), keep = 0.1)

  spdf_fortified <- broom::tidy(spdf_simple, region = "kommunenummer")

  setDT(spdf_fortified)
  spdf_fortified[, location_code := sprintf("municip%s", formatC(as.numeric(id), width = 4, flag = "0"))]

  return(invisible(spdf_fortified))
}
