#' norway_map_counties
#' https://kartkatalog.geonorge.no/metadata/uuid/cb02ab77-d3e6-4500-8a92-ea67367e7734
#' @param save_loc Location to save file to
norway_map_counties <- function(save_loc = file.path("inst", "createddata")) {
  id <- NULL

  require_namespace(c("geojsonio", "broom", "rmapshaper"))

  spdf <- geojsonio::geojson_read(
    system.file("extdata", "Fylker19.geojson", package = "fhidata"),
    what = "sp"
  )

  spdf_simple <- rmapshaper::ms_simplify(spdf, keep = 0.1)

  spdf_fortified <- broom::tidy(spdf_simple, region = "fylkesnummer")

  setDT(spdf_fortified)
  spdf_fortified[, location_code := sprintf("county%s", id)]

  if (dir.exists(save_loc)) {
    try(saveRDS(spdf_fortified, file.path(save_loc, "norway_map_counties.rds")), TRUE)
  }

  return(invisible(spdf_fortified))
}

#' norway_map_municips
#' https://kartkatalog.geonorge.no/metadata/uuid/cb02ab77-d3e6-4500-8a92-ea67367e7734
#' @param save_loc Location to save file to
norway_map_municips <- function(save_loc = file.path("inst", "createddata")) {
  id <- NULL

  require_namespace(c("geojsonio", "broom", "rmapshaper"))

  spdf <- geojsonio::geojson_read(
    system.file("extdata", "Kommuner19.geojson", package = "fhidata"),
    what = "sp"
  )

  spdf_simple <- rmapshaper::ms_simplify(rgeos::gBuffer(spdf,byid=TRUE, width=0), keep = 0.2)

  spdf_fortified <- broom::tidy(spdf_simple, region = "kommunenummer")

  setDT(spdf_fortified)
  spdf_fortified[, location_code := sprintf("municip%s", formatC(as.numeric(id), width=4, flag="0"))]

  if (dir.exists(save_loc)) {
    try(saveRDS(spdf_fortified, file.path(save_loc, "norway_map_municips.rds")), TRUE)
  }

  return(invisible(spdf_fortified))
}
