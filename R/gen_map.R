#' norway_map_counties
#' https://kartkatalog.geonorge.no/metadata/uuid/cb02ab77-d3e6-4500-8a92-ea67367e7734
#' @param save_loc Location to save file to
norway_map_counties <- function(save_loc = file.path("inst", "createddata")) {
  id <- NULL

  require_namespace(c("geojsonio","broom","rmapshaper"))

  spdf <- geojsonio::geojson_read(
    system.file("extdata", "Fylker19.geojson", package = "fhidata"),
    what = "sp")

  spdf_simple <- rmapshaper::ms_simplify(spdf, keep = 0.1)

  spdf_fortified <- broom::tidy(spdf_simple, region = "fylkesnummer")

  setDT(spdf_fortified)
  spdf_fortified[,county_code:=sprintf("county%s",id)]

  if (dir.exists(save_loc)) {
    try(saveRDS(spdf_fortified, file.path(save_loc, "norway_map_counties.rds")), TRUE)
  }

  return(invisible(spdf_fortified))
}
