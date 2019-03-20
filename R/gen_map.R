#' norway_map_counties
norway_map_counties <- function(save_loc = file.path("inst", "createddata")) {
  require_namespace(c("geojsonio","broom"))

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
