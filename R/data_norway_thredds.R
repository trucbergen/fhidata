#' 1x1km grids to Norwegian municipalities
#'
#' We classify the 1x1km grids from senorge into municipalities.
#'
#' @format
#' \describe{
#' \item{row}{The row of the [X,Y,time] matrix.}
#' \item{col}{The column of the [X,Y,time] matrix.}
#' \item{location_code}{Municipality code that this 1x1km grid belongs to.}
#' \item{year}{The year that location_code corresponds to.}
#' }
#' @source \url{http://thredds.met.no/thredds/catalog/senorge/seNorge_2018/Archive/catalog.html}
#' @examples
#' library(ggplot2)
#' q <- ggplot(mapping = aes(x = long, y = lat, group = group))
#' q <- q + geom_polygon(data = fhidata::norway_map_municips, color = "black")
#' q <- q + theme_void()
#' q <- q + coord_quickmap()
#' q
"senorge"

gen_senorge <- function(norway_locations_current, norway_map_municips) {
  id <- NULL
  location_code <- NULL
  long <- NULL
  lat <- NULL

  require_namespace(c("ncdf4", "SDMTools"))

  date <- stringr::str_remove_all(lubridate::today() - 5, "-")

  temp_dir <- tempdir()
  file <- glue::glue("seNorge2018_{date}.nc")
  temp_file <- fs::path(temp_dir, file)

  on.exit(fs::file_delete(temp_file))

  utils::download.file(
    glue::glue("http://thredds.met.no/thredds/fileServer/senorge/seNorge_2018/Latest/{file}"),
    temp_file
  )

  nc <- ncdf4::nc_open(temp_file)
  dlong <- ncdf4::ncvar_get(nc, "lon")
  dlat <- ncdf4::ncvar_get(nc, "lat")

  dlong <- reshape2::melt(dlong)
  setDT(dlong)
  setnames(dlong, c("row", "col", "long"))

  dlat <- reshape2::melt(dlat)
  setDT(dlat)
  setnames(dlat, c("row", "col", "lat"))

  gps <- merge(dlong, dlat, by = c("row", "col"))
  gps[, location_code := as.character(NA)]

  for (i in norway_locations_current$municip_code) {
    message(i)
    res <- SDMTools::pnt.in.poly(gps[, c("long", "lat")], norway_map_municips[location_code == i, c("long", "lat")])
    indexes <- which(res$pip > 0)
    gps[indexes, location_code := i]
  }

  gps[, year := 2019]
  gps[, long := NULL]
  gps[, lat := NULL]
  gps <- gps[!is.na(location_code)]

  return(invisible(gps))
}
