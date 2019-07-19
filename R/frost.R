split_equal <- function(x, size=10){
  split(x, ceiling(seq_along(x)/size))
}


#' Weather station list for max air temperatures in Norway
#'
#' We create a list of weather stations that contain max air temperature data for
#' each municipality in Norway. When a municipality does not have a relevant weather
#' stations, we choose a weather station that is close to the municipality.
#'
#' Municipalities are updated for the 2019 redistricting.
#'
#' @format
#' \describe{
#' \item{location_code}{The location code.}
#' \item{source}{Station code.}
#' }
#' @source \url{https://frost.met.no/}
"norway_weather_stations_max_air_temperature"

# Creates a station list for each municipality in norway that has max air temperature
# https://frost.met.no/
#' @import data.table
gen_norway_weather_stations_max_air_temperature <- function(norway_locations_long_current) {
  frost_client_id <- "c6d9bf2d-104c-4b5f-accf-d367b2220d62"

  sources <- frostr::get_sources(
    client_id = frost_client_id,
    types = "SensorSystem",
    country = "Norge",
    valid_time = "2000-01-01/now"
    )
  setDT(sources)
  sources <- sources[is.na(validTo)]
  sources[,time_from:=as.Date(stringr::str_sub(validFrom,1,10))]
  sources <- sources[!is.na(time_from)]
  sources <- sources[time_from>"1800-01-01"]
  sources <- sources[time_from<="2000-01-01"]
  sources[,location_code:=glue::glue("municip{x}",x=formatC(municipalityId,width=4,flag="0"))]

  sources[,is_met:=stationHolders=="MET.NO"]

  setorder(sources,location_code,-is_met)
  sources[,keep:=FALSE]
  sources[,within_id:=1:.N,by=location_code]

  i <- 1
  while(TRUE){
    message(i)
    wanted_sources <- sources[within_id==i]$id
    wanted_sources <- split_equal(wanted_sources, size=100)
    x <- NULL
    for(j in 1:length(wanted_sources)){
      y <- frostr::get_available_timeseries(
        client_id = frost_client_id,
        sources = wanted_sources[[j]]
      )
      x <- rbind(x,y)
    }
    setDT(x)
    x <- x[elementId=="max(air_temperature P1D)"]
    x[,sourceId:=stringr::str_remove_all(sourceId,":0$")]
    if(nrow(x) > 0){
      sources[id %in% x$sourceId,keep:=TRUE]
    }

    sources[,has_id:=max(keep),by=location_code]
    sources <- sources[has_id==0 | (has_id>0 & keep==T)]
    if(sum(!sources$keep)==0) break

    if(i>=max(sources$within_id)){
      break
    }
    i <- i + 1
  }
  sources <- sources[keep==T]

  chosen_sources <- fhidata::norway_locations_current[,"municip_code"]
  setnames(chosen_sources,"municip_code","location_code")
  chosen_sources[sources,on="location_code",sensor:=id]
  setorder(chosen_sources,location_code)
  chosen_sources[, sensor:=zoo::na.locf(sensor)]
  setnames(chosen_sources, "sensor", "source")

  return(invisible(chosen_sources))
}
