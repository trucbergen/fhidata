#' Childhood vaccination rates in Norway
#'
#' We conveniently package vaccine coverage data taken from "Kommunehelsa statistikkbank".
#' This data was last updated on 2019-04-09.
#'
#' This dataset contains national/county/municipality level (5 year average) vaccination coverage rates
#' for 16 year olds for the childhood vaccination program (diphtheria, hpv, measles,
#' mumps, poliomyelitis, pertussis, rubella, tetanus).
#'
#' Municipalities are updated for the 2019 redistricting.
#'
#' @format
#' \describe{
#' \item{year}{The middle year of a 5 year range (e.g. 2011 is the average of data from 2009-2013).}
#' \item{location_code}{The location code.}
#' \item{age}{The population age.}
#' \item{vax}{The vaccine.}
#' \item{proportion}{Proportion of people who are vaccinated.}
#' \item{imputed}{FALSE if real data. TRUE if it is the national average.}
#' }
#' @source \url{http://khs.fhi.no/webview/}
"norway_childhood_vax"

# Creates the childhood vaccination dataset
# http://khs.fhi.no/webview/
#' @import data.table
gen_norway_childhood_vax <- function(norway_locations_long_current) {

  # variables used in data.table functions in this function
  . <- NULL
  GEO <- NULL
  location_code <- NULL
  AAR <- NULL
  SPVFLAGG <- NULL
  ALDER <- NULL
  proportion <- NULL
  RATE <- NULL
  vax <- NULL
  VAKSINE <- NULL
  national <- NULL
  age <- NULL
  imputed <- NULL
  # end

  d <- fread(system.file("extdata", "SYSVAK_2019-04-09-14-17.csv", package = "fhidata"), encoding = "UTF-8")
  d[GEO == 0, location_code := "norway"]
  d[GEO > 0 & GEO < 100, location_code := glue::glue("county{X}", X = formatC(GEO, width = 2, flag = "0"))]
  d[GEO >= 100, location_code := glue::glue("municip{X}", X = formatC(GEO, width = 4, flag = "0"))]
  d[GEO >= 10000, location_code := glue::glue("district{X}", X = formatC(GEO, width = 6, flag = "0"))]

  d[, year := as.numeric(stringr::str_extract(AAR, "^[0-9][0-9][0-9][0-9]")) + 2]
  d <- d[SPVFLAGG == 0 & ALDER == "16_16" & !stringr::str_detect(location_code, "district")]
  d[, age := 16]
  d[, proportion := RATE / 100]
  d[, vax := as.character(forcats::fct_recode(VAKSINE,
    "measles" = "Meslinger",
    "diphtheria" = "Difteri",
    "hpv" = "HPV",
    "pertussis" = "Kikhoste",
    "mumps" = "Kusma",
    "poliomyelitis" = "Poliomyelitt",
    "rubella" = "Rodehunder",
    "tetanus" = "Stivkrampe"
  ))]
  d <- d[, c("location_code", "year", "age", "vax", "proportion")]
  d[, imputed := FALSE]
  national_results <- d[location_code == "norway", .(
    national = mean(proportion)
  ),
  keyby = .(year, vax)
  ]
  skeleton <- data.table(expand.grid(
    location_code = norway_locations_long_current$location_code,
    year = unique(d$year),
    vax = unique(d$vax),
    stringsAsFactors = F
  ))
  d <- merge(d, skeleton,
    by = c("location_code", "year", "vax"), all = T
  )
  d <- merge(d, national_results, by = c("year", "vax"))
  d[is.na(age), age := 16]
  d[is.na(proportion), imputed := TRUE]
  d[is.na(proportion), proportion := national]
  d[, national := NULL]
  d <- d[location_code != "norge"]

  setcolorder(d, c("year", "location_code", "age", "vax", "proportion", "imputed"))

  return(invisible(d))
}
