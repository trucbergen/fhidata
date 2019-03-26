#' Redistricting in Norway.
#'
#' This dataset is used to transfer "original" municipality level datasets to the current redistricting.
#'
#' Last updated 2019-03-14
#'
#' @format
#' \describe{
#' \item{municip_code_current}{The municipality code per today.}
#' \item{municip_code_original}{The municipality code as of 'year'.}
#' \item{year}{The year corresponding to 'municip_code_original'.}
#' \item{municip_name}{The municipality name per today.}
#' \item{county_code}{The county code per today.}
#' \item{county_name}{The county name per today.}
#' \item{region_code}{The region code per today.}
#' \item{region_name}{The region name per today.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_municip_merging"

# Creates the norway_municip_merging (kommunesammenslaaing) data.table
gen_norway_municip_merging <- function() {
  # variables used in data.table functions in this function
  year_start <- NULL
  municip_code <- NULL
  municip_code_current <- NULL
  level <- NULL
  county_code <- NULL
  region_code <- NULL
  year_end <- NULL
  municip_name <- NULL
  municip_code_end <- NULL
  county_name <- NULL
  region_name <- NULL
  realEnd <- NULL
  # end

  masterData <- data.table(readxl::read_excel(system.file("extdata", "norway_locations.xlsx", package = "fhidata")))
  maxYear <- max(data.table::year(lubridate::today()), max(masterData$year_start, na.rm = T)) + 2

  masterData[year_start <= 2006, year_start := 2006]
  setnames(masterData, "year_start", "year")
  skeleton <- expand.grid(year = as.numeric(2006:maxYear), municip_code = unique(masterData$municip_code), stringsAsFactors = FALSE)
  skeleton <- data.table(merge(skeleton, masterData, by = c("municip_code", "year"), all.x = T))
  setorder(skeleton, municip_code, year)
  skeleton[is.na(year_end), year_end := maxYear]
  skeleton[, year_end := min(year_end, na.rm = T), by = municip_code]
  skeleton <- skeleton[year <= year_end]
  skeleton[, year_end := NULL]
  skeleton[, year_start := 9999]
  skeleton[!is.na(municip_name), year_start := year]
  skeleton[, year_start := min(year_start, na.rm = T), by = municip_code]
  skeleton <- skeleton[year >= year_start]
  skeleton[, municip_code_end := zoo::na.locf(municip_code_end), by = municip_code]
  skeleton[, municip_name := zoo::na.locf(municip_name), by = municip_code]
  skeleton[, county_code := zoo::na.locf(county_code), by = municip_code]
  skeleton[, county_name := zoo::na.locf(county_name), by = municip_code]
  skeleton[, region_code := zoo::na.locf(region_code), by = municip_code]
  skeleton[, region_name := zoo::na.locf(region_name), by = municip_code]

  skeletonFinal <- skeleton[year == maxYear]
  skeletonFinal[, year := NULL]
  skeletonFinal[, municip_code_end := NULL]
  skeletonOther <- skeleton[, c("municip_code", "year", "municip_code_end")]

  mappings <- unique(skeleton[!is.na(municip_code_end), c("municip_code", "municip_code_end")])
  setnames(mappings, c("municip_code_end", "realEnd"))

  continueWithMerging <- TRUE
  while (continueWithMerging) {
    skeletonOther <- merge(skeletonOther, mappings, all.x = T, by = "municip_code_end")
    skeletonOther[!is.na(realEnd), municip_code_end := realEnd]

    if (sum(!is.na(skeletonOther$realEnd)) == 0) {
      continueWithMerging <- FALSE
    }
    skeletonOther[, realEnd := NULL]
  }
  skeletonOther[, realEnd := municip_code]
  skeletonOther[!is.na(municip_code_end), realEnd := municip_code_end]
  setnames(skeletonFinal, "municip_code", "realEnd")

  skeletonFinal <- merge(skeletonOther, skeletonFinal, by = c("realEnd"))
  skeletonFinal[is.na(municip_code_end), municip_code_end := municip_code]
  setorder(skeletonFinal, realEnd, year)
  skeletonFinal[, realEnd := NULL]
  skeletonFinal[, year_start := NULL]

  setnames(skeletonFinal, "municip_code_end", "municip_code_current")
  setnames(skeletonFinal, "municip_code", "municip_code_original")

  return(invisible(skeletonFinal))
}
