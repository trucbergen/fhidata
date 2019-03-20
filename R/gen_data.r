#' Creates the norway_locations data.table
#' @param save_loc Location to save file to
gen_norway_locations <- function(save_loc = file.path("inst", "createddata")) {

  # variables used by data.table
  is_current <- NULL
  year_end <- NULL
  #

  norway_locations <- readxl::read_excel(
    system.file("extdata", "norway_locations.xlsx", package = "fhidata")
  )
  setDT(norway_locations)

  norway_locations[, is_current := is.na(year_end)]

  norway_locations <- unique(norway_locations[
    ,
    c("is_current", "municip_code", "municip_name", "county_code", "county_name")
  ])

  if (dir.exists(save_loc)) {
    # Current municipalities
    try(saveRDS(
      norway_locations[is_current == TRUE, -"is_current"],
      file.path(save_loc, "norway_locations_current.rds")
    ), TRUE)

    # All/original municipalities
    try(saveRDS(
      norway_locations[, -"is_current"],
      file.path(save_loc, "norway_locations_original.rds")
    ), TRUE)
  }

  return(invisible(norway_locations))
}

#' Creates the norway_locations data.table
#' @param is_current_municips Do you want the population file to contain the current municipalities (i.e. after municipal merging) or the original municipalities that existed in that year?
#' @param save_loc Location to save file to
gen_norway_locations_long <- function(is_current_municips = TRUE,
                                      save_loc = file.path("inst", "createddata")) {
  if (is_current_municips) {
    final_name <- "norway_locations_long_current"
    mid_name <- "norway_locations_current"
  } else {
    final_name <- "norway_locations_long_original"
    mid_name <- "norway_locations_original"
  }

  a1 <- data.table(location_code = "norway", location_name = "Norway")
  a2 <- data.table(location_code = "norge", location_name = "Norge")
  b <- get_data(mid_name)[, c("municip_code", "municip_name")]
  c <- get_data(mid_name)[, c("county_code", "county_name")]
  setnames(b, c("location_code", "location_name"))
  setnames(c, c("location_code", "location_name"))

  retval <- unique(rbind(a1, a2, b, c))

  try(saveRDS(retval, file.path(save_loc, glue::glue("{final_name}.rds"))), TRUE)

  return(invisible(retval))
}

#' Creates the norway_municip_merging (kommunesammenslaaing) data.table
#'
#' Last updated 2019-03-14
#'
#' @param save_loc Location to save file to
#' @import data.table
gen_norway_municip_merging <- function(save_loc = file.path("inst", "createddata")) {
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

  if (dir.exists(save_loc)) {
    try(saveRDS(skeletonFinal, file.path(save_loc, "norway_municip_merging.rds")), TRUE)
  }

  return(invisible(skeletonFinal))
}

#' Creates the population dataset
#' https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/
#' @param is_current_municips Do you want the population file to contain the current municipalities (i.e. after municipal merging) or the original municipalities that existed in that year?
#' @param save_loc Location to save file to
#' @import data.table
gen_norway_population <- function(is_current_municips = TRUE,
                                  save_loc = file.path("inst", "createddata")) {

  # variables used in data.table functions in this function
  . <- NULL
  value <- NULL
  age <- NULL
  Var2 <- NULL
  agecont <- NULL
  pop <- NULL
  municip_code <- NULL
  municip_code_current <- NULL
  year_end <- NULL
  level <- NULL
  region <- NULL
  variable <- NULL
  agenum <- NULL
  imputed <- NULL
  county_code <- NULL
  municip_code_end <- NULL
  # end

  popFiles <- c(
    "Personer2005-2009.csv",
    "Personer2010-2014.csv",
    "Personer2015-2018.csv",
    "Personer2019.csv"
  )
  pop <- vector("list", length = length(popFiles))
  for (i in seq_along(pop)) {
    pop[[i]] <- fread(system.file("extdata", popFiles[i], package = "fhidata"), encoding = "UTF-8")
    pop[[i]] <- melt.data.table(pop[[i]], id.vars = c("region", "age"))
  }
  pop <- rbindlist(pop)
  pop[, municip_code := sprintf("municip%s", stringr::str_extract(region, "^[0-9][0-9][0-9][0-9]"))]
  pop[, year := as.numeric(stringr::str_extract(variable, "[0-9][0-9][0-9][0-9]$"))]
  pop[, agenum := as.numeric(stringr::str_extract(age, "^[0-9]*"))]

  pop[, age := NULL]
  setnames(pop, "agenum", "age")

  pop <- pop[, .(
    pop = sum(value)
  ), keyby = .(
    municip_code, age, year
  )]

  # Fixing broken parts in the population data
  # part 1
  pop2 <- pop[municip_code == "municip0710" & year <= 2017]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip0706"]
  pop2[, pop := round(pop / 3)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip_code == "municip0710" & year <= 2017]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip0719"]
  pop2[, pop := round(pop / 3)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip_code == "municip0710" & year <= 2017]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip0720"]
  pop2[, pop := round(pop / 3)]
  pop <- rbind(pop, pop2)

  # part 2
  pop2 <- pop[municip_code == "municip1756" & year <= 2012]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1723"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip_code == "municip1756" & year <= 2012]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1729"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  # part 3
  pop2 <- pop[municip_code == "municip5046" & year <= 2018]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1901"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip_code == "municip1756" & year <= 2018]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1915"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  # part 4
  pop2 <- pop[municip_code == "municip1505" & year <= 2008]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1503"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip_code == "municip1505" & year <= 2008]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1556"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)
  pop[, imputed := FALSE]

  missingYears <- max(pop$year):(lubridate::year(lubridate::today()) + 2)
  if (length(missingYears) > 1) {
    copiedYears <- vector("list", length = length(missingYears) - 1)
    for (i in seq_along(copiedYears)) {
      copiedYears[[i]] <- pop[year == missingYears[1]]
      copiedYears[[i]][, year := year + i]
    }
    copiedYears <- rbindlist(copiedYears)
    copiedYears[, imputed := TRUE]
    pop <- rbind(pop, copiedYears)
  }

  if (is_current_municips) {
    norway_merging <- get_data("norway_municip_merging")
    pop <- merge(
      pop,
      norway_merging[, c("year", "municip_code_current", "municip_code_original")],
      by.x = c("municip_code", "year"),
      by.y = c("municip_code_original", "year")
    )
    pop <- pop[, .(pop = sum(pop)),
      keyby = .(
        year,
        municip_code = municip_code_current,
        age,
        imputed
      )
    ]

    file_name <- "norway_population_current.rds"
  } else {
    file_name <- "norway_population_original.rds"
  }
  pop[, level := "Municipality"]

  counties <- merge(
    pop,
    get_data("norway_locations", is_current_municips = FALSE)[, c("municip_code", "county_code")],
    by = "municip_code"
  )

  check_ref_to_new(
    xref = unique(pop$municip_code),
    xnew = unique(counties$municip_code)
  )

  if (nrow(counties) != nrow(pop)) {
    stop("nrow(counties) != nrow(pop)")
  }

  counties <- counties[, .(
    pop = sum(pop)
  ), keyby = .(
    year,
    municip_code = county_code,
    age,
    imputed
  )]
  counties[, level := "County"]

  norway <- pop[, .(
    pop = sum(pop)
  ), keyby = .(
    year,
    age,
    imputed
  )]
  norway[, municip_code := "norway"]
  norway[, level := "National"]

  pop <- rbind(norway, counties, pop)

  final_order <- c("year", "municip_code", "level", "age", "pop", "imputed")
  setorderv(pop, final_order)
  setcolorder(pop, final_order)
  setnames(pop, "municip_code", "location_code")

  if (dir.exists(save_loc)) {
    try(saveRDS(pop, file.path(save_loc, file_name)), TRUE)
  }

  return(invisible(pop))
}

#' Get data.table sets from this package
#' @param name The name of the dataset. This interacts with `is_current_municips`.
#' If `is_current_municips` is `TRUE`/`FALSE``, then the following options are allowed:
#' - norway_population: Population data
#' - norway_locations: One row for each municipality containing columns 'municip_code', 'municip_name', 'county_code', and 'county_name'
#' - norway_locations_long: One column for 'code', one column for 'name'
#' - norway_municip_merging: Used to convert original municipalities into current municipalities
#' - norway_map_counties: Long/lats for Norwegian county borders
#' - norway_map_municips: Long/lats for Norwegian municipality borders
#'
#' Some of these datasets (`norway_population`, `norway_locations`, and `norway_locations_long`) have the option to display the data using municipality codes as they were originally or as they are today (after the merging of municipalities).
#'
#' If `is_current_municips` is `NULL``, then the following options are allowed:
#' - norway_population_current
#' - norway_population_original
#' - norway_locations_current
#' - norway_locations_original
#' - norway_locations_long_current
#' - norway_locations_long_original
#' - norway_municip_merging
#' - norway_map_counties
#' - norway_map_municips
#' @param is_current_municips If this is `NULL`, then `name` is used exclusively. If this is ``, then `name=name_current`, and if this is `` then `name=name_original`. This lets you work more programatically.
#' @param ... Not used currently
#' @examples
#' get_data("norway_population", is_current_municips = TRUE)
#' get_data("norway_population_current")
#' @md
#' @export
get_data <- function(name, is_current_municips = NULL, ...) {
  if (is.null(is_current_municips)) {
    working_name <- name
  } else {
    tag <- ifelse(is_current_municips, "current", "original")
    working_name <- glue::glue("{name}_{tag}")
  }

  valid_names <- c(
    "norway_population_current",
    "norway_population_original",
    "norway_locations_current",
    "norway_locations_original",
    "norway_locations_long_current",
    "norway_locations_long_original",
    "norway_municip_merging",
    "norway_map_counties",
    "norway_map_municips"
  )

  valid_names_with_ticks <- glue::glue("\u2713 {valid_names}")
  if (!working_name %in% valid_names) {
    stop(glue::glue("\n\n\u2716 '{name}' -> '{working_name}' not in: \n{glue::collapse(valid_names_with_ticks,sep='\n')}"))
  }

  if (is.null(data_storage[[working_name]])) {
    data_storage[[working_name]] <- readRDS(system.file(
      "createddata",
      glue::glue("{working_name}.rds"),
      package = "fhidata"
    ))
  }

  return(data_storage[[working_name]])
}

#' Creates the norway_locations, norway_municip_merging, and norway_population data.table
#' @param save_loc Location of data
gen_data <- function(save_loc = file.path("inst", "createddata")) {
  gen_norway_municip_merging(save_loc)

  gen_norway_locations(save_loc)

  gen_norway_locations_long(is_current_municips = TRUE, save_loc = save_loc)
  gen_norway_locations_long(is_current_municips = FALSE, save_loc = save_loc)

  gen_norway_population(is_current_municips = TRUE, save_loc)
  gen_norway_population(is_current_municips = FALSE, save_loc)
}
