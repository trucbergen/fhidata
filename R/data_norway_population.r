#' Population in Norway.
#'
#' We conveniently package population data taken from Statistics Norway.
#' This data is licensed under the Norwegian Licence for
#' Open Government Data (NLOD) 2.0.
#'
#' This dataset contains county/municipality level population data
#' for every age (0 to 105 years old) from 2006, and national data from 1846.
#' The counties and municipalities are updated for the current redistricting.
#'
#' @format
#' \describe{
#' \item{year}{Year.}
#' \item{location_code}{The location code.}
#' \item{level}{cational/county/municipality.}
#' \item{age}{1 year ages from 0 to 105.}
#' \item{pop}{Number of people.}
#' \item{imputed}{FALSE if real data. TRUE if it is the last real data point carried forward.}
#' }
#' @source \url{https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/}
"norway_population_current"

#' Population in Norway.
#'
#' We conveniently package population data taken from Statistics Norway.
#' This data is licensed under the Norwegian Licence for
#' Open Government Data (NLOD) 2.0.
#'
#' This dataset contains national/county/municipality level population data
#' for every age (0 to 105 years old) from 2006. The counties and
#' municipalities are not updated for the current redistricting.
#'
#' @format
#' \describe{
#' \item{year}{Year.}
#' \item{location_code}{The location code.}
#' \item{level}{National/County/Municipality.}
#' \item{age}{1 year ages from 0 to 105.}
#' \item{pop}{Number of people.}
#' \item{imputed}{FALSE if real data. TRUE if it is the last real data point carried forward.}
#' }
#' @source \url{https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/}
"norway_population_original"


# Creates the population dataset
# https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/
#' @import data.table
gen_norway_population <- function(is_current_municips = TRUE) {

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
  sex <- NULL
  contents <- NULL
  x <- NULL
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
    norway_merging <- gen_norway_municip_merging()
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
  pop[, level := "municipality"]

  counties <- merge(
    pop,
    gen_norway_locations(is_current_municips = FALSE)[, c("municip_code", "county_code")],
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
  counties[, level := "county"]

  # pull in full norwegian data
  norway <- data.table(utils::read.csv(url("https://data.ssb.no/api/v0/dataset/59322.csv?lang=en"), stringsAsFactors = FALSE))
  norway <- norway[sex == "0 Both sexes"]
  norway[, sex := NULL]
  norway[, contents := NULL]
  norway[, x := as.numeric(stringr::str_extract(age, "^[0-9][0-9][0-9]"))]
  norway[, age := NULL]
  setnames(norway, c("year", "pop", "age"))
  norway[, level := "national"]
  norway[, municip_code := "norway"]
  norway[, imputed := FALSE]
  for (i in missingYears) {
    popx <- norway[year == max(year)]
    popx[, year := i]
    popx[, imputed := TRUE]
    norway <- rbind(norway, popx)
  }

  pop <- rbind(norway, counties, pop)

  final_order <- c("year", "municip_code", "level", "age", "pop", "imputed")
  setorderv(pop, final_order)
  setcolorder(pop, final_order)
  setnames(pop, "municip_code", "location_code")

  return(invisible(pop))
}
