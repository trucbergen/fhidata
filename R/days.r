#' Dates of different days within isoweekyears
#'
#' @format
#' \describe{
#' \item{yrwk}{Isoweek-isoyear.}
#' \item{mon}{Date of Monday.}
#' \item{tue}{Date of Tuesday.}
#' \item{wed}{Date of Wednesday.}
#' \item{thu}{Date of Thursday.}
#' \item{fri}{Date of Friday.}
#' \item{sat}{Date of Saturday.}
#' \item{sun}{Date of Sunday.}
#' }
"days"

# Creates the norway_locations data.table
gen_days <- function() {
  . <- NULL
  yrwk <- NULL
  day <- NULL
  mon <- NULL
  tue <- NULL
  wed <- NULL
  thu <- NULL
  fri <- NULL
  sat <- NULL
  sun <- NULL

  days <- data.table(day = seq.Date(as.IDate("2000-01-01"), as.IDate("2030-01-01"), by = "days"))
  days[, yrwk := format.Date(day, format = "%G-%V")]
  days <- days[, .(mon = as.IDate(min(day))), by = .(yrwk)]
  days[, tue := mon + 1]
  days[, wed := mon + 2]
  days[, thu := mon + 3]
  days[, fri := mon + 4]
  days[, sat := mon + 5]
  days[, sun := mon + 6]
  days <- days[yrwk >= "2000-01"]

  return(days)
}
