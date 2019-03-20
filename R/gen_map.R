#' norway_map_counties
norway_map_counties <- function() {

  basefile <- options("shapefiles")$shapefiles
  if(is.null(basefile)){
    basefile <- tempdir()
  } else if(!fs::dir_exists(basefile)){
    basefile <- tempdir()
  }
  nor = GADMTools::gadm_sp_loadCountries("NOR", level=1, basefile=basefile)

  spdf_fortified <- broom::tidy(nor$spdf)
  setDT(spdf_fortified)
  mappings = list(
    'county02'='NOR_1',
    'county18'='NOR_2',
    'county05'='NOR_3',
    'county03'='NOR_4',
    'county11'='NOR_5',
    'county14'='NOR_6',
    'county50'=c('NOR_7','NOR_19'),
    'county08'='NOR_8',
    'county19'='NOR_9',
    'county10'='NOR_10',
    'county07'='NOR_11',
    'county01'='NOR_12',
    'county09'='NOR_13',
    'county06'='NOR_14',
    'county20'='NOR_15',
    'county04'='NOR_16',
    'county12'='NOR_17',
    'county15'='NOR_18'
  )
  spdf_fortified[stack(mappings), on = "id==values", id := ind]
  setnames(spdf_fortified, "id", "county_code")

  return(spdf_fortified)
}
