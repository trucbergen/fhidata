# Creates the norway_locations, norway_municip_merging, and norway_population data.table
# @param base_loc Folder where data will be saved
gen_data_all <- function(base_loc) {
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(base_loc)

  days <- gen_days()
  save(days, file = file.path(base_loc, "days.rda"), compress = "bzip2")

  norway_municip_merging <- gen_norway_municip_merging()
  save(norway_municip_merging, file = file.path(base_loc, "norway_municip_merging.rda"), compress = "bzip2")

  norway_locations_current <- gen_norway_locations(is_current_municips = TRUE)
  save(norway_locations_current, file = file.path(base_loc, "norway_locations_current.rda"), compress = "bzip2")
  norway_locations_original <- gen_norway_locations(is_current_municips = FALSE)
  save(norway_locations_original, file = file.path(base_loc, "norway_locations_original.rda"), compress = "bzip2")

  norway_locations_long_current <- gen_norway_locations_long(is_current_municips = TRUE)
  save(norway_locations_long_current, file = file.path(base_loc, "norway_locations_long_current.rda"), compress = "bzip2")
  norway_locations_long_original <- gen_norway_locations_long(is_current_municips = FALSE)
  save(norway_locations_long_original, file = file.path(base_loc, "norway_locations_long_original.rda"), compress = "bzip2")

  norway_population_current <- gen_norway_population(is_current_municips = TRUE)
  save(norway_population_current, file = file.path(base_loc, "norway_population_current.rda"), compress = "xz")
  norway_population_original <- gen_norway_population(is_current_municips = FALSE)
  save(norway_population_original, file = file.path(base_loc, "norway_population_original.rda"), compress = "xz")

  countries_nb_to_en <- gen_countries_nb_to_en()
  save(countries_nb_to_en, file = file.path(base_loc, "countries_nb_to_en.rda"))

  norway_childhood_vax <- gen_norway_childhood_vax(norway_locations_long_current)
  save(norway_childhood_vax, file = file.path(base_loc, "norway_childhood_vax.rda"), compress = "xz")

  # norway_map_counties <- gen_norway_map_counties()
  # save(norway_map_counties, file=file.path("/git","/fhidata","data","norway_map_counties.rda"), compress = "xz")
  # norway_map_municips <-  gen_norway_map_municips()
  # save(norway_map_municips, file=file.path("/git","/fhidata","data","norway_map_municips.rda"), compress = "xz")

  # load(file.path(base_loc,"norway_locations_current.rda"))
  # load(file.path(base_loc,"norway_map_municips.rda"))
  # senorge <- gen_senorge(norway_locations_current, norway_map_municips)
  # save(senorge, file=file.path(base_loc,"senorge.rda"), compress = "xz")
}
