
# do przejrzenia ----------------------------------------------------------------------------------------

https://github.com/ryanstraight/resources

# -----------------------------------------------------------------------------------------------------------------


osmextract::oe_get("Leipzig", 
                   layer = "multipolygons",
                   download_directory = "data")

sf::st_layers("data/bbbike_Leipzig.gpkg")
a <- osmextract::oe_read("data/bbbike_Leipzig.gpkg", 
                         query = "SELECT osm_id, other_tags, geometry ,
                         hstore_get_value(other_tags, 'postal_code') p_code,
                         hstore_get_value(other_tags, 'postal_code_level') p_level
                    FROM multipolygons WHERE boundary = 'postal_code'")
a
a |>
  subset(select = other_tags) |>
  tidyr::separate_rows(other_tags, sep = ",") |>
  tidyr::separate(other_tags, into = c("key", "value"), sep = "=>") |>
  dplyr::mutate_at(c("key", "value"), stringr::str_remove_all, '"') |>
  tidyr::pivot_wider(names_from = "key", values_from = "value")

osmextract::oe_get_keys(a, values = TRUE)

a |>
  tmap::tm_shape() +
  tmap::tm_polygons()
# granice administracyjne ---------------------------------------------------------------------

powiat <- "bolesławiecki"

teryt_county <- rgugik::county_names |>
  subset(NAME == powiat ) |>
  subset(select = "TERYT") |>
  as.character()

gminy <- rgugik::commune_names |>
  subset(grepl(paste0("^", teryt_county), TERYT))

i = 2

gugik_gr <- rgugik::borders_get(TERYT = gminy[i,"TERYT"])
terra::plot(gugik_gr)
gugik_gr

options(timeout = 10000)

bb <- osmdata::getbb(paste("gmina", gminy[i, "NAME"], ", powiat", powiat))

gr_adm <- osmdata::opq(bb, timeout = 60) |>
  osmdata::add_osm_features(
    features = c(
      "\"boundary\"= \"administrative\""
    )
  ) |> osmdata::osmdata_sf() |>
  osmdata::unique_osmdata()

osm_gr <- gr_adm$osm_multipolygons |>
  subset(admin_level == "7" & `teryt:terc` == as.character(gminy[i,"TERYT"])) |>
  subset(select = c("name", "name:prefix", "teryt:terc")) |>
  sf::st_transform(crs = sf::st_crs(gugik_gr))

sf::st_area(gugik_gr)/10^6
sf::st_area(osm_gr)/10^6
terra::plot(osm_gr$geometry, axes = TRUE)

bufor <- gugik_gr |>
  sf::st_cast(to = "MULTILINESTRING") |>
  sf::st_buffer(dist = 10)

bufor |>
  subset(select = "geom") |>
  terra::plot(col = "blue", add = TRUE)

a <- osm_gr |>
  sf::st_cast(to = "MULTILINESTRING")

di <- sf::st_difference(a, 
                  sf::st_intersection(a, bufor)
)

if(nrow(di) > 0) {
  terra::plot(sf::st_buffer(di$geometry, 100), col = "red", add =TRUE)  
} else {
  message("All good, nothing to plot.")
}



gr_wsi <- gr_adm$osm_multipolygons |>
  subset(admin_level == "8") |>
  sf::st_join(gr_gminy, join = sf::st_within, left = FALSE) |>
  dplyr::arrange("name.x") |>
  subset(select = c("name.x")) |>
  stats::setNames(c("name", "geometry"))


# gisco ---------------------------------------------------------------------------------------

df_latlon <- tibble::tibble("lats" =  c(52.5200, 48.8567, 47.1770),
                    "longs" = c(13.4050,  2.2885,  8.2539)) # DE-Berlin, FR-Paris, CH-Römerswil LU

df_latlon |>
  sf::st_as_sf(coords = c("longs", "lats"), crs = 4326) |>
  sf::st_join(giscoR::gisco_nuts)  |>
  subset(LEVL_CODE == 3)

giscoR::gisco_countries |>
  subset(CNTR_ID == "CH")


# metar ---------------------------------------------------------------------------------------

worldmet::getMeta(site = "Wroclaw")
met_epwr <- worldmet::importNOAA(code = "124280-99999", year = 2003)
a <- pmetar::metar_get_historical("EPWR")
pmetar::metar_cloud_coverage(a[1])
pmetar::metar_pressure(a[2])


# sp polygon ----------------------------------------------------------------------------------

UKJ32 <- sp::Polygon(cbind(c(-1.477037449999955, -1.366895449999959, -1.365159449999965, -1.477037449999955),
                           c(50.923958250000027, 50.94686525000003, 50.880069750000018, 50.923958250000027))) |>
  list() |>
  sp::Polygons(ID="UKJ32 - Southampton")

a <- tibble::tibble(lon = c(-1.4, 10), lat = c(50.9, 10))

sp::point.in.polygon(a$lon, a$lat, UKJ32@Polygons[[1]]@coords[,1], UKJ32@Polygons[[1]]@coords[,2])
