b <- osmdata::getbb("gmina Oborniki Śląskie")

a <- osmdata::opq(b, timeout = 60*20) |>
  osmdata::add_osm_feature(key = "boundary", value = "administrative") |>
  osmdata::add_osm_feature(key = "admin_level", value = "7") |>
  osmdata::osmdata_sf() |>
  osmdata::unname_osmdata_sf() |>
  osmdata::unique_osmdata()

ob <- a$osm_multipolygons[a$osm_multipolygons$name == "gmina Oborniki Śląskie",] |>
  subset(select = c(name, geometry))

ob |>
#  cartography::getPencilLayer(buffer = 0.1, size = 1200) |>
  cartography::hatchedLayer(pattern = "zigzag",
                            density = 4,
                            mode = "sfc") |>
  sf::st_transform(crs = "EPSG:2180") |>
  terra::plot(lwd = 0.3, col = "blue")



tm <- pmetar::ourairports |>
  subset(iso_country == "PL", select =c(type, name, longitude_deg, latitude_deg)) |>
  sf::st_as_sf(coords = c("longitude_deg", "latitude_deg"), crs = "EPSG:4326") |>
  tmap::tm_shape() +
  tmap::tm_dots(size = 0.5)

tmap::tmap_leaflet(tm, show = TRUE)
  

# -------------------------------------------------------------------------------------------------------


b <- osmdata::getbb("Uganda")
a <- osmdata::opq(b, timeout = 60*20) |>
  osmdata::add_osm_feature(key = "amenity", value = "doctors") |>
  osmdata::osmdata_sf()

poi <- a$osm_points[1:10, "amenity"=='doctors', ] |>
  sf::st_coordinates()

tmaptools::rev_geocode_OSM(x=poi[, "X"], y = poi[, "Y"])

poi[1,]

a$osm_points |>
  subset(amenity == "doctors")

tidygeocoder::reverse_geo(long = poi[, "X"], lat = poi[, "Y"], method = "osm")



# -----------------------------------------------------------------------------------------------------------------
#' https://stackoverflow.com/questions/75791459/r-calculating-the-distance-between-two-geographical-points/75859028#75859028
#' https://github.com/Project-OSRM/osrm-backend

library(osrm)
options("osrm.server" = "http://127.0.0.1:5000/")
options("osrm.profile" = "car") # Easiest to set this here as well

osrmRoute(src = c(-78.70202146, 46.31710423), dst = c( -77.78705358, 45.01146376), overview = "full") |>
  sf::st_write(dsn = "/home/sapi/Downloads/dupa2.gpkg", append = FALSE)

osrmIsochrone(loc = c(-78.70202146, 46.31710423), breaks = c(10, 20, 30, 60), res = 40) 


b <- osmdata::getbb("gmina Prusice")
prusice <- osmdata::opq(bbox = b, timeout = 60*20) |>
  osmdata::add_osm_feature(key = "boundary", value = "administrative") |>
  osmdata::osmdata_sf()

prusice_mun <- prusice$osm_multipolygons |>
  subset(admin_level == 7 & name == "gmina Prusice") |>
  sf::st_transform(crs = "EPSG:2180")

willages <- prusice$osm_multipolygons |>
  subset(admin_level == 8) |>
  sf::st_transform(crs = "EPSG:2180")

tmap::tm_shape(willages) +
  tmap::tm_grid(n.x = 5, n.y = 5, lwd = 0.5) +
  tmap::tm_polygons(fill = "gray95") +
  tmap::tm_shape(prusice_mun) +
  tmap::tm_lines(lwd = 3, col = "blue")

willages <- prusice$osm_multipolygons |>
  subset(admin_level == 8) |>
  sf::st_transform(crs = "EPSG:2180") |>
  sf::st_filter(prusice_mun, .predicate=sf::st_within)

tmap::tm_shape(willages) +
  tmap::tm_grid(n.x = 5, n.y = 5, lwd = 0.5) +
  tmap::tm_polygons(fill = "gray95") +
  tmap::tm_text(text = "name", size = 0.7) +
  tmap::tm_shape(prusice_mun) +
  tmap::tm_lines(lwd = 3, col = "blue") 

prusice_highways <- osmdata::opq(b, timeout = 60) |> 
  osmdata::add_osm_feature(key = "highway") |>
  osmdata::osmdata_sf()

highways <- prusice_highways$osm_lines |>
  subset(!is.na(highway)) |>
  sf::st_transform(crs = "EPSG:2180") |>
  sf::st_intersection(prusice_mun)

h <- highways |>
  #  subset(highway %in% c("trunk", "trunk_link", "secondary", "tertiary", "residential", "track"))
  subset(highway %in% c("trunk", "trunk_link", "secondary", "tertiary", "residential"))

linewidths <- data.frame(
  highway = c("trunk", "trunk_link", "secondary", "tertiary", "residential", "track"),
  lwd = c(2, 1.8, 1.6, 1, 0.8, 0.5)
)

h <- h |>
  dplyr::left_join(linewidths, by = "highway")

tm <- tmap::tm_shape(prusice_mun) +
  tmap::tm_borders() +
  tmap::tm_shape(h) +
  tmap::tm_lines(
    col = "highway",
    lwd = "lwd",
    col.legend = tmap::tm_legend(
      position = c("left", "bottom"),
      bg.color = "white"
    ),
    lwd.legend = tmap::tm_legend(
      show = FALSE
    )
  )

fire_stations <- osmdata::opq(b, timeout = 60) |> 
  osmdata::add_osm_feature(key = "amenity", value = "fire_station") |>
  osmdata::osmdata_sf()

osp <- fire_stations$osm_polygons[, "amenity" == "fire_station"] |>
  sf::st_centroid()

osp <- fire_stations$osm_points[, "amenity" == "fire_station"] |>
  sf::st_centroid() |>
  rbind(osp)

osp <- osp |>
  sf::st_transform(crs = sf::st_crs(prusice_mun)) |>
  sf::st_filter(prusice_mun, .predicate=sf::st_within)

osp

tm +
  tmap::tm_shape(osp) +
  tmap::tm_dots()

library(osrm)
options("osrm.server" = "http://127.0.0.1:5000/")
options("osrm.profile" = "car") # Easiest to set this here as well

osrmIsochrone(loc = c(-78.70202146, 46.31710423), breaks = c(10, 20, 30, 60), res = 40) 

locations <- osp |>
  sf::st_transform(crs = "EPSG:4326") 

locations

c <- locations
c <- c[0,]

for (i in 1:nrow(locations)) {
  a <- osrmIsochrone(loc = locations[i, ], breaks = c(0, 2, 4, 6, 8, 10), res = 40)
  a$num <- i
  c <- a |>
    rbind(c)
}

tm +
  tmap::tm_shape(osp) +
  tmap::tm_dots(size = 0.5) +
  tmap::tm_shape(c) + 
  tmap::tm_polygons("isomax", fill_alpha = 0.3,
                    col = "white", col_alpha = 0,
                    fill.legend = tmap::tm_legend(
                      position = c("left", "top"),
                      bg.color = "white"
                    ),
  )

options("osrm.server" = "http://127.0.0.1:5000/")
options("osrm.profile" = "car") # Easiest to set this here as well

osrmRoute(src = c(-78.70202146, 46.31710423), dst = c( -77.78705358, 45.01146376), overview = "full") |>
  sf::st_write(dsn = "/home/sapi/Downloads/a.gpkg", append = FALSE)


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
