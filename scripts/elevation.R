ele <- geodata::elevation_30s("United Kingdom", path = "data")
terra::plot(ele, ylim = c(49,61))

occ <- rgbif::occ_data(scientificName = "Calystegia pulchra", country = "GB", hasCoordinate = TRUE)

occ <- head(occ$data, 20) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = terra::crs(ele)) |>
  subset(select = c("key", "scientificName"))

occ |>
  sf::st_geometry() |>
  plot(add = TRUE)

# from raster with terra::extract()

occ |>
  dplyr::mutate(elevation = terra::extract(ele, terra::vect(geometry))$GBR_elv_msk) |>
  head(5)

# from elevatr

occ |>
  dplyr::mutate(ele = elevatr::get_elev_point(geometry, src = "aws"))

## bardziej udany raster

gb <- geodata::gadm("United Kingdom", path = "data") |>
  terra::project(terra::crs(ele))

bbox <- sf::st_bbox(gb)

# getting raster data
for (x in floor(bbox[1]):ceiling(bbox[3])) {
  for (y in floor(bbox[2]):ceiling(bbox[4])) {
    geodata::elevation_3s(lon = x, lat = y, path = "data")
  }
}

for (x in floor(bbox[1]):ceiling(bbox[3])) {
  for (y in floor(bbox[2]):60) {
    geodata::elevation_3s(lon = x, lat = y, path = "data")
  }
}

library(terra)
# merging rasters
if(!file.exists("data/merged.tif")) {
  r <- list.files(path = "data", pattern = "srtm_.+.tif", full.names = TRUE)
  r <- lapply(r, rast)
  r <- do.call(merge, r)
  r <- mask(r, gb)
  crs(r) <- crs(ele)
  writeRaster(r, filename = "data/merged.tif", overwrite = TRUE)
  rm("r")
  gc()
}

ele_3s <- rast("data/merged.tif")
terra::plot(ele_3s, ylim = c(49,61))

occ |>
  dplyr::mutate(elevation = terra::extract(ele, terra::vect(geometry))$GBR_elv_msk) |>
  dplyr::mutate(ele3s = terra::extract(ele_3s, terra::vect(geometry))$srtm_35_01)

library(CopernicusDEM)

polygon <- sf::st_as_sf(gb)

sf::sf_use_s2(use_s2 = FALSE)

if(!dir.exists("data/copernicus")) { dir.create("data/copernicus")}

aoi_geom_save_tif_matches(
  sf_or_file = polygon,
  dir_save_tifs = "data/copernicus",
  resolution = 30,
  crs_value = 4326,
  threads = parallel::detectCores(),
  verbose = TRUE
)

create_VRT_from_dir(
  dir_tifs = "data/copernicus",
  output_path_VRT = "data/copernicus/vrt_mosaic.vrt",
  verbose = TRUE
)
cop <- list.files("data/copernicus", pattern = '.tif', full.names = TRUE)

r <- list.files(path = "data", pattern = "srtm_.+.tif", full.names = TRUE)
vrt <- terra::vrt(r, "data/vrt.vrt", overwrite = TRUE) |>
  crop(gb) |>
  mask(gb)
terra::plot(vrt)
vrt



occ |>
  dplyr::mutate(elevation = terra::extract(ele, terra::vect(geometry))$GBR_elv_msk) |>
  dplyr::mutate(ele3s = terra::extract(ele_3s, terra::vect(geometry))$srtm_35_01) |>
  dplyr::mutate(cope = terra::extract(vrt, terra::vect(geometry))$vrt) |>
  dplyr::mutate(ele_aws = elevatr::get_elev_point(geometry, src = "aws")$elevation)



#### Tandem
if(!dir.exists("data/tandem")) { dir.create("data/tandem")}

TanDEM::download_TanDEM(
  lon = c(bbox[1], bbox[3]),
  lat = c(bbox[2], bbox[4]),
  usr = "grzegorz.sapijaszko@gmail.com",
  srv = "geoservice.dlr",
  dstdir = "data/tandem"
)

r <- list.files(path = "data/tandem/", pattern = "TDM1_.+.tif", full.names = TRUE)
vrt <- terra::vrt(r, "data/tandem/vrt.vrt", overwrite = TRUE) 

terra::NAflag(vrt) <- -32767

vrt <- vrt |>
  terra::mask(gb) |>
  terra::crop(gb)

terra::plot(vrt)

#|>

terra::plot(vrt)

