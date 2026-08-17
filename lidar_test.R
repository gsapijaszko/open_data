# Krzydlina Wielka

# get the data

bb <- osmdata::getbb("Krzydlina Wielka", format_out = "sf_polygon") |>
  sf::st_buffer(dist = 1000)
dem <- rgugik::DEM_request(bb) |>
  subset(product == "PointCloud" & year == "2019")

krd <- "data/krzydlina"
if(!dir.exists(krd)){ dir.create(krd, recursive = TRUE) }

rgugik::tile_download(dem, outdir = paste0(krd, "/laz"),
                      method = "wget",
                      extra = "--no-check-certificate -c --progress=bar:force -T 120 -t 3")

convertLAZ <- function(lazfile, outdir = "", filter = "-keep_class 2 9") {
  if(!dir.exists({{outdir}})) { dir.create({{outdir}}, recursive = TRUE)}
  print(lazfile)
  .file <- stringi::stri_replace_all_regex({{lazfile}}, "^.*/", "")
  .outfile <- paste0({{outdir}}, "/", stringi::stri_replace_all_fixed(.file, "laz", "las"))
  if(!file.exists(.outfile)) {
    las <- lidR::readLAS(files = {{lazfile}}, filter = {{filter}})
    lidR::writeLAS(las, file = .outfile, index = TRUE)
  }
  else {
    message("Output file ", .outfile, " already exists, skipping conversion.")
  }
}

f <- list.files(paste0(krd, "/laz"), pattern = "*.laz", full.names = TRUE)
lapply(f, convertLAZ, outdir = paste0(krd, "/las"), filter = "")

# mamy pliki, teraz chyba DEM


library(lidR)

if(!file.exists("data/krzydlina/dtm/rasterize_terrain.vrt")) {
  
  library(future)
  options(parallelly.availableCores.methods = "mc.cores")
  options(mc.cores = 3)
  plan(multisession)
#  parallelly::availableWorkers()
  
  
  ctg <- readLAScatalog("data/krzydlina/las/")
  crs(ctg) <- "EPSG:2180"
  plot(ctg)
  ctg@output_options$drivers$SpatRaster$param$overwrite <- TRUE
  opt_output_files(ctg) <- "data/krzydlina/dtm/tin_{XLEFT}_{YBOTTOM}"
  opt_chunk_size(ctg) <- 700
  opt_chunk_buffer(ctg) <- 100
  opt_filter(ctg) <- "-keep_class 2 9" # "-keep_class 3 4 5"
  summary(ctg)
  rt <- rasterize_terrain(ctg, 0.5, algorithm = tin())
  #rt <- rasterize_terrain(ctg, 0.25, algorithm = kriging(k = 40))
} else {
  
  rt <- terra::vrt("data/krzydlina/dtm/rasterize_terrain.vrt")
}

terra::plot(rt)

bb <- osmdata::getbb("Krzydlina Wielka")

rgugik::egib_layers |>
  subset(TERYT == "0222")

if(!file.exists("data/wolowski/0222.gpkg")) {
  dir.create("data/wolowski")
  rgugik::egib_download(TERYT = "0222", outdir = "data/wolowski")
}

b <- sf::st_read(dsn = "data/wolowski/0222.gpkg",
            query = "SELECT * FROM 'ms:dzialki' WHERE NAZWA_OBREBU = 'Krzydlina Wielka';") |>
  dplyr::summarise() |>
  sf::st_transform(crs = "EPSG:2180")

plot(rt)  
terra::plot(b, add = TRUE)

ext <- terra::ext(c(326000, 327000, 383500, 384500))
rt <- terra::vrt("data/krzydlina/dtm/rasterize_terrain.vrt") |>
  terra::crop(ext)

tr <- terra::terrain(3*terra::aggregate(rt, 6), v = c("slope", "aspect"), unit = "radians")

ts1 <- terra::shade(tr$slope, tr$aspect, angle = 15, direction = 315)
ts2 <- terra::shade(tr$slope, tr$aspect, angle = 15, direction = 45)

tm_shape(ts1) +
  tm_raster(col.scale = tm_scale_continuous(values = get_brewer_pal("-Greys", plot=FALSE))) +
  tm_grid(n.x = 4, n.y = 4, lwd = 0.5, lines = FALSE) +
  tm_layout(legend.show = FALSE)

tm_shape(ts2) +
  tm_raster(col.scale = tm_scale_continuous(values = get_brewer_pal("-Greys", plot=FALSE))) +
  tm_grid(n.x = 4, n.y = 4, lwd = 0.5, lines = FALSE) +
  tm_layout(legend.show = FALSE)

ts <- terra::shade(tr$slope, tr$aspect, angle = 15, direction = seq(22.5, 360, 22.5), normalize = TRUE)
terra::plot(ts, col = gray(0:250/250), legend = FALSE)

tm_shape(ts) +
  tm_raster(col.scale = tm_scale_continuous(values = get_brewer_pal("-Greys", plot=FALSE))) +
  tm_facets(ncol = 4) +
  tm_layout(legend.show = FALSE) 

ts

pca <- terra::prcomp(ts, scale = TRUE)
pca
# https://lemuscanovas.github.io/synoptreg/reference/raster_pca.html

eigs <- pca$sdev^2
info_variance <- rbind(
  SD = sqrt(eigs),
  Proportion = eigs/sum(eigs),
  Cumulative = cumsum(eigs)/sum(eigs))

info_variance
sc <- terra::predict(ts, pca)
terra::plot(sc, col = gray(0:250/250), legend = FALSE)

sc[[1:3]] |>
  terra::stretch() |>
  terra::plotRGB()
sum(sc) |>
  terra::plot(col = gray(0:250/250), legend = FALSE)



# Grochowa ----------------------------------------------------------------------------------------


ext <- terra::ext(c(375800, 376600, 387500, 388300))
rt <- terra::vrt("/media/sapi/Movies 31/zaw_prusice/rasterize_terrain.vrt") |>
  terra::crop(ext)

tr <- terra::terrain(7*terra::aggregate(rt, 6), v = c("slope", "aspect"), unit = "radians")

ts1 <- terra::shade(tr$slope, tr$aspect, angle = 15, direction = 315)
ts2 <- terra::shade(tr$slope, tr$aspect, angle = 15, direction = 45)

library(tmap)
library(tmaptools)

tm_shape(ts1) +
  tm_raster(col.scale = tm_scale_continuous(values = get_brewer_pal("-Greys", plot=FALSE))) +
  tm_grid(n.x = 4, n.y = 4, lwd = 0.5, lines = FALSE) +
  tm_layout(legend.show = FALSE)

tm_shape(ts2) +
  tm_raster(col.scale = tm_scale_continuous(values = get_brewer_pal("-Greys", plot=FALSE))) +
  tm_grid(n.x = 4, n.y = 4, lwd = 0.5, lines = FALSE) +
  tm_layout(legend.show = FALSE)

ts <- terra::shade(tr$slope, tr$aspect, angle = 15, direction = seq(22.5, 360, 22.5), normalize = TRUE)
terra::plot(ts, col = gray(0:250/250), legend = FALSE)

tm_shape(ts) +
  tm_raster(col.scale = tm_scale_continuous(values = get_brewer_pal("-Greys", plot=FALSE))) +
  tm_facets(ncol = 4) +
  tm_layout(legend.show = FALSE) 

ts

pca <- terra::prcomp(ts, scale = TRUE)
pca
# https://lemuscanovas.github.io/synoptreg/reference/raster_pca.html

eigs <- pca$sdev^2
info_variance <- rbind(
  SD = sqrt(eigs),
  Proportion = eigs/sum(eigs),
  Cumulative = cumsum(eigs)/sum(eigs))

info_variance
sc <- terra::predict(ts, pca)
terra::plot(sc, col = gray(0:250/250), legend = FALSE)

sc[[c(2, 3, 1)]] |>
  terra::stretch() |>
  terra::plotRGB()
sum(sc) |>
  terra::plot(col = gray(0:250/250), legend = FALSE)
terra::writeRaster(rt, filename = "~/Downloads/rt.tif")
