library(lidR)
library(terra)
l <- lidR::readLAS("../drogi_lidar/data/76503_1213480_M-33-34-B-d-1-4-2-2.las")
crs(l) <- "EPSG:2180"
r <- rasterize_terrain(l, res = 0.2)
terra::plot(r)
ext <- terra::ext(c(355600, 356000, 375200, 375600))
s <- terra::crop(r, ext)
terra::plot(s)
# agg <- terra::aggregate(s, fact = 50)
# terra::plot(agg)
# r2 <- resample(agg, s, "average", names="average") ## !!!!!! tego używać
# r2 <- resample(agg, s, "bilinear", names="bilinear")

r2 <- terra::focal(s, w = 25, fun = "mean")
r2 |>
  terra::plot()
(s-r2) |>
  terra::plot()
v <- (s-r2) |>
  terra::as.polygons(round = TRUE, digits = 1)
v
terra::subset(v, v$Z == 0L) |>
  terra::plot(add = TRUE)
v
v0 <- terra::subset(v, v$Z == 0L) |>
  terra::as.lines()

terra::plot(v0)

s0 <- terra::rasterize(v0, s)
terra::plot(s0)
s1 <- (s * s0)
terra::plot(s1)
s2 <- terra::focal(s1, w = 75, fun = "mean", na.policy = "only") 
s2 |>
  terra::plot()

(s - s2) |>
  terra::plot()

d <- terra::as.data.frame(agg, xy = TRUE)
lattice::wireframe(Z ~ x * y, data = d,
                   scales = list(arrows = FALSE))

terra::plot(agg)
terra::points(terra::as.points(agg), col = "white")
terra::plot(s)
ss <- s
ss[] <- NA


library(terra)
set.seed(1)
x <- rast(nrows=100, ncols=100, xmin=0, xmax=25, ymin=0, ymax=25)
values(x) <- sample(ncell(x))
x
terra::plot(x)
a <- aggregate(x, 10, mean)
a
terra::plot(a)

r1 <- resample(a, x, "bilinear", names="bilinear")
r2 <- resample(a, x, "average", names="average") ## !!!!!! tego używać

terra::plot(x)
terra::plot(a)
terra::plot(r1)
terra::plot(r2)

x
a
r1
r2
a
