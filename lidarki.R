library(lidR)

f <- "data/laz/NumDaneWys/DanePomiaroweLAZ/73026/73026_916209_M-33-34-B-d-1-1-2.laz"
las <- lidR::readLAS(f)
crs(las) <- "EPSG:2180"
r <- rasterize_terrain(las, res = 0.25)


library(tmaptools)
tmaptools::palette_explorer()
tmaptools::get_brewer_pal("GnBu")
pal <- rev(tmaptools::get_brewer_pal("GnBu", 255, plot = FALSE))

terra::plot(r, col = pal,
            xlim = c(353370, 353570),
            ylim = c(377350, 377550))

p <- sf::st_point(x = c(353470, 377460)) |>
  sf::st_sfc(crs = "EPSG:2180")

terra::points(p, pch = 18)

p1 <- p - c(70, 0)
p2 <- p + c(70, 0)
l1 <- c(p1, p2) |>
  sf::st_coordinates() |>
  sf::st_linestring()

terra::lines(l1)
terra::points(p1+c(0,5), pch = "A")
terra::points(p2+c(0,5), pch = "B")
x <- lidR::clip_transect(las, sf::st_coordinates(p1), sf::st_coordinates(p2), width = 6, xz = TRUE)  
x <- lidR::filter_poi(x, Classification != 12L)

class_cols <- c(
  "0" = "black",       # never classified
  "1" = "gray90",      # unassigned
  "2" = "gray50",      # ground
  "3" = "lightgreen",  # low vegetation
  "4" = "green",       # medium vegetation
  "5" = "darkgreen",   # high vegetation
  "6" = "brown",       # building
  "7" = "gray90",      # noise
  "8" = "gray90",      # reserved
  "9" = "blue",        # water
  "10" = "gray33",     # rail
  "11" = "gray33",     # road surface
  "12" = "black")      # reserved

library(ggplot2)

y <- lidR::filter_poi(x, Classification == 2L)
ggplot(y@data, aes(X, Z, color = Z)) +
  geom_point(size = 0.5) +
  coord_fixed(ratio = 5) +
  theme_minimal() +
  scale_color_gradientn(colours = pal)

plot(y@data$X, y@data$Y, xlim =c(0,5), ylim =c(0, 5))
d <- rasterize_density(y, 3)
plot(d)
