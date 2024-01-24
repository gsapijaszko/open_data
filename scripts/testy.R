# -------------------------------------------------------------------------------------------------------


sf::st_delete(dsn = "data/bbbike_Leipzig.gpkg", layer = "highways")

c_osm <- tmaptools::read_osm(leipzig_border, ext = 1.1, zoom = 10)

tmap::tm_shape(c_osm, crs = "EPSG:4839", bbox = sf::st_bbox(sf::st_buffer(leipzig_border, 150))) +
  tmap::tm_raster(col = tmap::tm_mv_dim("band", c("red", "green", "blue")), 
                  col.scale = tmap::tm_scale_rgb()) +
  tmap::tm_shape(leipzig_border) + 
  tmap::tm_borders() +
  tmap::tm_shape(hw) +
  tmap::tm_lines(lwd = 0.2)
map_c
tmap_save(map_c, filename="map_c.png")


# -------------------------------------------------------------------------------------------------------

url <- "https://blackadder.dev.openstreetmap.org/OSMStats/"
t <- rvest::read_html(url) |>
  rvest::html_table()
f <- t[[1]] |>
  subset(grepl(".csv", Name), select = c(Name, `Last modified`)) |>
  dplyr::mutate(date = as.Date(substr(`Last modified`, 1, 10))) |>
  dplyr::arrange(date) |>
  tail(1)
osm_stats <- read.csv(paste0(url, f$Name))

p <- osm_stats |>
  subset(select = c(1:14)) |>
  dplyr::mutate(Day.of.Month = as.Date(Day.of.Month, format = "%d/%m/%Y")) |>
  subset(format(Day.of.Month, "%d") == "01")


ratio = 1.5*with(p, max(Users, na.rm = TRUE)/max(Nodes.editors.week, na.rm = TRUE))

library(ggplot2)
library(scales)
ggplot(p) +
  geom_area(aes(x=Day.of.Month,y=Users), fill="grey80") +
  geom_col(aes(x=Day.of.Month,y=Nodes.editors.week), fill="#D86422", linewidth=0.6) +
  scale_y_log10(labels=comma, name="Daily Cases",
                     sec.axis = sec_axis(~ ./ratio, 
                                         labels=function(x) comma(round(10^x), accuracy=1),
                                         breaks=c(0:5, 0:10+0.5),
                                         name="Cumulative Cases"))

p$Users

max(p$Users, na.rm = TRUE)
  class()

plot(p$Day.of.Month, p$Users,
  pch = 18,
  col = "blue",
  log = "y",
  xlim = c(
    lubridate::as_date("2005-01-01"),
    lubridate::as_date("2025-01-01")
  ),
  xlab = "year",
  ylab = ""
)
lines(p$Day.of.Month, p$Nodes.editors.week)
legend("topleft",
  legend = c("# of users", "# of nodes editors/week"),
  text.col = c("blue", "black")
)



# -------------------------------------------------------------------------------------------------------


lu <- sf::st_read("data/bbbike_Leipzig.gpkg",
                  query = "SELECT osm_id, name, landuse, natural, \
                    'forest' AS type, ST_Area(ST_Transform(geometry, 25832)) AS area,
                     ST_Transform(geometry, 25832) \
                     FROM multipolygons \
                     WHERE landuse = 'forest' OR natural = 'wood' ")

lu
# -------------------------------------------------------------------------------------------------------

# - source: project
# quarto-pub:
#   - id: "5f3abafe-68f9-4c1d-835b-9d668b892001"
# url: "https://njones.quarto.pub/blog"
# 
# 
# quarto key = qpa_idLJrsGrFZkjchxmWZErfK4KnGg7VR0fJKaIqhdmqbGB1BCyML0jl84Uur3CVM8N


# -------------------------------------------------------------------------

a <- RefManageR::ReadBib(file = "packages.bib")
b <- RefManageR::ReadBib(file = "references.bib")
c <- a+b
RefManageR::WriteBib(c, file = "references.bib")


# ------------------------------------------------------------------------------------------------------

l <- list.files(path = "data/bibliography/new/", pattern = ".bib", full.names = TRUE)

a <- RefManageR::ReadBib(l[1])

for (i in 2:length(l)) {
  print(paste(i, "-", l[i]))
  b <- RefManageR::ReadBib(l[i])
  a <- b + a 
}

b <- RefManageR::ReadBib(file = "data/bibliography/all.bib")

c <- b |>
  as.data.frame()

c |>
  str()

library(quanteda)

corp <- c$title |>
  quanteda::corpus()

c$title |>
  subset(grepl("LIDAR", c$title))

t <- quanteda::tokens(corp, what = "word",
                      remove_numbers = TRUE,
                      remove_punct = TRUE,
                      remove_symbols = TRUE,
                      remove_separators = TRUE,
                      split_hyphens = TRUE
                      )
  
st <- c(
  quanteda::stopwords(language = "english"),
  "name", "hello",
  unique(weekdays(.leap.seconds)),
  letters[1:26],
  LETTERS[1:26],
  "openstreetmap", "OpenStreetMap",
  "OSM", "osm"
)

t <- quanteda::tokens_remove(t, pattern = st)

myDFM <- quanteda::dfm(t) |>
  quanteda::dfm_remove(st)

a <- quanteda::topfeatures(myDFM, 100)
set.seed(100)
a
wordcloud::wordcloud(words = names(a), 
                     freq = a,
                     min.freq = 6,
                     random.order = FALSE,
                     colors = RColorBrewer::brewer.pal(8, "Dark2")
                     )


#' TODO -- częstość słów w tytułach, zmiany w poszczególnych latach
#' TODO - częstość słów w abstraktach
#' TODO - referencje z innych servisów (RSelenium?)
#'  : google scholar
#'  : researchgate
#'  : taylor francis
#' 

b$title[1]
c <- b$doi[1:10] |>
  roadoi::oadoi_fetch(email = "grzegorz@sapijaszko.net")

c$best_oa_location[1]

# query = openstreetmap OR VGI OR "Volunteered Geographic Information" OR "Open Street Map" in title and abstracts in [Dimension](https://app.dimensions.ai/) on 2024-01-09

# -------------------------------------------------------------------------------------------------------

number_of_pubs = data.frame(
  year = c(2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008),
  pubs = c(559,   534,  560,  513,  497,  439,  414,  346,  301,  233,  199,  134,   99,   51,   32,   27)
)
plot(number_of_pubs$pubs ~ number_of_pubs$year)


# crossref ----------------------------------------------------------------------------------------------

a <- rcrossref::cr_works(query = "Open Street Map", limit = 100)
# a$data$doi |>

a$data |>
  head(20) |>
  subset(select = c("doi", "link")) |>
  dplyr::rowwise() |>
  dplyr::mutate(cc = get_link(link)[1], dd = get_link(link)[2])

!is.null()

unlist(a$data[4, "link"])[[1]]

head() |>
  dplyr::mutate(link_url = link[[1]]$URL) # |>
subset(select = c("doi", "link_url"))

a$data$link[12] |>
  unlist()

get_link <- function(x) {
  if(is.null(x)) {
    x <- c("", "")
  } else {
    link_url  <- unlist(x)[[1]]
    link_type <- unlist(x)[[2]]
    x <- c(link_url, link_type)
  }
  return(x)
}


a <- rcrossref::cr_works(filter = c(issn = '2543-6503'), limit = 100)
rcrossref::cr_works(filter = c(issn = '0043-5090'))
u <- a$data[1,]$link[[1]][2,"URL"]
u[[1]]
xml2::read_xml(u[[1]])

bib <- rcrossref::cr_cn(dois = a$data$doi, format = "bibtex")

bib

a$data$doi

# Wiadomości Botaniczne
# Article ID: 641
# DOI: 10.5586/wb.641
# 


# openalex ----------------------------------------------------------------------------------------------

options(openalexR.mailto = "grzegorz@sapijaszko.net")

# a <- openalexR::oa_fetch(search = "OpenStreetMap", mailto = "grzegorz@sapijaszko.net")
# saveRDS(a, file = "data/openalexer_osm_query.rds")

a <- readRDS(file = "data/openalexer_osm_query.rds")

a[1, "display_name"]

b <- a$doi[1:100] |>
  stringr::str_remove_all(pattern = "https://doi.org/") |>
  rcrossref::cr_cn(samo_doi, format = "bibtex")

b  

all <- RefManageR::ReadBib("data/bibliography/all.bib")

# b <- a |>
#   head(100) |>
#   dplyr::mutate(samo_doi = stringr::str_remove_all(doi, pattern = "https://doi.org/")) |>
#   dplyr::mutate(bibtex = rcrossref::cr_cn(samo_doi, format = "bibtex"))

c <- b[[1]] |>
  rbibutils::charToBib() |>
  RefManageR::as.BibEntry() 
c

for (i in 2:length(b)) {
  print(i)
  d <- b[[i]] |>
    rbibutils::charToBib() |>
    RefManageR::as.BibEntry() 
  c = c + d
}

a[21,"author"][[1]]
b[[21]] #|>
  rbibutils::charToBib() #|>
  RefManageR::as.BibEntry() |>
  RefManageR::toBiblatex()


d <- c |>
  as.data.frame()

d$ab <- stringr::str_replace(b$ab[b$samo_doi == d$doi], "^Abstract", "") |>
  stringr::str_trim()

d$ab

d |>
  RefManageR::as.BibEntry() |>
  RefManageR::WriteBib(file = "data/1.bib")

RefManageR::ReadBib("data/1.bib")

r <- a$doi[1:10] |>
  roadoi::oadoi_fetch(email = "grzegorz@sapijaszko.net")


s <- a$doi[1:10] |>
  stringr::str_remove_all(pattern = "https://doi.org/") |>
  rcrossref::cr_cn(format = "bibtex")

x <- s[[1]] |>
  rbibutils::charToBib() |>
  RefManageR::as.BibEntry() 


x$abstract <- a[1, "ab"]

a[1, "author"][[1]][[1]]$au_display_name |>
  length()

a[1,]
a[1, "doi"] |>
  roadoi::oadoi_fetch(email = "grzegorz@sapijaszko.net")

r <- a[1:10,]

r$doi[1:10]

s <- roadoi::oadoi_fetch(r$doi[1:10], email = "grzegorz@sapijaszko.net")
r |>
  dplyr::left_join(s, by = "doi")

x |>
  RefManageR::toBiblatex()

a[1,] |>
  tidyr::unnest(author) |>
  subset(select = c(au_id, au_display_name))

# openalexR::

bb <- biblio::read_bib("data/bibliography/all.bib")
bb |>
  as.data.frame() |>
  subset(bib_type == "Article")

unlist(r[1]) |>
  writeLines(con = "data/1.bib")

  RefManageR::ReadBib(file = "data/1.bib")

# -------------------------------------------------------------------------------------------------------
library(rnaturalearthdata)
library(spData)
library(osmextract)
file.remove("packages.bib")
softbib::softbib(output = "packages.bib", output_dir = "/home/sapi/projekty/open_data/")

openalexR::oa_fetch(search = "OpenStreetMap", 
                    options = list(sample = 20), 
                    mailto = "grzegorz@sapijaszko.net")


a[1, c("doi", "author")] |>
  tidyr::unnest("author") |>
  subset(select = c("doi", "au_display_name")) |>
  dplyr::group_by(doi) |>
  dplyr::summarise(y = paste0(au_display_name, colapse = " "))

a <- readRDS(file = "data/openalexer_osm_query.rds")

a <- a |>
  tidyr::unnest(doi)

my_doi <- a[3, "doi"][[1]]

bib <- a |>
  subset(doi == my_doi)

author <- bib |>
  subset(select = "author") |>
  tidyr::unnest(author) |>
  subset(select = au_display_name)

author <- author$au_display_name # |>  
  stringi::stri_join(collapse = " and ")
  
title <- bib$display_name[[1]]

abstract <- bib$ab |>
  stringi::stri_replace_first_regex(pattern = "^Abstract", replacement = "") |>
  stringi::stri_trim_both()

bib_type <- bib$type |>
    stringi::stri_trans_totitle()

journal <- bib$so

if(!is.na(bib$publication_year))  {
  year <- bib$publication_year
} else if(!is.na(bib$publication_date)) {
  year <- lubridate::year(bib$publication_date)    
} else {
  year <- NA
}

issue_date <- bib$publication_date

doi <- bib$doi |>
  stringr::str_remove_all(pattern = "https://doi.org/")
  
xx <- bibentry(
  bibtype = "Article",
  author = as.personList(author),
  title = title,
  journal = journal,
  year = year,
  abstract = abstract,
  volume = bib$volume,
  number = bib$issue,
  issn = bib$issn_l,
  pages = paste(bib$first_page, bib$last_page, sep = "--"),
  issue_date = issue_date,
  doi = doi,
  url = bib$oa_url
)

xx |>
  RefManageR::toBiblatex()

temp_df <- a[1,]
temp_df <- temp_df[-1,]

alex_doi <- a$doi |>
  stringr::str_remove_all(pattern = "https://doi.org/")

all_biblio <- RefManageR::ReadBib("data/bibliography/all.bib")

for (i in seq_len(length(alex_doi))) {
  adoi <- alex_doi[i]
  if(!is.na(adoi)) {
    l <- RefManageR::SearchBib(all_biblio, doi = adoi)
    if(length(l) < 1) {
      temp_df <- a |>
        subset(doi == paste0("https://doi.org/",adoi)) |>
        rbind(temp_df)
    }
  }
}

temp_df |>
  saveRDS(file = "data/bibliography/alex_roznica.rds")

new_doi <- temp_df |>
  subset(select = c("display_name", "ab", "doi")) |>
  subset(grepl(pattern = "OSM|Open Street Map|VGI", display_name) | grepl(pattern = "OSM|Open Street Map|VGI", ab)) |>
  subset(select = doi)

new_bib <- new_doi$doi |>
  stringr::str_remove_all(pattern = "https://doi.org/") |>
  rcrossref::cr_cn(samo_doi, format = "bibtex")

new_bib[[31]] #|>
  rbibutils::charToBib() |>
  RefManageR::as.BibEntry() |>
  RefManageR::toBiblatex() |>
  clipr::write_clip()

uchu# for (i in 2:length(new_bib)) {
#   print(i)
#   c <- new_bib[[i]] |>
#     rbibutils::charToBib() |>
#     RefManageR::as.BibEntry() 
#   b <- b + c
# }
  
new_bib[doi = "10.4018/978-1-5225-2446-5.ch007"]

l <- RefManageR::SearchBib(all_biblio, doi = "10.1177/0265813515604765")
length(l)


# 
# function Link (link)
#   if link.target:match '^https?%:' then
#     link.attributes.target = '_blank'
#     return link
#   end
# end
# 


