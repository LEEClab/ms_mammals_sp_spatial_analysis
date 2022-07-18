#' ---
#' title: spatial analysis - data papers - mammals sp
#' author: mauricio vancine
#' year: 2022-03-29
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(sf)
library(terra)
library(SpatialKDE)
library(tmap)

# options
sf::sf_use_s2(FALSE)

# directories
dir.create("maps")

# import data -------------------------------------------------------------

# ugrhi
ugrhi <- sf::st_read("data/geodata/ugrhi.shp")
ugrhi

# inventario florestal
inventario_florestal <- sf::st_read("data/geodata/inventario_florestal.shp")
sf::st_drop_geometry(inventario_florestal)

# unidades de conservacao
uc_pi <- sf::st_read("data/geodata/uc_pi.shp") %>% 
    sf::st_intersection(ugrhi) %>% 
    sf::st_union()
uc_pi

# data paper
data_papers <- sf::st_read("data/geodata/occ_v_sp.gpkg")
data_papers

orders <- data_papers %>% sf::st_drop_geometry() %>% count(order)
orders

families <- data_papers %>% sf::st_drop_geometry() %>% count(family)
families

genus <- data_papers %>% sf::st_drop_geometry() %>% count(genus)
genus

species <- data_papers %>% sf::st_drop_geometry() %>% count(species)
species

order_species <- data_papers %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct(order, species) %>% 
    group_by(order) %>% 
    summarise(n = n())
order_species$n %>% sum

# richness ----------------------------------------------------------------

## total ----
rich_ugrhi_total <- ugrhi %>% 
    sf::st_join(., data_papers) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(Nome, species) %>%
    dplyr::distinct(Nome, species) %>% 
    dplyr::group_by(Nome) %>% 
    tidyr::drop_na(species) %>% 
    dplyr::summarise(rich = n()) %>% 
    dplyr::left_join(ugrhi, .)

map_total <- tm_shape(rich_ugrhi_total) +
    tm_fill(col = "rich", pal = "-Spectral",
            title = "Richness", n = 10, alpha = .8, textNA = "No Data") +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2) +
    tm_shape(data_papers) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = "Mammals",
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 2,
              legend.title.fontface = "bold",
              legend.text.size = 1.2)
map_total
tmap_save(map_total, paste0("maps/map_occ_total.png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## artiodactyla ---- 
rich_ugrhi_artiodactyla <- ugrhi %>% 
    sf::st_join(., dplyr::filter(data_papers, order == orders[1, 1])) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(Nome, species) %>%
    dplyr::distinct(Nome, species) %>% 
    dplyr::group_by(Nome) %>% 
    tidyr::drop_na(species) %>% 
    dplyr::summarise(rich = n()) %>% 
    dplyr::left_join(ugrhi, .)
occ_artiodactyla <- dplyr::filter(data_papers, order == orders[1, 1])

map_artiodactyla <- tm_shape(rich_ugrhi_artiodactyla) +
    tm_fill(col = "rich", pal = "-Spectral", style = "cat",
            title = "Richness", n = 7, alpha = .8, textNA = "No Data") +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2) +
    tm_shape(occ_artiodactyla) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[1, 1],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 2,
              legend.title.fontface = "bold",
              legend.text.size = 1.2)
map_artiodactyla
tmap_save(map_artiodactyla, paste0("maps/map_occ_", orders[1], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## carnivora ---- 
rich_ugrhi_carnivora <- ugrhi %>% 
    sf::st_join(., dplyr::filter(data_papers, order == orders[, 2])) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(Nome, species) %>%
    dplyr::distinct(Nome, species) %>% 
    dplyr::group_by(Nome) %>% 
    tidyr::drop_na(species) %>% 
    dplyr::summarise(rich = n()) %>% 
    dplyr::left_join(ugrhi, .)

occ_carnivora <- dplyr::filter(data_papers, order == orders[2])

map_carnivora <- tm_shape(rich_ugrhi_carnivora) +
    tm_fill(col = "rich", pal = "-Spectral", textNA = "No Data", 
            title = "Richness", n = 7, alpha = .8) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2) +
    tm_shape(occ_carnivora) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[2],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 2,
              legend.title.fontface = "bold",
              legend.text.size = 1.2)
tmap_save(map_carnivora, paste0("maps/map_occ_", orders[2], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## chiroptera ---- 
rich_ugrhi_chiroptera <- ugrhi %>% 
    sf::st_join(., dplyr::filter(data_papers, order == orders[3])) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(Nome, species) %>%
    dplyr::distinct(Nome, species) %>% 
    dplyr::group_by(Nome) %>% 
    tidyr::drop_na(species) %>% 
    dplyr::summarise(rich = n()) %>% 
    dplyr::left_join(ugrhi, .)

occ_chiroptera <- dplyr::filter(data_papers, order == orders[3])

map_chiroptera <- tm_shape(rich_ugrhi_chiroptera) +
    tm_fill(col = "rich", pal = "-Spectral", textNA = "No Data", 
            title = "Richness", n = 7, alpha = .8) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2) +
    tm_shape(occ_chiroptera) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[3],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 2,
              legend.title.fontface = "bold",
              legend.text.size = 1.2)
tmap_save(map_chiroptera, paste0("maps/map_occ_", orders[3], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## cingulata ---- 
rich_ugrhi_cingulata <- ugrhi %>% 
    sf::st_join(., dplyr::filter(data_papers, order == orders[4])) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(Nome, species) %>%
    dplyr::distinct(Nome, species) %>% 
    dplyr::group_by(Nome) %>% 
    tidyr::drop_na(species) %>% 
    dplyr::summarise(rich = n()) %>% 
    dplyr::left_join(ugrhi, .)

occ_cingulata <- dplyr::filter(data_papers, order == orders[4])

map_cingulata <- tm_shape(rich_ugrhi_cingulata) +
    tm_fill(col = "rich", pal = "-Spectral", textNA = "No Data", 
            title = "Richness", n = 7, alpha = .8) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2) +
    tm_shape(occ_cingulata) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[4],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 2,
              legend.title.fontface = "bold",
              legend.text.size = 1.2)
tmap_save(map_cingulata, paste0("maps/map_occ_", orders[4], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## didelphimorphia ---- 
rich_ugrhi_didelphimorphia <- ugrhi %>% 
    sf::st_join(., dplyr::filter(data_papers, order == orders[5])) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(Nome, species) %>%
    dplyr::distinct(Nome, species) %>% 
    dplyr::group_by(Nome) %>% 
    tidyr::drop_na(species) %>% 
    dplyr::summarise(rich = n()) %>% 
    dplyr::left_join(ugrhi, .)

occ_didelphimorphia <- dplyr::filter(data_papers, order == orders[5])

map_didelphimorphia <- tm_shape(rich_ugrhi_didelphimorphia) +
    tm_fill(col = "rich", pal = "-Spectral", textNA = "No Data", 
            title = "Richness", n = 7, alpha = .8) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2) +
    tm_shape(occ_didelphimorphia) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[5],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 2,
              legend.title.fontface = "bold",
              legend.text.size = 1.2)
tmap_save(map_didelphimorphia, paste0("maps/map_occ_", orders[5], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## lagomorpha ---- 
rich_ugrhi_lagomorpha <- ugrhi %>% 
    sf::st_join(., dplyr::filter(data_papers, order == orders[6])) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(Nome, species) %>%
    dplyr::distinct(Nome, species) %>% 
    dplyr::group_by(Nome) %>% 
    tidyr::drop_na(species) %>% 
    dplyr::summarise(rich = n()) %>% 
    dplyr::left_join(ugrhi, .)

occ_lagomorpha <- dplyr::filter(data_papers, order == orders[6])

map_lagomorpha <- tm_shape(rich_ugrhi_lagomorpha) +
    tm_fill(col = "rich", pal = "-Spectral", style = "cat",
            title = "Richness", n = 3, alpha = .8, textNA = "No Data") +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2) +
    tm_shape(occ_lagomorpha) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[6],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 2,
              legend.title.fontface = "bold",
              legend.text.size = 1.2)
tmap_save(map_lagomorpha, paste0("maps/map_occ_", orders[6], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## perissodactyla ---- 
rich_ugrhi_perissodactyla <- ugrhi %>% 
    sf::st_join(., dplyr::filter(data_papers, order == orders[7])) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(Nome, species) %>%
    dplyr::distinct(Nome, species) %>% 
    dplyr::group_by(Nome) %>% 
    tidyr::drop_na(species) %>% 
    dplyr::summarise(rich = n()) %>% 
    dplyr::left_join(ugrhi, .)

occ_perissodactyla <- dplyr::filter(data_papers, order == orders[7])

map_perissodactyla <- tm_shape(rich_ugrhi_perissodactyla) +
    tm_fill(col = "rich", pal = "-Spectral", style = "cat",
            title = "Richness", n = 2, alpha = .8, textNA = "No Data") +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2) +
    tm_shape(occ_perissodactyla) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[7],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 2,
              legend.title.fontface = "bold",
              legend.text.size = 1.2)
tmap_save(map_perissodactyla, paste0("maps/map_occ_", orders[7], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## pilosa ---- 
rich_ugrhi_pilosa <- ugrhi %>% 
    sf::st_join(., dplyr::filter(data_papers, order == orders[8])) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(Nome, species) %>%
    dplyr::distinct(Nome, species) %>% 
    dplyr::group_by(Nome) %>% 
    tidyr::drop_na(species) %>% 
    dplyr::summarise(rich = n()) %>% 
    dplyr::left_join(ugrhi, .)

occ_pilosa <- dplyr::filter(data_papers, order == orders[8])

map_pilosa <- tm_shape(rich_ugrhi_pilosa) +
    tm_fill(col = "rich", pal = "-Spectral", style = "cat",
            title = "Richness", n = 4, alpha = .8, textNA = "No Data") +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2) +
    tm_shape(occ_pilosa) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[8],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 2,
              legend.title.fontface = "bold",
              legend.text.size = 1.2)
tmap_save(map_pilosa, paste0("maps/map_occ_", orders[8], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## primates ---- 
rich_ugrhi_primates <- ugrhi %>% 
    sf::st_join(., dplyr::filter(data_papers, order == orders[9])) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(Nome, species) %>%
    dplyr::distinct(Nome, species) %>% 
    dplyr::group_by(Nome) %>% 
    tidyr::drop_na(species) %>% 
    dplyr::summarise(rich = n()) %>% 
    dplyr::left_join(ugrhi, .)

occ_primates <- dplyr::filter(data_papers, order == orders[9])

map_primates <- tm_shape(rich_ugrhi_primates) +
    tm_fill(col = "rich", pal = "-Spectral", style = "cat",
            title = "Richness", n = 7, alpha = .8, textNA = "No Data") +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2) +
    tm_shape(occ_primates) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[9],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 2,
              legend.title.fontface = "bold",
              legend.text.size = 1.2)
tmap_save(map_primates, paste0("maps/map_occ_", orders[9], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## rodentia ---- 
rich_ugrhi_rodentia <- ugrhi %>% 
    sf::st_join(., dplyr::filter(data_papers, order == orders[10])) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(Nome, species) %>%
    dplyr::distinct(Nome, species) %>% 
    dplyr::group_by(Nome) %>% 
    tidyr::drop_na(species) %>% 
    dplyr::summarise(rich = n()) %>% 
    dplyr::left_join(ugrhi, .)

occ_rodentia <- dplyr::filter(data_papers, order == orders[10])

map_rodentia <- tm_shape(rich_ugrhi_rodentia) +
    tm_fill(col = "rich", pal = "-Spectral", textNA = "No Data", 
            title = "Richness", n = 7, alpha = .8) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2) +
    tm_shape(occ_rodentia) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[10],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 2,
              legend.title.fontface = "bold",
              legend.text.size = 1.2)
tmap_save(map_rodentia, paste0("maps/map_occ_", orders[10], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

# kernel ------------------------------------------------------------------

## data ----
# ugrhi
ugrhi_poly <- sf::st_transform(ugrhi, crs = 5880)
ugrhi_poly

# data_paper
data_papers_poly <- sf::st_transform(data_papers, crs = 5880)
data_papers_poly

# cell size and band width
cell_size <- 5000
band_width <- 50000

# grid
ugrhi_raster <- ugrhi_poly %>% 
    create_raster(cell_size = cell_size, side_offset = band_width)
ugrhi_raster

## total ----
kde_total <- data_papers_poly %>%
    dplyr::select(geom) %>% 
    SpatialKDE::kde(band_width = band_width, 
                    kernel = "quartic", 
                    grid = ugrhi_raster) %>% 
    raster::crop(ugrhi_poly) %>% 
    raster::mask(ugrhi_poly)

kde_total_geo <- raster::projectRaster(from = kde_total,
                                       crs = "+proj=longlat +datum=WGS84 +no_defs", 
                                       res = .0462963)
kde_total_geo

occ_total <- data_papers

map_kernel_total <- tm_shape(kde_total_geo, bbox = c(-53.3, -25.45, -44, -19.6)) +
    tm_raster(pal = "-Spectral", n = 10, title = "Kernel Density") +
    tm_shape(ugrhi) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2, alpha = .5) +
    tm_shape(occ_total) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = "Mammals",
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 1.5,
              legend.title.fontface = "bold",
              legend.text.size = .7,
              legend.position = c("left", "bottom"))
map_kernel_total
tmap_save(map_kernel_total, paste0("maps/map_kernel_total.png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## artiodactyla ---- 
kde_artiodactyla <- data_papers_poly %>%
    dplyr::filter(order == orders[1]) %>% 
    dplyr::select(geometry) %>% 
    SpatialKDE::kde(band_width = band_width, 
                    kernel = "quartic", 
                    grid = ugrhi_raster) %>% 
    raster::crop(ugrhi_poly) %>% 
    raster::mask(ugrhi_poly)

kde_artiodactyla_geo <- raster::projectRaster(from = kde_artiodactyla,
                                              crs = "+proj=longlat +datum=WGS84 +no_defs", 
                                              res = .0462963)
kde_artiodactyla_geo

occ_artiodactyla <- dplyr::filter(data_papers, order == orders[1])

map_kernel_artiodactyla <- tm_shape(kde_artiodactyla_geo, bbox = c(-53.3, -25.45, -44, -19.6)) +
    tm_raster(pal = "-Spectral", n = 20, title = "Kernel Density") +
    tm_shape(ugrhi) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2, alpha = .5) +
    tm_shape(occ_artiodactyla) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[1],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 1.5,
              legend.title.fontface = "bold",
              legend.text.size = .7,
              legend.position = c("left", "bottom"))
tmap_save(map_kernel_artiodactyla, paste0("maps/map_kernel_", orders[1], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## carnivora ---- 
kde_carnivora <- data_papers_poly %>%
    dplyr::filter(order == orders[2]) %>% 
    dplyr::select(geometry) %>% 
    SpatialKDE::kde(band_width = band_width, 
                    kernel = "quartic", 
                    grid = ugrhi_raster) %>% 
    raster::crop(ugrhi_poly) %>% 
    raster::mask(ugrhi_poly)

kde_carnivora_geo <- raster::projectRaster(from = kde_carnivora,
                                           crs = "+proj=longlat +datum=WGS84 +no_defs", 
                                           res = .0462963)
kde_carnivora_geo

occ_carnivora <- dplyr::filter(data_papers, order == orders[2])

map_kernel_carnivora <- tm_shape(kde_carnivora_geo, bbox = c(-53.3, -25.45, -44, -19.6)) +
    tm_raster(pal = "-Spectral", n = 15, title = "Kernel Density") +
    tm_shape(ugrhi) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2, alpha = .5) +
    tm_shape(occ_carnivora) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[2],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 1.5,
              legend.title.fontface = "bold",
              legend.text.size = .8,
              legend.position = c("left", "bottom"))
tmap_save(map_kernel_carnivora, paste0("maps/map_kernel_", orders[2], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## chiroptera ---- 
kde_chiroptera <- data_papers_poly %>%
    dplyr::filter(order == orders[3]) %>% 
    dplyr::select(geometry) %>% 
    SpatialKDE::kde(band_width = band_width, 
                    kernel = "quartic", 
                    grid = ugrhi_raster) %>% 
    raster::crop(ugrhi_poly) %>% 
    raster::mask(ugrhi_poly)

kde_chiroptera_geo <- raster::projectRaster(from = kde_chiroptera,
                                            crs = "+proj=longlat +datum=WGS84 +no_defs", 
                                            res = .0462963)

occ_chiroptera <- dplyr::filter(data_papers, order == orders[3])

map_kernel_chiroptera <- tm_shape(kde_chiroptera_geo, bbox = c(-53.3, -25.45, -44, -19.6)) +
    tm_raster(pal = "-Spectral", n = 15, title = "Kernel Density") +
    tm_shape(ugrhi) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2, alpha = .5) +
    tm_shape(occ_chiroptera) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[3],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 1.5,
              legend.title.fontface = "bold",
              legend.text.size = .8,
              legend.position = c("left", "bottom"))
tmap_save(map_kernel_chiroptera, paste0("maps/map_kernel_", orders[3], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## cingulata ---- 
kde_cingulata <- data_papers_poly %>%
    dplyr::filter(order == orders[4]) %>% 
    dplyr::select(geometry) %>% 
    SpatialKDE::kde(band_width = band_width, 
                    kernel = "quartic", 
                    grid = ugrhi_raster) %>% 
    raster::crop(ugrhi_poly) %>% 
    raster::mask(ugrhi_poly)

kde_cingulata_geo <- raster::projectRaster(from = kde_cingulata,
                                           crs = "+proj=longlat +datum=WGS84 +no_defs", 
                                           res = .0462963)
kde_cingulata_geo[kde_cingulata_geo < 0] <- 0

occ_cingulata <- dplyr::filter(data_papers, order == orders[4])

map_kernel_cingulata <- tm_shape(kde_cingulata_geo, bbox = c(-53.3, -25.45, -44, -19.6)) +
    tm_raster(pal = "-Spectral", n = 15, title = "Kernel Density") +
    tm_shape(ugrhi) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2, alpha = .5) +
    tm_shape(occ_cingulata) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[4],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 1.5,
              legend.title.fontface = "bold",
              legend.text.size = .8,
              legend.position = c("left", "bottom"))
tmap_save(map_kernel_cingulata, paste0("maps/map_kernel_", orders[4], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## didelphimorphia ---- 
kde_didelphimorphia <- data_papers_poly %>%
    dplyr::filter(order == orders[5]) %>% 
    dplyr::select(geometry) %>% 
    SpatialKDE::kde(band_width = band_width, 
                    kernel = "quartic", 
                    grid = ugrhi_raster) %>% 
    raster::crop(ugrhi_poly) %>% 
    raster::mask(ugrhi_poly)

kde_didelphimorphia_geo <- raster::projectRaster(from = kde_didelphimorphia,
                                                 crs = "+proj=longlat +datum=WGS84 +no_defs", 
                                                 res = .0462963)
kde_didelphimorphia_geo[kde_didelphimorphia_geo < 0] <- 0

occ_didelphimorphia <- dplyr::filter(data_papers, order == orders[5])

map_kernel_didelphimorphia <- tm_shape(kde_didelphimorphia_geo, bbox = c(-53.3, -25.45, -44, -19.6)) +
    tm_raster(pal = "-Spectral", n = 20, title = "Kernel Density") +
    tm_shape(ugrhi) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2, alpha = .5) +
    tm_shape(occ_didelphimorphia) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[5],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 1.5,
              legend.title.fontface = "bold",
              legend.text.size = .6,
              legend.position = c("left", "bottom"))
tmap_save(map_kernel_didelphimorphia, paste0("maps/map_kernel_", orders[5], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## lagomorpha ---- 
kde_lagomorpha <- data_papers_poly %>%
    dplyr::filter(order == orders[6]) %>% 
    dplyr::select(geometry) %>% 
    SpatialKDE::kde(band_width = band_width, 
                    kernel = "quartic", 
                    grid = ugrhi_raster) %>% 
    raster::crop(ugrhi_poly) %>% 
    raster::mask(ugrhi_poly)

kde_lagomorpha_geo <- raster::projectRaster(from = kde_lagomorpha,
                                            crs = "+proj=longlat +datum=WGS84 +no_defs", 
                                            res = .0462963)
kde_lagomorpha_geo

occ_lagomorpha <- dplyr::filter(data_papers, order == orders[6])

map_kernel_lagomorpha <- tm_shape(kde_lagomorpha_geo, bbox = c(-53.3, -25.45, -44, -19.6)) +
    tm_raster(pal = "-Spectral", n = 15, title = "Kernel Density") +
    tm_shape(ugrhi) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2, alpha = .5) +
    tm_shape(occ_lagomorpha) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[6],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 1.5,
              legend.title.fontface = "bold",
              legend.text.size = .8,
              legend.position = c("left", "bottom"))
tmap_save(map_kernel_lagomorpha, paste0("maps/map_kernel_", orders[6], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## perissodactyla ---- 
kde_perissodactyla <- data_papers_poly %>%
    dplyr::filter(order == orders[7]) %>% 
    dplyr::select(geometry) %>% 
    SpatialKDE::kde(band_width = band_width, 
                    kernel = "quartic", 
                    grid = ugrhi_raster) %>% 
    raster::crop(ugrhi_poly) %>% 
    raster::mask(ugrhi_poly)

kde_perissodactyla_geo <- raster::projectRaster(from = kde_perissodactyla,
                                                crs = "+proj=longlat +datum=WGS84 +no_defs", 
                                                res = .0462963)
kde_perissodactyla_geo

occ_perissodactyla <- dplyr::filter(data_papers, order == orders[7])

map_kernel_perissodactyla <- tm_shape(kde_perissodactyla_geo, bbox = c(-53.3, -25.45, -44, -19.6)) +
    tm_raster(pal = "-Spectral", n = 15, title = "Kernel Density") +
    tm_shape(ugrhi) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2, alpha = .5) +
    tm_shape(occ_perissodactyla) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[7],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 1.5,
              legend.title.fontface = "bold",
              legend.text.size = .8,
              legend.position = c("left", "bottom"))
tmap_save(map_kernel_perissodactyla, paste0("maps/map_kernel_", orders[7], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## pilosa ---- 
kde_pilosa <- data_papers_poly %>%
    dplyr::filter(order == orders[8]) %>% 
    dplyr::select(geometry) %>% 
    SpatialKDE::kde(band_width = band_width, 
                    kernel = "quartic", 
                    grid = ugrhi_raster) %>% 
    raster::crop(ugrhi_poly) %>% 
    raster::mask(ugrhi_poly)

kde_pilosa_geo <- raster::projectRaster(from = kde_pilosa,
                                        crs = "+proj=longlat +datum=WGS84 +no_defs", 
                                        res = .0462963)
kde_pilosa_geo

occ_pilosa <- dplyr::filter(data_papers, order == orders[8])

map_kernel_pilosa <- tm_shape(kde_pilosa_geo, bbox = c(-53.3, -25.45, -44, -19.6)) +
    tm_raster(pal = "-Spectral", n = 20, title = "Kernel Density") +
    tm_shape(ugrhi) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2, alpha = .5) +
    tm_shape(occ_pilosa) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[8],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 1.5,
              legend.title.fontface = "bold",
              legend.text.size = .8,
              legend.position = c("left", "bottom"))
tmap_save(map_kernel_pilosa, paste0("maps/map_kernel_", orders[8], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## primates ---- 
kde_primates <- data_papers_poly %>%
    dplyr::filter(order == orders[9]) %>% 
    dplyr::select(geometry) %>% 
    SpatialKDE::kde(band_width = band_width, 
                    kernel = "quartic", 
                    grid = ugrhi_raster) %>% 
    raster::crop(ugrhi_poly) %>% 
    raster::mask(ugrhi_poly)

kde_primates_geo <- raster::projectRaster(from = kde_primates,
                                          crs = "+proj=longlat +datum=WGS84 +no_defs", 
                                          res = .0462963)
kde_primates_geo

occ_primates <- dplyr::filter(data_papers, order == orders[9])

map_kernel_primates <- tm_shape(kde_primates_geo, bbox = c(-53.3, -25.45, -44, -19.6)) +
    tm_raster(pal = "-Spectral", n = 15, title = "Kernel Density") +
    tm_shape(ugrhi) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2, alpha = .5) +
    tm_shape(occ_primates) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[9],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 1.5,
              legend.title.fontface = "bold",
              legend.text.size = .6,
              legend.position = c("left", "bottom"))
tmap_save(map_kernel_primates, paste0("maps/map_kernel_", orders[9], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

## rodentia ---- 
kde_rodentia <- data_papers_poly %>%
    dplyr::filter(order == orders[10]) %>% 
    dplyr::select(geometry) %>% 
    SpatialKDE::kde(band_width = band_width, 
                    kernel = "quartic", 
                    grid = ugrhi_raster) %>% 
    raster::crop(ugrhi_poly) %>% 
    raster::mask(ugrhi_poly)

kde_rodentia_geo <- raster::projectRaster(from = kde_rodentia,
                                          crs = "+proj=longlat +datum=WGS84 +no_defs", 
                                          res = .0462963)
kde_rodentia_geo

occ_rodentia <- dplyr::filter(data_papers, order == orders[10])

map_kernel_rodentia <- tm_shape(kde_rodentia_geo, bbox = c(-53.3, -25.45, -44, -19.6)) +
    tm_raster(pal = "-Spectral", n = 20, title = "Kernel Density") +
    tm_shape(ugrhi) +
    tm_borders(col = "white") +
    tm_shape(inventario_florestal) +
    tm_fill(col = "forestgreen", alpha = .7) +
    tm_shape(uc_pi) +
    tm_fill(col = "gray", alpha = .3) +
    tm_borders(col = "gray30", lwd = 1.2, alpha = .5) +
    tm_shape(occ_rodentia) + 
    tm_bubbles(size = .1, col = "black", alpha = .5) +
    tm_graticules(lines = FALSE, labels.size = .8) +
    tm_compass(size = 3) +
    tm_scale_bar(text.size = .8) +
    tm_layout(main.title = orders[10],
              main.title.size = 3,
              main.title.position = "center",
              main.title.fontface = "bold",
              legend.title.size = 1.5,
              legend.title.fontface = "bold",
              legend.text.size = .6,
              legend.position = c("left", "bottom"))
tmap_save(map_kernel_rodentia, paste0("maps/map_kernel_", orders[10], ".png"), 
          wi = 30, he = 20, un = "cm", dpi = 300)

# end ---------------------------------------------------------------------