install.packages("remotes")  # se ainda não tiver
install.packages("rnaturalearth")
remotes::install_github("pachamaltese/chilemapas")

library(chilemapas)
head(chilemapas::mapa_comunas)
head(chilemapas::codigos_territoriales)

class(latam)
class(chile_regiones)

class(rm_comunas)
names(rm_comunas)



library(tidyverse)
library(sf)
library(rnaturalearth)
library(chilemapas)
library(cowplot)
library(ggspatial)

# --- 1. Mapa 1: América Latina, destacando o Chile ---------------------
latam <- ne_countries(continent = "South America", scale = "medium", returnclass = "sf") |>
  bind_rows(ne_countries(country = c("Mexico", "Guatemala", "Belize", "Honduras",
                                     "El Salvador", "Nicaragua", "Costa Rica",
                                     "Panama", "Cuba", "Dominican Republic"),
                         scale = "medium", returnclass = "sf"))

map_latam <- ggplot(latam) +
  geom_sf(aes(fill = name == "Chile"), colour = "grey40", linewidth = 0.15) +
  scale_fill_manual(values = c("TRUE" = "gold1", "FALSE" = "grey85"), guide = "none") +
  annotation_scale(location = "bl", text_cex = 0.5) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),
                         height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        axis.ticks = element_line(linewidth = 0.2),
        plot.margin = margin(t = 20, r = 5, b = 5, l = 5))

# --- 2. Mapa 2: Chile, destacando a Región Metropolitana (Santiago) ----
chile_regiones <- chilemapas::mapa_comunas |>
  left_join(
    chilemapas::codigos_territoriales |>
      select(codigo_comuna, nombre_region),
    by = "codigo_comuna"
  ) |>
  group_by(codigo_region) |>
  summarise(geometry = st_union(geometry)) |>
  st_as_sf()

map_chile <- ggplot(chile_regiones) +
  geom_sf(aes(fill = codigo_region == "13"), colour = "grey40", linewidth = 0.1) +
  scale_fill_manual(values = c("TRUE" = "gold1", "FALSE" = "grey85"), guide = "none") +
  annotation_scale(location = "bl", text_cex = 0.6) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),
                         height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        axis.ticks = element_line(linewidth = 0.2),
        plot.margin = margin(t = 20, r = 5, b = 5, l = 5))

# --- 3. Mapa 3: comunas da Región Metropolitana, destacando Santiago ---
rm_comunas <- chilemapas::mapa_comunas |>
  left_join(
    chilemapas::codigos_territoriales |>
      select(codigo_comuna, nombre_comuna),
    by = "codigo_comuna"
  ) |>
  filter(codigo_region == "13") |>
  st_as_sf()

rm_comunas <- chilemapas::mapa_comunas |>
  left_join(
    chilemapas::codigos_territoriales |>
      select(codigo_comuna, nombre_comuna),
    by = "codigo_comuna"
  ) |>
  filter(codigo_region == "13") |>
  st_as_sf()

bbox_rm <- st_bbox(rm_comunas)
centro_x <- mean(c(bbox_rm["xmin"], bbox_rm["xmax"]))
centro_y <- mean(c(bbox_rm["ymin"], bbox_rm["ymax"]))
largura  <- max(bbox_rm["xmax"] - bbox_rm["xmin"], bbox_rm["ymax"] - bbox_rm["ymin"]) / 2 * 1.05

map_rm <- ggplot(rm_comunas) +
  geom_sf(aes(fill = nombre_comuna == "Santiago"), colour = "grey40", linewidth = 0.15) +
  scale_fill_manual(values = c("TRUE" = "#C1502E", "FALSE" = "grey85"), guide = "none") +
  annotation_scale(location = "bl", text_cex = 0.7) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  coord_sf(xlim = c(centro_x - largura, centro_x + largura),
           ylim = c(centro_y - largura, centro_y + largura)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        axis.ticks = element_line(linewidth = 0.2),
        plot.margin = margin(t = 20, r = 5, b = 5, l = 5))

# --- 4. Combinar os 3 mapas lado a lado ---------------------------------
p <- plot_grid(map_latam, map_chile, map_rm,
               nrow = 1)
p

ggsave("plots/mapa_localizacao.png", p, width = 14, height = 5, dpi = 300)
