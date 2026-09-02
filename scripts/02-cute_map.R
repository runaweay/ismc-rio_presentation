# =========================================================================
# Sampling points — Santiago de Chile
# Basemap: CARTO Positron (greyscale)
# =========================================================================

library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)
library(ggspatial)   # annotation_scale / annotation_north_arrow
library(maptiles)    # descarga de teselas
library(tidyterra)   # geom_spatraster_rgb

# --- Configuración -------------------------------------------------------
MARGEN    <- 0.0015      # ~150 m de margen alrededor de los puntos
TILE_ZOOM <- 17         # nivel de zoom de las teselas (17 = más detalle)
OUTFILE   <- "plots/puntos.png"

COL_PUNTOS <- "grey10"   # casi negro
COL_FONDO  <- "grey95"   # gris muy claro

# --- 1. Puntos -----------------------------------------------------------
puntos <- tibble::tribble(
  ~id,     ~lat,        ~lon,
  "PAV1",  -33.435856,  -70.644480,
  "PAV2",  -33.437085,  -70.643343,
  "PAV3",  -33.437829,  -70.647144,
  "OPEN1", -33.434713,  -70.644219,
  "OPEN2", -33.439846,  -70.644171
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# --- 2. Bounding box con margen ------------------------------------------
bb <- st_bbox(puntos)
bb["xmin"] <- bb["xmin"] - MARGEN
bb["xmax"] <- bb["xmax"] + MARGEN
bb["ymin"] <- bb["ymin"] - MARGEN
bb["ymax"] <- bb["ymax"] + MARGEN

# --- 3. Basemap ----------------------------------------------------------
carto_grey <- list(
  src = "carto_positron",
  q   = "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png?key={apikey}",
  sub = NA,
  cit = "© OpenStreetMap contributors © CARTO"
)

tiles <- get_tiles(
  st_as_sfc(bb),
  provider = carto_grey,
  apikey   = Sys.getenv("CARTO_KEY"),
  zoom     = TILE_ZOOM,
  crop     = TRUE
)

# --- 4. Gráfico ----------------------------------------------------------
p <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = Inf) +
  geom_sf(data = puntos, colour = COL_PUNTOS, size = 3.5, shape = 20) +
  geom_label_repel(
    data = puntos,
    aes(label = id, geometry = geometry),
    stat = "sf_coordinates",
    colour = COL_PUNTOS, size = 10, fontface = "bold",
    fill = alpha("white", 0.8), label.size = 0,
    min.segment.length = 0, segment.colour = COL_PUNTOS
  ) +
  annotation_scale(
    location = "br",
    text_col = COL_PUNTOS,
    line_col = COL_PUNTOS,
    bar_cols = c(COL_PUNTOS, COL_FONDO)
  ) +
  annotation_north_arrow(
    location = "tr",
    style    = north_arrow_minimal(line_col = COL_PUNTOS, text_col = COL_PUNTOS)
  ) +
  coord_sf(
    xlim   = c(bb["xmin"], bb["xmax"]),
    ylim   = c(bb["ymin"], bb["ymax"]),
    expand = FALSE,
    crs    = 4326
  ) +
  theme(
    plot.caption.position = "plot",
    plot.background = element_rect(fill = COL_FONDO, colour = COL_FONDO),
    text         = element_text(colour = COL_PUNTOS),
    axis.text    = element_text(colour = COL_PUNTOS),
    axis.ticks   = element_line(colour = COL_PUNTOS),
    axis.title   = element_blank(),
    panel.grid   = element_line(colour = alpha(COL_PUNTOS, 0.25),
                                linetype = "dotted"),
    plot.caption = element_text(size = 15)
  )

# --- 5. Guardar ----------------------------------------------------------
dir.create(dirname(OUTFILE), showWarnings = FALSE, recursive = TRUE)

ggsave(
  filename = OUTFILE,
  plot     = p,
  device   = "png",
  height   = 7,
  width    = 6,
  dpi      = 300,
  bg       = COL_FONDO
)

p
