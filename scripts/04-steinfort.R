library(tidyverse)
library(pdftools)

pat <- "^\\s*(.+?)\\s+(\\d\\.\\d)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s*$"

steinfort <- pdf_text("data/raw/Steinfort2020.pdf")[6] |>   # p. 95, Table 2
  str_split_1("\n") |>
  str_subset(pat) |>
  str_match(pat) |>
  as_tibble(.name_repair = "minimal") |>
  set_names("raw", "sitio", "pH", "EC", "MO", "N", "P", "K") |>
  transmute(
    sitio = str_remove_all(sitio, "[*+ ]+$"),   # drop footnote markers
    across(c(pH, MO), as.numeric)
  ) |>
  mutate(
    sitio = if_else(duplicated(sitio) | duplicated(sitio, fromLast = TRUE),
                    paste0(sitio, " ", ave(sitio, sitio, FUN = seq_along)), sitio),
    C = MO / 1.724                              # Van Bemmelen
  )

stopifnot(nrow(steinfort) == 37)
stopifnot(abs(mean(steinfort$MO) - 2.4) < 0.05,     # matches paper's summary row
          abs(mean(steinfort$pH) - 7.7) < 0.05)

write_csv(steinfort, "data/processed/steinfort2020_tabla2.csv")

st_long <- steinfort |>
  pivot_longer(c(pH, C), names_to = "variable", values_to = "valor") |>
  mutate(variable = factor(variable, c("pH", "C"), labels = c("pH", "C (%)")))

p <- ggplot(st_long, aes(x = variable, y = valor)) +
  geom_jitter(width = 0.10, alpha = 0.45, size = 2, colour = "grey25") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.15, linewidth = 0.5) +
  stat_summary(fun = mean, geom = "point",
               size = 3.2, shape = 21, fill = "white", stroke = 1) +
  facet_wrap(~ variable, scales = "free") +
  labs(x = NULL, y = NULL,
       title = "Steinfort et al. (2020)") +
  theme_bw(base_size = 24) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.background = element_rect(fill = "grey95", colour = NA))

p
ggsave("plots/steinfort.png", p, width = 11, height = 4, dpi = 300)
