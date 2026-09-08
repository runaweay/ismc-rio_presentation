library(tidyverse)

pal <- c("PAV" = "#C1502E", "Non-PAV" = "#2E6C8E")

sl <- suelos_long |>
  filter(variable != "limo") |>
  mutate(
    grupo = factor(if_else(str_starts(as.character(id), "PAV"), "PAV", "Non-PAV"),
                   levels = c("PAV", "Non-PAV")),
    prof = fct_relevel(prof, "0-10", "10-20", "20-30"),
    variable = factor(variable,
                      levels = c("pH", "C", "arena", "arcilla"),
                      labels = c("pH", "C (%)", "Sand (%)", "Clay (%)"))
  )

pd <- position_dodge(width = 0.5)

p <- ggplot(sl, aes(prof, valor, colour = grupo)) +
  geom_point(alpha = 0.35, size = 1.8, shape = 16,
             position = position_jitterdodge(jitter.width = 0.08,
                                             dodge.width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = 0.14, linewidth = 0.5, position = pd) +
  stat_summary(fun = mean, geom = "point",
               size = 3, shape = 21, fill = "white", stroke = 1, position = pd) +
  facet_wrap(~ variable, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = pal) +
  labs(x = "Depth (cm)", y = NULL, colour = NULL) +
  theme_bw(base_size = 25) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey95", colour = NA),
        legend.position = "bottom",
        panel.spacing = unit(2.5, "lines"))   # aumenta o espaço entre os painéis

p
ggsave("plots/dotplot_mean_se.png", p, width = 16, height = 4.5, dpi = 300)
