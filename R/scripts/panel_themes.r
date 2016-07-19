kobe_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "Arial"),
    plot.title = element_text(colour = "#552683", face = "bold", size = 10, vjust = 1, family = "Arial"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 7, family = "Arial"),
    panel.grid.major.x = element_line(colour = "#E7A922"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Arial", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}
kobe_theme2 <- function() {
  theme(
    legend.position = "bottom", legend.title = element_text(family = "Arial", colour = "#552683", size = 7),
    legend.background = element_rect(fill = "#E2E2E3"),
    legend.key = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    legend.text = element_text(family = "Arial", colour = "#E7A922", size = 7),
    legend.title = element_text(family = "Arial", size = 7),
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "Arial", size = 7),
    plot.title = element_text(colour = "#552683", face = "bold", size = 10, vjust = 1, family = "Arial"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 10, family = "Arial"),
    panel.grid.major.y = element_line(colour = "#E7A922"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Arial", colour = "white",size = 5),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}