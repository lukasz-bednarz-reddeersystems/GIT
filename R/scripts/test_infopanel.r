setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
#Data
y1 <- round(rnorm(n = 36, mean = 7, sd = 2)) 
y2 <- round(rnorm(n = 36, mean = 21, sd = 6))
y3 <- round(rnorm(n = 36, mean = 50, sd = 8))
x <- rep(LETTERS[1:12], 3)
grp <- rep(c("Grp 1", "Grp 2", "Grp 3"), each = 12)
dat <- data.frame(grp, x, y1, y2, y3)

#Panel components
library(ggplot2)
library(grid)
library(useful)
library(extrafont)
p1 <- ggplot(data = dat, aes(x = x, y = y1)) + geom_bar(stat = "identity", fill = "#552683") +
  coord_flip() + ylab("Y LABEL") + xlab("X LABEL") + facet_grid(. ~ grp) +
  ggtitle("TITLE OF THE FIGURE")

#font_import() # Import all fonts
#fonts() # Print list of all fonts

# Configure Theme
kobe_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "Impact"),
    plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "Impact"),
    panel.grid.major.x = element_line(colour = "#E7A922"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Impact", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}

kobe_theme2 <- function() {
  theme(
    legend.position = "bottom", legend.title = element_text(family = "Impact", colour = "#552683", size = 10),
    legend.background = element_rect(fill = "#E2E2E3"),
    legend.key = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    legend.text = element_text(family = "Impact", colour = "#E7A922", size = 10),
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "Impact"),
    plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "Impact"),
    panel.grid.major.y = element_line(colour = "#E7A922"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Impact", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}

x_id <- rep(12:1, 3) # use this index for reordering the x ticks
p1 <- ggplot(data = dat, aes(x = reorder(x, x_id), y = y1)) + geom_bar(stat = "identity", fill = "#552683") +
  coord_flip() + ylab("Y LABEL") + xlab("X LABEL") + facet_grid(. ~ grp) +
  ggtitle("TITLE OF THE FIGURE") + kobe_theme()

p2 <- ggplot(data = dat, aes(x = x, y = y2, group = factor(grp))) +
  geom_line(stat = "identity", aes(linetype = factor(grp)), size = 0.7, colour = "#552683") +
  ylab("Y LABEL") + xlab("X LABEL") + ggtitle("TITLE OF THE FIGURE") + kobe_theme2() + scale_linetype_discrete("GROUP")

p3 <- ggplot(data = dat, aes(x = reorder(x, rep(1:12, 3)), y = y3, group = factor(grp))) +
  geom_bar(stat = "identity", fill = "#552683") + coord_polar() + facet_grid(. ~ grp) +
  ylab("Y LABEL") + xlab("X LABEL") + ggtitle("TITLE OF THE FIGURE") + kobe_theme2()

png("test_infopanel.png", width = 10, height = 20, units = "in", res = 500)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(4, 3)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("INFOGRAPHIC", y = unit(1, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar(fontfamily = "Impact", col = "#A9A8A7", cex = 12, alpha = 0.3))
grid.text("RProgramming", y = unit(0.94, "npc"), gp = gpar(fontfamily = "Impact", col = "#E7A922", cex = 6.4))
grid.text("BY AL-AHMADGAID B. ASAAD", vjust = 0, y = unit(0.92, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
grid.text("ANALYSIS WITH PROGRAMMING", vjust = 0, y = unit(0.913, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
grid.text("alstatr.blogspot.com", vjust = 0, y = unit(0.906, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
print(p3, vp = vplayout(4, 1:3))
print(p1, vp = vplayout(3, 1:3))
print(p2, vp = vplayout(2, 1:3))
grid.rect(gp = gpar(fill = "#E7A922", col = "#E7A922"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.11, "npc"))
grid.text("CATEGORY", y = unit(0.82, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(fontfamily = "Impact", col = "#CA8B01", cex = 13, alpha = 0.3))
grid.text("A VERY VERY VERY VERY LONG TITLE", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 1.2))
grid.text("DATA INFO", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(fontfamily = "Impact", col = "white", cex = 1.2))
grid.text(paste(
  "Syndicated to",
  "Source",
  "Author",
  "Maintainer",
  "Frequency of Update",
  "Granularity",
  "Temporal Date", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.79, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
grid.text(paste(
  "http://alstatr.blogspot.com",
  "http://alstatr.blogspot.com",
  "Analysis with Programming",
  "Al-Ahmadgaid B. Asaad",
  "Annually",
  "National",
  "2011-2013", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.15, "npc"), y = unit(0.79, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
dev.off()