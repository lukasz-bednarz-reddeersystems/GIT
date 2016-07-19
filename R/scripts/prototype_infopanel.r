setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
library(ggplot2)
library(grid)
library(useful)
library(extrafont)
sourceTo("panel_themes.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
#Data
y1 <- round(rnorm(n = 36, mean = 7, sd = 2)) 
y2 <- round(rnorm(n = 36, mean = 21, sd = 6))
y3 <- round(rnorm(n = 36, mean = 50, sd = 8))
x <- rep(LETTERS[1:12], 3)
grp <- rep(c("Grp 1", "Grp 2", "Grp 3"), each = 12)
dat <- data.frame(grp, x, y1, y2, y3)
x_id <- rep(12:1, 3) # use this index for reordering the x ticks

histogram <- function(data,title,x_ticks,y_data,y_label,x_label,group_names,stat="identity",fill="black",vertical=FALSE,theme_fn=NULL,facet_spec=NULL){
  H <- ggplot(data = data, aes(x = x_ticks, y = y_data)) + geom_bar(stat = stat, fill = fill) +
       ylab(y_label) + xlab(x_label) + ggtitle(title) 
  if(length(facet_spec)>0)H <- H + facet_spec
  if(vertical)H <- H + coord_flip()
  if(length(theme_fn)>0)H <- H + theme_fn()
  return(H)
}
p1 <- histogram(dat,"Title",reorder(x, x_id),y1,"Y","X",grp,theme_fn=kobe_theme,facet_spec=facet_grid(.~grp))

multiline_series_plot <- function(data,title,x_ticks,y_data,y_label,x_label,group_names,legend_title,stat="identity",line_size=0.7,colour="black",theme_fn=NULL){
  S <- ggplot(data = data, aes(x = x_ticks, y = y_data, group = factor(group_names))) +
       geom_line(stat = stat, aes(linetype = factor(group_names)), size = line_size, colour = colour) +
       ylab(y_label) + xlab(x_label) + ggtitle(title) + scale_linetype_discrete(legend_title)
  if(length(theme_fn)>0)S <- S + theme_fn()
  return(S)
}
p2 <- multiline_series_plot(dat,"Title",x,y1,"Y","X",grp,"GROUP",theme_fn=kobe_theme2)

radar <- function(data,title,x_ticks,y_data,y_label,x_label,group_names,stat="identity",fill="black",theme_fn=NULL,facet_spec=NULL){
  R <- ggplot(data = data, aes(x = x_ticks, y = y_data, group = factor(group_names))) +
       geom_bar(stat = stat, fill = fill) + coord_polar() +
       ylab(y_label) + xlab(x_label) + ggtitle(title)
  if(length(facet_spec)>0)R <- R + facet_spec
  if(length(theme_fn)>0)R <- R + theme_fn()  
  return(R)
}
p3 <- radar(dat,"Title",reorder(x,rep(1:12, 3)),y3,"Y","X",grp,theme_fn=kobe_theme2,facet_spec=facet_grid(. ~ grp))

#Expect 4 quarters of data:
#Col1 : Quar

loadfonts()
pdf("test_infopanel.pdf", width=7.5, height= 11.2,family="Impact")
grid.newpage() 
pushViewport(viewport(layout = grid.layout(4, 3)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("INFOGRAPHIC", y = unit(1, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar(fontfamily = "Impact", col = "#A9A8A7", cex = 6, alpha = 0.3))
grid.text("RProgramming", y = unit(0.94, "npc"), gp = gpar(fontfamily = "Impact", col = "#E7A922", cex = 3.2))
grid.text("BY AL-AHMADGAID B. ASAAD", vjust = 0, y = unit(0.92, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.4))
grid.text("ANALYSIS WITH PROGRAMMING", vjust = 0, y = unit(0.913, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.4))
grid.text("alstatr.blogspot.com", vjust = 0, y = unit(0.906, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.4))
print(p3, vp = vplayout(4, 1:3))
print(p1, vp = vplayout(3, 1:3))
print(p2, vp = vplayout(2, 1:3))
grid.rect(gp = gpar(fill = "#E7A922", col = "#E7A922"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.11, "npc"))
grid.text("CATEGORY", y = unit(0.82, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(fontfamily = "Impact", col = "#CA8B01", cex = 6.5, alpha = 0.3))
grid.text("A VERY VERY VERY VERY LONG TITLE", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.6))
grid.text("DATA INFO", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(fontfamily = "Impact", col = "white", cex = 0.6))
grid.text(paste(
  "Syndicated to",
  "Source",
  "Author",
  "Maintainer",
  "Frequency of Update",
  "Granularity",
  "Temporal Date", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.79, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.4))
grid.text(paste(
  "http://alstatr.blogspot.com",
  "http://alstatr.blogspot.com",
  "Analysis with Programming",
  "Al-Ahmadgaid B. Asaad",
  "Annually",
  "National",
  "2011-2013", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.15, "npc"), y = unit(0.79, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.4))
dev.off()