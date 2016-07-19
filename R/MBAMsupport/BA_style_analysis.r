setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/MBAMsupport")
library(ggplot2)
library(RGraphics)
library(gridExtra)
library(grid)
library(extrafont)
library(useful)

summary_back = 'slategray1'
heading_color = 'slategray4'

#Requries following ggplots objects to be loaded in the workspace
#1. crr_strat     : histogram of strategy return to factor return correlations
#2. rtn_cmp       : timeseries plot factor/portfolio return 
#3. n_ext_trades  : count of extended trades
#4. extension_rtns: average daily return to extended positions

text = paste(capture.output(licence()),collapse=" ")

loadfonts()
pdf("test.pdf", width=7.5, height= 11.2,family="Arial")

grid.newpage() 
pushViewport(viewport(layout = grid.layout(10, 5)))
grid.text("Barry Anten", y = unit(0.5, "npc"), gp = gpar(fontfamily = "Arial", col = heading_color, cex = 1), vp = vplayout(1,1:5))
grid.rect(gp = gpar(fill = summary_back, col = summary_back),vp = vplayout(2:3,1:5))
grid.text("Summary", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.1, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.8),vp = vplayout(1, 1:5))
grid.draw(splitTextGrob(text,vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.8),vp = vplayout(2, 1:5)))

grid.newpage()
pushViewport(viewport(layout = grid.layout(20, 5)))
grid.text("Factor correlation over 2015", y = unit(0.5, "npc"), gp = gpar(fontfamily = "Arial", col = heading_color, cex = 1), vp = vplayout(2,1:5))
print(rtn_cmp, vp = vplayout(4:10, 1:5))
grid.draw(splitTextGrob(text,vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.8),vp = vplayout(11:20, 1:2)))
print(crr_strat, vp = vplayout(11:20, 3:5))

grid.newpage()
pushViewport(viewport(layout = grid.layout(20, 7)))
grid.text("Extended trades over 2015", y = unit(0.5, "npc"), gp = gpar(fontfamily = "Arial", col = heading_color, cex = 1), vp = vplayout(2,1:7))
print(n_buys_sells, vp = vplayout(3:10, 1:7))
print(extension_rtns, vp = vplayout(12:19, 1:7))

dev.off()
