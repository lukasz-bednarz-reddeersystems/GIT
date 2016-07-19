setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(TeachingDemos)

rep_module <- history_analysis_module_builder

trader   <- 11
to_date  <- Sys.Date()
key_func <- function(){dated_three_monthly_lookback(trader,as.character(to_date))}
analysis <- createAnalysisModule(rep_module,key_func)
analysis <- updateAnalysisModel(analysis)
df  <- subset(analysis@ppmdl@modeldata@data,analysis@ppmdl@modeldata@data$Age<30)
df  <- subset(df,!is.na(df$TradeID))
df  <- unique(df[c('TradeID','Instrument','TradeDate','PsnLong','Long','MarketValue','InitialValue','PtvePnLOutof','Hit1D','TodayPL')])

draw_rects <- function(fill_colors,col,text,font=1,fscale=1){
  space    <- 0.025
  t_height <- 0.05
  side_1  <- length(fill_colors)
  side_2  <- length(fill_colors[[1]])
  layout(c(1,1))
  plot(c(0, 1), c(0, 1), type="n", axes=FALSE, xlab="",ylab="",main="Positions newer than 30d")
  bottom_left_x <- ((0:(side_1-1))/side_1)
  top_right_y <- ((1:(side_2))/side_2)
  for(i in 1:side_1){
    for(j in 1:side_2){
      rect(bottom_left_x[i]+(space/4),top_right_y[j]-(1/side_2)+space/4,bottom_left_x[i]+(1/side_1)-(space/4),top_right_y[j]-(space/4),col=col[1+round(10*fill_colors[[i]][[j]])])
      cntr <- c(bottom_left_x[i]+(1/(2*side_1)),top_right_y[j]-(1/(2*side_2)))
      for(t in 1:length(text[[i]][[j]])){
         text(cntr[1],cntr[2]+(length(text[[i]][[j]])*(t_height/2))-(t-1)*t_height-space,labels=text[[i]][[j]][[t]],font=font[[((t-1)%%length(font))+1]],cex=fscale)
      }
    }
  }
}

#Context matrix for long/short,  offside/onside, loosing/winning trades.
contexts <- list(list(list(TRUE,TRUE,TRUE),list(TRUE,TRUE,FALSE),list(TRUE,FALSE,TRUE),list(TRUE,FALSE,FALSE)),list(list(FALSE,TRUE,TRUE),list(FALSE,TRUE,FALSE),list(FALSE,FALSE,TRUE),list(FALSE,FALSE,FALSE)))
text <- list()
hit_rate   <- list()
win_loss   <- list()
bucket_pl  <- list()
extraction <- list()
for(i in 1:length(contexts)){
  hit1d <- list()
  wl <- list()
  ext <- list()
  pl <- list()
  txt <- list()
  for(j in 1:length(contexts[[i]])){
    access <- df$PsnLong==contexts[[i]][[j]][[1]]&(abs(df$MarketValue)<abs(df$InitialValue))==contexts[[i]][[j]][[2]]&df$PtvePnLOutof==contexts[[i]][[j]][[3]]  
    sdf <- subset(df,access)
    #sdf <- unique(sdf[c('Hit1D','TodayPL','TradeID')])
    hit1d[[j]] <- mean(sdf$Hit1D,na.rm=TRUE)
    avg_loss<- mean(sdf$TodayPL[sdf$TodayPL<0],na.rm=TRUE)
    avg_win <- mean(sdf$TodayPL[sdf$TodayPL>0],na.rm=TRUE)
    wl[[j]] <- avg_win/abs(avg_loss)
    ext[[j]] <- wl[[j]]*hit1d[[j]]
    pl[[j]] <- sum(sdf$TodayPL,na.rm=TRUE)
    if(contexts[[i]][[j]][[1]]){
      side <- 'Long'
    }
    else{
      side <- 'Short'
    }
    if(contexts[[i]][[j]][[2]]){
      ons <- 'Off side'
    }
    else{
      ons <- 'Onside'
    }
    if(contexts[[i]][[j]][[3]]){
      losses <- 'No Loosing trades'
    }
    else{
      losses <- 'Loosing trades'
    }
    txt[[j]] <- list(paste(side,", ",ons,", ",losses,sep=""),sprintf("%.2f%% hit rate",hit1d[[j]]*100),sprintf("%.2f win loss",wl[[j]]),sprintf("%.2f extraction",ext[[j]]),sprintf("$%d PL",round(pl[[j]])))
  }
  hit_rate[[i]] <- hit1d
  win_loss[[i]] <- wl
  bucket_pl[[i]] <- pl
  extraction[[i]] <- ext
  text [[i]] <- txt
}

glob_min <- min(unlist(extraction),na.rm=TRUE)
glob_max <- max(unlist(extraction),na.rm=TRUE)
r_cols <- list()
for(i in 1:length(contexts)){
  r_cols[[i]] <- lapply(extraction[[i]],function(x)(x-glob_min)/(glob_max-glob_min))
}
  
draw_rects(r_cols,heat.colors(11),text,font=c(2,1,1,1,2),fscale=0.75)
