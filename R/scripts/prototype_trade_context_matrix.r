sourceTo("../analysis_modules/analysis_module_resultsdaypsn_traded.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
rep_module <- results_daypsntraded_analysis_module_builder

trader   <- 11
to_date  <- Sys.Date()
key_func <- function(){dated_three_monthly_lookback(trader,as.character(to_date))}

earnings <- createAnalysisModule(rep_module,key_func)
earnings <- updateAnalysisModel(earnings)
earnings <- runAnalysisModule(earnings)
analysis <- earnings

draw_rects <- function(fill_colors,col,text,font=1){
  space    <- 0.025
  t_height <- 0.05
  side  <- length(fill_colors)
  layout(c(1,1))
  plot(c(0, 1), c(0, 1), type="n", axes=FALSE, xlab="",ylab="",main="Results day, position increases")
  bottom_left_x <- ((0:(side-1))/side)
  top_right_y <- ((1:(side))/side)
  for(i in 1:side){
    for(j in 1:side){
      rect(bottom_left_x[i]+(space/4),top_right_y[j]-(1/side)+space/4,bottom_left_x[i]+(1/side)-(space/4),top_right_y[j]-(space/4),col=col[1+round(10*fill_colors[[i]][[j]])])
      cntr <- c(bottom_left_x[i]+(1/(2*side)),top_right_y[j]-(1/(2*side)))
      for(t in 1:length(text[[i]][[j]])){
         text(cntr[1],cntr[2]+(length(text[[i]][[j]])*(t_height/2))-(t-1)*t_height,labels=text[[i]][[j]][[t]],font=font[[((t-1)%%length(font))+1]])
      }
    }
  }
}

#Context matrix for long/short new/old positions increasing.
contexts <- list(list(list(TRUE,TRUE),list(TRUE,FALSE)),list(list(FALSE,TRUE),list(FALSE,FALSE)))
decreases <- FALSE
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
  for(j in 1:length(contexts)){
    df  <- analysis@ppmdl@modeldata@data
    if(decreases){
      access <- (df$PsnLong==contexts[[i]][[j]][[1]])&(!df$Long==contexts[[i]][[j]][[1]])&df$NewPosition==contexts[[i]][[j]][[2]]  
    }
    else{
      access <- (df$PsnLong==contexts[[i]][[j]][[1]])&(df$Long==contexts[[i]][[j]][[1]])&df$NewPosition==contexts[[i]][[j]][[2]]
    }
    sdf <- subset(df,access)
    sdf <- unique(sdf[c('Hit1D','TodayPL','TradeID')])
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
      age <- 'New Position'
    }
    else{
      age <- 'Old Position'
    }
    txt[[j]] <- list(paste(side,", ",age,sep=""),sprintf("%.2f%% hit rate",hit1d[[j]]*100),sprintf("%.2f win loss",wl[[j]]),sprintf("%.2f extraction",ext[[j]]),sprintf("$%d PL",round(pl[[j]])))
  }
  hit_rate[[i]] <- hit1d
  win_loss[[i]] <- wl
  bucket_pl[[i]] <- pl
  extraction[[i]] <- ext
  text [[i]] <- txt
}

glob_min <- min(unlist(extraction))
glob_max <- max(unlist(extraction))
r_cols <- list()
for(i in 1:length(contexts)){
  r_cols[[i]] <- lapply(extraction[[i]],function(x)(x-glob_min)/(glob_max-glob_min))
}
  
draw_rects(r_cols,heat.colors(11),text,font=c(2,1,1,1,1))
