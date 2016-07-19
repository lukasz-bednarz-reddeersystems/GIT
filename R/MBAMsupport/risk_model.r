#setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
#sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
#sourceTo("../reporting/raid_data_import.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("hedging_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)
library(quantmod)
library(ggplot2)
library(useful)
library(grid)

rm_date_start = '2015-10-01'
rm_date_end   = '2015-12-31'
lookback      = 150

lback = ymd(rm_date_start) %m-% days(lookback)
all_stk <- get_region_stock_returns(as.Date(lback),rm_date_end,c(3))
all_fct <- get_risk_factor_returns(as.Date(lback),rm_date_end)
all_fct <- pivot_frame(all_fct,'FactorName','Return','Date')
all_fx  <- get_FX_returns(as.Date(lback),rm_date_end)
all_oil <- get_commodity_returns(as.Date(lback),rm_date_end)
all_sct <- unique(get_sector_returns(as.Date(lback),rm_date_end))
all_sct <- pivot_frame(all_sct,'FactorName','Return','Date')

cl <- declare_local_cluster(5)
prepare_cluster(cl)

first <- TRUE
for(d in 1:(as.Date(rm_date_end)-as.Date(rm_date_start))){
  rm_date <- ymd(rm_date_start) %m+% days(d-1)
  rm_date <- as.Date(rm_date)
  if(wday(rm_date)!=7&wday(rm_date)!=1){
    message(paste("Computing:",rm_date))
    lback = ymd(rm_date) %m-% days(150)
    lback <- as.Date(lback)
    stk <- all_stk[all_stk$Date>=lback&all_stk$Date<=rm_date,]
    fct <- all_fct[all_fct$Date>=lback&all_fct$Date<=rm_date,]
    fx  <- all_fx[all_fx$Date>=lback&all_fx$Date<=rm_date,]
    oil <- all_oil[all_oil$Date>=lback&all_oil$Date<=rm_date,]
    sct <- all_sct[all_sct$Date>=lback&all_sct$Date<=rm_date,]
    beta_fct_frame <- merge(fct,fx,by='Date')
    beta_fct_frame <- merge(beta_fct_frame,oil,by='Date')
    beta_fct_frame <- merge(beta_fct_frame,sct,by='Date')
    message("Computing betas ...")
    universe_betas <- stock_betas(stk,beta_fct_frame,cl)
    message("Building composite regression model ...")
    ir <- tryCatch({
                    composite_model(stk[stk$Date==rm_date,],list(universe_betas))   
                   },error=function(cond){
                    message(paste("Factor block regression failed:",cond)) 
                   })
    if(length(ir)>0){
      if(first){
        implied_fct_rtns <- cbind(Date=rm_date,ir[[1]])
        betas <- cbind(Date=rm_date,universe_betas)
        first <- FALSE
      }
      else{
        implied_fct_rtns <- rbind(implied_fct_rtns,cbind(Date=rm_date,ir[[1]]))
        betas <- rbind(betas,cbind(Date=rm_date,universe_betas))
      } 
    }
  }
}

stopCluster(cl)
save.image(paste("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/MBAMsupport/risk_model_",rm_date_start,"_",rm_date_end,".RData",sep=""))
# hedge_data <- unique(history_data[c('Strategy','TradeDate','Instrument','MarketValue','TodayPL','MarketRelPL','Name')])
# colnames(hedge_data)[colnames(hedge_data)=='TradeDate'] <- 'Date'
# hedge_values <- aggregate(hedge_data[c('TodayPL','MarketValue','MarketRelPL')],list(Date=hedge_data$Date),function(x)sum(abs(x),na.rm=TRUE))
# hedge_data <- rbind(hedge_data,cbind(Strategy='HEDGE',hedge_data[unlist(Map(function(x)x%in%hedge_strats,hedge_data$Strategy)),c('Date','Instrument','MarketValue','TodayPL','MarketRelPL','Name')]))
# hedge_data <- rbind(hedge_data,cbind(Strategy='NONHEDGE',hedge_data[!unlist(Map(function(x)x%in%hedge_strats,hedge_data$Strategy)),c('Date','Instrument','MarketValue','TodayPL','MarketRelPL','Name')]))
# strats <- unique(hedge_data$Strategy)

# first <- TRUE
# for(d in 1:(as.Date(rm_date_end)-as.Date(rm_date_start))){
#   rm_date <- ymd(rm_date_start) %m+% days(d)
#   rm_date <- as.Date(rm_date)
#   if(wday(rm_date)!=7&wday(rm_date)!=1){
#     hedge_ttls <- hedge_values[hedge_values$Date==rm_date,]
#     for(s in strats){
#       hedge_ins <- hedge_data[hedge_data$Strategy==s,]
#       hedge_pfo <- hedge_ins[hedge_ins$Date==rm_date,]
#       if(nrow(hedge_pfo)>0){
#         hedge_pfo <- merge(hedge_pfo,hedge_ttls[c('Date','MarketValue')],by=c('Date'))
#         hedge_pfo$Weight <- hedge_pfo$MarketValue.x/hedge_pfo$MarketValue.y
#         hedge_pfo <- hedge_pfo[c('Instrument','Weight')]
#         pfo_betas <- merge(hedge_pfo,betas[betas$Date==rm_date,2:ncol(betas)],by=c('Instrument'))
#         pfo_betas <- t(pfo_betas[,2])%*%as.matrix(pfo_betas[,3:ncol(pfo_betas)])
#         fr <- implied_fct_rtns[implied_fct_rtns$Date==rm_date,2:ncol(implied_fct_rtns)]
#         rtns <- pfo_betas*fr
#         if(first){
#           pfo       <- cbind(Strategy=s,Date=rm_date,hedge_pfo)
#           all_betas <- cbind(Strategy=s,Date=rm_date,as.data.frame(pfo_betas))
#           all_rtns  <- cbind(Strategy=s,Date=rm_date,as.data.frame(rtns))
#           first <- FALSE
#         }
#         else{
#           pfo       <- rbind(pfo,cbind(Strategy=s,Date=rm_date,hedge_pfo))
#           all_betas <- rbind(all_betas,cbind(Strategy=s,Date=rm_date,as.data.frame(pfo_betas)))
#           all_rtns  <- rbind(all_rtns,cbind(Strategy=s,Date=rm_date,as.data.frame(rtns)))
#         }
#        }
#      }
#    }
# }

# all_rtns <- all_rtns[order(all_rtns$Date),]
# all_betas <- all_betas[order(all_betas$Date),]
# all_rtns[is.na(all_rtns)] <- 0
# rtypes <- c('FX','Commodity','Factor','Sector')

# llim = list(FX=1,Commodity=17,Factor=18,Sector=28)
# ulim = list(FX=16,Commodity=17,Factor=27,Sector=45)

# first <- TRUE
# for(s in strats){
#   dtr  <- all_betas[all_betas$Strategy==s,] 
#   rn   <- all_rtns[all_rtns$Strategy==s,] 
#   for(type in rtypes){
#     for(fct in colnames(pfo_betas)[llim[[type]]:ulim[[type]]]){
#       b <- cbind(Strategy=s,Quantity=fct,Type=type,data.frame(Date=dtr$Date,Value=dtr[[fct]]))
#       r <- cbind(Strategy=s,Quantity=fct,Type=type,data.frame(Date=rn$Date,Value=exp(cumsum(rn[[fct]]))))
#       if(first){
#         first <- FALSE
#         beta_plt_frame <- b
#         rtn_plt_frame  <- r
#       }
#       else{
#         beta_plt_frame <- rbind(beta_plt_frame,b)
#         rtn_plt_frame  <- rbind(rtn_plt_frame,r)
#       }
#     } 
#   }
# }

# rtn_plts <- list()
# for(s in strats){
#   typ_plts <- list()
#   for(t in rtypes){
#     typ_plts[[t]] <- ggplot(rtn_plt_frame[rtn_plt_frame$Strategy==s&rtn_plt_frame$Type==t,],aes(x=Date,y=Value,group=Strategy,colour=Strategy)) +
#       geom_line(size=1) +
#       labs(colour=t) +
#       facet_grid(Quantity~Strategy,scales="free_y") 
#   }
#   rtn_plts[[s]] <- typ_plts
# }

# beta_plts <- list()
# for(s in strats){
#   typ_plts <- list()
#   for(t in rtypes){
#     typ_plts[[t]] <- ggplot(beta_plt_frame[beta_plt_frame$Strategy==s&beta_plt_frame$Type==t,],aes(x=Date,y=Value,group=Strategy,colour=Strategy)) +
#       geom_line(size=1) +
#       labs(colour=t) +
#       facet_grid(Quantity~Strategy,scales="free_y")
    
#   }
#   beta_plts[[s]] <- typ_plts
# }

# mnthly_beta_hdg <- beta_plt_frame[beta_plt_frame$Strategy=='HEDGE'|beta_plt_frame$Strategy=='NONHEDGE',]
# mnthly_beta_hdg <- mnthly_beta_hdg[order(mnthly_beta_hdg$Date),]
# mnthly_beta_hdg$Month <- format(mnthly_beta_hdg$Date,'%Y-%m')
# mnthly_beta_hdg[is.na(mnthly_beta_hdg)] <- 0
# mnthly_beta_hdg <- aggregate(mnthly_beta_hdg$Value,list(Month=mnthly_beta_hdg$Month,Strategy=mnthly_beta_hdg$Strategy,Type=mnthly_beta_hdg$Type,Quantity=mnthly_beta_hdg$Quantity),function(x)(prod(1+x)^(1/length(x)))-1)
# typ_plts <- list()
# for(t in rtypes){
#   typ_plts[[t]] <- ggplot(mnthly_beta_hdg[mnthly_beta_hdg$Type==t,],aes(x=Quantity,fill=Strategy)) +
#     geom_bar(aes(weight=x),position="dodge") +
#     labs(fill='Strategy Group') + ylab("Factor") + xlab("Beta") +
#     coord_flip() +
#     facet_wrap(~Month,ncol=3) 
# }

# rtn_typ_plts <- list()
# pfo_rtn_plts <- list()
# for(t in rtypes){
#   first <- TRUE
#   for(fct in colnames(implied_fct_rtns)[(llim[[t]]+1):(ulim[[t]]+1)]){
#     df <- cbind(Type=t,Factor=fct,data.frame(Date=implied_fct_rtns$Date,Value=exp(cumsum(implied_fct_rtns[[c(fct)]]))))
#     if(first){
#       plt_frame <- df
#       first <- FALSE
#     }
#     else{
#       plt_frame <- rbind(plt_frame,df)  
#     }
#   }
#   rtn_typ_plts[[t]] <- ggplot(plt_frame,aes(x=Date,y=Value,group=Factor,colour=Factor)) +
#                        geom_line(size=1) + ylab("Factor Value") + xlab("")
#   pfo_rtn_plts[[t]] <- ggplot(rtn_plt_frame[(rtn_plt_frame$Strategy=='HEDGE'|rtn_plt_frame$Strategy=='NONHEDGE')&rtn_plt_frame$Type==t,],aes(x=Date,y=Value,group=Quantity,colour=Quantity)) +
#                        geom_line(size=1) +
#                        labs(colour="Strategy Group") +
#                        facet_grid(Strategy~.,scales="free_y")
# }

# for(t in rtypes){
#   grid.newpage() 
#   pushViewport(viewport(layout = grid.layout(2, 1)))
#   print(rtn_typ_plts[[t]], vp = vplayout(1, 1))
#   #print(pfo_rtn_plts[[t]], vp = vplayout(2, 1))
#   print(typ_plts[[t]], vp = vplayout(2, 1))
# }
