sourceTo("risk_model_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(ggplot2)
library(grid)
library(useful)

trader <- 70

rm_date_start  <- '2016-01-01'
rm_date_end    <- '2016-04-25'
hedge_strats <- c("BA_SHEDGE")

history_data <- build_portfolio_history(trader,rm_date_start,rm_date_end)
history_data <- rbind(history_data,cbind(Strategy='HEDGE',history_data[unlist(Map(function(x)x%in%hedge_strats,history_data$Strategy)),setdiff(colnames(history_data),'Strategy')]))
history_data <- rbind(history_data,cbind(Strategy='NONHEDGE',history_data[!unlist(Map(function(x)x%in%hedge_strats,history_data$Strategy)),setdiff(colnames(history_data),'Strategy')]))
colnames(history_data)[colnames(history_data)=='InstrumentID'] <- 'Instrument'
history_data <- history_data[c('Strategy','Trader','Instrument','Date','Weight')]
strats <- unique(history_data$Strategy)

first <- TRUE
for(d in 1:(as.Date(rm_date_end)-as.Date(rm_date_start))){
  rm_date <- ymd(rm_date_start) %m+% days(d)
  rm_date <- as.Date(rm_date)
  if(wday(rm_date)!=7&wday(rm_date)!=1){
    for(s in strats){
      hedge_ins <- history_data[history_data$Strategy==s,]
      hedge_pfo <- hedge_ins[hedge_ins$Date==rm_date,]
      if(nrow(hedge_pfo)>0){
        hedge_pfo <- hedge_pfo[c('Instrument','Weight')]
        pfo_betas <- merge(hedge_pfo,betas[betas$Date==rm_date,2:ncol(betas)],by=c('Instrument'))
        pfo_betas <- t(pfo_betas[,2])%*%as.matrix(pfo_betas[,3:ncol(pfo_betas)])
        fr <- implied_fct_rtns[implied_fct_rtns$Date==rm_date,2:ncol(implied_fct_rtns)]
        rtns <- pfo_betas*fr
        if(first){
          pfo       <- cbind(Strategy=s,Date=rm_date,hedge_pfo)
          all_betas <- cbind(Strategy=s,Date=rm_date,as.data.frame(pfo_betas))
          all_rtns  <- cbind(Strategy=s,Date=rm_date,as.data.frame(rtns))
          first <- FALSE
        }
        else{
          pfo       <- rbind(pfo,cbind(Strategy=s,Date=rm_date,hedge_pfo))
          all_betas <- rbind(all_betas,cbind(Strategy=s,Date=rm_date,as.data.frame(pfo_betas)))
          all_rtns  <- rbind(all_rtns,cbind(Strategy=s,Date=rm_date,as.data.frame(rtns)))
        }
       }
     }
   }
}

all_rtns <- all_rtns[order(all_rtns$Date),]
all_betas <- all_betas[order(all_betas$Date),]
all_rtns[is.na(all_rtns)] <- 0
rtypes <- c('FX','Commodity','Factor','Sector')

llim = list(FX=11,Commodity=27,Factor=1,Sector=28)
ulim = list(FX=26,Commodity=27,Factor=10,Sector=45)

first <- TRUE
for(s in strats){
  dtr  <- all_betas[all_betas$Strategy==s,] 
  rn   <- all_rtns[all_rtns$Strategy==s,] 
  for(type in rtypes){
    for(fct in colnames(pfo_betas)[llim[[type]]:ulim[[type]]]){
      b <- cbind(Strategy=s,Quantity=fct,Type=type,data.frame(Date=dtr$Date,Value=dtr[[fct]]))
      r <- cbind(Strategy=s,Quantity=fct,Type=type,data.frame(Date=rn$Date,Value=exp(cumsum(rn[[fct]]))))
      w <- cbind(Strategy=s,Quantity=fct,Type=type,data.frame(Date=rn$Date,Value=rn[[fct]]))
      if(first){
        first <- FALSE
        beta_plt_frame <- b
        rtn_plt_frame  <- r
        raw_rtn_frame  <- w
      }
      else{
        beta_plt_frame <- rbind(beta_plt_frame,b)
        rtn_plt_frame  <- rbind(rtn_plt_frame,r)
        raw_rtn_frame  <- rbind(raw_rtn_frame,w)
      }
    } 
  }
}

rtn_plts <- list()
for(s in strats){
  typ_plts <- list()
  for(t in rtypes){
    typ_plts[[t]] <- ggplot(rtn_plt_frame[rtn_plt_frame$Strategy==s&rtn_plt_frame$Type==t,],aes(x=Date,y=Value,group=Strategy,colour=Strategy)) +
      geom_line(size=1) +
      labs(colour=t) +
      facet_grid(Quantity~Strategy,scales="free_y") 
  }
  rtn_plts[[s]] <- typ_plts
}

beta_plts <- list()
for(s in strats){
  typ_plts <- list()
  for(t in rtypes){
    typ_plts[[t]] <- ggplot(beta_plt_frame[beta_plt_frame$Strategy==s&beta_plt_frame$Type==t,],aes(x=Date,y=Value,group=Strategy,colour=Strategy)) +
      geom_line(size=1) +
      labs(colour=t) +
      facet_grid(Quantity~Strategy,scales="free_y")

  }
  beta_plts[[s]] <- typ_plts
}

mnthly_beta_hdg <- beta_plt_frame[beta_plt_frame$Strategy=='HEDGE'|beta_plt_frame$Strategy=='NONHEDGE',]
mnthly_beta_hdg <- mnthly_beta_hdg[order(mnthly_beta_hdg$Date),]
mnthly_beta_hdg$Month <- format(mnthly_beta_hdg$Date,'%Y-%m')
mnthly_beta_hdg[is.na(mnthly_beta_hdg)] <- 0
mnthly_beta_hdg <- aggregate(mnthly_beta_hdg$Value,list(Month=mnthly_beta_hdg$Month,Strategy=mnthly_beta_hdg$Strategy,Type=mnthly_beta_hdg$Type,Quantity=mnthly_beta_hdg$Quantity),function(x)(prod(1+x)^(1/length(x)))-1)
typ_plts <- list()
for(t in rtypes){
  typ_plts[[t]] <- ggplot(mnthly_beta_hdg[mnthly_beta_hdg$Type==t,],aes(x=Quantity,fill=Strategy)) +
    geom_bar(aes(weight=x),position="dodge") +
    labs(fill='Strategy Group') + ylab("Factor") + xlab("Beta") +
    coord_flip() +
    facet_wrap(~Month,ncol=3) 
}

rtn_typ_plts <- list()
pfo_rtn_plts <- list()
for(t in rtypes){
  first <- TRUE
  for(fct in colnames(implied_fct_rtns)[(llim[[t]]+1):(ulim[[t]]+1)]){
    df <- cbind(Type=t,Factor=fct,data.frame(Date=implied_fct_rtns$Date,Value=exp(cumsum(implied_fct_rtns[[c(fct)]]))))
    if(first){
      plt_frame <- df
      first <- FALSE
    }
    else{
      plt_frame <- rbind(plt_frame,df)  
    }
  }
  rtn_typ_plts[[t]] <- ggplot(plt_frame,aes(x=Date,y=Value,group=Factor,colour=Factor)) +
                       geom_line(size=1) + ylab("Factor Value") + xlab("")
  pfo_rtn_plts[[t]] <- ggplot(rtn_plt_frame[(rtn_plt_frame$Strategy=='HEDGE'|rtn_plt_frame$Strategy=='NONHEDGE')&rtn_plt_frame$Type==t,],aes(x=Date,y=Value,group=Quantity,colour=Quantity)) +
                       geom_line(size=1) +
                       labs(colour="Strategy Group") +
                       facet_grid(Strategy~.,scales="free_y")
}

for(t in rtypes){
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(3, 1)))
  print(rtn_typ_plts[[t]], vp = vplayout(1, 1))
  print(pfo_rtn_plts[[t]], vp = vplayout(2, 1))
  print(typ_plts[[t]], vp = vplayout(3, 1))
}

#Overall residual betas and return
overall_return <- merge(rtn_plt_frame[rtn_plt_frame$Strategy=='HEDGE',],rtn_plt_frame[rtn_plt_frame$Strategy=='NONHEDGE',],by=c('Quantity','Type','Date'))
overall_return$Value <- 1+(overall_return$Value.x-overall_return$Value.y)
overall_return <- overall_return[c('Quantity','Type','Date','Value')]

overall_mnthly_return <- merge(raw_rtn_frame[raw_rtn_frame$Strategy=='HEDGE',],raw_rtn_frame[raw_rtn_frame$Strategy=='NONHEDGE',],by=c('Quantity','Type','Date'))
overall_mnthly_return$Value.x <- exp(overall_mnthly_return$Value.x)
overall_mnthly_return$Value.y <- exp(overall_mnthly_return$Value.y)
overall_mnthly_return$Value <- 1+(overall_mnthly_return$Value.x-1)-(overall_mnthly_return$Value.y-1)
overall_mnthly_return$Month <- format(overall_mnthly_return$Date,'%Y-%m')
overall_mnthly_return <- aggregate(overall_mnthly_return[c('Value','Value.x','Value.y')],list(Month=overall_mnthly_return$Month,Type=overall_mnthly_return$Type,Quantity=overall_mnthly_return$Quantity),function(x)prod(x))
overall_mnthly_return$HedgeScore <- log((abs(overall_mnthly_return$Value.x-1)+abs(overall_mnthly_return$Value.y-1))/abs(overall_mnthly_return$Value-1))
overall_mnthly_return <- overall_mnthly_return[c('Quantity','Type','Month','Value','HedgeScore')]
overall_mnthly_return$Value <- 10000*(overall_mnthly_return$Value-1)

ovrll_rtn_plts <- list()
resid_rtn_plts <- list()
hedge_scr_plts <- list()
for(t in rtypes){
  ovrll_rtn_plts[[t]] <- ggplot(overall_return[overall_return$Type==t,],aes(x=Date,y=Value,group=Quantity,colour=Quantity)) +
    geom_line(size=1) +
    labs(colour="Factor") 
  resid_rtn_plts[[t]] <- ggplot(overall_mnthly_return[overall_mnthly_return$Type==t,],aes(x=reorder(Quantity,Value),fill=reorder(Quantity,Value))) +
    geom_bar(aes(weight=Value)) +
    labs(fill='Factor') + ylab("Return") + xlab("Factor") +
    coord_flip() +
    facet_wrap(~Month,ncol=3) 
  hedge_scr_plts[[t]] <- ggplot(overall_mnthly_return[overall_mnthly_return$Type==t,],aes(x=reorder(Quantity,Value),fill=reorder(Quantity,Value))) +
    geom_bar(aes(weight=HedgeScore)) +
    labs(fill='Factor') + ylab("HedgeScore") + xlab("Factor") +
    coord_flip() +
    facet_wrap(~Month,ncol=3) 
}

overall_beta <- merge(beta_plt_frame[beta_plt_frame$Strategy=='HEDGE',],beta_plt_frame[beta_plt_frame$Strategy=='NONHEDGE',],by=c('Quantity','Type','Date'))
overall_beta$Value <- (overall_beta$Value.x+overall_beta$Value.y)
mnthly_beta <- overall_beta[order(overall_beta$Date),]
mnthly_beta$Month <- format(mnthly_beta$Date,'%Y-%m')
mnthly_beta[is.na(mnthly_beta)] <- 0
mnthly_beta <- aggregate(mnthly_beta[c('Value','Value.x','Value.y')],list(Month=mnthly_beta$Month,Type=mnthly_beta$Type,Quantity=mnthly_beta$Quantity),function(x)(prod(1+x)^(1/length(x)))-1)
mnthly_beta$HedgeScore <- log((abs(mnthly_beta$Value.x)+abs(mnthly_beta$Value.y))/abs(mnthly_beta$Value))
mnthly_beta <- mnthly_beta[c('Quantity','Type','Month','Value','HedgeScore')]

ovrll_beta_plts <- list()
ovrll_beta_scrs <- list()
for(t in rtypes){
  ovrll_beta_plts[[t]] <- ggplot(mnthly_beta[mnthly_beta$Type==t,],aes(x=reorder(Quantity,Value),fill=reorder(Quantity,Value))) +
    geom_bar(aes(weight=Value)) +
    labs(fill='Factor') + ylab("Factor") + xlab("Beta") +
    coord_flip() +
    facet_wrap(~Month,ncol=3) 
  ovrll_beta_scrs[[t]] <- ggplot(mnthly_beta[mnthly_beta$Type==t,],aes(x=reorder(Quantity,Value),fill=reorder(Quantity,Value))) +
    geom_bar(aes(weight=HedgeScore)) +
    labs(fill='Factor') + ylab("Factor") + xlab("Hedge Score") +
    coord_flip() +
    facet_wrap(~Month,ncol=3) 
}

#Plot beta scores
for(t in rtypes){
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(2, 1)))
  print(ovrll_beta_plts[[t]], vp = vplayout(1, 1))
  print(ovrll_beta_scrs[[t]], vp = vplayout(2, 1))
}
#Plot returns scores
for(t in rtypes){
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(2, 1)))
  print(resid_rtn_plts[[t]], vp = vplayout(1, 1))
  print(hedge_scr_plts[[t]], vp = vplayout(2, 1))
}

