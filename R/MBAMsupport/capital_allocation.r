sourceTo("../reporting/raid_data_import.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("risk_model_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)
library(ggplot2)
library(MASS)
#Capital allocation models provide a recommendation for the capital
#division between traders.
#This is categorised as a portfolio computation.
#This model weights capital usage and the Sharpe ratio
#over serveral lookbacks to reweight the current allocation.

#Weightings for the model
wt <- c(0.3,0.7)
wp <- c(0.2,0.2,0.3,0.3)
wf <- rbind(data.frame(quantity= 'cap',
                       tframe  = 1,
                       weight  = wt[1]*wp[1]),
            data.frame(quantity= 'cap',
                       tframe  = 3,
                       weight  = wt[1]*wp[2]),
            data.frame(quantity= 'cap',
                       tframe  = 6,
                       weight  = wt[1]*wp[3]),
            data.frame(quantity= 'cap',
                       tframe  = 12,
                       weight  = wt[1]*wp[4]),
            data.frame(quantity= 'srp',
                       tframe  = 1,
                       weight  = wt[2]*wp[1]),
            data.frame(quantity= 'srp',
                       tframe  = 3,
                       weight  = wt[2]*wp[2]),
            data.frame(quantity= 'srp',
                       tframe  = 6,
                       weight  = wt[2]*wp[3]),
            data.frame(quantity= 'srp',
                       tframe  = 12,
                       weight  = wt[2]*wp[4]))

get_allocmodel_component <- function(performance_frame,component){
  rval <- switch(component,
                 rtn = performance_frame$AnnualisedReturnOnInvested,
                 vol = performance_frame$AnnualisedStdDeviationOfReturns,
                 cap = performance_frame$AverageInvestedUSD,
                 allc= performance_frame$AverageAllocated
  )
  return(rval)
}

vswitch <- function(data){
  if(length(data)==0||is.na(data)){
    rval <- NA
  }
  else{
    rval <- data
  }
  return(rval)
}

scale<-0.1
aum_adj<-0.25
af <- function(x,aa=aum_adj,sc=scale){
  return(aum_adj*scale*x)
}

compute_allocation_increment <- function(wdata,alloc_fn=af){
  alloc <- aggregate(wdata$weighted,list(wdata$trader,wdata$month),function(x)sum(alloc_fn(x),na.rm=TRUE))
  return(alloc)
}

#Simulate trader sharpe ratios as draws from a multidimensional dimensional Guassian.
#Simulate trader capital deployment as an AR1 process based on this
#Based on monthly intervals
simulate_group_perf <- function(months,rtn_means,rtn_vol,rtn_corr,agression,aversion,mean_cap){
  cvr <- as.matrix(rtn_vol)%*%t(as.matrix(rtn_vol))*rtn_corr
  rtns <- mvrnorm(months,rtn_means,cvr) 
  c <- (1-aversion)*mean_cap
  cap <- as.data.frame(t(as.matrix(c + aversion*mean_cap + agression*(rtns[1,]-rtn_means))))
  for(mnth in 2:months){
    rw <- c + aversion*cap[(mnth-1),] + agression*(rtns[1,]-rtn_means)
    cap <- rbind(cap,rw)
  }
  return(list(returns=as.data.frame(rtns),cap=cap))
}

simulate_data <- function(months,traders,rtn_means,rtn_vol,rtn_corr,agression,aversion,mean_cap,base_data=NULL){
  if(length(base_data)==0){
    base_data <- simulate_group_perf(length(months)+12,rtn_means,rtn_vol,rtn_corr,agression,aversion,mean_cap)  
  }
  cap <- base_data$cap
  rtn <- base_data$returns
  first <- TRUE
  for(mnth in 1:length(months)){
    for(tf in c(1,3,6,12)){  
      for(t in 1:length(traders)){
        shrp <- ((prod(rtn[(12-tf+mnth):(12+mnth-1),t]+1)-1)/(rtn_vol[t]*sqrt(tf)))
        cape <- mean(cap[(12-tf+mnth):(12+mnth),t])
        df <- rbind(data.frame(month=months[mnth],quantity='cap',trader=traders[t],tframe=tf,value=cape),
                    data.frame(month=months[mnth],quantity='srp',trader=traders[t],tframe=tf,value=shrp))
        if(first){
          simulated <- df
          first <- FALSE
        } else {
          simulated <- rbind(simulated,df)
        }
      }
    }
  }
  simulated <- merge(simulated,aggregate(simulated$value,list(month=simulated$month,quantity=simulated$quantity,tframe=simulated$tframe),mean),by=c('month','quantity','tframe'))
  simulated$diff <- simulated$value - simulated$x
  colnames(simulated) <- c('month','quantity','tframe','trader','value','group','diff')
  return(simulated)
}

simulate_dynamic <- function(simulation_list,traders=c('A','B','C'),initial=c(1/3,1/3,1/3),n=3,nm=3.6){
  first <- TRUE
  for(swdata in simulation_list){
    salloc_changes <- compute_allocation_increment(swdata)
    colnames(salloc_changes) <- c('trader','month','delta')
    salloc <- salloc_changes
    salloc$delta[salloc$trader==traders[1]] <- initial[1] + cumsum(salloc_changes[salloc_changes$trader==traders[1],]$delta)
    salloc$delta[salloc$trader==traders[2]] <- initial[2] + cumsum(salloc_changes[salloc_changes$trader==traders[2],]$delta)
    salloc$delta[salloc$trader==traders[3]] <- initial[3] + cumsum(salloc_changes[salloc_changes$trader==traders[3],]$delta)
    mnths <- unique(salloc$month)
    salloc$alt_delta[salloc$trader==traders[1]] <- initial[1]
    salloc$alt_delta[salloc$trader==traders[2]] <- initial[2]
    salloc$alt_delta[salloc$trader==traders[3]] <- initial[3]
    for(mdex in 2:length(mnths)){
      m <- mnths[mdex]
      lm<- mnths[mdex-1]
      salloc$alt_delta[salloc$trader==traders[1]&salloc$month==m] <- (1-sum(salloc$alt_delta[salloc$month==lm]))/3 + salloc$alt_delta[salloc$trader==traders[1]&salloc$month==lm] + nm*(((1/3)-salloc$alt_delta[salloc$trader==traders[1]&salloc$month==lm])^n) + salloc_changes[salloc_changes$trader==traders[1]&salloc$month==m,]$delta 
      salloc$alt_delta[salloc$trader==traders[2]&salloc$month==m] <- (1-sum(salloc$alt_delta[salloc$month==lm]))/3 + salloc$alt_delta[salloc$trader==traders[2]&salloc$month==lm] + nm*(((1/3)-salloc$alt_delta[salloc$trader==traders[2]&salloc$month==lm])^n) + salloc_changes[salloc_changes$trader==traders[2]&salloc$month==m,]$delta 
      salloc$alt_delta[salloc$trader==traders[3]&salloc$month==m] <- (1-sum(salloc$alt_delta[salloc$month==lm]))/3 + salloc$alt_delta[salloc$trader==traders[3]&salloc$month==lm] + nm*(((1/3)-salloc$alt_delta[salloc$trader==traders[3]&salloc$month==lm])^n) + salloc_changes[salloc_changes$trader==traders[3]&salloc$month==m,]$delta 
    }
    initial[1] <- salloc$alt_delta[salloc$trader==traders[1]&salloc$month==m]
    initial[2] <- salloc$alt_delta[salloc$trader==traders[2]&salloc$month==m]
    initial[3] <- salloc$alt_delta[salloc$trader==traders[3]&salloc$month==m]
    if(first){
      sim_data <- salloc
      first <- FALSE
    }
    else{
      sim_data <- rbind(sim_data,salloc)
    }
  }
  colnames(sim_data) <- c('trader','month','allocation','alt_allocation')
  return(sim_data)
}

continue_performance_data <- function(new_data,old_data){
  nr <- nrow(new_data$returns)
  new_data$returns[(nr-11):nr,] <- old_data$returns[1:12,]
  new_data$cap[(nr-11):nr,]     <- old_data$cap[1:12,]
  return(new_data)
}

#1. Obtain historical performance data and compute resulting allocations
traders<-c(11,70,101)
start  <- '2015-01-01'
end    <- '2016-05-01'
mnts <- seq(ymd(start),ymd(end),by='months')

first<-TRUE
for(trader in traders){
  for(mdex in 1:length(mnts)){
    m <- as.Date(mnts[mdex])
    for(tframe in c(1,3,6,12)){
      prf<- get_trader_performance_simple(trader,as.Date(m%m-%months(tframe)),m)
      for(qty in c('srp','cap'))
      {
        if(qty=='cap'){
          vlu <- vswitch(get_allocmodel_component(prf,'cap')/get_allocmodel_component(prf,'allc'))
        } else{
          vlu <- vswitch(get_allocmodel_component(prf,'rtn')/get_allocmodel_component(prf,'vol'))
        }
        df <- data.frame(
                         month    = m,
                         trader   = vswitch(prf$Trader),
                         tframe   = tframe,
                         quantity = qty,
                         value    = vlu
                        )
        if(first){
          raw_data <- df
          first <- FALSE
        }
        else{
          raw_data <- rbind(raw_data,df)
        }
      }
    }
  }
}
all_data <- raw_data
group_stats <- aggregate(all_data$value,list(month=all_data$month,quantity=all_data$quantity,tframe=all_data$tframe),function(x)mean(x,na.rm=TRUE))
colnames(group_stats) <- c('month','quantity','tframe','group')
all_data <- merge(all_data,group_stats,by=c('month','quantity','tframe'))
all_data$diff <- all_data$value-all_data$group
colnames(all_data) <- c('month','quantity','tframe','trader','value','group','diff')
wdata <- merge(all_data,wf,by=c('quantity','tframe'))
wdata$weighted <- wdata$diff*wdata$weight

#We are here implicitly assuming that regardless of allocation, the trader would deploy
#the same fraction of capital. This is not true however.
alloc <- simulate_dynamic(list(wdata),traders=c('JS','BA','DK'))
historic_alloc <- ggplot(alloc,aes(x=month,y=allocation,group = trader,colour=trader)) +
                  geom_line(size=1) 
alt_historic_alloc <- ggplot(alloc,aes(x=month,y=alt_allocation,group = trader,colour=trader)) +
  geom_line(size=1) 

#2. Behaviour of the model under stable but fluctuating sharpe
#To add a degree of realism we add the assumption that the trader adjusts their capital deployment
#to follow performance. Hence the capital deployment is modelled as an AR1 process process driven by the 
#trader Sharpe ratio where the timeconstant of adjustment is 1/aversion and the weigthing of the 
#innovation (from the return) is the agression coefficient.
agression <- 1
aversion <- 0.2
mean_cap <- 0.6
rtn_means <- c(0.01,0.01,0.01)
rtn_vol <- c(0.02,0.02,0.02)
rtn_cor <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
months <- as.Date(seq(ymd('2012-01-01'),ymd('2016-04-01'),by='months'))

sm <- simulate_data(months,c('A','B','C'),rtn_means,rtn_vol,rtn_cor,agression,aversion,mean_cap)
swdata <- merge(sm,wf,by=c('quantity','tframe'))
swdata$weighted <- swdata$diff*swdata$weight
salloc <- simulate_dynamic(list(swdata))
simulated_alloc <- ggplot(salloc,aes(x=month,y=allocation,group = trader,colour=trader)) +
  geom_line(size=1) 
alt_simulated_alloc <- ggplot(salloc,aes(x=month,y=alt_allocation,group = trader,colour=trader)) +
  geom_line(size=1) 

#3. Behaviour of the model when sharpe/cap-alloc is shocked (i.e. relaxation behavior)
simulation_list <- list()
agression <- 1
aversion <- 0.2
mean_cap <- 0.6

rtn_means <- c(0.01,0.01,0.01)
rtn_vol <- c(0.02,0.02,0.02)
rtn_cor <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
months <- as.Date(seq(ymd('2000-01-01'),ymd('2004-12-01'),by='months'))

base_data_1 <- simulate_group_perf(length(months)+12,rtn_means,rtn_vol,rtn_cor,agression,aversion,mean_cap)  
sm <- simulate_data(months,c('A','B','C'),rtn_means,rtn_vol,rtn_cor,aggression,aversion,mean_cap,base_data=base_data_1)
swdata <- merge(sm,wf,by=c('quantity','tframe'))
swdata$weighted <- swdata$diff*swdata$weight
simulation_list[[1]] <- swdata

rtn_means <- c(0.02,0.01,0.01)
rtn_vol <- c(0.02,0.02,0.02)
rtn_cor <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
months <- as.Date(seq(ymd('2005-01-01'),ymd('2009-12-01'),by='months'))

base_data_2 <- simulate_group_perf(length(months)+12,rtn_means,rtn_vol,rtn_cor,agression,aversion,mean_cap)  
base_data <- continue_performance_data(base_data_2,base_data_1)
sm <- simulate_data(months,c('A','B','C'),rtn_means,rtn_vol,rtn_cor,aggression,aversion,mean_cap,base_data=base_data)
swdata <- merge(sm,wf,by=c('quantity','tframe'))
swdata$weighted <- swdata$diff*swdata$weight
simulation_list[[2]] <- swdata
output <- simulate_dynamic(simulation_list)

rtn_means <- c(0.01,0.02,0.01)
rtn_vol <- c(0.02,0.02,0.02)
rtn_cor <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
months <- as.Date(seq(ymd('2010-01-01'),ymd('2012-12-01'),by='months'))

base_data_3 <- simulate_group_perf(length(months)+12,rtn_means,rtn_vol,rtn_cor,agression,aversion,mean_cap)  
base_data <- continue_performance_data(base_data_3,base_data_2)
sm <- simulate_data(months,c('A','B','C'),rtn_means,rtn_vol,rtn_cor,aggression,aversion,mean_cap,base_data=base_data)
swdata <- merge(sm,wf,by=c('quantity','tframe'))
swdata$weighted <- swdata$diff*swdata$weight
simulation_list[[3]] <- swdata
output <- simulate_dynamic(simulation_list)

rtn_means <- c(0.01,0.01,0.02)
rtn_vol <- c(0.02,0.02,0.02)
rtn_cor <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
months <- as.Date(seq(ymd('2013-01-01'),ymd('2016-12-01'),by='months'))

base_data_4 <- simulate_group_perf(length(months)+12,rtn_means,rtn_vol,rtn_cor,agression,aversion,mean_cap)  
base_data <- continue_performance_data(base_data_4,base_data_3)
sm <- simulate_data(months,c('A','B','C'),rtn_means,rtn_vol,rtn_cor,aggression,aversion,mean_cap,base_data=base_data)
swdata <- merge(sm,wf,by=c('quantity','tframe'))
swdata$weighted <- swdata$diff*swdata$weight
simulation_list[[4]] <- swdata
output <- simulate_dynamic(simulation_list)

return_shock_alloc <- ggplot(output,aes(x=month,y=alt_allocation,group = trader,colour=trader)) +
  labs(colour="Manager") + ggtitle("Capital allocation to simulated performance") + labs(x="Month") + labs(y="Allocation (fraction of AUM)") +
  theme(axis.title = element_text(size=17))+
  theme(plot.title = element_text(face="bold", size=20))+
  geom_line(size=1) 

