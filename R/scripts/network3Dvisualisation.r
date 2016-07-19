library(networkD3)

#Manipulate a correlation matrix so that it can be viewed as a graph in
#networkD3.

factor_visualisation_data <- function(matrix,con_threshold){
  matrix[!upper.tri(matrix)] <- NA
  matrix <- t(matrix)
  market_factors <- c('Value','Strength','Growth','Size','StreetSentiment','PriceMomentum1M','PriceMomentum12M','TrendExtension','Earnings','Volatility')
  fx_factors <- c('JPY','GBP','EUR','CNY','RUB','ZAR','HKD','AUD','DKK','NOK','SEK','CHF','ILS','PLN','HUF','TRY')
  commodities <- c('WTI')
  sectors <- c('SX3P','SX4P','SX6P','SX7P','SX86P','SX8P','SXAP','SXDP','SXEP','SXFP','SX1P','SXKP','SXMP','SXNP','SXOP','SXPP','SXQP','SXRP','SXTP')
  df <- as.data.frame(matrix)
  factors <- colnames(df)
  nfirst <- TRUE
  ntve_net <- NA
  pfirst <- TRUE
  ptve_net <- NA
  first <- TRUE
  for(factor in factors){
    if(factor %in% market_factors){
      grp <- 1
      l <- length(market_factors)
    }
    else if(factor %in% fx_factors){
      grp <- 2
      l <-length(fx_factors)
    }
    else if(factor %in% commodities){
      grp <- 3
      l <- length(commodities)
    }
    else{
      grp <- 4
      l <- length(sectors)
    }
    pv <- 10*df[!is.na(df[factor])&df[factor]>con_threshold,factor]
    if(sum(!is.na(pv))>0){
      ptve_connections <- data.frame(factor,src=which(colnames(df)==factor)-1,tgt=which(df[factor]>con_threshold)-1,value=pv)
      ptve_connections <- ptve_connections[!is.na(ptve_connections$value),]  
      if(pfirst){
        ptve_net <- ptve_connections
        pfirst <- FALSE
      }
      else{
        ptve_net <- rbind(ptve_net,ptve_connections)
      }
    }
    nv <- 10*abs(df[!is.na(df[factor])&df[factor]<(-1*con_threshold),factor])
    if(sum(!is.na(nv))>0){
      ntve_connections <- data.frame(src=which(colnames(df)==factor)-1,tgt=which(df[factor]<(-1*con_threshold))-1,value=nv)
      ntve_connections <- ntve_connections[!is.na(ntve_connections$value),]      
      if(nfirst){
        ntve_net <- ntve_connections
        nfirst <- FALSE
      }
      else{
        ntve_net <- rbind(ntve_net,ntve_connections)
      }
    }
    nodes <- data.frame(name=factor,group=grp,size=1)
    if(first){
      all_nodes <- nodes
      first <- FALSE
    }
    else{
      all_nodes <- rbind(all_nodes,nodes)
    }
  }
  return(list(postive_net=ptve_net,negative_net=ntve_net,nodes=all_nodes))
}
#forceNetwork(Links = cnet[[1]][c('src','tgt','value')], Nodes = cnet[[3]],
#                           Source = "src", Target = "tgt",
#                           Value = "value", NodeID = "name",
#                           Group = "group", opacity = 0.7, zoom=TRUE)

stock_visualisation_data <- function(matrix,con_threshold){
  matrix[!upper.tri(matrix)] <- NA
  matrix <- t(matrix)
  df <- as.data.frame(matrix)
  factors <- colnames(df)
  nfirst <- TRUE
  ntve_net <- NA
  pfirst <- TRUE
  ptve_net <- NA
  first <- TRUE
  for(factor in factors){
    pv <- 10*df[!is.na(df[factor])&df[factor]>con_threshold,factor]
    if(sum(!is.na(pv))>0){
      ptve_connections <- data.frame(factor,src=which(colnames(df)==factor)-1,tgt=which(df[factor]>con_threshold)-1,value=pv)
      ptve_connections <- ptve_connections[!is.na(ptve_connections$value),]  
      if(pfirst){
        ptve_net <- ptve_connections
        pfirst <- FALSE
      }
      else{
        ptve_net <- rbind(ptve_net,ptve_connections)
      }
    }
    nv <- 10*abs(df[!is.na(df[factor])&df[factor]<(-1*con_threshold),factor])
    if(sum(!is.na(nv))>0){
      ntve_connections <- data.frame(src=which(colnames(df)==factor)-1,tgt=which(df[factor]<(-1*con_threshold))-1,value=nv)
      ntve_connections <- ntve_connections[!is.na(ntve_connections$value),]      
      if(nfirst){
        ntve_net <- ntve_connections
        nfirst <- FALSE
      }
      else{
        ntve_net <- rbind(ntve_net,ntve_connections)
      }
    }
    nodes <- data.frame(name=factor,group=1,size=1)
    if(first){
      all_nodes <- nodes
      first <- FALSE
    }
    else{
      all_nodes <- rbind(all_nodes,nodes)
    }
  }
  return(list(postive_net=ptve_net,negative_net=ntve_net,nodes=all_nodes))
}

