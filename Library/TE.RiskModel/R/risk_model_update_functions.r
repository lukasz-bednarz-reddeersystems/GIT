#' @include risk_model_load.r
NULL

.__DEFAULT_RISK_MODEL_NCORES__ <- as.integer(Sys.getenv("NUMBER_OF_PROCESSORS", 5L)) - 3L

#' update missing risk model entries up to date
#'
#' checks existing risk model data and computes model values
#' for entries up to specified date
#'
#' @param risk_model object of class "VirtualRiskModel" carrying parameters of the model
#' @param date "Date" date of the model for which the entries should be computed
#' These are callendar days therefore the model can actually contain less trading days.
#' @param force "logical" should the values be replaced if already existing
#' @param copy_history "logical" should we look for some components history in earlier
#' risk model objectstores
#'
#' @export

update_risk_model_on_date <- function(risk_model,date, force = FALSE, copy_history = FALSE) {

  model_prefix <- getRiskModelPrefix(risk_model)
  lookback <- getRiskModelLookback(risk_model)

  name  <- paste(model_prefix,format(as.Date(date),'%Y-%m'),sep="_")
  rmstr_current <- risk_model_objectstore_factory(name,lookback)
  rmstr_curr_last_rm_date <- getMostRecentRiskModelDate(rmstr_current,getID(rmstr_current),lookback, date)
  rmstr_curr_last_betas_date<- getMostRecentRiskModelBetasDate(rmstr_current,getID(rmstr_current),lookback, date)

  if (force & copy_history) {
    # object store exists for given month, possibly has betas and other calculations in it and
    # there is history of valid model in previous file that needs to be copied
    rmstr_previous <- get_most_recent_model_objectstore(model_prefix, date %m-% months(1))
    rmstr_current <- reInitializeRiskModelComponents(rmstr_current,name ,lookback, components = setdiff(getRiskModelComponents(rmstr_current), 'Betas'))

    if (!is.null(rmstr_curr_last_betas_date)) {
      # do not copy betas
      rmstr_current <- copyRiskModelHistory(rmstr_current,rmstr_previous, getID(rmstr_previous), as.Date(date), lookback,
                                     cmp_to_update = c('ImpliedFactorReturns','ResidualReturns','FactorCorrelation','FactorVariance','MarketStyle'), force = TRUE)
    } else {
      # copy also betas
      rmstr_current <- copyRiskModelHistory(rmstr_current,rmstr_previous, getID(rmstr_previous), as.Date(date), lookback, force = TRUE)
    }

    rmstr_previous <- NULL
    rmstr_curr_last_rm_date <- getMostRecentRiskModelDate(rmstr_current,getID(rmstr_current),lookback)
    rm_date_start <- rmstr_curr_last_rm_date + 1

  } else if (force) {
    # object store exists for given month and has betas and risk model that needs update
    # all calculations apart of betas have to be redone also for lookback history
    rmstr_current <- reInitializeRiskModelComponents(rmstr_current,name ,lookback, setdiff(getRiskModelComponents(rmstr_current), 'Betas'))
    rm_date_start = date - lookback
  } else if (!is.null(rmstr_curr_last_rm_date)) {

    # object store exists for given month and date, has betas and valid risk model
    # and doesn't need update
    if (rmstr_curr_last_rm_date >= date) {
      message(paste('Risk Model data found for all components on dateon date',date))
      message(paste('Model has not been updated. Run update_risk_model(...,force = TRUE) to force recomputation'))
      return(FALSE)
    } else {
    # object store exists for given month and has betas and valid risk model
    # needs just new calculation for new date
      rm_date_start <-  rmstr_curr_last_rm_date + 1
    }


  } else if (copy_history & is.null(rmstr_curr_last_rm_date)) {
      # object store doesn't exist for given month and needs creation of new object
      # and copying of history if valid history exist
      rmstr_previous <- get_most_recent_model_objectstore(model_prefix, date %m-% months(1))
      rmstr_current <- copyRiskModelHistory(rmstr_current,rmstr_previous, getID(rmstr_previous), as.Date(date), lookback)
      rmstr_previous <- NULL
      rm_date_start <- date
  } else {
    # object store doesn't exist for given month and needs creation of new object
    # and copying of history if valid history exist
    rm_date_start <- date - lookback

  }

  compute_risk_model_on_dates(risk_model, rmstr_current, as.Date(rm_date_start), date, force)
  update_risk_model_db(risk_model, rmstr_current, as.Date(rm_date_start), as.Date(date))

  return(TRUE)
}


update_risk_model_db <- function(risk_model, rmstr, date_start, date_end) {

  model_prefix <- getRiskModelPrefix(risk_model)
  lookback <- getRiskModelLookback(risk_model)

  # update betas and returns
  rm_type <- get_model_type_id(model_prefix, lookback)

  store_name <- getID(rmstr)

  betas <- getData(queryDailyRiskModelObjectStore(rmstr,getID(rmstr), lookback,'Betas'))
  betas <- betas[betas$Date >= date_start & betas$Date <= date_end,]
  bulk_load_factor_betas(betas, rm_type)

  returns <- getData(queryDailyRiskModelObjectStore(rmstr,getID(rmstr), lookback,'ImpliedFactorReturns'))
  returns <- returns[returns$Date >= date_start & returns$Date <= date_end,]
  bulk_load_implied_factor_returns(returns, rm_type)



  # update daily computations
  days <-  seq(from= date_start, to = date_end, by = '1 day')
  days <- days[wday(days)!=7&wday(days)!=1]


  for (day in days) {

    day <- as_date(day)

    insert_model_definition(as_date(day), today(), lookback, model_prefix)
    model_id <- query_model_id(rm_type, as_date(day), today() )

    data <- getRiskModelComponentOnDate(rmstr,store_name, 'FactorVariance', day, lookback)
    data <- data[data$Date == day, ]

    if (nrow(data) == 1) {
      bulk_load_factor_variances(data, model_id)
    }

    data <- getRiskModelComponentOnDate(rmstr,store_name, 'FactorCorrelation', day, lookback)
    data <- data[data$Date == day, ]

    if (nrow(data) == (ncol(data)-1)) {
      bulk_load_factor_correlations(data, model_id)
    }

    data <- getRiskModelComponentOnDate(rmstr,store_name, 'MarketStyle', day, lookback)
    data <- data[data$Date == day, ]
    if (nrow(data) == 1) {
      bulk_load_market_style(data, model_id)
    }

    data <- getRiskModelComponentOnDate(rmstr,store_name, 'ResidualReturns', day, lookback)
    data <- data[data$Date, ]
    if (nrow(data) > 0) {
      bulk_load_residual_returns(data, model_id)
    }

  }
}

push_risk_model_component_to_db <- function(risk_model, component){


}

get_betas_composite <- function(universe_betas){

  factor_info <- get_factors()

  factor_types <- c("Currency", "Oil", "Market", "Sector")

  betas_composite <- list()

  for (type in factor_types) {

    factors <- as.character(factor_info$sFactorName[factor_info$sFactorTypeName == type])

    factors <- intersect(factors, colnames(universe_betas))

    betas_composite[[type]] <- universe_betas[c("Instrument", factors)]

  }

  return(betas_composite)
}


compute_risk_model_on_dates <- function(risk_model,
                                        rm_store,
                                        rm_date_start,
                                        date =  today() -1,
                                        force = TRUE) {

  model_prefix <- getRiskModelPrefix(risk_model)
  lookback <- getRiskModelLookback(risk_model)

  rm_name       <- getID(rm_store)

  last_model_date <- getMostRecentRiskModelDate(rm_store, rm_name,lookback, date)
  last_implied_fct_rtns <- getData(queryDailyRiskModelObjectStore(rm_store,rm_name,lookback, 'ImpliedFactorReturns'))

  if (date > (today()-1)) {
    rm_date_end   <- today()-1
  } else (
    rm_date_end <- date
  )

  # move end date to last working date
  while (wday(rm_date_end)==7 || wday(rm_date_end)==1) {
    rm_date_end <- rm_date_end - 1
  }

  if (rm_date_end < rm_date_start) {
    message("Nothing to compute in the required time span...")
    message(paste("Requested start date :", rm_date_start))
    message(paste("Requested end date :", date))

    return()
  }


  betas_last_date <- getMostRecentRiskModelDate(rm_store,rm_name,lookback, date)

  historical_betas <- getData(queryDailyRiskModelObjectStore(rm_store, rm_name, lookback, 'Betas'))

  historical_betas <- historical_betas[historical_betas$Date <= rm_date_end,]

  historical_betas_dates <- unique(historical_betas$Date)

  if (nrow(historical_betas) > 0) {
    betas_date_start <- max(historical_betas_dates + 1)
  } else {
    betas_date_start <- rm_date_start
  }

  rm_date_start <- as.Date(rm_date_start)
  rm_date_end <- as.Date(rm_date_end)

  if (betas_date_start <= rm_date_end ) {
    calculate_betas_on <- seq(from = betas_date_start, to = rm_date_end, by = 1)
    calculate_betas_on <- calculate_betas_on[wday(calculate_betas_on)!=7&wday(calculate_betas_on)!=1]
    betas_date_start <- min(calculate_betas_on)

    if (betas_date_start <= rm_date_end ) {
      lback   <- ymd(min(calculate_betas_on)) %m-% days(lookback-1)
      all_fct <- getRiskModelMarketFactorReturns(risk_model, as.Date(lback),rm_date_end)
      all_fx  <- getRiskModelCurrencyFactorReturns(risk_model, as.Date(lback),rm_date_end)
      all_oil <- getRiskModelCommodityFactorReturns(risk_model, as.Date(lback),rm_date_end)
      all_sct <- getRiskModelSectorFactorReturns(risk_model,as.Date(lback),rm_date_end)
    }

  } else {
    lback <- rm_date_start
  }

  if (lback > rm_date_start) {
    message("Stock info cannot be obtained for requested time span...")
    message(paste("Requested start date :", lback))
    message(paste("Requested end date :", rm_date_end))

    return()
  }

  all_stk <- get_region_stock_returns(as.Date(lback),as.Date(rm_date_end),c(3))


  first <- TRUE
  for(d in 0:(as.Date(rm_date_end)-as.Date(rm_date_start))){
    rm_date <- ymd(rm_date_start) %m+% days(d)
    rm_date <- as.Date(rm_date)

    if(wday(rm_date)!=7&wday(rm_date)!=1){

      lback = ymd(rm_date) %m-% days(lookback)
      lback <- as.Date(lback)

      if (rm_date %in% historical_betas_dates) {
        message(paste("Betas are already computed for date:",rm_date))
        message(paste("Skipping computation..."))
        universe_betas <- historical_betas[historical_betas$Date == rm_date, ]
        stk <- all_stk[all_stk$Date == rm_date,]

      } else {
        message(paste("Computing:",rm_date))
        stk <- all_stk[all_stk$Date>=lback&all_stk$Date<=rm_date,]
        fct <- all_fct[all_fct$Date>=lback&all_fct$Date<=rm_date,]
        fx  <- all_fx[all_fx$Date>=lback&all_fx$Date<=rm_date,]
        oil <- all_oil[all_oil$Date>=lback&all_oil$Date<=rm_date,]
        sct <- all_sct[all_sct$Date>=lback&all_sct$Date<=rm_date,]
        beta_fct_frame <- merge(fct,fx,by='Date')
        beta_fct_frame <- merge(beta_fct_frame,oil,by='Date')
        beta_fct_frame <- merge(beta_fct_frame,sct,by='Date')
        message("Computing betas ...")
        message(paste("Time Start :", now()))

        # start betas computation
        cl <- declare_local_cluster(.__DEFAULT_RISK_MODEL_NCORES__)
        prepare_cluster(cl)
        on.exit(function(){ if(nrow(showConnections())> 0) { stopCluster(cl); closeAllConnections();cl = NULL} } )
        universe_betas <- stock_betas(stk,beta_fct_frame,cl)
        stopCluster(cl)
        cl = NULL
        gc()

        # store betas for posterity
        betas <- cbind(Date=rm_date,universe_betas)
        rm_store<-pushRiskModelComponent(rm_store,betas,rm_name,lookback,"Betas")
        message("Commiting data to RiskModelObject")
        commitDailyRiskModelObjectStore(rm_store)
      }

      message("Building composite regression model ...")
      betas_composite <- get_betas_composite(universe_betas)

      ir <- tryCatch({
        composite_model(stk[stk$Date==rm_date,],betas_composite)
      },error=function(cond){
        message(paste("Factor block regression failed:",cond))
      })
      if(length(ir)>0){
        implied_fct_rtns <- cbind(Date=rm_date,ir[[1]])
        residual_rtns <- cbind(Date=rm_date,ir[[2]])
        implied_fct_rtns <- rbind(last_implied_fct_rtns,cbind(Date=rm_date,ir[[1]]))
        implied_fct_rtns <- implied_fct_rtns[implied_fct_rtns$Date >= lback & implied_fct_rtns$Date <= rm_date,]
        last_implied_fct_rtns <- implied_fct_rtns

        rm_store<-pushRiskModelComponent(rm_store,residual_rtns,rm_name,lookback,"ResidualReturns", force)
        rm_store<-pushRiskModelComponent(rm_store,cbind(Date=rm_date,ir[[1]]),rm_name,lookback,"ImpliedFactorReturns", force)

        if (nrow(implied_fct_rtns) > 1) {
          fct_cor<- factor_correlation(implied_fct_rtns)
          fct_sd <- unlist(Map(sd,implied_fct_rtns[setdiff(colnames(implied_fct_rtns),'Date')]))
          fct_cov<- factor_covariance(fct_cor,fct_sd)
          es <- eigen(fct_cov)
          ev <- cbind(Date=rm_date,as.data.frame(t(as.numeric(scale(Re(es[[2]][,1]))))))
          colnames(ev) <- colnames(implied_fct_rtns)
          rm_store<-pushRiskModelComponent(rm_store,cbind(Date=as.Date(rm_date),as.data.frame(fct_cor)),rm_name,lookback,"FactorCorrelation", force)
          rm_store<-pushRiskModelComponent(rm_store,cbind(Date=as.Date(rm_date),as.data.frame(t(fct_sd))),rm_name,lookback,"FactorVariance", force)
          rm_store<-pushRiskModelComponent(rm_store,ev,rm_name,lookback,"MarketStyle")

        }

      }

    }

  }

  message("Commiting data to RiskModelObject")
  commitDailyRiskModelObjectStore(rm_store)

}
