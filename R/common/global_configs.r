#This could be an enivronment variable that is automatically set according
#to the host name.
ENV <- 'prod'
DATA<- 'prod'

#It was necessary to use the live version of the trading analysis
#middleware URL so ConfigSwitcher was added to permit this.
#Can be used as the basis to do this for all URLs/paths etc...
setClass(
  Class = "ConfigSwitcher",
  representation    = representation(
    switcher        = "function",
    prod_value      = "character",
    dev_value       = "character"
  ),
  prototype  = prototype(
    switcher = switch
  )
)
setGeneric("getConfigSwitch",function(object,env){standardGeneric("getConfigSwitch")})
setMethod("getConfigSwitch","ConfigSwitcher",
  function(object,env){
    rval <- switch(env,dev=object@dev_value,prod=object@prod_value,
                   function(){message("ConfigSwitcher: Unrecognised environment")
                              return(object@dev_value)}
                  ) 
    return(rval)
  }
)

positions_url_switch   <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/positions/positionhistory",dev_value="http://localhost:8083/positions/positionhistory")
trade_hist_url_switch  <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/trading/tradehistory",dev_value="http://localhost:8083/trading/tradehistory")
strategies_url_switch  <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/trading/strategies",dev_value="http://localhost:8083/trading/strategies")
instr_hist_url_switch  <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/instruments/instrumenthistory",dev_value="http://localhost:8083/instruments/instrumenthistory")
instr_ids_url_switch   <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/instruments/instrumentidentifiers",dev_value="http://localhost:8083/instruments/instrumentidentifiers")
instr_dts_url_switch   <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/instruments/instrumentdetails",dev_value="http://localhost:8083/instruments/instrumentdetails")
instr_sect_url_switch  <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/sector/djsupersector",dev_value="http://localhost:8083/sector/djsupersector")
static_factors1_switch <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/history/static",dev_value="http://localhost:8083/history/static")
static_factors2_switch <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/history/static2",dev_value="http://localhost:8083/history/static2")
dynamic_factors_switch <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/history/dynamic",dev_value="http://localhost:8083/history/dynamic")
event_types_url_switch <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/events/types",dev_value="http://localhost:8083/events/types")
events_url_switch      <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/events/between",dev_value="http://localhost:8083/events/between")
ext_psns_url_switch    <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/extpositions/history",dev_value="http://localhost:8083/extpositions/history")
dealing_url_switch     <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/trademeta/dealing",dev_value="http://localhost:8083/trademeta/dealing")
psn_diary_url_switch   <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/trademeta/diary",dev_value="http://localhost:8083/trademeta/diary")
trade_level_url_switch <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/trademeta/levelsforinstrument",dev_value="http://localhost:8083/trademeta/levelsforinstrument")
trader_perf_url_switch <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/tradinganalysis/traderperformance",dev_value="http://localhost:8083/tradinganalysis/traderperformance")
ratios_url_switch      <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/tradinganalysis/portfolioanalysis",dev_value="http://localhost:8083/tradinganalysis/portfolioanalysis")
top_psn_url_switch     <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/tradinganalysis/toppositions",dev_value="http://localhost:8083/tradinganalysis/toppositions")
bottom_psn_url_switch  <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/tradinganalysis/bottompositions",dev_value="http://localhost:8083/tradinganalysis/bottompositions")
inst_country_url_switch<- new("ConfigSwitcher",prod_value="http://raidapp2:8083/refdata/country",dev_value="http://localhost:8083/refdata/country")
inst_price_url_switch  <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/refdata/price",dev_value="http://localhost:8083/refdata/price")
risk_exp_url_switch    <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/rawrisk/scores",dev_value="http://localhost:8083/rawrisk/scores")
risk_fct_rtn_url_switch<- new("ConfigSwitcher",prod_value="http://raidapp2:8083/rawrisk/returns",dev_value="http://localhost:8083/rawrisk/returns")
allocation_url_switch  <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/refdata/allocations",dev_value="http://localhost:8083/refdata/allocations")
watchlist_url_switch   <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/watchlists/razorlist",dev_value="http://localhost:8083/watchlists/razorlist")
all_razor_fns_switch   <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/razor/razorfunctions",dev_value="http://localhost:8083/razor/razorfunctions")
liquidity_smmry_switch <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/tradinganalysis/liquiditysummary",dev_value="http://localhost:8083/tradinganalysis/liquiditysummary")
liquidity_psns_switch  <- new("ConfigSwitcher",prod_value="http://raidapp2:8083/tradinganalysis/liquiditypositionsummary",dev_value="http://localhost:8083/tradinganalysis/liquiditypositionsummary")

setClass(
  Class = "MiddleWareURLS",
  representation    = representation(
    #If these were changed from character slots to slots that hold
    #config switchers, then the setting of the URLs could be made 
    #dynamic by only accessing them with an accessor method.
    #This could permit runtime override of certain settings.
  	positions_url   = "character",
  	trade_hist_url  = "character",
  	instr_hist_url  = "character",
  	instr_ids_url   = "character",
    instr_dts_url   = "character",
    instr_sect_url  = "character",
    strategies_url  = "character",
    static_factors1 = "character",
    static_factors2 = "character", 
    dynamic_factors = "character",
    event_types_url = "character",
    events_url      = "character",
    ext_psns_url	  = "character",
    dealing_url     = "character",
    psn_diary_url   = "character",
    trade_level_url = "character",
    trader_perf_url = "character",
    ratios_url      = "character",
    top_psn_url     = "character",
    bottom_psn_url  = "character",
    inst_country_url= "character",
    inst_price_url  = "character",
    risk_exp_url    = "character",
    risk_fct_rtn_url= "character",
    allocation_url  = "character",
    watchlist_url   = "character",
    all_razor_fns   = "character",
    liquidity_smmry = "character",
    liquidity_psns  = "character"
  ),
  prototype      = prototype(
  	positions_url   = getConfigSwitch(positions_url_switch,ENV),
  	trade_hist_url  = getConfigSwitch(trade_hist_url_switch,ENV),
  	strategies_url  = getConfigSwitch(strategies_url_switch,ENV),
  	instr_hist_url  = getConfigSwitch(instr_hist_url_switch,ENV),
  	instr_ids_url	  = getConfigSwitch(instr_ids_url_switch,ENV),
    instr_dts_url   = getConfigSwitch(instr_dts_url_switch,ENV),
    instr_sect_url  = getConfigSwitch(instr_sect_url_switch,ENV),
    static_factors1 = getConfigSwitch(static_factors1_switch,ENV),
    static_factors2 = getConfigSwitch(static_factors2_switch,ENV),
    dynamic_factors = getConfigSwitch(dynamic_factors_switch,ENV),
    event_types_url = getConfigSwitch(event_types_url_switch,ENV),
    events_url      = getConfigSwitch(events_url_switch,ENV),
    ext_psns_url    = getConfigSwitch(ext_psns_url_switch,ENV),
    dealing_url     = getConfigSwitch(dealing_url_switch,ENV),
    psn_diary_url   = getConfigSwitch(psn_diary_url_switch,ENV),
    trade_level_url = getConfigSwitch(trade_level_url_switch,ENV),
    trader_perf_url = getConfigSwitch(trader_perf_url_switch,ENV),
    ratios_url      = getConfigSwitch(ratios_url_switch,ENV),
    top_psn_url     = getConfigSwitch(top_psn_url_switch,ENV),
    bottom_psn_url  = getConfigSwitch(bottom_psn_url_switch,ENV),
    inst_country_url= getConfigSwitch(inst_country_url_switch,ENV),
    inst_price_url  = getConfigSwitch(inst_price_url_switch,ENV),
    risk_exp_url    = getConfigSwitch(risk_exp_url_switch,ENV),
    risk_fct_rtn_url= getConfigSwitch(risk_fct_rtn_url_switch,ENV),
    allocation_url  = getConfigSwitch(allocation_url_switch,ENV),
    watchlist_url   = getConfigSwitch(watchlist_url_switch,ENV),
    all_razor_fns   = getConfigSwitch(all_razor_fns_switch,ENV),
    liquidity_smmry = getConfigSwitch(liquidity_smmry_switch,ENV),
    liquidity_psns  = getConfigSwitch(liquidity_psns_switch,ENV)
  )
)

setClass(
  Class = "FeatureDefaults",
  representation = representation(
  	default_window = "integer",
    default_window_long = "integer"
  ),
  prototype = prototype(
  	default_window = as.integer(7),
    default_window_long = as.integer(20)
  )
)

setClass(
  Class = "WareHouseDefaults",
  representation = representation(
  	default_dly_data_pad = "integer",
  	default_date_key     = "character",
  	default_fctr_data_str= "character",
  	long_strats          = "character",
  	short_strats         = "character"
  ),
  prototype = prototype(
  	default_dly_data_pad = as.integer(20),
  	default_date_key = "DateTime",
  	default_fctr_data_str ="factor_datastore",
  	long_strats = c('JS_LPOST','JS_LDISC','JS_LNI','BA_LEVENT','BA_LHYBRID','BA_LARB','BA_LDISC','DK_LPAT','DK_LSPSIT','DK_LCORE','DK_LHDG'),
  	short_strats= c('JS_SHEDGE','JS_SPOST','JS_SDISC','JS_SNI','BA_SEVENT','BA_SHYBRID','BA_SARB','BA_SDISC','BA_SHEDGE','DK_SPAT','DK_SSPSIT','DK_SCORE','DK_SHDG')
  )
)

model_data_switch  <- new("ConfigSwitcher",prod_value="T:/data",dev_value="C:/Development/TradingEnhancementEngine/R/model_data")
setClass(
  Class = "ModelDefaults",
  representation  = representation(
    data_path  = 'character',
    risk_models= 'character'
  ),
  prototype    = prototype(
    data_path  = getConfigSwitch(model_data_switch,DATA),
    risk_models= 'C:/Development/TradingEnhancementEngine/R/model_data/risk_data'
  )
)

setMethod("initialize", "ModelDefaults", function(.Object){
  r_root <- gsub('\\\\', '/',Sys.getenv("R_RAID_ROOT"))
  if(file.exists(r_root) & (r_root != "")) {
    .Object@data_path <- paste0(r_root, '/Services/Raid.Services.TradingEnhancementEngine/R/model_data')
  }
  return(.Object)
})

setClass(
  Class = "VisualisationDefaults",
  representation = representation(
    file_path    = 'character'
  ),
  prototype      = prototype(
    file_path    = 'C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/templates'
  )
)

setMethod("initialize", "VisualisationDefaults", function(.Object){
  r_root <- gsub('\\\\', '/',Sys.getenv("R_RAID_ROOT"))
  if(file.exists(r_root) & (r_root != "")) {
    .Object@file_path <- paste0(r_root, '/Services/Raid.Services.TradingEnhancementEngine/R/templates')
  }
  return(.Object)
})

setClass(
  Class = "AnalysisModuleDefaults",
  representation = representation(
      data_store = 'character',
      analysis_store = 'character'
  ),
  prototype = prototype(
      data_store = 'trade_warehouse',
      analysis_store = 'analysis_warehouse'
  )
)

setClass(
  Class = "RiskModelDefaults",
  representation = representation(
    store_database = 'character'
  ),
  prototype = prototype(
    store_database =  'RAIDSTAGEDB'
  )
)
setMethod("initialize", "RiskModelDefaults", function(.Object){
  db <-  Sys.getenv("R_RISK_MODEL_DB") 
  if ((db != ""))
    .Object@store_database <- db
    return(.Object)
  }
)


setClass(
  Class = "SocketDefaults",
  representation = representation(
    remote_host  = "character",
    port         = "numeric",
    server       = "logical",
    read_lim     = "numeric"
  ),
  prototype = prototype(
    remote_host  = "localhost",
    port         = 130,
    server       = TRUE,
    read_lim     = 5000000
  )
)

setClass(
  Class          = "EngineDefaults",
  representation = representation(
    store_name_prefix = "character"
  ),
  prototype      = prototype(
    store_name_prefix = "analysis_store_"
  )
)

setClass(
  Class          = "ReportingDefaults",
  representation = representation(
    analy_store  = "character"
  ),
  prototype      = prototype(
    analy_store  = 'analysis_store_'
  )
)

db_config_switcher = new("ConfigSwitcher",prod_value="RAIDLIVEDB",dev_value="RAIDSTAGEDB")
setClass(
  Class          = "DataBaseConfig",
  representation = representation(
    database     = "character",
    dbuser       = "character"
  ),
  prototype      = prototype(
    database     = getConfigSwitch(db_config_switcher,ENV),
    dbuser       = "guy.billings"
  )
)

warehouse_defaults <- new("WareHouseDefaults")
feature_defaults<- new("FeatureDefaults")
middleware_urls <- new("MiddleWareURLS")
model_defaults <- new("ModelDefaults")
visualisation_defaults <- new("VisualisationDefaults")
analysis_defaults <- new("AnalysisModuleDefaults")
socket_defaults <- new("SocketDefaults")
engine_defaults <- new("EngineDefaults")
reporting_defaults <- new("ReportingDefaults")
database_config <- new("DataBaseConfig")