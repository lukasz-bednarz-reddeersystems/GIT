#Creates a pie chart of turnover, AUM and a bar plot of PL by strategies appearing in the input
#data and optionally compares to a previous period.
library(lubridate)
sourceTo("../analysis_modules/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClass(
	Class  	  				= "AllocationTurnoverByStrategy",
	representation(
		#Determines whether the object should produce comparitive
		#statistics to a previous time period present in the 
		#input data.
		compare_to_previous = 'character',
		date 				= 'Date'
	),
	prototype               = prototype(
		compare_to_previous = character()
	),
	contains  = "HistoryAnalysisModule"
)

setMethod("generateOutputData","AllocationTurnoverByStrategy",
	function(object){
		thisQ <- quarter(object@date-1)
		#Compute AUM, PL and number of positions at the position level: NB much of this could be transered to standard methods of the PFO class, eg. Position totals
		psn_data   <- unique(object@base_data[c('Strategy','Instrument','TradeDate','MarketValue','TodayPL')])
		psn_data$Q <- quarter(psn_data$TradeDate) #Need to implement computation over data this quarter and previous quarters based on the compare_to_previous
		strat_sz_smrry <- aggregate(psn_data['MarketValue'],list(Strategy=psn_data$Strategy,Quarter=psn_data$Q,Date=psn_data$TradeDate),function(x)sum(abs(x),na.rm=TRUE))
		strat_sz_smrry <- aggregate(strat_sz_smrry['MarketValue'],list(Strategy=strat_sz_smrry$Strategy,Quarter=(strat_sz_smrry$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
		strat_pl_smrry <- aggregate(psn_data['TodayPL'],list(Strategy=psn_data$Strategy,Quarter=psn_data$Q),function(x)sum(x,na.rm=TRUE))
		strat_pl_smrry <- aggregate(strat_pl_smrry['TodayPL'],list(Strategy=strat_pl_smrry$Strategy,Quarter=(strat_pl_smrry$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
		strat_n_psns   <- aggregate(psn_data['Instrument'],list(Strategy=psn_data$Strategy,Quarter=psn_data$Q),function(x)length(unique(x[!is.na(x)])))
		strat_n_psns   <- aggregate(strat_n_psns['Instrument'],list(Strategy=strat_n_psns$Strategy,Quarter=(strat_n_psns$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
		#Compute value traded, PL Day 0 and the number of trades: Likewise, much of this functionality could be transferred to the ppmdl class, eg. Trade totals
		trd_data <- unique(object@base_data[c('Strategy','Instrument','TradeDate','ValueUSD','TodayPL','TradeID')])
		trd_data$Q <- quarter(trd_data$TradeDate)
		trd_data <- trd_data[!is.na(trd_data$TradeID),]
		trd_pl_smrry <- aggregate(trd_data[c('TodayPL','ValueUSD')],list(Strategy=trd_data$Strategy,Quarter=trd_data$Q),function(x)sum(x,na.rm=TRUE))
		trd_pl_smrry <- aggregate(trd_pl_smrry[c('TodayPL','ValueUSD')],list(Strategy=trd_pl_smrry$Strategy,Quarter=(trd_pl_smrry$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
		trd_n_trds <- aggregate(trd_data['Instrument'],list(Strategy=trd_data$Strategy,Quarter=trd_data$Q),function(x)length(unique(x[!is.na(x)])))
		trd_n_trds <- aggregate(trd_n_trds['Instrument'],list(Strategy=trd_n_trds$Strategy,Quarter=(trd_n_trds$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
		#Create a convenient tabular for of the data
		object@output_data[['strategy_data']] <- rbind(cbind(Type='Position level',Quantity='Value',Value=strat_sz_smrry$MarketValue,strat_sz_smrry[c('Strategy','Quarter')]),
                       		   						   cbind(Type='Position level',Quantity='PL',Value=strat_pl_smrry$TodayPL,strat_pl_smrry[c('Strategy','Quarter')]),
                               						   cbind(Type='Position level',Quantity='Count',Value=strat_n_psns$Instrument,strat_n_psns[c('Strategy','Quarter')]),
                               						   cbind(Type='Trade level',Quantity='Value',Value=trd_pl_smrry$ValueUSD,trd_pl_smrry[c('Strategy','Quarter')]),
                               						   cbind(Type='Trade level',Quantity='PL',Value=trd_pl_smrry$TodayPL,trd_pl_smrry[c('Strategy','Quarter')]),
                               						   cbind(Type='Trade level',Quantity='Count',Value=trd_n_trds$Instrument,trd_n_trds[c('Strategy','Quarter')]))
		return(object)
	}	
)

setMethod("generateOutputObjects","AllocationTurnoverByStrategy",
		function(object){
			#Create ggplot piechart of AUM and turnover
			sd <- object@output_data[['strategy_data']]
			sdagg <- aggregate(sd['Value'],list(Type=sd$Type,Quarter=sd$Quarter),sum)
  			sd <- merge(sd,sdagg,by=c('Type','Quarter'))
  			sd <- sd[c('Type','Value.x','Value.y','Strategy')]
  			sd$Value <- 100*(sd$Value.x/sd$Value.y)
  			sd$Delta <- sd$Value.x - sd$Value.y
  			sd$breaks[sd$Type=='Position level'] <- cumsum(sd$Value[sd$Type=='Position level']) - sd$Value[sd$Type=='Position level']/2
			sd$breaks[sd$Type=='Trade level'] <- cumsum(sd$Value[sd$Type=='Trade level']) - sd$Value[sd$Type=='Trade level']/2
			sd$angle[sd$Type=='Position level'] <- (90-360*(sd$breaks[sd$Type=='Position level']/sum(sd$Value[sd$Type=='Position level'])))
			sd$angle[sd$Type=='Trade level'] <- (90-360*(sd$breaks[sd$Type=='Trade level']/sum(sd$Value[sd$Type=='Trade level'])))
			sd$dlabel <- NA
			sd$dlabel[abs(sd$Delta)>5] <- sd$Delta[abs(sd$Delta)>5]
			lbels <- paste(round(sd$dlabel),"%",sep="")
			lbels <- gsub("NA%","",lbels)
			sd$Type <- as.character(sd$Type)
			sd$Type[sd$Type=='Position level'] <- 'AUM'
			sd$Type[sd$Type=='Trade level'] <- 'Turnover'
			object@output_objs[["aum_and_turnover"]] <- ggplot(data=sd, aes(x=factor(1), y=Value, fill=Strategy)) +
  									geom_bar(stat="identity") +
  									geom_text(aes(x= factor(1.2), y=breaks, label = Strategy, angle=angle),size=3) +  
  									geom_text(aes(x= factor(1), y=breaks, label = lbels),size=4,fontface='bold') +  
  									theme(legend.position = "none",axis.ticks = element_blank(),panel.background = element_rect(fill = "white")) +
  									coord_polar(theta = "y") +
  									scale_y_continuous(breaks=NULL) +
  									scale_x_discrete(breaks=NULL) +
  									labs(x="",y="") +	
  									ggtitle('AUM and turnover by strategy') +
  									facet_grid(facets = Type~.) 
  			#Create plot of PL by strategy
  			sd <- object@output_data[['strategy_data']]
  			pl_data <- sd[sd$Type=='Position level'&sd$Quantity=='PL',]
			pl_data <- merge(pl_data[pl_data$Quarter,],pl_data[!pl_data$Quarter,],by=c('Type','Quantity','Strategy'))
			pl_data$Value <- pl_data$Value.x
			pl_data$Delta <- pl_data$Value.x - pl_data$Value.y
			pl_data$Delta <- 100*(pl_data$Delta/abs(pl_data$Value.y))
			lbls <- paste(round(pl_data$Delta),"%",sep="")
			lbls <- gsub('NaN%','',lbls)
			object@output_objs[["strategy_pl"]] <- ggplot(data=pl_data, aes(x=reorder(Strategy,Value), fill=Strategy)) +
  									geom_bar(aes(weight=Value)) +
  									coord_flip() +
  									geom_text(aes(x= Strategy, y=Value, label = lbls),size=3) +  
  									theme(legend.position = "none") +
  									ylab("PL $") + xlab("Strategy") + ggtitle('Total PL and PL change by strategy') +
  									theme(axis.text.x = element_text(angle = 90, hjust = 1))
  			return(object)
		}
)