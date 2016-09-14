library(R.utils)
library(TE.DataAccess)

#1. Start the engine by excuting ../engine/engine_service_main.r within a 
#   sepatate process. 

#2. Run the following:
client_emulation <- new("ProcessSocket",server=FALSE)
client_emulation <- openConnection(client_emulation)
client_emulation <- writeToConnection(client_emulation,"TEST")
client_emulation <- readConnection(client_emulation)
test_data <- getDataFromSocket(client_emulation)
message(paste("Got:",test_data))

#3. Test building a module and running
client_emulation <- writeToConnection(client_emulation,"SETTRADER|BE500 Index")
client_emulation <- readConnection(client_emulation)
resp <- getDataFromSocket(client_emulation)
message(paste("Got:",resp))
client_emulation <- writeToConnection(client_emulation,paste("SETDATE|2016-09-01",sep=""))
client_emulation <- readConnection(client_emulation)
resp <- getDataFromSocket(client_emulation)
message(paste("Got:",resp))
client_emulation <- writeToConnection(client_emulation,"SETLOOKBACK|dated_three_monthly_lookback")
client_emulation <- readConnection(client_emulation)
resp <- getDataFromSocket(client_emulation)
message(paste("Got:",resp))
client_emulation <- writeToConnection(client_emulation,"SETAPPHOST|localhost")
client_emulation <- readConnection(client_emulation)
resp <- getDataFromSocket(client_emulation)
message(paste("Got:",resp))
client_emulation <- writeToConnection(client_emulation,"SETAPPPORT|51741")
client_emulation <- readConnection(client_emulation)
resp <- getDataFromSocket(client_emulation)
message(paste("Got:",resp))
client_emulation <- writeToConnection(client_emulation,"SETMODULE|MarketStyleFactorStatisticAnalysisBlock")
client_emulation <- readConnection(client_emulation)
resp <- getDataFromSocket(client_emulation)
message(paste("Got:",resp))
client_emulation <- writeToConnection(client_emulation,"SETAPP|builder")
client_emulation <- readConnection(client_emulation)
resp <- getDataFromSocket(client_emulation)
message(paste("Got:",resp))
client_emulation <- writeToConnection(client_emulation,"RUNAPP")
#Now should be able to acces the shiny app in a browser
client_emulation <- readConnection(client_emulation)
resp <- getDataFromSocket(client_emulation)
message(paste("Got:",resp))
client_emulation <- writeToConnection(client_emulation,"STOP")
client_emulation <- closeConnection(client_emulation)


