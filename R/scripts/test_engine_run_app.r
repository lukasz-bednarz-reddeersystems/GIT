library(R.utils)
options(modifiedOnlySource=TRUE)
sourceTo("../lib/sockets.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#1. Start the engine by excuting ../engine/engine_service_main.r within a 
#   sepatate process. 

#2. Run the following:
client_emulation <- new("ProcessSocket",server=FALSE)
client_emulation <- openConnection(client_emulation)
client_emulation <- writeToConnection(client_emulation,"TEST")
client_emulation <- readConnection(client_emulation)
test_data <- getDataFromSocket(client_emulation)
message(paste("Got:",test_data))

#3. Test triggering a shiny app
client_emulation <- writeToConnection(client_emulation,"SETAPP|test")
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