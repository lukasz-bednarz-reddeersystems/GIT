sourceTo("../lib/sockets.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#1. Run on test server
server_socket <- new("ProcessSocket")
server_socket <- openConnection(server_socket)
server_socket <- readConnection(server_socket)
print(getData(server_socket))
server_socket <- writeToConnection(server_socket,"belong to us!")
server_socket <- closeConnection(server_socket)

#2. Run on test client
client_socket <- new("ProcessSocket",server=FALSE)
client_socket <- openConnection(client_socket)
client_socket <- writeToConnection(client_socket,"all your base are")
client_socket <- readConnection(client_socket)
print(getData(client_socket))
client_socket <- closeConnection(client_socket)