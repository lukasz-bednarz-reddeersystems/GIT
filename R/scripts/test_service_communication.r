sourceTo("../lib/sockets.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#1. Run on test server
server_socket <- new("ProcessSocket")
server_socket <- openConnection(server_socket)
server_socket <- readConnection(server_socket)
command <- getData(server_socket)
server_socket <- closeConnection(server_socket)

message(command)
message(eval(parse(text=command)))