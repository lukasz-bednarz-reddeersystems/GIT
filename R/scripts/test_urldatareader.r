sourceTo("../lib/urldatareader.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

test_matrix_conv = TRUE
test_frame_conv = TRUE

if(test_matrix_conv){
  matrix_tester <- new("URLParser",parser_type = "XMLToMatrix")
  matrix_tester <- runURLs(matrix_tester,c("http://localhost:8083/positions/currencies"))
} 
if(test_frame_conv){
  frame_tester <- new("URLParser",parser_type = "XMLToFrame")
  frame_tester <- runURLs(frame_tester,c("http://localhost:8083/positions/positionlevels","http://localhost:8083/trading/strategies"))
} 