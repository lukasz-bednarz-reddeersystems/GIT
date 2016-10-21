context("Test DataPlex")

#############################
#
# factor_datastore Tests
#
#############################

tested.data_store = "factor_datastore"
valid.data_columns = "rDailyN"
valid.key_vals     = data.frame(lInstrumentID= 4454L,
                                dtDateTime   = seq(as.Date("2016-01-01"),
                                                   as.Date("2016-12-31"),
                                                   1)
                                )

test_that(sprintf("Can retrieve data from %s", tested.data_store),{

  ret_data <- data_request(tested.data_store, valid.key_vals, valid.data_columns)


})

#############################
#
# features_factor_datastore Tests
#
#############################

tested.data_store = "features_factor_datastore"
valid.data_columns = "rDailyN"
valid.key_vals     = data.frame(lInstrumentID= 4454L,
                                dtDateTime   = seq(as.Date("2016-01-01"),
                                                   as.Date("2016-12-31"),
                                                   1)
)

test_that(sprintf("Can retrieve data from %s", tested.data_store),{

  ret_data <- data_request(tested.data_store, valid.key_vals, valid.data_columns)


})

