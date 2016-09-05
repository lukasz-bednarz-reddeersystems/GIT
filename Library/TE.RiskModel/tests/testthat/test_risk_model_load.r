context("Testing Risk Model Load/Read functions")

##################################################################################################################
#
# ModelFactors
#
##################################################################################################################

valid.model_class    <- "RiskModel.DevelopedEuropePrototype150"
valid.risk_model     <- new(valid.model_class)
valid.model_factors  <- getRiskModelFactorNames(valid.risk_model)
valid.model_name     <- getRiskModelPrefix(valid.risk_model)
valid.model_lookback <- getRiskModelLookback(valid.risk_model)
valid.model_type_id  <- TE.RiskModel:::get_model_type_id(valid.model_name, valid.model_lookback)



test_that("Can insert_model_type_factors to DB",{

  valid.ret.factors <- data.frame(lModelTypeID  = valid.model_type_id,
                                  sFactorName   = valid.model_factors)

  valid.factor_info <- TE.RiskModel:::get_factors()

  valid.ret.factors <- merge(valid.ret.factors,
                             valid.factor_info,
                             sort = FALSE)[c("lModelTypeID", "lFactorID")]


  ret.factors <- TE.RiskModel:::insert_model_type_factors(valid.model_name,
                                                          valid.model_lookback,
                                                          valid.model_factors)

  expect_equal(ret.factors, valid.ret.factors)
})
