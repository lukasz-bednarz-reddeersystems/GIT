USE FileTableDB

EXEC prMultiFactorRisk_JointFileTable_UpdateByTbNameTraderIDInstrumentIDLegStartDateLegEndDate
@sJointTableName  = N'tRDTE_TradesObjectstore',
@sHashID = '786BA2524BB53CAA0B114446D650214F0725B862C895458D0102DA9FF20F24B5F8EDB963EB26A895D228475F8CB58094F6D63344BF068D15E4E8E6B074A1503E' , 
@lTraderID  = '1984' , 
@lInstrumentID  = '4454' , 
@sDirection  = 'Buy' , 
@sStrategy  = 'LB_TEST' , 
@dtLegStartDate  = '2016-03-29' , 
@dtLegEndDate  = '2016-04-01' , 
@sLegStatus  = 'Closed' , 
@dtCreatedDate  = '2016-09-25' , 
@sCreatedByUserID  = 'Lukasz.Bednarz' , 
@sFileName  = 'trade_store_1984_4454_Buy_LB_TEST_2016-03-29_2016-04-01.rds'
