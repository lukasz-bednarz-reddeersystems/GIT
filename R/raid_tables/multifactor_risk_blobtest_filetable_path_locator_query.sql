
DECLARE
@sReferencedTableName sysname = 'ftRDTE_AnalysisObjectstore',
@sFileName nvarchar(256) = 'analysis_MarketStyleFactorStatisticAnalysisBlock_RiskModel.DevelopedEuropePrototype150.1.1_2016-05-31_2016-08-31_objectstore_2016-09-27_Lukasz.Bednarz.rds',
@SQL       nvarchar(max),
@hPathLocator varchar(max)

SET  @SQL = N'SELECT TOP 1 @hPathLocator  =  [path_locator].ToString() ' + 
			' FROM ' +
			@sReferencedTableName +
			N' WHERE ' + 
			N' name = ' + '''' + @sFileName + ''''

EXEC sp_executesql 
	@SQL,
	N'@sReferencedTableName sysname, @sFileName nvarchar(max), @hPathLocator varchar(max) OUTPUT',
	@sReferencedTableName,
	@sFileName,
	@hPathLocator OUTPUT



SELECT @hPathLocator

--SELECT @SQL