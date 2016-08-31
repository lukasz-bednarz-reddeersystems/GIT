
DECLARE
@sReferencedTableName sysname = 'ftMultiFactorRiskBlobTest',
@sFileName nvarchar(255) = 'temp2.txt',
@SQL       nvarchar(max),
@hPathLocator varchar(max)

SET  @SQL = N'SELECT TOP 1 @hPathLocator  =  [path_locator].ToString() ' + 
			' FROM ' +
			@sReferencedTableName +
			N' WHERE ' + 
			N' name = ' + QUOTENAME(@sFileName,'''')

EXEC sp_executesql 
	@SQL,
	N'@sReferencedTableName sysname, @sFileName nvarchar(255), @hPathLocator varchar(max) OUTPUT',
	@sReferencedTableName,
	@sFileName,
	@hPathLocator OUTPUT



SELECT @hPathLocator