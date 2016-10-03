DECLARE @sJointTableName	sysname
DECLARE @sReferencedTableName	sysname
DECLARE @lTraderID           bigint
DECLARE @dtStartDate        datetime
DECLARE @dtEndDate        datetime

SET @sJointTableName = 'tMultiFactorRiskBlobTest'
SET @lTraderID       = 11
SET @dtStartDate     = '2016-01-01'
SET @dtEndDate     = '2016-01-01'

USE FileTableDB

BEGIN

SET @sReferencedTableName = (

SELECT TOP 1 
   OBJECT_NAME(f.referenced_object_id) ReferencedTableName
FROM 
   sys.foreign_keys AS f
INNER JOIN 
   sys.indexes AS fi 
      ON fi.OBJECT_ID = f.referenced_object_id
INNER JOIN 
   sys.tables t 
      ON t.OBJECT_ID = f.referenced_object_id
INNER JOIN
	sys.foreign_key_columns AS fc
		ON f.OBJECT_ID = fc.constraint_object_id
WHERE 
   OBJECT_NAME (f.parent_object_id) = @sJointTableName
   )


DECLARE 
@SQL nvarchar(max)

SET 

@SQL = 

N'SELECT ' + 
	N' jt.lTraderID, ' +
	N' jt.dtStartDate, ' +
	N' jt.dtEndDate, ' +
	N' jt.dtCreatedDate, ' +
	N' ft.name AS sFileName ' +
' FROM ' +  
   @sJointTableName + N' jt' +
N' INNER JOIN ' +
   @sReferencedTableName + N' ft' +
N' ON jt.hPathLocator = ft.path_locator' + 
N' WHERE ' + 
N' jt.lTraderID = ' + QUOTENAME(@lTraderID, '''')   + 
N' AND jt.dtStartDate =' + QUOTENAME(@dtStartDate, '''') + 
N' AND jt.dtEndDate = ' + QUOTENAME(@dtEndDate, '''')


EXEC (@SQL)


END

GO