DECLARE @sJointTableName	sysname
DECLARE @sReferencedTableName	sysname
DECLARE @bHashID  varchar(130)

SET @sJointTableName = 'tMultiFactorRiskBlobTest'
SET @bHashID = '0x901da74a1ea68152eefe77d9406f6e935c8613bdf203e4629b55b6538026dcbbcdb1956bcf0dddefcd5800d9ffa533b5e73e212062671cdba4fe05348a7b3e28'

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
	N' jt.bHashID, ' +
	N' jt.dtCreatedDate, ' +
	N' ft.name AS sFileName ' +
' FROM ' +  
   @sJointTableName + N' jt' +
N' INNER JOIN ' +
   @sReferencedTableName + N' ft' +
N' ON jt.hPathLocator = ft.path_locator' + 
N' WHERE ' + 
--N' jt.bHashID = CONVERT( varbinary(64) , ' + QUOTENAME(@bHashID, '''')   +  ', 1)'
N' jt.bHashID = CONVERT( varbinary(64) , ' + QUOTENAME(@bHashID, '''')   +  ', 1)'

EXEC (@SQL)


END

GO