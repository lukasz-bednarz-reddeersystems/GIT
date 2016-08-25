
USE FileTableDB

SELECT DISTINCT
   OBJECT_NAME(f.referenced_object_id) ReferencedTableName, 
   COL_NAME(fc.parent_object_id,fc.parent_column_id) ParentColName,
   COL_NAME(fc.referenced_object_id,fc.referenced_column_id) ReferencedColName
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
   OBJECT_NAME (f.parent_object_id) = 'tMultiFactorRiskBlobTest'