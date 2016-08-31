USE [FileTableDB]
GO

DECLARE	@jointTable TABLE
(
lTraderID			bigint,
dtStartDate			datetime,
dtEndDate			datetime,
hPathLocator        hierarchyid,
dtCreatedDate		datetime,
sCreatedByUserID	varchar(50)
)

DECLARE @tempTable TABLE
(
sReferencedTableName varchar(50),
sReferencedColumnName varchar(50),
sParentColumnName varchar(50)
)

DECLARE @sRefTableName varchar(50)

INSERT INTO @tempTable

EXEC	[dbo].[prMultiFactorRisk_ReferencedFileTable_SelectByParentTableName]
		@sParentTableName = 'tMultiFactorRiskBlobTest'


--SELECT @sRefTableName = (SELECT TOP 1 sReferencedTableName FROM @tempTable WHERE sReferencedTableName IS NOT NULL)

SELECT @sRefTableName = (SELECT TOP 1 sReferencedTableName FROM @tempTable WHERE sReferencedTableName IS NOT NULL)


INSERT INTO @jointTable

EXEC	[dbo].[prMultiFactorRisk_JointFileTable_QueryByTbNameTraderIDStartDateEndDate]
		@sJointTableSchema = FileTableDB,
		@sJointTableName = tMultiFactorRiskBlobTest,
		@lTraderID = 11,
		@dtStartDate = N'2016-01-01',
		@dtEndDate = N'2016-01-01'

SELECT	* FROM @jointTable
 INNER JOIN 

GO
