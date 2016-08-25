USE FileTableDB
GO

BEGIN TRANSACTION
SET QUOTED_IDENTIFIER ON
SET ARITHABORT ON
SET NUMERIC_ROUNDABORT OFF
SET CONCAT_NULL_YIELDS_NULL ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
COMMIT
BEGIN TRANSACTION
GO
CREATE TABLE dbo.tMultiFactorRiskBlobTest
	(
	lTraderID bigint NOT NULL,
	dtStartDate datetime NOT NULL,
	dtEndDate datetime NOT NULL,
	hPathLocator hierarchyid NOT NULL,
	CONSTRAINT AK_FileTableDB_BlobTest_TraderIDStartEndFactor UNIQUE(lTraderID,dtStartDate,dtEndDate),
	CONSTRAINT FK_FileTableDB_ModelTypeID_Betas_ModelType FOREIGN KEY (hPathLocator) REFERENCES dbo.ftMultiFactorRiskBlobTest (path_locator) ON DELETE CASCADE,
	)

GO
ALTER TABLE dbo.tMultiFactorRiskBlobTest SET (LOCK_ESCALATION = TABLE)
GO
COMMIT
