-- =========================================
-- Create FileTable template
-- =========================================
USE FileTableDB
GO

IF OBJECT_ID([dbo].[ftMultiFactorRiskBlobTest]) IS NOT NULL
  DROP TABLE [dbo].[ftMultiFactorRiskBlobTest]
GO

CREATE TABLE [dbo].[ftMultiFactorRiskBlobTest] AS FILETABLE
  WITH
  (
    FILETABLE_DIRECTORY = N'BlobTest',
    FILETABLE_COLLATE_FILENAME = Latin1_General_CI_AS
  )
GO
