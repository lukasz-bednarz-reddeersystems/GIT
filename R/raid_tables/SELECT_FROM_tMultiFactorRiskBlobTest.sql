/****** Script for SelectTopNRows command from SSMS  ******/
DECLARE @bHashID  varchar(130)

SET @bHashID = '0x901da74a1ea68152eefe77d9406f6e935c8613bdf203e4629b55b6538026dcbbcdb1956bcf0dddefcd5800d9ffa533b5e73e212062671cdba4fe05348a7b3e28'


SELECT TOP 1000 [bHashID]
      ,[name]
      ,[hPathLocator]
      ,[dtCreatedDate]
      ,[sCreatedByUserID]
  FROM [FileTableDB].[dbo].[tMultiFactorRiskBlobTest]
  INNER JOIN [FileTableDB].[dbo].[ftMultiFactorRiskBlobTest] as ft
  ON ft.path_locator = hPathLocator

  WHERE 
  bHashID = CONVERT(varbinary(64), @bHashID,1)