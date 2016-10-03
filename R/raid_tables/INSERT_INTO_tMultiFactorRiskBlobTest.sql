USE [FileTableDB]
GO

INSERT INTO [dbo].[tMultiFactorRiskBlobTest]
           ([bHashID]
           ,[hPathLocator]
           ,[dtCreatedDate]
           ,[sCreatedByUserID])
     VALUES
           (CONVERT(varbinary(64),'0x901da74a1ea68152eefe77d9406f6e935c8613bdf203e4629b55b6538026dcbbcdb1956bcf0dddefcd5800d9ffa533b5e73e212062671cdba4fe05348a7b3e28', 1)
           ,'/198035132157021.33263262692097.2287113310/'
           ,'2016-09-25'
           ,'Lukasz.Bednarz')
GO


