/****** Script for SelectTopNRows command from SSMS  ******/
SELECT name 
  FROM [FileTableDB].[dbo].[ftMultiFactorRiskBlobTest]
  WHERE name = 'temp.txt'