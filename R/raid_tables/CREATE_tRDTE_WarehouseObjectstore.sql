USE [FileTableDB]
GO

IF EXISTS (SELECT * FROM sysobjects WHERE name = 'tRDTE_WarehouseObjectstore' AND type = 'U')
		DROP TABLE [dbo].[tRDTE_WarehouseObjectstore]
GO

/****** Object:  Table [dbo].[tRDTE_WarehouseObjectstore]    Script Date: 31/08/2016 14:14:05 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[tRDTE_WarehouseObjectstore](
	[bHashID] binary(64) NOT NULL,
	[hPathLocator] hierarchyid NOT NULL,
	[dtCreatedDate] datetime NOT NULL,
	[sCreatedByUserID] varchar(50) NOT NULL,
 CONSTRAINT [AK_FileTableDB_WarehouseObjectstore_HashIDPathLocator] UNIQUE NONCLUSTERED 
(
	[bHashID] ASC,
	[hPathLocator] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY],
 CONSTRAINT [AK_FileTableDB_WarehouseObjectstore_HashIDCreatedUser] UNIQUE NONCLUSTERED 
(
	[bHashID] ASC,
	[dtCreatedDate] ASC,
	[sCreatedByUserID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO

ALTER TABLE [dbo].[tRDTE_WarehouseObjectstore]  WITH CHECK ADD  CONSTRAINT [FK_ftRDTE_WarehouseObjectstore_PathLocator] FOREIGN KEY([hPathLocator])
REFERENCES [dbo].[ftRDTE_WarehouseObjectstore] ([path_locator])
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[tRDTE_WarehouseObjectstore] CHECK CONSTRAINT [FK_ftRDTE_WarehouseObjectstore_PathLocator]
GO


