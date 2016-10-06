USE [FileTableDB]
GO

IF EXISTS (SELECT * FROM sysobjects WHERE name = 'tRDTE_TradesObjectstore' AND type = 'U')
		DROP TABLE [dbo].[tRDTE_TradesObjectstore]
GO

/****** Object:  Table [dbo].[tRDTE_TradesObjectstore]    Script Date: 31/08/2016 14:14:05 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[tRDTE_TradesObjectstore](
    [bHashID] binary(64) NOT NULL,
	[lTraderID] bigint NOT NULL,
	[lInstrumentID] bigint NOT NULL,
	[sDirection] varchar(50) NOT NULL,
	[sStrategy] varchar(50) NOT NULL,
	[dtLegStartDate] datetime NOT NULL,
	[dtLegEndDate] datetime NOT NULL,
	[sLegStatus]  varchar(50) NOT NULL,
	[hPathLocator] hierarchyid NOT NULL,
	[dtCreatedDate] datetime NOT NULL,
	[sCreatedByUserID] varchar(50) NOT NULL,
CONSTRAINT [AK_FileTableDB_TradesObjectstore_HashIDPathLocator] UNIQUE NONCLUSTERED 
(
	[bHashID] ASC,
	[hPathLocator] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY],
 CONSTRAINT [AK_FileTableDB_TradesObjectstore_HashIDCreatedUser] UNIQUE NONCLUSTERED 
(
	[lTraderID] ASC,
	[lInstrumentID] ASC,
	[sDirection] ASC,
	[sStrategy] ASC,
	[dtLegStartDate] ASC,
	[dtLegEndDate] ASC,
	[sLegStatus] ASC,
	[dtCreatedDate] ASC,
	[sCreatedByUserID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO

ALTER TABLE [dbo].[tRDTE_TradesObjectstore]  WITH CHECK ADD  CONSTRAINT [FK_ftRDTE_TradesObjectstore_PathLocator] FOREIGN KEY([hPathLocator])
REFERENCES [dbo].[ftRDTE_TradesObjectstore] ([path_locator])
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[tRDTE_TradesObjectstore] CHECK CONSTRAINT [FK_ftRDTE_TradesObjectstore_PathLocator]
GO


