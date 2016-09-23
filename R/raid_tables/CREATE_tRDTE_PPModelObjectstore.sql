USE [FileTableDB]
GO

IF EXISTS (SELECT * FROM sysobjects WHERE name = 'tRDTE_PPModelObjectstore' AND type = 'U')
		DROP TABLE [dbo].[tRDTE_PPModelObjectstore]
GO

/****** Object:  Table [dbo].[tRDTE_PPModelObjectstore]    Script Date: 31/08/2016 14:14:05 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[tRDTE_PPModelObjectstore](
    [sPPModelClass] [varchar](256) NULL,
	[lTraderID] [bigint] NOT NULL,
	[dtStartDate] [datetime] NOT NULL,
	[dtEndDate] [datetime] NOT NULL,
	[hPathLocator] [hierarchyid] NOT NULL,
	[dtCreatedDate] [datetime] NULL,
	[sCreatedByUserID] [varchar](50) NULL,
 CONSTRAINT [AK_FileTableDB_PPModelObjectstore_PPModelTraderIDStartEndCreatedUserFactor] UNIQUE NONCLUSTERED 
(
	[sPPModelClass] ASC,
	[lTraderID] ASC,
	[dtStartDate] ASC,
	[dtEndDate] ASC,
	[dtCreatedDate] ASC,
	[sCreatedByUserID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO

ALTER TABLE [dbo].[tRDTE_PPModelObjectstore]  WITH CHECK ADD  CONSTRAINT [FK_ftRDTE_PPModelObjectstore_PathLocator] FOREIGN KEY([hPathLocator])
REFERENCES [dbo].[ftRDTE_PPModelObjectstore] ([path_locator])
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[tRDTE_PPModelObjectstore] CHECK CONSTRAINT [FK_ftRDTE_PPModelObjectstore_PathLocator]
GO


