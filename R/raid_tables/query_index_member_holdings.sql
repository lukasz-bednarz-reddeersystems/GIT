/****** Script for SelectTopNRows command from SSMS  ******/
--SELECT DISTINCT CONVERT (DATE, [dtDateTime]) as dtDate, sIndexTicker, sMemberTicker

--  FROM [Research_Archive].[dbo].[tIndexMembersHolding_Archive]
--  WHERE sIndexTicker LIKE 'BE500%'
--  --CONVERT (DATE, [dtDateTime]) LIKE '2015-10-23'
--  ORDER BY dtDate DESC


DECLARE 

@sIndexTicker	nvarchar(50) = 'BE500 Index',
@dtDateStart    datetime = N'2015-06-01',
@dtDateEnd    datetime = N'2016-09-06'


--SELECT TOP 1 dtDateTime, sIndexTicker, sMemberTicker, dblWeight
--  FROM [Research_Archive].[dbo].[tIndexMembersHolding_Archive]
--  INNER JOIN t
--  --INNER JOIN tInstrument TI
--  --ON sMemberTicker = sBBGTicker
--  WHERE sIndexTicker LIKE 'BE500%' 
--  AND
--  dtDateTime >= @dtDateStart 
--  AND
--  dtDateTime <= @dtDateEnd
  
  --CONVERT (DATE, [dtDateTime]) LIKE '2015-10-23'


SELECT	DISTINCT
		CONVERT(DATE, tHolding.dtDateTime) AS dtDate
,       tMember.lInstrumentID AS lInstrumentID
,		tHolding.sMemberTicker AS sInstrumentName
,		tHolding.dblWeight AS dblWeight

FROM Research_Archive.dbo.tIndexMembersHolding_Archive AS tHolding

INNER JOIN tInstrument AS tIndex
ON tHolding.sIndexTicker = tIndex.sBBGTicker

LEFT JOIN tInstrument AS tMember
--ON tHolding.sMemberTicker LIKE tMember.lInstrumentID
ON tMember.sBBGTicker LIKE tHolding.sMemberTicker + ' Equity'

WHERE tHolding.sIndexTicker = @sIndexTicker
  AND
  dtDateTime >= @dtDateStart 
  AND
  dtDateTime <= @dtDateEnd
  AND
  tMember.lInstrumentID = '4454'

ORDER BY  dtDate DESC