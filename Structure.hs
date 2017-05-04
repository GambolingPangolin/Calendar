{-
	A calendar item has the structure
	STARTDATE [STARTTIME] [ENDDATE [ENDTIME]] BODY [(NOTE)] [REPEAT]
-}
data CalItem = Point {
    date :: DateTime,
    body :: CalBody,
    repeat :: Repeat,
    subItems :: [CalItem]}
  | Interval {
    start :: DateTime,
    end :: DateTime,
    body :: CalBody,
    repeat :: Repeat,
    subItems :: [CalItem]}

data CalBody = Body | (Body, Note)
data DateTime = Date | (Date, Time)
data Body = String
data Note = String
data Repeat = Daily | Weekly | Monthly | None
