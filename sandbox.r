area <- sws_query(class.path="../sws/ojdbc14.jar", dbquery="select area, name_e from FAOSTAT.AREA ")

item <- sws_query(class.path="../sws/ojdbc14.jar", dbquery="select item, name_e from FAOSTAT.item ")

ele <- sws_query(class.path="../sws/ojdbc14.jar", dbquery="select * from FAOSTAT.ele ")

y <- FAOSTAT.TS_ICS_WORK_YR
head(x)
str(x)

y1 <- sws_query(area=33, ele=31, item=15, class.path="../sws/ojdbc14.jar")

items <- sws_query(class.path="../sws/ojdbc14.jar", dbquery="select * from FAOSTAT.ITEM")




sws_query(class.path="../sws/ojdbc14.jar", 
          dbquery="select area.name_e, TS_ICS_WORK_YR.item from FAOSTAT.AREA, FAOSTAT.TS_ICS_WORK_YR 
where area.area = 33 and area.area = TS_ICS_WORK_YR.area")

sws_query(class.path="../sws/ojdbc14.jar", 
          dbquery=
"select area.name_e, item.name_e, TS_ICS_WORK_YR.item, TS_ICS_WORK_YR.ele
from FAOSTAT.AREA, FAOSTAT.TS_ICS_WORK_YR, FAOSTAT.ITEM
where area.area = 33 and area.area = TS_ICS_WORK_YR.area 
and item.item = TS_ICS_WORK_YR.item")
