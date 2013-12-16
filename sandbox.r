area <- sws_query(class.path="../sws/ojdbc14.jar", dbquery="select area, name_e from FAOSTAT.AREA ")

item <- sws_query(class.path="../sws/ojdbc14.jar", dbquery="select item, name_e from FAOSTAT.item ")

ele <- sws_query(class.path="../sws/ojdbc14.jar", dbquery="select * from FAOSTAT.ele ")

y <- FAOSTAT.TS_ICS_WORK_YR
head(x)
str(x)

y1 <- sws_query(area=33, ele=c(31, 51), item=15, year=1960:1970, class.path="../sws/ojdbc14.jar")

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

y <- sws_query(class.path="../sws/ojdbc14.jar", dbquery=
"SELECT 
FAOSTAT.ITEM.ITEM, 
FAOSTAT.ITEM.NAME_E AS ItemName, 
FAOSTAT.ITEM_TYP_ELE_DISPLAY_ELE.ELE,
FAOSTAT.ITEM_TYP_ELE_DISPLAY_ELE.DISPLAY_ELE, 
FAOSTAT.ITEM_TYP_ELE_DISPLAY_ELE.STD_UNIT, 
FAOSTAT.ITEM_TYP_ELE_DISPLAY_ELE.LONG_NAME_E AS EleName
FROM FAOSTAT.ITEM INNER JOIN FAOSTAT.ITEM_TYP_ELE_DISPLAY_ELE ON 
FAOSTAT.ITEM.ITEM_TYP = FAOSTAT.ITEM_TYP_ELE_DISPLAY_ELE.ITEM_TYP
")

# Get name of ele based on item and ele codes.
"SELECT 
i.ITEM,
i.NAME_E AS ItemName,
e.ELE,
e.DISPLAY_ELE,
e.STD_UNIT,
e.LONG_NAME_E AS EleName 
FROM FAOSTAT.ITEM AS i 
INNER JOIN FAOSTAT.ITEM_TYP_ELE_DISPLAY_ELE AS e ON 
i.ITEM_TYP = e.ITEM_TYP

"


y1 <- sws_query(ele=c(31, 51), item=15, year=1960:1980, class.path="../sws/ojdbc14.jar")
library(stringr)
library(reshape2)

y1l <- melt(y1, measure.vars=names(y1)[str_detect(names(y1), '(^num_)|(^symb_)')])


str_extract(names(y1), '([0-9]{2}$)')

x <- names(y1)[5]
orignumb <- as.numeric(str_extract(x, '([0-9]{2}$)'))
corryear <- orignumb + 1959
corrname <- str_c(str_replace(x, '(^.*)([0-9]{2}$)', '\\1'), corryear)
corrname
                  
str_detect(c('area', 'num_01', '!symb_01'), perl('(!symb)'))

df <- data.frame(a = runif(10, 30, 40), b = rnorm(10, 35, 5))
l <- list(alessb = 'a < b', bless35 = 'b < 35')
r <- logic_check(df, l)
# Check the row has at least one error
aaply(r, .(1), function(x) sum(x) < length(x))

# What data columns take part in rules
laply(names(df), function(x) sum(str_detect(unlist(l), x)) > 0)

# Check the rule has at least one trigger
aaply(r, .(2), function(x) sum(x) < length(x))


# Mondrian
x1 <- sws_query(33:40, item=866, ele=11, year=1996:2000)
x1$flag[x1$flag==' '] <- 'good'
write.table(x1, 'x1.tsv', quote=F, sep='\t', row.names=F)
