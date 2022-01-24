

#########################################################
################## TO DATABASE ##########################
#########################################################

library(DBI)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "./data/iso_standards.sqlite")

members <- unnest(members, cols = c(data))
liaison <- unnest(liaison, cols = c(data))

dbWriteTable(con, "members", members)
dbWriteTable(con, "liaison", liaison)

dbListTables(con)
