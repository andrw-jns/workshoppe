library(DBI)
library(odbc)
library(dbplyr)


# 1. Establish Connection --------------------------------------------

# Create a connection object using DBI::dbconnect and odbc:
connection <- dbConnect(odbc::odbc(),
                     driver = "SQL Server",
                     server = "CSU-SQL-03",
                     database = "HESData", 
                     port = 1433 # always 1433
)


hes_ip_o75 <- DBI::dbGetQuery(connection,
                             "

SELECT  
  --[epikey] 
  -- to enable check for unique records
                           
  [LSOA11] as [LSOA]
  , [startage] as age
  , [admimeth]
  , [admisorc]
  , [classpat]
  
  
  FROM dbo.tbInpatients1516
  
  
  --LEFT OUTER JOIN [StrategicReference].[dbo].[LSOA01_LSOA11]
  --ON tbInpatients1415.LSOA01 = [StrategicReference].[dbo].[LSOA01_LSOA11].[LSOA01CD]
  --collate database_default
  
  
  WHERE epiorder = 1
  
  AND startage >= 75
  AND startage <= 110
  
  AND admimeth LIKE '2%' -- EMERGENCY ADMISSIONS
"
)

hes_ip_o75[3:5] <- map_df(hes_ip_o75[3:5], factor) %>% 
  select(-classpat)

# or mutate each

tabyl(hes_ip_o75, admi)

ggplot(hes_ip_o75, aes(classpat))+
  geom_histogram(stat = "count")

saveRDS(hes_ip_o75, "./data/hes_ip_o75.RDS")
test2 <- read_rds("./data/hes_ip_o75.RDS")
