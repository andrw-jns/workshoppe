
###########################################################################
"EDA of CQC directory"
"care homes"
###########################################################################

library(janitor)
library(readxl)
library(GGally)
library(stringr)
library(tidyverse)

# cqc_dir <- read_excel("./data/01 October 2015 HSCA Active Locations.xlsx", 
#                       skip = 6)
# 
# cqc_dir <- clean_names(cqc_dir)
# 
# # cqc_dir[c(3,32:91)][cqc_dir[c(3,32:91)] == "Y"] <- 1
# # cqc_dir[c(3,32:91)][cqc_dir[c(3,32:91)] == "N" | is.na(cqc_dir[c(3,32:91)])] <- 0
# 
# cqc_dir <- cqc_dir %>% 
#   mutate_at(vars(c(3,32:91)),
#             funs(ifelse(. == "N" | is.na(.), 0, 1)
#                  )
#             )
# 
# 
# cqc_dir <- cqc_dir %>% filter(care_home == 1)
# 
# saveRDS(cqc_dir, "./data/cqc_dir_ch.RDS")
# cqc_dir <- read_rds("./data/cqc_dir_ch.RDS")
# file.choose()
# janitor::get_dupes(cqc_dir %>% select(-location_id))
# No duplicates

# cqc_sample <- cqc_dir %>% sample_n(1000) %>% 
#   rename(reg_nur_per = regulated_activity_accommodation_for_persons_who_require_nursing_or_personal_care,
#          reg_nur     = regulated_activity_nursing_care,
#          reg_per     = regulated_activity_personal_care,
#          st_chwn     = service_type_care_home_service_with_nursing,
#          st_chwon    = service_type_care_home_service_without_nursing,
#          sub_dement  = service_user_band_dementia,
#          sub_older   = service_user_band_older_people,
#          sub_whole   = service_user_band_whole_population)
# 
# pairs_plot <- ggpairs(cqc_sample, columns = c(33, 51, 52, 81, 84))

# cqc_big <- cqc_dir[c(2, 3, 4, 8:18, 21, 22, 33, 81, 84)] %>%
#   mutate(id = row_number())

# Assumption 1:
# Just Service user band (SUB) older people or SUB dementia. Will mean
# there may be homes with older people who are not categorised as such.

# cqc_dir %>%
#   tabyl(service_user_band_older_people)

# What about bed count?

# cqc_dir %>% 
#   group_by(service_user_band_older_people) %>% 
#   summarise(beds = sum(care_homes_beds)) %>% 
#   pull(beds) %>% 
#   prop.table()

# This is more like expected. 87% of beds for older people SUB.

# cqc_small <- cqc_big %>% 
#   filter(service_user_band_older_people == 1) %>% 
#   select(location_name,
#          care_homes_beds,
#          postal_code)


# (1.1) POPS LSOA OVER 75 ------------------------------------------
# 
# lsoa_pop <- read_excel(path  = "./data/SAPE18DT1-mid-2015-lsoa-syoa-estimates.xls",
#                        sheet = "Mid-2015 Persons",
#                        col_types = c("text", "skip", "text", "numeric", rep("skip", 60), rep("numeric", 31)),
#                        skip  = 3)
#                      
# lsoa_pop1 <- lsoa_pop %>%
#   select(-c(4:18)) %>%
#   clean_names() %>% 
#   rename(area_code = area_codes,
#          area_name = x_1) %>%
#   filter(!is.na(area_name)) %>%  # or,  na.omit() # omit all LOA aggregations
#   filter(grepl("E", area_code)) %>%  # in England
# # AGGREGATE AGE 75 TO 90+ COLUMNS
#   mutate(over_75 = rowSums(.[4:19])) %>%
#   select(-c(4:19))
# 
# saveRDS(lsoa_pop1, "./data/lsoa_pop_o75.RDS")
lsoa_pop <- read_rds("./data/lsoa_pop_o75.RDS")


# (1.3) HES DATA ----------------------------------------------------------

# hes_75_plus <- read_csv("HES-IP1516-over75.csv")
# can we make exclusions based on certain fields?
# ie. people from care homes must have admimeth = ambulance
# No - see below.

# hes_ip_o75 <- read_rds("./data/hes_ip_o75.RDS")
# 
# hes_ip_o75 <- hes_ip_o75 %>%  select(1, 2)
# 
# hes_grouped <- hes_ip_o75 %>% group_by(LSOA) %>% summarise(admissions = n())

# saveRDS(hes_grouped, "./data/hes.RDS")
hes <- read_rds("./data/hes.RDS")

hes <- hes %>% filter(grepl("E*", LSOA))?

# ggplot(hes_ip_o75, aes(admimeth))+
#   geom_bar()

# 95 % of  admisorc is usual place of res 
# admimeth as expected


# Postcode to LSOA --------------------------------------------------------

# postc_lsoa <- read_csv("./data/Postcode-to-LSOA.csv", 
#                        col_types = cols(LAD11CD   = col_skip(), 
#                                         LAD11NM   = col_skip(),
#                                         LAD11NMW  = col_skip(), 
#                                         MSOA11CD  = col_skip(), 
#                                         MSOA11NM  = col_skip(), 
#                                         OA11CD    = col_skip(), 
#                                         PCDOASPLT = col_skip(),
#                                         LSOA11NM  = col_skip()
#                                         ))
# # test <- postc_lsoa %>% 
# #   slice(1:10)
# 
# # saveRDS(postcode_lsoa, ".\data\postc")
# 
# 
# cqc_with_lsoa1 <- inner_join(cqc_big, postc_lsoa, by = c("postal_code"= "PCD7"))
# cqc_with_lsoa2 <- inner_join(cqc_big, postc_lsoa, by = c("postal_code"= "PCD8"))
# 
# # 750 homes without an LSOA match?
# 
# cqc_with_lsoa1 <- cqc_with_lsoa1 %>% select(-20)
# cqc_with_lsoa2 <- cqc_with_lsoa2 %>% select(-20)
# 
# 
# # join and remove unnec. cols:
# cqc_big2 <-  bind_rows(cqc_with_lsoa1,cqc_with_lsoa2) 
# 
# saveRDS(cqc_big2, "./data/cqc_with_lsoa.RDS")

# newer postcodes?
# missings <- anti_join(cqc_big, cqc_big2, by = c("id"))
# %>%
#   select(-c(8,10))

# rm(cqc_with_lsoa1, cqc_with_lsoa2)

# search for missings:
# test <- postc_lsoa %>% filter(str_detect(PCD7,"EX1*"))

# deprivation -------------------------------------------------------------

# deprivation <- read_csv("./data/societal-wellbeing-imd-indices.csv" , skip = 6)
# deprivation <- deprivation[-1]
# 
# deprivation <- deprivation %>%
#   separate(`Reference area` , c("Name", "LSOA") , sep = -10)
# 
# ## Check all indices in regression  model ##
# deprivation <- deprivation[-1]
# colnames(deprivation) <- c("lsoa", "imd", "income", "employment", "education",
#                                     "health" , "crime", "services", "environment",
#                                     "children",  "idaopi")
# 
# # See: English_Indices_of_Deprivation_2015_-_Statistical_Release.pdf
# 
# 
# # Quintile reduces model complexity and shouldn't lose anything.
# deprivation <- deprivation %>% 
#   mutate(idaopi_quint = case_when(
#     deprivation$idaopi %in% c(1,2)  ~ 1,
#     deprivation$idaopi %in% c(3,4)  ~ 2,
#     deprivation$idaopi %in% c(5,6)  ~ 3,
#     deprivation$idaopi %in% c(7,8)  ~ 4,
#     deprivation$idaopi %in% c(9,10) ~ 5
#   ))

# saveRDS(deprivation, "./data/deprivation.rds")


# ccg ---------------------------------------------------------------------

# from the ONS open geography portal:
lsoa_to_ccg <- read_csv("./data/Lower_Layer_Super_Output_Area_2011_to_Clinical_Commissioning_Group_to_Local_Authority_District_April_2016_Lookup_in_England.csv")
lsoa_to_ccg <- lsoa_to_ccg[-c(2,3,6:8)]
colnames(lsoa_to_ccg) <- c("lsoa", "ccg_code", "ccg_name")

saveRDS(lsoa_to_ccg, "./data/ccg_lsoa.rds")


# quick load individual -------------------------------------------------------------


hes      <- read_rds("./data/hes.RDS") %>% filter(str_detect(LSOA, "E"))
lsoa_pop <- read_rds("./data/lsoa_pop_o75.RDS")
cqc_big  <- read_rds("./data/cqc_with_lsoa.RDS")
"Note missing entries"

ccg_lsoa    <- read_rds("./data/ccg_lsoa.rds")
deprivation <- read_rds("./data/deprivation.rds") %>% select(lsoa, imd, idaopi, idaopi_quint)
"deaths data"
"cqc ratings"


# joins -------------------------------------------------------------------

# map(list(lsoa_pop, hes, ccg_lsoa, cqc_big, deprivation), names)

# the straightforward joins:
join1 <- left_join(lsoa_pop, ccg_lsoa, by = c("area_code" = "lsoa"))
join2 <- left_join(join1, deprivation, by = c("area_code" = "lsoa"))


join3 <- full_join(join2, hes, by = c("area_code" = "LSOA"))
# examine anti joins
sum(is.na(join3$admissions)) # that's ok!!! 53 LSOAs don't have and admission
# but there are no admissions that don't have an LSOA

cqc_grouped <- cqc_big %>%
  group_by(LSOA11CD) %>%
  summarise(beds = sum(care_homes_beds))

join4 <- full_join(join3, cqc_grouped, by = c("area_code" = "LSOA11CD"))
# examine anti-joins, but that is it for now.

dummy <- join4 %>% mutate(deaths = round(rnorm(32844, over_75/10, over_75/20)),
                          ranking = round(runif(32844, 1, 5))) %>% 
  mutate(deaths = if_else(deaths < 0 , 0, deaths))

dummy_data <- saveRDS(dummy, "./data/carehome_dummy.rds")

care_homes <- read_rds("./data/carehome_dummy.rds")

