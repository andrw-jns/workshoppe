###########################################################################
" CARE_V3"
# created "2017-05-22 11:46:40 BST"
# lastedit "2017-09-05 16:11:11 BST"

###########################################################################

library(tidyverse)
library(readxl)
library(magrittr)


project <- FALSE

# Aims:
# 0. The model is only as good as the data it's fed. Improve this?
# 1. Put into RMarkdown.
# 2. Look at a smaller geographic area, and map.
# 


# ***** --------------------------------------------------------------



# (1) LOAD DATA -----------------------------------------------------------

base_dir <- if(project){getwd()}else{"//clients.its.local/csu/users01/andrew.jones/Desktop/care_home_incarnations/CARE_v3"}
data_dir <- paste0(base_dir, "/Data")
setwd(data_dir)


# (1.1) POPS LSOA OVER 75 ------------------------------------------

# The original dataset would not read properly (old version of Excel?) 
# - due absence of full list of col names, so temporarily added
# a colname, "lsoa name" , to the "_edit" version.

lsoa_pop15 <- read_excel("SAPE18DT1-mid-2015-lsoa-syoa-estimates_edit.xlsx", 
                         sheet = "Mid-2015 Persons", 
                         col_types = c("text", "skip", "text", rep("numeric", 92) ), 
                         skip = 3)



# NB. IF DF HAVE UN-NAMED COLS - NAME AS SOON AS POSSIBLE TO PREVENT PROBLEMS!
colnames(lsoa_pop15)[c(1:3)] <- c("LSOA", "LSOA11NM", "total")


pop_75 <- lsoa_pop15 %>% 
  select(1:3,79:94) %>% 
  filter(!is.na(LSOA11NM)) %>% 
  filter(grepl("E", LSOA))
# Because no need for WALES AND SCOTLAND

# AGGREGATE AGE 75 TO 90+ COLUMNS
lsoa_pop_2015 <- pop_75 %>% 
  mutate(Over75 = rowSums(pop_75[4:19])) %>%
  select(-c(4:19))

# rm(list=setdiff(ls(), "lsoa_table15"))


# saveRDS(lsoa_pop15, "lsoa_pop15.rds")

# (1.2) CQC DIR. -------------------------------------------------
"The CQC directory is a very poor data source"
# For this version load only the October directory is used (mid way through
# finacial year)

# CQC October 2015
CQC_dir <- read_excel("01 October 2015 HSCA Active Locations.xlsx",
                      skip = 6)

# added bonus that it gives time for it to be up to date

CQC_dir1 <- CQC_dir %>%
  filter(`Care home?` == "Y"
                #`Service user band - Older People` == "Y" |
                #  `Service user band - Dementia` == "Y"
  ) %>%
  dplyr::select(4,8,10,12,13,16,51) 



# Now including 5,000 other care homes are for learning disabilities/ mental/physical health
# It's in the 80% assumption. 
# See how this affects the model.


colnames(CQC_dir1)[c(6,7)] = c('postcode',"nursing")

# saveRDS(CQC_dir1, "cqc_dir1.rds")

rm(lsoa_pop15, pop_75)
rm(CQC_dir)
gc()

# (1.3) HES DATA ----------------------------------------------------------

hes_75_plus <- read_csv("HES-IP1516-over75.csv")
# can we make exclusions based on certain fields?
# ie. people from care homes cannot be...

"IMPORTANT... OR:"
# Can one use ambulance data?
# SWB ambulance data?

# (1.4) CCG LOOKUP --------------------------------------------------------

# from the ONS open geography portal:
lsoa_to_ccg <- read_csv("Lower_Layer_Super_Output_Area_2011_to_Clinical_Commissioning_Group_to_Local_Authority_District_April_2016_Lookup_in_England.csv")
lsoa_to_ccg <- lsoa_to_ccg[-c(2,3,6:8)]
colnames(lsoa_to_ccg)[1] <- "LSOA"

#saveRDS(ccg_lsoa, "ccg_lsoa.rds")


# (1.5) POSTCODE - LSOA LOOKUP --------------------------------------------

postc_lsoa <- read_csv("Postcode-to-LSOA.csv", 
                       col_types = cols(LAD11CD   = col_skip(), 
                                        LAD11NM   = col_skip(),
                                        LAD11NMW  = col_skip(), 
                                        MSOA11CD  = col_skip(), 
                                        MSOA11NM  = col_skip(), 
                                        OA11CD    = col_skip(), 
                                        PCDOASPLT = col_skip(),
                                        LSOA11NM  = col_skip()
                                        )
                       )

colnames(postc_lsoa)[1:3] <- c("postcode_1spc", "postcode_2spc", "LSOA")


# saveRDS(postc_lsoa, "postc_lsoa.rds")

# (1.6) DEPRIVATION DATA ----------------------------------------------

deprivation <- read_csv("societal-wellbeing-imd-indices.csv" , skip = 6)
deprivation <- deprivation[-1]

deprivation <- deprivation %>%
  separate(`Reference area` , c("Name", "LSOA") , sep = -10)

## Check all indices in regression  model ##
deprivation <- deprivation[-1]
colnames(deprivation)[c(2:11)] <- c("IMD", "Income", "Employment", "Education",
                                    "Health" , "Crime", "Services", "Environment",
                                    "Children",  "IDAOPI")

# See: English_Indices_of_Deprivation_2015_-_Statistical_Release.pdf


# Quintile reduces model complexity and shouldn't lose anything.
deprivation %<>% 
  mutate(IDAOPI = case_when(
    deprivation$IDAOPI %in% c(1,2)  ~ 1,
    deprivation$IDAOPI %in% c(3,4)  ~ 2,
    deprivation$IDAOPI %in% c(5,6)  ~ 3,
    deprivation$IDAOPI %in% c(7,8)  ~ 4,
    deprivation$IDAOPI %in% c(9,10) ~ 5
  ))


# saveRDS(deprivation, "deprivation.rds")


# ***** --------------------------------------------------------------



# (2) WRANGLE: CQC CORRECTIONS -----------------------------------------------------
setwd(base_dir)

# Add LSOA to CQC Directory 

# Because the 7 and 8 character postcodes in lookup are different:
cqc_with_lsoa1 <- inner_join(CQC_dir1, postc_lsoa, by = c("postcode"= "postcode_1spc"))
cqc_with_lsoa2 <- inner_join(CQC_dir1, postc_lsoa, by = c("postcode"= "postcode_2spc"))

# join and remove unnec. cols:
cqc_with_lsoa <- bind_rows(cqc_with_lsoa1,cqc_with_lsoa2) %>%
  select(-c(8,10))

rm(cqc_with_lsoa1,cqc_with_lsoa2)


# Remove duplicates --------------------------------------------------

# to check that all have LSOA - FINE
# cqc_with_lsoa %>%
#   dplyr::filter(LSOA == NA)

# Remove duplicates: 
" PERHAPS MORE WORK NEEDS TO BE DONE HERE - perhaps not"
" PERHAPS DON'T REMOVE THESE?"

cqc_test <- CQC_dir1 %>%
  count(`Location Name`, `Street Address`, `Care homes beds`) %>%
  arrange(desc(n)) %>% 
  filter(n > 1)
# 32 instances
# EG. The first one of these means: remove entries where location name and Street Address
# and beds is duplicated.
cqc_filter1 <- distinct(cqc_with_lsoa,`Location Name`, `Street Address`, `Care homes beds`, .keep_all = TRUE) 

# Look again at this logic.
setdiff(cqc_with_lsoa, cqc_filter1)

cqc_test <- cqc_filter1 %>%
  count(`Location Name`,`Care homes beds`, Postcode) %>%
  arrange(desc(n)) 
# These two homes are, in fact, two distinct homes. But they are duplicated.
cqc_filter2 <- distinct(cqc_filter1,`Location Name`, `Care homes beds`, Postcode, .keep_all = TRUE) 


" NB. EVERYTHING ELSE LOOKS IN ORDER (OTHER POTENTIAL PERMETATIONS OF DUPLICATES CHECK OUT)"


# (3.1) CREATE A TABLE FOR CAREHOME LSOAS --------------------------

# COUNT ADMISSIONS BY LSOA
admissions_by_lsoa <- hes_75_plus %>%
  # REMOVE WALES AND SCOTLAND HERE
  filter(grepl("E", LSOA) | is.na(LSOA)) %>% 
  count(LSOA) %>%
  arrange(desc(n)) 

# saveRDS(admissions_by_lsoa, "admissions.rds")

# admissions_by_lsoa %>% filter(is.na(LSOA))

6203/1652090
# 0.38% have no given lsoa
# promising improvement over data in previous year

admissions_by_lsoa %<>%  
  na.omit()

# qplot(data = admissions_by_lsoa, n , binwidth = 3)
# excellent negative binomial
# could expect some LSOAs (in inner city - as shown in Brum) to have no over75 residents, thus
# MASS::fitdistr(admissions_by_lsoa$n, "negative binomial")


bed_count_lsoa <- cqc_with_lsoa %>%
  group_by(LSOA) %>%
  summarise(beds = sum(`Care homes beds`)) %>%
  arrange(desc(beds))

# qplot(data = bed_count_lsoa, Beds, binwidth = 1)

adm_c_hom <- left_join(admissions_by_lsoa, bed_count_lsoa, by = "LSOA")

ifelse(is.na(adm_c_hom$beds))

# Join beds to admissions. Add binary variable for homes
adm_c_hom <- inner_join(admissions_by_lsoa, bed_count_lsoa, by = "LSOA")
# there is perhaps one duplicate entry?

# qplot(data = carehome_lsoas, x= Beds , y= n)



# (3.2) NON CAREHOME LSOAS ------------------------------------------------

adm_no_c_home <- anti_join(admissions_by_lsoa, bed_count_lsoa, by = "LSOA")
#colnames(noncarehome_lsoas)[2] <- "Admissions_75_plus"
adm_no_c_home$beds <- 0
#noncarehome_lsoas$Home <- 0

# (3.3) JOIN ALL TO LSOA POP DATA ----------------------------------------------

# NB. "Over-75 admissions" variable is named "n"

BOTH_lsoas <- bind_rows(carehome_lsoas,noncarehome_lsoas)

# qplot(data = BOTH_lsoas, x= Beds , y = n)

n_distinct(lsoa_table15)

"NOTE DISCREPANCY IN NUMBERS, WHICH SEEMS LOGICAL - DIFFERENT COVERAGES 
COUNTRYWIDE"

# JOIN TO POPn DATA:
master_carehome <- left_join(BOTH_lsoas, lsoa_table15, by = "LSOA") %>%
  arrange(desc(n))

master_carehome <- left_join(master_carehome, ccg_lsoa, by = "LSOA")



# (4) ADD OCCUPANCY RATE (NO LONGER APPLICABLE HERE - adjust afterwards)------

# Estimated 88.4% (all care - national rate) occupancy from # http://content.knightfrank.com/research/548/documents/en/2015-3267.pdf
# And of those 82.5% over 75 http://www.cpa.org.uk/information/reviews/Bupa-Census-2012.pdf


# master_carehome <- master_carehome %>%
#   mutate(BedOcc = round(Beds* 0.884*0.825)) %>%
#   #mutate(Flag = if_else(BedOcc > Over75 , 1, 0)) %>%
#   mutate(BedOcc  = ifelse(BedOcc > Over75, Over75, BedOcc)) # constrain to over75 pop
#mutate(Over75  = ifelse(Flag == 1 ,BedOcc, Over75))
# So that Over75 population not exceeded (and later does not go -ve)
# Only one case : Cannock Chase 010B

"THERE IS NOW NO BED CONSTRAINT"
# master_carehome %>% filter(Beds > Over75)
"120 LSOAs have more beds than over75 residents. Which may present probs
if beds is a proxy for population"

#master_carehome %<>% 
# mutate(bed_cnstr = ifelse)

master_carehome <- inner_join(master_carehome, deprivation, by = "LSOA")

# sum(master_carehome$BedOcc)
# We're getting the right kind of numbers
# 0.825*325000


# (5) REGRESSION ANALYSIS ------------------------------------------------

# This version contains only the final model. For all models and 
# comparisons, see notes in "care-v06.R" 

# THIS SEEMS TO ADDRESS CONCERN ABOUT OFFSET:
# http://stats.stackexchange.com/questions/66791/where-does-the-offset-go-in-poisson-negative-binomial-regression

# Following procedure on:
# http://www.ats.ucla.edu/stat/r/dae/nbreg.htm

# MORE NOTES IN "care-v04.R" - found in original development project directory.


FINAL_TABLE <- master_carehome

FINAL_TABLE$IMD    <-  as.factor(FINAL_TABLE$IMD)
FINAL_TABLE$IDAOPI <-  as.factor(FINAL_TABLE$IDAOPI)
# FINAL_TABLE$Health <-  as.factor(FINAL_TABLE$Health) ignore due colinearity

FINAL_TABLE$CCG16NM <- as.factor(FINAL_TABLE$CCG16NM) 
# FINAL_TABLE$Home    <- as.factor(FINAL_TABLE$Home) 



# THE MODEL:

#library(R2admb)    # special "negbinom" model 
#library(glmmADMB)  # special "negbinom" model 
# model_home <- glmmadmb(formula = n ~ BedOcc + CCG16NM + Health + Home + offset(log(Over75))
#                        , data = FINAL_TABLE, family = "nbinom2"
#                        , zeroInflation = F) 

# REGRESSION: WENT WITH PACKAGE MASS AS ANSWER NO DIFFERENCE TO ALTERNATIVES AND MORE WIDELY RECOGNISED/TESTED?
# library(MASS) # nb.glm

# IDAOPI is used to avoid colinearity problems with Health (it uses Admissions to Hospital and Disability)
# http://geoconvert.mimas.ac.uk/help/imd-2007-manual.pdf

model_home <- MASS::glm.nb(n ~ Beds +  IDAOPI + CCG16NM + offset(log(Over75)),
                           data = FINAL_TABLE)


"MUCH MORE PROMISING !!!"
BIC(model_home)
# [1] 261020 - DECILE
# [1] 261490.3 - QUINTILE

# [1] 261384.7 - QUINTILE WITH JUST BEDS (unconstrained).


# summary(model_home)
# (est <- cbind(Estimate = coef(model_home), confint(model_home)))
# exp(est)



# () SECTION 2 -------------------------------------------------------


# (1) LOAD DATA (2) -----------------------------------------------------------

setwd(data_dir)

# Unwisely and arbitrarily chosen to suffix everything associated with nur/care 
# dichotomy as "7" ;)
CQC_dir7 <- CQC_dir %>%
  dplyr::filter(`Care home?` == "Y",
                (`Service user band - Older People` == "Y" &
                   (`Service type - Care home service with nursing` == "Y"|
                      `Service type - Care home service without nursing` == "Y")) |
                  `Service user band - Dementia` == "Y" ) %>%
  dplyr::select(4,8,10,12,13,16,51,52,81) # added sans nursing, and dementia
# REASSURING TO KNOW THIS RETURNS THE SAME NUMBERS AS CQC DIR

" ASSUMPTIONS MADE HERE"
" !!! "
# ALLOCATE BEDS TO CARE / NURSING HOMES (DEMENTIA CANNOT BE SEPARATED FROM NURSING)
# BELOW CAN BE CLEARER WITH SWITCH FUNCTION?

CQC_dir7 <- CQC_dir7 %>% 
  mutate(ch_beds = ifelse(`Service type - Care home service without nursing` == "Y" &
                            is.na(`Service type - Care home service with nursing`),
                          `Care homes beds`, 0)) %>%
  mutate(nur_beds = ifelse(`Service type - Care home service with nursing` =="Y" &
                             is.na(`Service type - Care home service without nursing`),
                           `Care homes beds`, 0)) %>%   # AND A FEW HOMES HAVE BOTH SO DIVIDE BY TWO IN THIS CASE
  mutate(both_beds = ifelse(`Service type - Care home service with nursing` == "Y" &
                              `Service type - Care home service without nursing` == "Y",
                            `Care homes beds`, 0))

CQC_dir7$both_beds[is.na(CQC_dir7$both_beds)] <- 0

# AND A FEW HOMES HAVE BOTH SO DIVIDE BY TWO IN THIS CASE: HALF BEDS TO EACH

CQC_dir7 <- CQC_dir7 %>% 
  mutate(care_beds = ch_beds + ceiling(both_beds/2)) %>%
  mutate(nurse_beds = nur_beds + floor(both_beds/2))
# Round up care beds , down nursing beds.

CQC_dir7 <- CQC_dir7 %>% 
  select(c(1:6,13,14))



colnames(CQC_dir7)[c(6)] = c('Postcode')

# (3) CQC CORRECTIONS -----------------------------------------------------
setwd(base_dir)


# Add LSOA to CQC Directory 

# Because the 7 and 8 character postcodes in lookup are different:
cqc7_with_lsoa1 <- inner_join(CQC_dir7, postc_lsoa, by = c("Postcode"= "Postcode1"))
cqc7_with_lsoa2 <- inner_join(CQC_dir7, postc_lsoa, by = c("Postcode"= "Postcode2"))

# join and remove unnec. cols.
cqc_with_lsoa7 <- bind_rows(cqc7_with_lsoa1,cqc7_with_lsoa2)

# to clean workspace 
rm(cqc7_with_lsoa1,cqc7_with_lsoa2)

# to check that all have LSOA - FINE
# cqc_with_lsoa %>%
#   dplyr::filter(LSOA == NA)


# Now select homes from directory for only LSOAs in WM.


cqc_filter1 <- distinct(cqc_with_lsoa7,`Location Name`, `Street Address`, `Care homes beds`, .keep_all = TRUE) 

cqc_filter2 <- distinct(cqc_filter1,`Location Name`, `Care homes beds`, Postcode, .keep_all = TRUE) 


# NB. EVERYTHING ELSE LOOKS IN ORDER (OTHER POTENTIAL DUPLICATES CHECK OUT)




# (4.1) CREATE A TABLE FOR CAREHOME LSOAS --------------------------

# Care and nursing, BY LSOA
bed_count_lsoa7 <- cqc_filter2 %>%
  group_by(LSOA, LSOA11NM) %>%
  summarise(beds = sum(`Care homes beds`),
            nurse_beds = sum(nurse_beds),
            care_beds = sum(care_beds))
# qplot(data = bed_count_lsoa, Beds, binwidth = 1)

# Join beds to admissions. Add binary variable for homes
carehome_lsoas7 <- inner_join(admissions_by_lsoa, bed_count_lsoa7)
#carehome_lsoas$Home <- 1  

# qplot(data = carehome_lsoas, x= Beds , y= n)



# (4.2) NON CAREHOME LSOAS ------------------------------------------------

noncarehome_lsoas7 <- anti_join(admissions_by_lsoa, bed_count_lsoa7)
#colnames(noncarehome_lsoas)[2] <- "Admissions_75_plus"
noncarehome_lsoas7$beds <- 0
noncarehome_lsoas7$nurse_beds <- 0
noncarehome_lsoas7$care_beds <- 0

# noncarehome_lsoas$Home <- 0



# (4.3) JOIN ALL TO POP DATA ----------------------------------------------

# NB. "Over-75 admissions" variable is named "n"

BOTH_lsoas7 <- bind_rows(carehome_lsoas7,noncarehome_lsoas7) %>% 
  select(-3) # remove lsoa name 


# qplot(data = BOTH_lsoas, x= Beds , y = n)

# JOIN TO POPn DATA:
master_carehome7 <- left_join(BOTH_lsoas7, lsoa_table15,  by = "LSOA") %>%
  arrange(desc(n))

master_carehome7 <- left_join(master_carehome7, ccg_lsoa, by = "LSOA")


# (5) ADD OCCUPANCY RATE (NO LONGER APPLICABLE - ADD POST REGRESSION)------

# Estimated occupancy from knight frank 2016 care home survey.
# And of those 82.5% over 75 http://www.cpa.org.uk/information/reviews/Bupa-Census-2012.pdf

# master_carehome %>%
#   filter(Flag == 1)
# 
# master_carehome7 <- master_carehome7 %>%
#   
#   mutate(BedOcc = round(beds* 0.888*0.825)) %>% # this is the WM figure * care home age profile 
#   mutate(care_occ = round(care_beds*.907*0.825)) %>% # national figure * care home age profile 
#   mutate(nurse_occ = round(nurse_beds*.877*0.825)) %>% # national figure * care home age profile 
#   
#   
#   mutate(Flag = if_else(care_occ + nurse_occ > Over75 , 1, 0)) 


# 49 cases where care home beds exceed over75 pop
# make adjustments to take this into account.
# 
# master_carehome8 <- master_carehome7 %>% 
#   mutate(BedOcc  = ifelse(BedOcc > Over75, Over75, BedOcc)) %>% # constrain to over75 pop
#   mutate(care_occ2   = ifelse(Flag == 1 , round(Over75*(care_occ/(nurse_occ + care_occ))), care_occ)) %>% 
#   mutate(nurse_occ2  = ifelse(Flag == 1 , round(Over75*(nurse_occ/(nurse_occ + care_occ))), nurse_occ))
# #mutate(Over75  = ifelse(Flag == 1 ,BedOcc, Over75))
# # So that Over75 population not exceeded (and later does not go -ve)
# # Only one case : Cannock Chase 010B
# 

master_carehome7 <- left_join(master_carehome7, deprivation, by = "LSOA")

# sum(master_carehome8$nurse_occ2) + sum(master_carehome8$care_occ2)
# A slightly higher care home population with this differentiation
# split almost equally - as you may expect give the allocation method (see chapter 1)

# (6) REGRESSION ANALYSIS ------------------------------------------------

# This version contains only the final model. For all models and comparisons, see
# notes in "care-v06.R" 

# THIS SEEMS TO ADDRESS CONCERN ABOUT OFFSET:
# http://stats.stackexchange.com/questions/66791/where-does-the-offset-go-in-poisson-negative-binomial-regression

# Following procedure on:
# http://www.ats.ucla.edu/stat/r/dae/nbreg.htm

# MORE NOTES IN "care-v04.R" - found in original development project directory.


FINAL_TABLE7 <- master_carehome7

FINAL_TABLE7$IMD    <-  as.factor(FINAL_TABLE7$IMD)
FINAL_TABLE7$IDAOPI <-  as.factor(FINAL_TABLE7$IDAOPI)
# FINAL_TABLE7$Health <-  as.factor(FINAL_TABLE7$Health) colinearity

FINAL_TABLE7$CCG16NM <- as.factor(FINAL_TABLE7$CCG16NM) 
# FINAL_TABLE7$Home    <- as.factor(FINAL_TABLE7$Home) 



# THE MODEL:

#library(R2admb)    # special "negbinom" model 
#library(glmmADMB)  # special "negbinom" model 
# model_home <- glmmadmb(formula = n ~ BedOcc + CCG16NM + Health + Home + offset(log(Over75))
#                        , data = FINAL_TABLE, family = "nbinom2"
#                        , zeroInflation = F) 

# REGRESSION: WENT WITH PACKAGE MASS AS ANSWER NO DIFFERENCE TO ALTERNATIVES AND MORE WIDELY RECOGNISED/TESTED?
# library(MASS) # nb.glm

# IDAOPI is used to avoid colinearity problems with Health (it uses Admissions to Hospital and Disability)
# http://geoconvert.mimas.ac.uk/help/imd-2007-manual.pdf

model_home7 <- MASS::glm.nb(n ~ care_beds + nurse_beds + IDAOPI + CCG16NM + offset(log(Over75)),
                            data = FINAL_TABLE7)

BIC(model_home7)


# TEST INTERACTION TERMS:
model_h7_i1 <- MASS::glm.nb(n ~ care_beds*IDAOPI + nurse_beds + CCG16NM 
                            + offset(log(Over75)),
                            data = FINAL_TABLE7)


i1 <- broom::tidy(model_h7_i1)




# () SECTION THREE ---------------------------------------------------



# (1) LOAD DATA -----------------------------------------------------------

# setwd(data_dir)

# (1.1) OVER 85 LSOA POPULATIONS ------------------------------------------

#lsoa_pop15 <- read_excel("SAPE18DT1-mid-2015-lsoa-syoa-estimates.xls", sheet = "Mid-2015 Persons", skip = 3)

pop_proport <- lsoa_pop15 %>% 
  select(1:3,79:94)  %>% 
  filter(!is.na( LSOA11NM )) %>% 
  filter(grepl("E", LSOA))


# PROPORTION OF OVER 85s
pop_proport <- pop_proport %>% 
  mutate(Over85  = rowSums(pop_proport[14:19])) %>% 
  mutate(prop_85 = rowSums(pop_proport[14:19])/rowSums(pop_proport[4:19])) %>%
  select(-c(2:19))


# check to see linear / monotonic relationship:
ggplot(master_carehome9, aes(prop_85, n))+
  geom_point(alpha = 0.008)+
  geom_smooth()


# check to see linear with over75 or beds
ggplot(master_carehome9, aes(Over75, n))+
  geom_point(alpha = 0.008)+
  geom_smooth()


# (5) SKIP TO STEP ON JOIN (TO NUR/CARE FINAL DATA) ------------------

master_carehome9 <- left_join(master_carehome7, pop_proport, by = "LSOA")



# (6) INSERT DEATH RATE DATA ----------------------------------------------

library(stringr)

# deaths registered in that calendar year.
deaths_lsoa <- read_excel("//clients.its.local/csu/users01/andrew.jones/Desktop/CARE_v3/Data/LSOA deaths 2015 Final.xlsx", 
                          sheet = "Table",
                          skip = 6)

# remove Wales:
deaths_lsoa %<>%
  filter(str_detect(deaths_lsoa$`LSOA Code`, "E"))


deaths <- deaths_lsoa %>% 
  filter(Age >= 75) %>% 
  group_by(`LSOA Code`) %>% 
  summarise(deaths = sum(Deaths)) %>% 
  `colnames<-`(c("LSOA", "deaths"))

master_carehome9 <- left_join(master_carehome9, deaths, by = "LSOA")

master_carehome9$deaths[is.na(master_carehome9$deaths)] <- 0

master_carehome9 %<>%
  mutate(death_rate_75 = deaths/Over75)


ggplot(master_carehome9, aes(death_rate_75, n))+
  geom_point(alpha= 0.1)+
  geom_smooth()

" AGAIN THERE IS THE ISSUE OF NON MONOTONIC RELATIONSHIP FOR THE HIGHER VALUES "
" IS IT WORTH MAKING IT A CATEGORICAL ?"
" IS IT WORTH MAKING PROP AGE A CATEGORICAL"


# () DEATH RATE AS CATEGORICAL ---------------------------------------
# 
# master_carehome9 %<>%
#   mutate(death_cat = case_when(
#     master_carehome9$death_rate_75 == 0 ~ "0",
#     master_carehome9$death_rate_75 > 0    & master_carehome9$death_rate_75 <= 0.04 ~  "to_0.04",
#     master_carehome9$death_rate_75 > 0.04 & master_carehome9$death_rate_75 <= 0.08 ~ "to_0.08",
#     master_carehome9$death_rate_75 > 0.08 & master_carehome9$death_rate_75 <= 0.12 ~ "to_0.12",
#     master_carehome9$death_rate_75 > 0.12 & master_carehome9$death_rate_75 <= 0.16 ~ "to_0.16",
#     master_carehome9$death_rate_75 > 0.16 & master_carehome9$death_rate_75 <= 0.20 ~ "to_0.20",
#     master_carehome9$death_rate_75 > 0.2 ~ "over_0.20" 
#   ))
# 
# master_carehome9 %<>%
#   mutate(death_cat = case_when(
#     master_carehome9$death_rate_75 == 0 ~ "0",
#     master_carehome9$death_rate_75 > 0    & master_carehome9$death_rate_75 <= 0.2 ~  "to_0.2",
#     master_carehome9$death_rate_75 > 0.2 ~ "over_0.20" 
#   ))
# 
# 
# master_carehome9$death_cat <- as.factor(master_carehome9$death_cat)
# 
# levels(master_carehome9$death_cat)


# () CHANGE TO BEDS PER POPULATION -----------------------------------

master_carehome9 %<>%
  mutate(nrs_beds_per_pop = nurse_beds / Over75) %>% 
  mutate(care_beds_per_pop = care_beds / Over75)


# (6) REGRESSION ANALYSIS ------------------------------------------------

# This version contains only the final model. For all models and comparisons, see
# notes in "care-v06.R" 

# THIS SEEMS TO ADDRESS CONCERN ABOUT OFFSET:
# http://stats.stackexchange.com/questions/66791/where-does-the-offset-go-in-poisson-negative-binomial-regression

# Following procedure on:
# http://www.ats.ucla.edu/stat/r/dae/nbreg.htm

# MORE NOTES IN "care-v04.R" - found in original development project directory.


FINAL_TABLE9 <- master_carehome9

FINAL_TABLE9$IMD    <-  as.factor(FINAL_TABLE9$IMD)
FINAL_TABLE9$IDAOPI <-  as.factor(FINAL_TABLE9$IDAOPI)
FINAL_TABLE9$Health <-  as.factor(FINAL_TABLE9$Health)

FINAL_TABLE9$CCG16NM <- as.factor(FINAL_TABLE9$CCG16NM) 
# str(FINAL_TABLE7$prop_85)

# FINAL_TABLE7$Home    <- as.factor(FINAL_TABLE7$Home) 


# THE MODEL:

#library(R2admb)    # special "negbinom" model 
#library(glmmADMB)  # special "negbinom" model 
# model_home <- glmmadmb(formula = n ~ BedOcc + CCG16NM + Health + Home + offset(log(Over75))
#                        , data = FINAL_TABLE, family = "nbinom2"
#                        , zeroInflation = F) 

# REGRESSION: WENT WITH PACKAGE MASS AS ANSWER NO DIFFERENCE TO ALTERNATIVES AND MORE WIDELY RECOGNISED/TESTED?
# library(MASS) # nb.glm

# IDAOPI is used to avoid colinearity problems with Health (it uses Admissions to Hospital and Disability)
# http://geoconvert.mimas.ac.uk/help/imd-2007-manual.pdf

model_home9 <- MASS::glm.nb(n ~ care_beds + nurse_beds + IDAOPI + prop_85 + CCG16NM + offset(log(Over75)),
                            data = FINAL_TABLE9)

BIC(model_home9)

# Proportion: another improvement - bigger jump than with nurse/care dichotomy.
# [1] 260574.8
# 
# [1] 260962.3 - QUINTILE with prop_85
# [1] 257374 - QUINT WITH death_rate_75  (continuous)
# [1] 258094.6 - with death_cat NB. still very significant.

summary(model_home9)

"WITH A BEDS PER POPULATION:"

model_home10 <- MASS::glm.nb(n ~ care_beds_per_pop + nrs_beds_per_pop + IDAOPI + death_rate_75 + CCG16NM + offset(log(Over75)),
                            data = FINAL_TABLE9)

BIC(model_home10)
# [1] 257097.5
"THIS IS AN IMPROVEMENT AND ALSO MAKES MORE SENSE"


# est7 <- cbind(Estimate = coef(model_home7), confint(model_home7))
# est7 <-  exp(est7)

# details <- broom::tidy(model_home7)



# () TEST INTERACTIONS -----------------------------------------------

FINAL_TABLE9 %<>%
  mutate(beds_per_pop = beds / Over75)


model_home11 <- MASS::glm.nb(n ~ beds_per_pop*CCG16NM + IDAOPI + death_rate_75 + CCG16NM + offset(log(Over75)),
                             data = FINAL_TABLE9)

BIC(model_home11)

test1 <- broom::tidy(model_home11)

exp(test1$p.value[216:422])

# (6.1) PREDICTIONS -----------------------------------------


# SEE NOTES IN WRITE UP ABOUT METHOD TO ESTIMATE CARE HOME ADMISSIONS
SIMPLE_TAB27 <- FINAL_TABLE9[-c(12:14, 17:25)]

# CREATE COPY OF SIMPLETAB2 BUT ASSUME NO CAREHOME ( RESIDENTS = 0 ):

SIMPLE_AGE <- SIMPLE_TAB27 %>%
  mutate(Over75 = Over75 - care_occ2 - nurse_occ2 ) %>%
  mutate(Over85 = round(Over85 - 0.6255*(care_occ2 + nurse_occ2)))

# New over 85 population created by taking away the number of Over85s in care homes
# based on age structure provided in the BUPA Census where 62.5% of Over75 population
# where also over 85.

SIMPLE_AGE <- SIMPLE_AGE %>%
  mutate(BedOcc = 0) %>% 
  mutate(care_occ2 = 0) %>% 
  mutate(nurse_occ2 = 0) %>%
  mutate(prop_85 = Over85 / Over75) #%>% 
# mutate(prop_85 = if(is.nan(prop_85)){})



# Some proportions are negative. Reset these to zero.
SIMPLE_AGE %<>%
  mutate(prop_85 = ifelse(prop_85 < 0 | is.na(prop_85), 0, prop_85)) %>% 
  mutate(prop_85 = ifelse(is.infinite(prop_85), 1, prop_85)) 


# Method to obtain CH Admissions: 
#  1) Use the carehome model for Res = BedOcc and Res = 0 then apply this ratio to Admissions for scaling


SIMPLE_TAB27 <- SIMPLE_TAB27 %>%
  mutate(predict = round(predict(model_home7, newdata = SIMPLE_TAB27, type = "response"))) %>%
  #mutate(predict2 = round(predict(model_special2, newdata = FINAL_TABLE2, type = "response"))) %>%
  
  mutate(differ = predict - n) %>%
  #mutate(`pred-actual2` = predict2 - Admissions_75_plus) %>%
  # mutate(perc_err = abs(round((1 - (predict/n)),2))) %>% # WRONG CALCULATION
  
  mutate(predict0  = round(predict(model_home7, newdata = SIMPLE_AGE, type = "response"))) %>%
  mutate(HomeAdmis = predict - predict0)
# I DO NOT THINK THAT SCALING IS HELPFUL - DEFEATS THE POINT OF A REGRESSION MODEL
# HOWEVER; STILL THE QUESTION OF WHAT TO DO WITH PREDICTED OUTNUMBERING ACTUAL.

# %>%
#   mutate(HomeAdmisScaled = round(HomeAdmis/predict*n))

# DOES IT MAKE SENSE TO SCALE, AS THAT SEEMS TO BE USING THE MODEL TO
# SECOND GUESS THE MODEL
# I THINK SO - THE MODEL WORKS ON AVERAGES AND WOULD BE ADJUSTING SINGLE POINT 

#SIMPLE_TAB2 <- SIMPLE_TAB2[c(1:7,26,27,8:25)]


# (7) SUMMARY STATISTICS FOR PAPER ----------------------------------------

ggplot(FINAL_TABLE7, aes(prop_85, n))+
  geom_point(alpha =0.04)

cor.test(FINAL_TABLE7$prop_85, FINAL_TABLE7$n)


sum(SIMPLE_TAB27$differ)
#  predicts 11735 more over 75 admissions than actual.

sum(SIMPLE_TAB27$predict) / sum(SIMPLE_TAB27$n) # which is  0.71% more than actual.
# 
# SIMPLE_TAB27 %>% 
#   # dplyr::filter(BedOcc > 20) %>% 
#   filter(perc_err >= 0.25 | perc_err <= -0.25)
# 
# 11967/32791
# 
# 32791/ 32844

# 63.5 % within 25% of actual value.

# Prediction intervals using HH package - UNECESSARY???
# prediction_intervals <- HH::interval(model_home7, type = "response", conf.level = .5)

# one in four chance LSOA result is outside the prediction interval given
# 
# test <- na.omit(lsoa_table15)
# qplot(test$Over75)
# median(test$Over75)

SIMPLE_TAB27 %>% 
  filter(HomeAdmis > n) %>% 
  mutate(Difference =  HomeAdmis - n) %>% 
  arrange(desc(Difference)) %>% 
  select(6, Difference)

# 153 cases where predicted home admissions greater than total admissions. 
# Need some constraint.

z <- master_carehome8 %>% 
  filter(beds == 0) 

quantile(z$n)

40*(.9*.8)

sum(SIMPLE_TAB27$HomeAdmis)


# (8) DESCRIPTIVE PLOTS ---------------------------------------------------

graph_homes <- SIMPLE_TAB27 %>% 
  filter(beds > 0) %>% 
  arrange(n) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

graph_all <- SIMPLE_TAB27 %>% 
  arrange(n) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())


ggplot(graph_all)+
  geom_line(aes(id, n))+
  geom_line(aes(id, predict), alpha = 0.2)+
  theme(axis.ticks = element_blank(),
        axis.title = element_text(colour = "darkgrey"),
        plot.title = element_text(colour = "darkgrey"),
        plot.subtitle = element_text(colour = "darkgrey"),
        legend.position = "none",
        panel.grid.minor = element_blank())+
  scale_y_continuous(limits = c(0, 300))+
  geom_text(aes(label = "Observed"), size = 3.5, y= 55 , x = 10000 )+
  #geom_label(aes(label = "90% target", size = 0.2), y= 50 , x = 10000 , fill = "white")+
  geom_text(aes(label = "Predicted"), size = 3.5, y= 140 , x = 19000, colour = "darkgrey" )+
  ylab("Inpatient Admissions")+
  xlab("LSOA number")+
  ggtitle("Observed vs. Predicted Admissions\nOver 75s, LSOAs in England", subtitle = "Source: HES Inpatients 15/16") 





ggplot(graph_homes)+
  geom_line(aes(id, n))+
  geom_point(aes(id, HomeAdmis), alpha = 0.03)+
  theme(axis.ticks = element_blank(),
        axis.title = element_text(colour = "darkgrey"),
        plot.title = element_text(colour = "darkgrey"),
        plot.subtitle = element_text(colour = "darkgrey"),
        legend.position = "none",
        panel.grid.minor = element_blank())+
  scale_y_continuous(limits = c(0, 300))+
  geom_text(aes(label = "Observed"), size = 3.5, y= 90 , x = 2000 )+
  geom_text(aes(label = "Care Home\nAdmissions"), size = 3.5, y= 200 , x = 4000, colour = "darkgrey" )+
  xlab("LSOA number")+
  ylab("Inpatient Admissions")+
  ggtitle("Care home admissions vs.\nobserved admissions\nOver 75s, LSOAs with Care Homes", subtitle = "Source: HES Inpatients 15/16") 


