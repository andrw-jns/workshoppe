##################################################
"Modelling Principles: Caret applied to carehomes"
##################################################

library(here)
library(caret)
library(modelr)
library(tidyverse)


#  -------------------------------------------------------------------

care_homes <- read_rds(here("data", "carehome_dummy.rds"))

# test <- care_homes %>% filter(is.na(beds)) 
" For the purposes of this exercise let us just fill NAs with zeros,
but at some stage this will have to be looked at properly."

care_homes <- care_homes %>% 
  mutate_at(vars(beds, admissions), funs(ifelse(is.na(.), 0, .))) %>% 
  # leave as factors and see what happens for now:
  mutate_at(vars(ccg_name, idaopi_quint), funs("factor"))


# Simple implementation of caret train -------------------------------

set.seed(102)
test_data <- care_homes %>% sample_n(2000)


samp <- createDataPartition(test_data$admissions, p = 0.8, list = FALSE)
training <- test_data[samp,]
testing  <- test_data[-samp,]


nb1 <- train(admissions ~ beds + idaopi_quint + ccg_name + offset(log(over_75)),
             data       = training,
             # preProcess = c("center", "scale"),
             # in order to pre-process need to do dummy transform.
             # and have a dataframe rather than tibble.
             method     = "glm.nb",
             tuneGrid   = data.frame(link = "log"))
"In parameters need to specify: link = log "

nb1
"
However, even good data is sometimes inadequate, at least numerically so. (Why do bad things happen to good data?) The problem here may be model related. It may lie in nothing more than a poor choice of units. It may stem from the computer programming done to solve the problem. (Ugh! Where to start?)

First, lets talk about units and scaling. Suppose I try to solve a problem where one variable is MANY orders of magnitude larger than another. For example, suppose I have a problem that involves my height and my shoe size. I'll measure my height in nanometers. So my height would be roughly 1.78 billion (1.78e9) nanometers. Of course, I'll choose to measure my shoe size in kilo-parsecs, so 9.14e-21 kilo-parsecs. When you do regression modeling, linear regression is all about linear algebra, which involves linear combinations of variables. The problem here is these numbers are different by hugely many orders of magnitude (and not even the same units.) The mathematics will fail when a computer program tries to add and subtract numbers that vary by so many orders of magnitude (for a double precision number, that absolute limit is roughly 16 powers of 10.)

The trick is usually to use common units, but on some problems even that is an issue when variables vary by too many orders of magnitude. More important is to scale your numbers to be similar in magnitude.

Next, you may see problems with big numbers and small variation in those numbers. Thus, suppose you try to build a moderately high order polynomial model with data where your inputs all lie in the interval [1,2]. Squaring, cubing, etc., numbers that are on the order of 1 or 2 will cause no problems when working in double precision arithmetic. Alternatively, add 1e12 to every number. In theory, the mathematics will allow this. All it does is shift any polynomial model we build on the x-axis. It would have exactly the same shape, but be translated by 1e12 to the right. In practice, the linear algebra will fail miserably due to rank deficiency problems. You have done nothing but translate the data, but suddenly you start to see singular matrices popping up.
"

# # rf1 <- train(annual_pm~., data = air, method = "rf")
# 
# mowdel_home <- MASS::glm.nb(admissions ~ beds + idaopi_quint + ccg_name + offset(log(over_75)),
#                             data = test_data,
#                             link = log)
# 
# # broom::tidy(mowdel_home)
# 
# 
# test_results <- test_data %>%
#   mutate(pred = round(predict(mowdel_home, newdata = test_data, type = "response"))) %>% 
#   mutate(pred_error = pred - admissions)
# 
# sqrt(mean(test_results$pred_error^2)) # link sqrt = 14.50, link log = 10.28
# mean(abs(test_results$pred_error)) # sqrt = 10.12, log = 7.14
# 
# "The identity link function can create nonsense probabilites (less than 0 greater than 1) - so
# use the logit (?)"
# 
# broom::glance(mowdel_home)
# broom::augment()
# 
# test_augment <- broom::augment(mowdel_home, test_results)
# 
# "Test CCGs by mean deprivation"
# care_homes %>% group_by(ccg_name) %>% summarise(result = mean(imd)) %>% arrange((result))

