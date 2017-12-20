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
  # NAs set as zero.
  mutate_at(vars(beds, admissions), funs(ifelse(is.na(.), 0, .))) %>% 
  # leave as factors and see what happens for now:
  mutate_at(vars(ccg_name, idaopi_quint, imd), funs("factor"))


# Simple implementation of caret train -------------------------------

set.seed(102)
test_data <- care_homes %>% sample_n(5000)

test_data <- test_data %>% 
  select(over_75,
         ccg_name,
         imd,
         idaopi_quint,
         admissions,
         beds
         )

full_data <- care_homes %>% 
  select(over_75,
         ccg_name,
         imd,
         idaopi_quint,
         admissions,
         beds
  )

str(test_data)
str(full_data)

#Converting every categorical variable to numerical using dummy variables:
# dmy <- dummyVars(" ~. ", data = test_data, fullRank = T)
# train_transformed <- data.frame(predict(dmy, newdata = test_data))


samp <- createDataPartition(full_data$admissions, p = 0.8, list = FALSE)
training <- full_data[samp,]
testing  <- full_data[-samp,]

# training <- as.data.frame(training)
# class(training)

# Can produce the formula as a string:
# xnam <- paste("x", 1:25, sep="")
# fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))

"need to remove over_75 from the model"
"May be better to do trainControl and preProcessing outside the regression" 

"If centre and scale the outcome (and the over_75) this shouldn't affect the outcome"
"Check this by looking at quartiles of rates?"

pre_proc <-  preProcess(select(training,
                               #-ccg_name,
                               -admissions,
                               -over_75),
                        method = c("center","scale"))

pre_proc$method

train_processed <- predict(pre_proc, training)

"Research: Principal Component Analysis (pca) and when and whether it's useful"

nb1 <- train(admissions ~ beds + idaopi_quint + ccg_name + offset(log(over_75)),
      # train(admissions ~ beds + idaopi_quint + ccg_name + offset(log(over_75)),
             # data       = train_transformed,
             data = train_processed, # is now the pre_processed data
             # preProcess = c("center","scale"),
             trControl  = trainControl(number = 8), # number of resamples
             tuneGrid   = data.frame(link = c("log", "sqrt")),
             method     = "glm.nb"
      ) # specify 1 parameter (not sqrt or identity)

"In parameters need to specify: link = log "
" The rank deficiency is caused by ccg_name: 211 levels."

# center and scaling results in a higher model rsquared.
# But does the low Rsquared actually result in a useless model.

# Model R^2 and McFadden's Pseudo R^2:
# http://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/

nb1
summary(nb1$finalModel$residuals) 
# you could test fit of sqrt vs. log link by comparing plot of residuals.
# or fitted vs. actual.


https://stats.stackexchange.com/questions/46418/why-is-the-square-root-transformation-recommended-for-count-data
"You should evaluate R-squared values in conjunction with residual plots, other model statistics, and subject area knowledge in order to round out the picture (pardon the pun)."
"Graph the predictions against original for log and sqrt link functions"

broom::tidy(nb1$finalModel)
broom::glance(nb1$finalModel)
rsquare(nb1$finalModel, train_processed)
class(nb1$finalModel)
# predict the outcome on a test set
test_pred <- predict(nb1, train_processed)
# compare predicted outcome and true outcome
confusionMatrix(test_pred, train_processed$admissions)



"https://stats.stackexchange.com/questions/35071/what-is-rank-deficiency-and-how-to-deal-with-it
"
"
However, even good data is sometimes inadequate, at least numerically so.
(Why do bad things happen to good data?) The problem here may be model related.
It may lie in nothing more than a poor choice of units.
It may stem from the computer programming done to solve the problem. (Ugh! Where to start?)

First, lets talk about units and scaling. Suppose I try to solve a problem where
one variable is MANY orders of magnitude larger than another. For example, suppose
I have a problem that involves my height and my shoe size. I'll measure my height
in nanometers. So my height would be roughly 1.78 billion (1.78e9) nanometers.
Of course, I'll choose to measure my shoe size in kilo-parsecs, so 9.14e-21 kilo-parsecs.
When you do regression modeling, linear regression is all about linear algebra
, which involves linear combinations of variables. The problem here is these
numbers are different by hugely many orders of magnitude (and not even the same
units.) The mathematics will fail when a computer program tries to add and
subtract numbers that vary by so many orders of magnitude (for a double
precision number, that absolute limit is roughly 16 powers of 10.)

The trick is usually to use common units, but on some problems even that
is an issue when variables vary by too many orders of magnitude. More important
is to scale your numbers to be similar in magnitude.

Next, you may see problems with big numbers and small variation in those numbers.
Thus, suppose you try to build a moderately high order polynomial model with
data where your inputs all lie in the interval [1,2]. Squaring, cubing, etc.,
numbers that are on the order of 1 or 2 will cause no problems when working
in double precision arithmetic. Alternatively, add 1e12 to every number.
In theory, the mathematics will allow this. All it does is shift any polynomial
model we build on the x-axis. It would have exactly the same shape, but be
translated by 1e12 to the right. In practice, the linear algebra will fail
miserably due to rank deficiency problems. You have done nothing but
translate the data, but suddenly you start to see singular matrices popping up.
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

