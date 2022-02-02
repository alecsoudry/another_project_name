# Date: 3/8/21
# Authors: Alec Soudry

# factor variable #
eBayAuctions <- read.csv("eBayAuctions.csv", stringsAsFactors = TRUE)

# convert to factor
eBayAuctions$Duration <- factor(eBayAuctions$Duration,                                            
                                levels = c(1, 3, 5, 7, 10),                                         
                                labels = )
str(eBayAuctions)


# summarize #
aggregate(. ~ Category, eBayAuctions, mean)
aggregate(. ~ currency, eBayAuctions, mean)
aggregate(. ~ Duration, eBayAuctions, mean)
aggregate(. ~ endDay, eBayAuctions, mean)

# dummy variable #
library(fastDummies)
eBayAuctions <- dummy_cols(eBayAuctions,
                           select_columns = "Category",
                           remove_first_dummy = TRUE,
                           remove_selected_columns = TRUE)
eBayAuctions <- dummy_cols(eBayAuctions,
                           select_columns = "currency",
                           remove_first_dummy = TRUE,
                           remove_selected_columns = TRUE)
eBayAuctions <- dummy_cols(eBayAuctions,
                           select_columns = "endDay",
                           remove_first_dummy = TRUE,
                           remove_selected_columns = TRUE)
str(eBayAuctions)

###### partitioning the data ######
set.seed(1)

## partitioning into training (60%) and validation (40%)
train.rows = sample(rownames(eBayAuctions),
                    nrow(eBayAuctions)*0.6)
# collect all the columns with training row ID into training set:
train.data = eBayAuctions[train.rows, ]

# assign row IDs that are not already in the training set into validation
valid.rows = setdiff(rownames(eBayAuctions), train.rows)
valid.data = eBayAuctions[valid.rows, ]

head(train.data)
head(valid.data)

# use regsubsets() to perform exhaustive search
library(leaps)
eBay.search <- regsubsets(Competitive. ~ .,                   
                          data = train.data,           
                          nbest = 1,                   
                          nvmax = ncol(train.data),    
                          method = "exhaustive")       

search.summary <- summary(eBay.search)      
search.summary$which                          

# compare the models returned by the exhaustive search
options(digits = 8)
t(t(search.summary$rsq))          
t(t(search.summary$adjr2))        
t(t(search.summary$cp))           
t(t(search.summary$bic))          

####### Create Models #######
eBay.lm <- lm(Competitive. ~ ., data = train.data)         
eBay.lm.null <- lm(Competitive. ~ 1, data = train.data)    

####### Backward Elimination #######
eBay.lm.back <- step(eBay.lm,                    
                     direction = "backward")       

summary(eBay.lm.back)

####### Stepwise Regression #######
eBay.lm.step <- step(eBay.lm.null,                                    
                     scope = list(eBay.lm.null, upper = eBay.lm),   
                     direction = "both")                                

summary(eBay.lm.step)

install.packages("stargazer", dependencies = TRUE)
library(stargazer)

table <- stargazer(eBay.lm,eBay.lm.back,eBay.lm.step, title="Model Comparison", type="text", align=TRUE)
table

####### full logistic regression model #######
# use glm() (generalized linear model) with family = "binomial" to fit a logistic
# regression
logit.reg <- glm(Competitive. ~ . - ClosePrice, data = train.data, family = "binomial")
options(scipen = 999)
summary(logit.reg)

# use predict() with type = "response" to compute predicted probabilities
logit.reg.pred <- predict(logit.reg,                # use the logistic regression model
                          valid.data,                 # to predict outcomes in the validation data
                          type = "response")        # returns probability of success

data.frame(actual = valid.data$Competitive.[1:20], predicted = logit.reg.pred[1:20])

####### confusion matrix #######
library(caret)
install.packages('e1071', dependencies=TRUE)
confusionMatrix(as.factor(ifelse(logit.reg.pred >= 0.5, "1", "0")),  # predicted class
                as.factor(valid.data$Competitive.),                   # actual class
                positive = "1")                                      # class of interest

####### lift chart #######
library(gains)
gain <- gains(valid.data$Competitive, logit.reg.pred, groups = length(logit.reg.pred))
gain
# plot lift chart
plot(c(0, gain$cume.pct.of.total * sum(valid.data$Competitive.)) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(valid.data$Competitive.)) ~ c(0, nrow(valid.data)), lty = 2)

####### decile lift chart #######
gain <- gains(valid.data$Competitive., logit.reg.pred)
heights <- gain$mean.resp / mean(valid.data$Competitive.)
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0, 2.5),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")
exp(coef(logit.reg))
