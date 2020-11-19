rm(list=ls())
data <- read.csv("C://ZLIU//SIT//513//Final Project//attrition_data.csv", stringsAsFactors = F)

#### EDA data ####
summary(data)
sum(is.na(data))

table(data$ETHNICITY)
data[5,2935]
data <- data[-2935,]

table(data$SEX)
table(data$MARITAL_STATUS)
table(data$JOB_SATISFACTION)
table(data$NUMBER_OF_TEAM_CHANGED)
table(data$REFERRAL_SOURCE)
table(data$HIRE_MONTH)
table(data$REHIRE)
table(data$TERMINATION_YEAR)
table(data$IS_FIRST_JOB)
table(data$TRAVELLED_REQUIRED)
table(data$PERFORMANCE_RATING)
table(data$DISABLED_EMP)
table(data$DISABLED_VET)
table(data$EDUCATION_LEVEL)
table(data$STATUS)
table(data$JOB_GROUP)
table(data$PREVYR_1)
table(data$PREVYR_2)
table(data$PREVYR_3)
table(data$PREVYR_4)
table(data$PREVYR_5)

#### normalized function ####
normalized <- function(x, minx, maxx) {i <- ((x - minx) / (maxx - minx)) 
return(i)
}

#### normalized & as.factor data ####
data2 <- NULL
data2$ANNUAL_RATE <- normalized(data$ANNUAL_RATE, min(data$ANNUAL_RATE), max(data$ANNUAL_RATE))
data2$HRLY_RATE <- normalized(data$HRLY_RATE, min(data$HRLY_RATE), max(data$HRLY_RATE))
#data2$ETHNICITY <- as.factor(data$ETHNICITY)
data2$ETHNICITY <- as.factor(ifelse(data$ETHNICITY=="AMIND",1,ifelse(data$ETHNICITY=="ASIAN",2,ifelse(data$ETHNICITY=="BLACK",3,ifelse(data$ETHNICITY=="HISPA",4,ifelse(data$ETHNICITY=="PACIF",5,ifelse(data$ETHNICITY=="TWO",6,ifelse(data$ETHNICITY=="WHITE",7,NA))))))))
#data2$SEX <- as.factor(data$SEX)
data2$SEX <- as.factor(ifelse(data$SEX == "M", 1, 2))
#data2$MARITAL_STATUS <- as.factor(data$MARITAL_STATUS)
data2$MARITAL_STATUS <- as.factor(ifelse(data$MARITAL_STATUS=="Divorced",1,ifelse(data$MARITAL_STATUS=="Married",2,ifelse(data$MARITAL_STATUS=="Single",3,0))))
data2$JOB_SATISFACTION <- as.factor(data$JOB_SATISFACTION)
data2$AGE <- normalized(data$AGE, min(data$AGE), max(data$AGE))
#data2$NUMBER_OF_TEAM_CHANGED <- as.factor(data$NUMBER_OF_TEAM_CHANGED)
data2$NUMBER_OF_TEAM_CHANGED <- as.factor(ifelse(data$NUMBER_OF_TEAM_CHANGED=="0",1,ifelse(data$NUMBER_OF_TEAM_CHANGED=="1",2,ifelse(data$NUMBER_OF_TEAM_CHANGED=="2",3,ifelse(data$NUMBER_OF_TEAM_CHANGED=="3",4,ifelse(data$NUMBER_OF_TEAM_CHANGED=="3+",5,0))))))
#data2$REFERRAL_SOURCE <- as.factor(replace(data$REFERRAL_SOURCE, data$REFERRAL_SOURCE=="","Unknown"))
data$REFERRAL_SOURCE <- replace(data$REFERRAL_SOURCE, data$REFERRAL_SOURCE=="","Unknown")
data2$REFERRAL_SOURCE <- as.factor(ifelse(data$REFERRAL_SOURCE=="Agency",1,ifelse(data$REFERRAL_SOURCE=="Client Referral",2,ifelse(data$REFERRAL_SOURCE=="College Recommendation",3,ifelse(data$REFERRAL_SOURCE=="College Recruiting(UnderGrad)",4,ifelse(data$REFERRAL_SOURCE=="Corporate Career Site",5,ifelse(data$REFERRAL_SOURCE=="Direct Sourcing",6,ifelse(data$REFERRAL_SOURCE=="E-Strat",7,ifelse(data$REFERRAL_SOURCE=="Executive Referral",8,ifelse(data$REFERRAL_SOURCE=="Former Employee/Intern",9,ifelse(data$REFERRAL_SOURCE=="Inroads",10,ifelse(data$REFERRAL_SOURCE=="Job Boards/Online Advertising",11,ifelse(data$REFERRAL_SOURCE=="Job Fair",12,ifelse(data$REFERRAL_SOURCE=="Job Posting",13,ifelse(data$REFERRAL_SOURCE=="Open House",14,ifelse(data$REFERRAL_SOURCE=="Other Source",15,ifelse(data$REFERRAL_SOURCE=="Print Advertisement",16,ifelse(data$REFERRAL_SOURCE=="Staffing Agency",17,ifelse(data$REFERRAL_SOURCE=="Unknown",18,ifelse(data$REFERRAL_SOURCE=="Unsolicited",19,0))))))))))))))))))))
#data2$HIRE_MONTH <- as.factor(data$HIRE_MONTH)
data2$HIRE_MONTH <- as.factor(ifelse(data$HIRE_MONTH=="January",1,ifelse(data$HIRE_MONTH=="February",2,ifelse(data$HIRE_MONTH=="March",3,ifelse(data$HIRE_MONTH=="April",4,ifelse(data$HIRE_MONTH=="May",5,ifelse(data$HIRE_MONTH=="June",6,ifelse(data$HIRE_MONTH=="July",7,ifelse(data$HIRE_MONTH=="August",8,ifelse(data$HIRE_MONTH=="September",9,ifelse(data$HIRE_MONTH=="October",10,ifelse(data$HIRE_MONTH=="November",11,ifelse(data$HIRE_MONTH=="December",12,0)))))))))))))
#data2$REHIRE <- as.factor(data$REHIRE)
data2$REHIRE <- as.factor(ifelse(data$REHIRE == "TRUE", 1,0))
data$TERMINATION_YEAR <- replace(data$TERMINATION_YEAR, is.na(data$TERMINATION_YEAR),"0")
data2$TERMINATION_YEAR <- as.factor(data$TERMINATION_YEAR)
#data2$IS_FIRST_JOB <- as.factor(data$IS_FIRST_JOB)
data2$IS_FIRST_JOB <- as.factor(ifelse(data$IS_FIRST_JOB == "Y", 1,0))
#data2$TRAVELLED_REQUIRED <- as.factor(data$TRAVELLED_REQUIRED)
data2$TRAVELLED_REQUIRED <- as.factor(ifelse(data$TRAVELLED_REQUIRED == "Y", 1,0))
data2$PERFORMANCE_RATING <- as.factor(data$PERFORMANCE_RATING)
#data2$DISABLED_EMP <- as.factor(data$DISABLED_EMP)
data2$DISABLED_EMP <- as.factor(ifelse(data$DISABLED_EMP == "Y", 1,0))
#data2$DISABLED_VET <- as.factor(data$DISABLED_VET)
data2$DISABLED_VET <- as.factor(ifelse(data$DISABLED_VET == "Y", 1,0))
#data2$EDUCATION_LEVEL <- as.factor(data$EDUCATION_LEVEL)
data2$EDUCATION_LEVEL <- as.factor(ifelse(data$EDUCATION_LEVEL=="LEVEL 1",1,ifelse(data$EDUCATION_LEVEL=="LEVEL 2",2,ifelse(data$EDUCATION_LEVEL=="LEVEL 3",3,ifelse(data$EDUCATION_LEVEL=="LEVEL 4",4,ifelse(data$EDUCATION_LEVEL=="LEVEL 5",5,0))))))
#data2$STATUS <- as.factor(data$STATUS)
data2$STATUS <- as.factor(ifelse(data$STATUS=="T",1,2))
#data2$JOB_GROUP <- as.factor(data$JOB_GROUP)
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Accounting","Finance & Accounting")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Accounts Payable","Finance & Accounting")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Advanced Research","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Analytical/Microbiology","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Applied Research","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Brand Operations","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Claims Substantiation","Finance & Accounting")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Corporate Supply Chain","Supply Chain")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Creative Service/Copy","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Customer Care","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Customer Relationship Mgmt","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Demand Planning","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Demi-Grand","Administrative Personnel Department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Digital","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Distribution/Administration","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="eCommerce","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Engineering","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Finance","Finance & Accounting")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Flows & Sub-Contracting","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="General Administration","Administrative Personnel Department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="General Management","Administrative Personnel Department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Human Resources","Administrative Personnel Department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Industrial Quality","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Insurance & Risk Management","Finance & Accounting")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Integrated Marketing Comm","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Integrated Mktg Communications","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Intellectual Proprty & Patents","Administrative Personnel Department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="IT Architecture and Integrtion","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="IT Business Applications","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="IT Digital","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="IT Governance and Management","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="IT Security/Risk and Quality","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="IT Technologies and Infrstrctr","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Legal","Administrative Personnel Department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Logistics - Distribution","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Logistics - Manufacturing","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Manufacturing Supply Chain","Supply Chain")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Market Research","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Market Supply Logistics","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Marketing - Direct","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Marketing - Global","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Marketing Support/Services","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Multi-Channel","Supply Chain")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Package Development","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Physical Flows","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Plant & Facilities Maintenance","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Plant Management","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Prod Planning & Inventory Ctl","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Production & Operations","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Promotional Purchasing","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Public Relations","Administrative Personnel Department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Quality Assurance","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="R&I Development/Pre-Develpmnt","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="R&I Evaluation","Technical department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="R&I General Management","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="R&I Safety Evaluation","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Regulatory Affairs","Administrative Personnel Department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Social Media","Marketing department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Sourcing","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Supply Chain Administration","Supply Chain")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Supply Chain Finance","Supply Chain")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Tax","Finance & Accounting")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Technical Packaging","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Transportation & Warehousing","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Treasury","Production department")
data$JOB_GROUP <- replace(data$JOB_GROUP, data$JOB_GROUP=="Web","Technical department")

data2$JOB_GROUP <- as.factor(ifelse(data$JOB_GROUP=="Finance & Accounting",6,ifelse(data$JOB_GROUP=="Technical department",2,ifelse(data$JOB_GROUP=="Marketing department",3,ifelse(data$JOB_GROUP=="Supply Chain",4,ifelse(data$JOB_GROUP=="Administrative Personnel Department",5,ifelse(data$JOB_GROUP=="Production department",1,0)))))))


#PREVYR as int
data2$PREVYR_1 <- data$PREVYR_1
data2$PREVYR_2 <- data$PREVYR_2
data2$PREVYR_3 <- data$PREVYR_3
data2$PREVYR_4 <- data$PREVYR_4
data2$PREVYR_5 <- data$PREVYR_5

data2 <- as.data.frame(data2)

#### Corr ####
data3 <- NULL
data3$ANNUAL <- data2$ANNUAL_RATE
data3$HRLY <- data2$HRLY_RATE
data3$ETHNICITY <- as.numeric(data2$ETHNICITY)
data3$SEX <- as.numeric(data2$SEX)
data3$MARITAL <- as.numeric(data2$MARITAL_STATUS)
data3$JOB_SAT <- as.numeric(data2$JOB_SATISFACTION)
data3$AGE <- data2$AGE
data3$CHANGED <- as.numeric(data2$NUMBER_OF_TEAM_CHANGED)
#data3$REFERRAL <- as.numeric(data2$REFERRAL_SOURCE)
data3$HIRE <- as.numeric(data2$HIRE_MONTH)
data3$REHIRE <- as.numeric(data2$REHIRE)
#data3$TERM_Y <- as.numeric(data2$TERMINATION_YEAR)
data3$FIRST <- as.numeric(data2$IS_FIRST_JOB)
data3$TRAVEL <- as.numeric(data2$TRAVELLED_REQUIRED)
data3$PERFORMANCE <- as.numeric(data2$PERFORMANCE_RATING)
data3$EMP <- as.numeric(data2$DISABLED_EMP)
data3$VET <- as.numeric(data2$DISABLED_VET)
data3$EDU <- as.numeric(data2$EDUCATION_LEVEL)
data3$STATUS <- as.numeric(data2$STATUS)
data3$GROUP <- as.numeric(data2$JOB_GROUP)
data3$P_1 <- data2$PREVYR_1
data3$P_2 <- data2$PREVYR_2
data3$P_3 <- data2$PREVYR_3
data3$P_4 <- data2$PREVYR_4
data3$P_5 <- data2$PREVYR_5
data3 <- as.data.frame(data3)


mcor <- cor(data3)
round(mcor, digits = 2)
library(corrplot)
corrplot(mcor, tl.cex = 0.8)

#### Logistic regression ####
data2_glm <- glm(formula = STATUS ~ PREVYR_1 + PREVYR_2 + PREVYR_3 + PREVYR_4 + PREVYR_5, family = "binomial", data = data2)
summary(data2_glm)

#### Train and Test ####
data4 <- data2[, -12]
set.seed(123)
idx <- sort(sample(nrow(data4),as.integer(.70 * nrow(data4))))
training <- data4[idx,]
test <- data4[-idx,]

#### Decision Tree ####
library(rpart)
library(rpart.plot)
CART <- rpart( STATUS ~ ., data = training)
rpart.plot(CART)
CART_predict <- predict(CART, test, type="class")
df <- as.data.frame(cbind(test, CART_predict))
table(Actual = test[,"STATUS"], CART = CART_predict)
CART_wrong<-sum(test[,"STATUS"] != CART_predict)
error_rate <- CART_wrong/length(test$STATUS)
error_rate

#### SVM ####
library(e1071)
svm.model <- svm(STATUS ~ ., data = training)
svm.pred <- predict(svm.model, test)

table(actual = test$STATUS, svm.pred)
svm_wrong <- (test$STATUS != svm.pred)
rate <- sum(svm_wrong) / length(svm_wrong)
rate

#### Random Forest ####
library(randomForest)
RF <- randomForest(STATUS ~ ., data = training, importance = TRUE, ntree = 1000)
importance(RF)
varImpPlot(RF)
prediction <- predict(RF, test)
table(actual = test$STATUS, prediction)

wrong <- (test$STATUS != prediction)
error_rate <- sum(wrong) / length(wrong)
error_rate
