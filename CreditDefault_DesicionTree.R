# Loading the data
credit<-read.csv('credit.csv')
View(credit)
str(credit)

table(credit$checking_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
credit$default<- factor(credit$default, levels = c(1,2), labels = c('No','Yes'))

table(credit$default)

RNGversion('3.5.2'); set.seed(123)
train_sample <- sample(1000,900)
str(train_sample)

credit_train<-credit[train_sample, ]
credit_test<-credit[-train_sample, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

install.packages('C50')
library(C50)

credit_model <- C5.0(credit_train[-17],credit_train$default)

credit_model
summary(credit_model)

credit_pred <- predict(credit_model, credit_test)
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c('actual defailt', 'predicted default'))

credit_model10 <- C5.0(credit_train[-17],credit_train$default, trials = 10)

credit_model10
summary(credit_model10)

credit_boost_pred10 <- predict(credit_model10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c('actual defailt', 'predicted default'))

matrix_dimensions <- list(c('No','Yes'), c('No','Yes'))
names(matrix_dimensions)<- c('predicted','actual')

matrix_dimensions$actual

error_cost<- matrix(c(0,1,4,0), nrow=2, dimnames = matrix_dimensions)
error_cost

credit_cost<- C5.0(credit_train[-17],credit_train$default, costs = error_cost)
credit_cost_pred<- predict(credit_cost,credit_test)
CrossTable(credit_test$default, credit_cost_pred, prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))


