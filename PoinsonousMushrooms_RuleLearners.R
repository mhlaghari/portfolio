#Identifying Poisonous Mushrooms with Rule Learners

# Each year, many people fall ill and sometimes even die 
# from ingesting poisonous wild mushrooms. 
# Since many mushrooms are very similar to each other 
# in appearance, occasionally even experienced mushroom 
# gatherers are poisoned. 

# Unlike the identification of harmful plants, 
# such as a poison oak or poison ivy, 
# there are no clear rules like "leaves of three, 
# let them be" for identifying whether a wild mushroom 
# is poisonous or edible. Complicating matters, many 
# traditional rules such as "poisonous mushrooms are 
# brightly colored" provide dangerous or misleading 
# information. If simple, clear, and consistent rules 
# were available for identifying poisonous mushrooms, 
# they could save the lives of foragers.
# 
# Lantz, Brett. Machine Learning with R: Expert techniques for predictive modeling, 3rd Edition (p. 158). Packt Publishing. Kindle Edition. 

mushrooms <- read.csv('mushrooms.csv', stringsAsFactors = TRUE)

str(mushrooms)
str(mushrooms$veil_type)
View(mushrooms)

mushrooms$veil_type <- NULL

(mushrooms$type=='edible')
mushrooms$type<- factor(mushrooms$type, levels = c('e','p'), labels = c('edible','poisonous'))
table(mushrooms$type)

install.packages("OneR")
library(OneR)

mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R

mushroom_1R_pred <- predict(mushroom_1R, mushrooms)
table(actual = mushrooms$type, predicted = mushroom_1R_pred)

library(RWeka)
install.packages('RWeka')

mushroom_jrip <- JRip(type ~ ., data=mushrooms)
mushroom_jrip

-say
