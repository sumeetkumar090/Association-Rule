#Task 3 - SK521 - Sumeet Kumar
#install package if not available
#install.packages("arulesViz")
#install.packages("arules")
#load library
library(arules)
library(arulesViz)

getwd()
#set working directory to current
setwd('./')

#read the csv file
successDataSet <- read.csv("./A1_success_data.csv")

#examine the dataset
head(successDataSet)
summary(successDataSet)

#convert the data frame to transactions so we can use it in apriori algorithm
student_data_transaction <- as(successDataSet, 'transactions')

#view the summary of the transaction data
summary(student_data_transaction)
student_data_transaction


#Task 1 Generate frequent itemsets by applying various support thresholds and inspect these itemsets by displaying their support, confidence, and lift values

#support threshold at 0.01
itemset1 = apriori(student_data_transaction, parameter = list(support = 0.01, confidence = 0.4))

#support threshold at 0.02
itemset2 = apriori(student_data_transaction, parameter = list(support = 0.02,confidence = 0.6))

#support threshold at 0.5
itemset3 = apriori(student_data_transaction, parameter = list(support = 0.5,confidence = 0.8,minlen = 1,maxlen = 8))

#Summary of support, confidence, and lift
#frequent itemset1
summary(itemset1@quality)
itemset1

#frequent itemset2
summary(itemset2@quality)
itemset2

#frequent itemset3
summary(itemset3@quality)
itemset3

#Inspect the itemsets by displaying the property support, confidence, and lift
inspect(head(sort(itemset1, by = "support"), 40))
inspect(head(sort(itemset2, by = "support"), 40))
inspect(head(sort(itemset3, by = "support"), 40))


#Task 2 - Set the right hand side (rhs) as the attribute Success to generate the frequent itemsets that can help to predict if a student can pass this test or not based on his/her grade, gender and/or enrolment.
rhs_rule = apriori(student_data_transaction, parameter = list(support = 0.02,confidence = 0.6),appearance = list(rhs = c('Success=No', 'Success=Yes'), default = 'lhs'))

inspect(head(sort(rhs_rule, by = 'lift'), 20))


#Task 3 - Visualize the rules generated in the last step by 
#1) showing the relationship among support, confidence and lift

plot(rhs_rule@quality)


#2)using the graph visualization based on the sorted lift value.
#The following code provides a visualization of the top five rules with the highest lift
liftRules <- head(sort(rhs_rule, by = "lift"), 5)
plot(liftRules, method = "graph", control = list(type = "items"))