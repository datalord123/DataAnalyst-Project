#http://www.salemmarafi.com/code/market-basket-analysis-with-r/comment-page-1/

##This is just to help people who are using csv files
##use read.transactions() than read.csv() for the reason that read.csv() would return data frame with automatic column names
##Using MyData<-read.csv() would return data frame in MyData but now when you pass this MyData to apriori, it will accept it but give the column names as V1 , V2 and the result will be distorted
##hence use MyData<- read.transactions()


#We can represent our items as an item set as follows:
#  I = { i1,i2,…,in }
#Therefore a transaction is represented as follows:
#  tn = { ij,ik,…,in }
#This gives us our rules which are represented as follows:
#{ i1,i2} => { ik}
#Which can be read as “if a user buys an item in the item set on the left hand side,
#then the user will likely buy the item on the right hand side too”. 
#A more human readable example is:
#{coffee,sugar} => {milk}

#Support: The fraction of which our item set occurs in our dataset.
#Confidence: probability that a rule is correct for a new transaction with items on the left.
#Lift: The ratio by which by the confidence of a rule exceeds the expected confidence. 
#Note: if the lift is 1 it indicates that the items on the left and right are independent.

library(arules)
library(arulesViz)
library(datasets)

#Load the data set
#Groceries<-read.csv('groceries.csv')
data(Groceries)
str(Groceries)
head(Groceries)

#Lets explore the data a bit
names(Groceries)
ggplot(aes(x=))

itemFrequencyPlot(Groceries,topN=20,type="absolute")

#We are now ready to mine some rules!
#You will always have to pass he minimum required supoprt and confidence

#We set the minimum support to 0.001
#We sset the minimum confidence to 0.8
#We then show the top 5 rules

#Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

#Shoe teh top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:5])

#Inspect the output.
# This reads easily, for example: if someone buys yogurt and cereals, they are 81%
# likely to buy whole milk too.

summary(rules)
#We can get teh summary info. about the rules that give us some interesting infromation such as:
#The number of rules generated: 410
# The distribution of rules by length: Most rules are 4 items long

#The summar of quality measures: interesting to see ranges of support, lift, and coonfidence
# The information on teh data mined: total data mined, and minimum paramers.

#Sorting Stuff out
#The first issue we see here is that the rules are not sorted. 
# Often we will want the most relevant rules first. 
# Lets say we wanted to have the most likely rules. 
# We can easily sort by confidence by executing the following code.

rules<-sort(rules, by='confidence',decreasing=TRUE)

inspect(rules[1:5])

#Notice the most relevent rules appear now.

#Rule 4 is perhaps excessively long. Lets say ou wanted more concise rules. That is also
#easy to do by adding a
#   maxlen parameter to your apriori function:
rules<-apriori(Groceries,parameter = list(supp = 0.001,conf = 0.8,maxlen=3))
rules<-sort(rules, by='confidence',decreasing=TRUE)

inspect(rules[1:5])

#Dealing with Redundancies
#Sometimes rules will repeat. Redundancy indicates that one item might be a given. 
# As an analyst you can elect to drop the item from the dataset. Alternatively,
# you can remove redundant rules generated.

#We can eliminate the repeated rules using the following snippets of code:
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

inspect(rules[1:5])

#Targeting Items

#Now that we know how to generate rules, limit the output, lets say we wanted to
# target items to generate rules. There are two types of targets we might be interested
# in that are illustrated example 'whole' milk.

#What are customers likely to buy before buying whole milk
# What are customers likely to buy if they purchase whole milk?

#This essentially means we want to set either the Left Hand Side and Right Hand Side. This is not
# difficult to do with R!

#Answering the first question we adjust our apriori() function as follows:
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])


#Likewise, we can set the left hand side to be 'whole milk' and find its antecedents.
#Note the following.

#We set the confidence to 0.15 since we get no rules with 0.8
#We set a minimum length of 2 to avoid empty left hand side items.

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:6])

#Visualization
#The last step if visualization. Lets say you wanted to map out the rules in a graph. 
#We can do that with another library 'arulesViz'
install.packages('tcltk2')
install.packages("igraph")
library(igraph)
levels(diamonds$cut)
library(arulesViz)
library(tcltk2)
plot(rules,method='graph',interactive=TRUE,shading=NA)
