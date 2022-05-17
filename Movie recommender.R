#This project aims to build a movie recommender system by using Apriori association
#rule mining. 

#importing all the necessary packages and libraries
install.packages("arules")
library(arules)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("arulesViz")
library(arulesViz)
install.packages('shiny')
library(shiny)



#import dataset in csv format into R 
movie_data <- read.csv("movie_recomm2.csv", header = F)


#inspect dataset features
names(movie_data)
head(movie_data)
tail(movie_data)
summary(movie_data)
str(movie_data)

#inspect dimensions of the dataset
dim(movie_data)
nrow(movie_data)
ncol(movie_data)

#transform dataset to transactions format
movie_trans <- read.transactions('C:/Users/FAITH/OneDrive - University of Salford/Documents/Uni Salford/Msc Data Science/ASDM/Coursework/movie_recomm2.csv',
                                 format = 'basket', sep = ',')
movie_trans
summary(movie_trans)

write.csv(movie_trans, "C:/Users/FAITH/OneDrive - University of Salford/Documents/Uni Salford/Msc Data Science/ASDM/Coursework/movie_recomm3.csv")


#plot the frequency of the top 15 movies
itemFrequencyPlot(movie_trans, topN = 15, type = "absolute", col = brewer.pal(6,'Dark2'),
                  main = "Top 15 Movies Frequency Plot")


#create model using apriori function
movie_rules <- apriori(movie_trans, parameter = list(supp = 0.0003, conf = 0.70))
movie_rules


#view the summary of the rules created
summary(movie_rules)

#sort rules in descending order by confidence level
movie_rules <- sort(movie_rules, by='confidence', decreasing = TRUE)


#inspect created rules
inspect(movie_rules[1:10])

#plot the top rules
movie_rules_top <- movie_rules[1:20]
plot(movie_rules_top)
plot(movie_rules_top, method="graph")
plot(movie_rules_top, method = "grouped")
plot(movie_rules_top, method = "matrix")

#mine rules for The avengers
movie = "TheAvengers"
movie_rules_2 = apriori(movie_trans, parameter=list(supp=0.0003,conf = 0.50, minlen=2),
                appearance = list(default="rhs",lhs=movie),
                control = list(verbose=F))
movie_rules_2

#view the summary of the rules created
summary(movie_rules_2)

#sort rules in descending order by confidence level
movie_rules_2 <- sort(movie_rules_2, by='confidence', decreasing = TRUE)
#inspect the rules 
inspect(movie_rules_2)


#shiny dashboard rule explorer
ruleExplorer(movie_rules, sidebarWidth = 2, graphHeight = '600px')
