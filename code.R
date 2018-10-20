library(odbc)
library(implyr)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)

#Method 1: read as transaction
pos<-read.transactions("pos_sampledata.csv",format = "single", sep = ",", cols = c("transaction_number", "prod_category"))

#Method 2: convert into matrix, then convert into transaction
pos<-read.csv("pos_sampledata.csv",header=TRUE)
pos$const= TRUE
pos_mat_prep <- reshape(data = pos,
                        idvar = "transaction_number",
                        timevar = "prod_category",
                        direction = "wide")
pos_matrix <- as.matrix(pos_mat_prep[,-1])
pos_matrix[is.na(pos_matrix)] <- FALSE
colnames(pos_matrix) <- gsub(x=colnames(pos_matrix),
                             pattern="const\\.", replacement="")
pos_trans<-as(pos_matrix,"transactions")

#calculate support assume minimum occurence of 500
500/nrow(pos_matrix)

#Find frequent sets
itemsets <- apriori(pos_matrix, parameter = list(target = "frequent",
                                                 supp=0.0004, minlen = 2, maxlen=4))
quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = pos_matrix1)
inspect(sort(itemsets, by = "lift"))

#Find associative rules
r <- apriori(pos_trans, parameter = list(sup = 0.0004,conf=0, target="rules"))
inspect(sort(r,by="lift"))
subrule <- subset(r, subset = lhs %in% "DRINKS")
inspect(sort(rules.sub,by="lift"))
