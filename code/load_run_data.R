
#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
#setwd('C:/Apps/projects/DrillBit/')
setwd("Z:/project/DrillBit")

#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory
library(tidyverse)
library(randomForest)
library(reshape)
library(plotly)

library(MASS)
library(rpart)				  # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)		# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script


#--------------------------------------------------------
# Paths
#-------------------------------------------------------- 
data_run_path = "./data/data_run.csv"
data_run_label_path = "./data/run_label.csv"


#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
d.run <- read.csv(data_run_path, header=T)
d.run.label <- read.csv(data_run_label_path, header=T)


d1 <- d.run.label[1,] %>% gather("run", "name", X1:X40)
d2 <- d.run.label[2,] %>% gather("run", "wear", X1:X40)
d.run.label <- left_join(d1, d2, by="run")
#write.csv(d.run.label, "runs_label.csv", row.names = F)


#--------------------------------------------------------
# Plot run data
#-------------------------------------------------------- 
runs <- data.frame(t=1:nrow(d.run), d.run[30:40])  
runs.melted <- melt(runs, id = "t")
runs.melted[is.na(runs.melted$value),3] <- 0  # fill NA value
runs.melted[runs.melted$value>50|runs.melted$value<0.1,3] <- 0  # cut 0.1< <50
runs.melted <- runs.melted %>% rename(run=variable)
runs.melted <- left_join(runs.melted, d.run.label, by="run")
runs.melted$wear <- as.factor(runs.melted$wear)

p <- ggplot(data = runs.melted, aes(x = t, y = value, color=wear)) + geom_line() +scale_colour_manual(values=c("darkgreen","darkred")) #geom_point() #
a=p + facet_grid(run~.)
ggsave("r30-40.png")

# p <- ggplot(data = runs.melted, aes(x = t, y = value, group=variable, color=variable)) +
#     geom_line() #geom_point() #
# ggplotly(p)


#--------------------------------------------------------
# Modelling
#-------------------------------------------------------- 

# calculate features: time spend at each pre-defined WF interval
cal_feature <- function(x, cut) {
  
  x <- x[!is.na(x)]  #rm na value
  count <- NULL
  for(i in 1:length(cut)){
    
    if(i==1){ 
      count <- c(count, sum(x>0.1 & x < cut[i]))
    } else {
      count <- c(count, sum(x>cut[i-1] & x < cut[i]))
    }
    
  }
  count <- c(count, sum(x>cut[i] & x < 50))
  
  count <- count*10/3600
  
  return(count)

}

# cut off of WF range
cut <- c(5,8)
cut <- c(2,4,6,8)
cut <- c(2,4,6,8,10)

# calulated features: time spend at each pre-defined WF interval
feature <- as.data.frame(t(sapply(d.run, cal_feature, cut=cut)))


## rule based prediction
rule.pred <- feature %>% mutate(rule.pred=ifelse(V2>5|V3>0.1, 1, 0))
rule <- cbind(d.run.label, pred=rule.pred[,"rule.pred"])
# exclude wrong labeled records
exclude <-c("X9", "X15", "X31", "X33", "X40")
rule <- rule[!rule$run%in%exclude,]
table(rule[,3], rule[,4])
#names(feature) <- c("f1", "f2", "f3")



d <- cbind(d.run.label, feature)
d$wear <- as.factor(d$wear)
d <- d[!d$run%in%exclude,]

## single tree
set.seed(777)
tree <- rpart(wear ~ V1 + V2 + V3 + V4 + V5 + V6 , data = d, method = 'class')
tree.pred <- predict(tree, d, type = 'class')
table(d[,3], tree.pred)

## Random forest
set.seed(111)
(mod.rf <- randomForest(wear ~ ., data = d[,-c(1,2)], ntree=100))
#(mod.rf <- randomForest(wear ~V1+V2+V4+V5, data = d[,-c(1,2)], ntree=10000))
(VI_F=importance(mod.rf))
varImpPlot(mod.rf,type=2)
barplot(t(VI_F/sum(VI_F)))

rf.pred <- predict(mod.rf, d[,-c(1,2)])
table(d[,3], rf.pred)


# logistic regression
mod.logit <- glm(wear ~., data=d[,-c(1,2)], family = binomial(link = 'logit'))
summary(mod.logit)
# anova
#anova(mod.logit, test = 'Chisq')

# prediction on test data
pred <- predict(mod.logit, newdata = d, type = "response")
pred.sol <- ifelse(pred>0.5, 1, 0)
table(d[,3], pred.sol)


############################
# Evaluate model performance
############################
# ROC
pr <- prediction(pred2.sol, test$first_completed)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf)

# AUC
auc(test$first_completed,pred2.sol) # 0.75
