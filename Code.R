setwd("~/Uni/5.WS2223/Fallstudien I/Projekt 2")

data<- read.table("mietspiegel2015.csv", header = TRUE)

str(data)

summary(data)
plot(data$wfl, data$nmqm)

aggregate(nmqm ~ as.factor(bez), data = data, mean)

barplot(aggregate(nmqm ~ as.factor(bez), data = data, mean)[,2] )

