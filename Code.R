setwd("C:/Users/Rudi2/iCloudDrive/Uni/Data Science B.Sc/Semester 5/FS1/Projekt 4")
data <- read.csv("Gesundheitszustand.csv")
nrow(data)
data <- subset(data, data$year==1991)
data <- data[,-(1:2)]
colnames(data)
nrow(data)
head(data)

x <- glm(docvis~., data=data, family = "poisson")
summary(x)
round(x$coefficients,4)
mod <- step(x)
round(mod$coefficients,4)



hist(data$docvis, breaks=seq(0,100,1))
barplot(table(data$docvis))
hist(data$hospvis)
barplot(table(data$hospvis))
