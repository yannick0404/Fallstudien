setwd("~/Uni/5.WS2223/Fallstudien I/Projekt 2")

data<- read.table("mietspiegel2015.csv", header = TRUE)

str(data)




#summary(data)
#plot(data$wfl, data$nmqm)

#aggregate(nmqm ~ as.factor(bez), data = data, mean)

#barplot(aggregate(nmqm ~ as.factor(bez), data = data, mean)[,2] )

# Formatierung der nominalen Vektoren in Faktoren
data$wohngut <- factor(data$wohngut, levels=c(0,1), labels=c("Schlecht", "Gut"))
data$wohnbest <- factor(data$wohnbest, levels=c(0,1), labels=c("SchlechtOderGut", "Best"))
data$ww0 <- factor(data$ww0, levels=c(0,1), labels=c("Ja","Nein"))
data$zh0 <- factor(data$zh0, levels=c(0,1), labels=c("Ja","Nein"))
data$badkach0 <- factor(data$badkach0, levels=c(0,1), labels=c("Gefliest","Ungefliest"))
data$badextra <- factor(data$badextra, levels=c(0,1), labels=c("Normal","Gehoben"))
data$kueche <- factor(data$kueche, levels=c(0,1), labels=c("Normal","Gehoben"))
data$bez <- factor(data$bez)

data <- data[,-1]

## Run LiMo
mod <- lm(data$nmqm~., data=data)
summary(mod)
t <- summary(mod)
t$coefficients[,4]
round(t$coefficients[t$coefficients[,4]<0.2,],3)

mod2 <- lm(nmqm~wfl+rooms+bj+wohngut+zh0+badkach0+badextra+kueche, data=data)
summary(mod2)

## Plot Coefficients
plot(x=1:length(mod$coefficients),y=sort(mod$coefficients), axes = F)
axis(1, at=1:length(mod$coefficients), labels = names(sort(mod$coefficients)))
axis(2)
box()

