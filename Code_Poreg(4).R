# setwd("C:/Users/Rudi2/iCloudDrive/Uni/Data Science B.Sc/Semester 5/FS1/Projekt 4")
# setwd("~/Uni/5.WS2223/Fallstudien I/Projekt 4")

data <- read.csv("Gesundheitszustand.csv")
nrow(data)
names(data)
summary(data)


anpassung <- function() {
  if(ncol(data) != 28) return(warning("Daten sind bereits vorverarbeitet"))
  else {
    data <<- subset(data, data$year==1991)
    data <<- data[,-c(1:2, 27:28)]
  }
}
anpassung()


colnames(data)
nrow(data)
head(data)



table(data$hsat)
table(data$handdum)
table(data$hhinc)
table(data$educ)


# Modell
x <- glm(docvis~., data=data, family = "poisson")
summary(x)
round(x$coefficients,4)
mod <- step(x)
round(mod$coefficients,4)



hist(data$docvis, breaks=seq(0,100,1))
barplot(table(data$docvis))
hist(data$hospvis)
barplot(table(data$hospvis))
