### Projekt 2

## Einlesen 
# setwd("~/Uni/5.WS2223/Fallstudien I/Projekt 2")

data<- read.table("mietspiegel2015.csv", header = TRUE)

## Uebersicht
str(data)

summary(data)

## nominale Daten zu Faktoren
fact.mietspiegel <- function() {
  if(is.factor(data$wohngut)) stop("schon fertig")
  else {
    data$wohngut <<- factor(data$wohngut, levels=c(0,1), labels=c("andere Lagekategorie", "Gut"))
    data$wohnbest <<- factor(data$wohnbest, levels=c(0,1), labels=c("andere Lagekategorie", "Best"))
    data$ww0 <<- factor(data$ww0, levels=c(0,1), labels=c("Ja","Nein"))
    data$zh0 <<- factor(data$zh0, levels=c(0,1), labels=c("Ja","Nein"))
    data$badkach0 <<- factor(data$badkach0, levels=c(0,1), labels=c("Gefliest","Ungefliest"))
    data$badextra <<- factor(data$badextra, levels=c(0,1), labels=c("Normal","Gehoben"))
    data$kueche <<- factor(data$kueche, levels=c(0,1), labels=c("Normal","Gehoben"))
    data$bez <<- factor(data$bez)
  }
}
fact.mietspiegel()


# Grafiken
attach(data)

plot(wfl, nmqm)

hist(nmqm,  xlab = "Nettomiete pro qm", freq = F)

hist(wfl, xlab = " Wohnflaeche", freq = F)

barplot(table(rooms))

hist(bj, xlab = "Baujahr", freq = F)

barplot(aggregate(nmqm ~ as.factor(bez),FUN =  mean)[,2],
        main = "Bissel unuebersichtlich", xlab = "Bezirkname in Muenchen " )

# entdummien
barplot(table(as.numeric(wohngut) + as.numeric(wohnbest) +
                as.numeric(wohnbest)-3), names.arg = c("Schlecht","Gut", "Best"),
        xlab = "Lage")

# barplot(table(wohngut))

# barplot(table(wohnbest))


barplot(table(ww0), xlab = "Warmwasserversorgung vom Vermieter gestellt")

barplot(table(zh0), xlab = "Zentralheizung verfuegbar")

barplot(table(badkach0), xlab = "Gefliestes/Gekacheltes Bad")

barplot(table(badextra), xlab = "Ausstattung des Bades")

barplot(table(kueche), xlab = "Ausstattung der Kueche")

detach(data)




## Ueberpruefung der Voraussetzungen
cor(data[2:5])
# eta koeff fuer nominale oder so?


## Lineares Modell
mod_all <- lm(nmqm ~ wfl + rooms + bj + bez + wohngut + wohnbest + ww0 + zh0 + 
                badkach0 + badextra + kueche, data = data)
summary(mod_all)


# Probieren
mod_opt <- lm(nmqm ~ rooms + I(bj-mean(bj)) + wohngut + wohnbest + ww0 + zh0 + 
             badkach0 + badextra + kueche, data = data)
summary(mod_opt)
plot(mod_opt) 

mod_opt2 <- lm(nmqm ~ rooms + I(bj-mean(bj)) + wohngut + wohnbest + ww0 + zh0 + 
                 badkach0 + badextra + kueche, data = data)

summary(mod_opt2)

plot(data$nmqm, predict(mod_opt2))

# data$zh0[data$zh0 == 0] <- -1 
# Ueber einstimmung mit factor Ergebnissen
summary(lm(nmqm ~ zh0 ,data = data))
summary(lm(nmqm ~ as.factor(zh0) ,data = data))

# Differenzieller effekt beachten bei Bezirken 
# aufpassen bei Interpretation bei bezug


plot(nmqm ~I(log(wfl)), data = data)


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


# Probieren geht ueber studieren
#######################################################



plot(hatvalues(test), type = "h")

plot(cooks.distance(test), type = "h")

which(cooks.distance(test) > 0.01)
data[1975,]


sum(lm(nmqm ~ bez, data = data)$coeff)

# Problem mit Rang s. S 138 im Script
