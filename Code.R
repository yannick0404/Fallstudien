setwd("~/Uni/5.WS2223/Fallstudien I/Projekt 2")

data<- read.table("mietspiegel2015.csv", header = TRUE)

str(data)

summary(data)
plot(data$wfl, data$nmqm)

aggregate(nmqm ~ as.factor(bez), data = data, mean)

barplot(aggregate(nmqm ~ as.factor(bez), data = data, mean)[,2] )

# Formatierung der nominalen Vektoren in Faktoren
factors <- function(df){
  df$wohngut[df$wohngut == 1] <- "Gute Lage"
  df$wohngut[df$wohngut == 0] <- "Normale Lage"
  df$wohngut <- as.factor(df$wohngut)

  df$wohnbest[df$wohnbest == 1] <- "Beste Lage"
  df$wohnbest[df$wohnbest == 0] <- "Normale Lage"
  df$wohnbest <- as.factor(df$wohnbest)

  df$ww0[df$ww0 == 1] <- "nein"
  df$ww0[df$ww0 == 0] <- "ja"
  df$ww0 <- as.factor(df$ww0)

  df$zh0[df$zh0 == 1] <- "nein"
  df$zh0[df$zh0 == 0] <- "ja"
  df$zh0 <- as.factor(df$zh0)

  df$badkach0[df$badkach0 == 1] <- "ungefliest"
  df$badkach0[df$badkach0 == 0] <- "gefliest"
  df$badkach0 <- as.factor(df$badkach0)

  df$badextra[df$badextra == 1] <- "gehoben"
  df$badextra[df$badextra == 0] <- "normal"
  df$badextra <- as.factor(df$badextra)

  df$kueche[df$kueche == 1] <- "gehoben"
  df$kueche[df$kueche == 0] <- "normal"
  df$kueche <- as.factor(df$kueche)
  
  df$bez <- as.factor(df$bez)
  return(df)
}

data <- factors(data)
