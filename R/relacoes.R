library(psych)
setwd("~/TCC/Working/Nov2016")
priv <- read.csv2("bancoFinal--PRIVADO--v2--14-11-16.csv")
pub <- read.csv2("bancoFinal--PUBLICO--v3--14-11-16.csv")

cor(pub$mat, pub$znotaMT, "complete.obs")
cor(priv$mat, priv$znotaMT, "complete.obs")

#----
cnpub <- data.frame(pub$fisica, pub$mat, pub$quimica, pub$bio, pub$znotaCN)
cor(cnpub, y=NULL, "complete.obs", method = "pearson")

cnpriv <- data.frame(priv$fisica, priv$mat, priv$quimica, priv$bio, priv$znotaCN)
cor(cnpriv, y=NULL, "complete.obs", method = "pearson")

#----

chpub <- data.frame(pub$geo, pub$hist, pub$socilogia, pub$filosofia, pub$znotaCH)
cor(chpub, y=NULL, "complete.obs", method="pearson")

chpriv <- data.frame(priv$geo, priv$hist, priv$socilogia, priv$filosofia, priv$znotaCH)
cor(chpriv, y=NULL, "complete.obs", method="pearson")

#----

