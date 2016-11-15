setwd("~/TCC/Working/Nov2016")
enem <- read.csv2("ENEM--PUB--BRUTOPORMUNICIPIO--V1--14-11-2016.csv")
turmas <- read.csv2("TURMAS--BRUTOPORMUNICIPIO--PUB--V1--14-11-2016.csv")
escolas <- read.csv2("ESCOLAS--BRUTOPORMUNICIPIO--PUB--V1--14-11-2016.CSV")

colnames(escolas)[2] <- "munescolas"
colnames(turmas)[2] <- "munturmas"

#ENEM & Escolas
selenem <- escolas$munescolas[match(enem$dfEnem, escolas$munescolas)]
enem <- subset(enem, enem$dfEnem==selenem)
selescolas <- enem$dfEnem[match(escolas$munescolas, enem$dfEnem)]
escolas <- subset(escolas, escolas$munescolas==selescolas)
enem[, 27:101] <- escolas

#ENEMESCOLAS + turmas
selturmas <- enem$dfEnem[match(turmas$munturmas, enem$dfEnem)]
turmas <- subset(turmas, turmas$munturmas==selturmas)
selfinal <- turmas$munturmas[match(enem$dfEnem, turmas$munturmas)]
enem <- subset(enem, enem$dfEnem==selfinal)
enem[, 101:147] <- turmas

write.csv2(enem, "bancoFinal--PUBLICO--v4--14-11-16.csv")





#PRIVADAS----
setwd("~/TCC/Working/Nov2016")
enem <- read.csv2("ENEM--PRIV--BRUTOPORMUNICIPIO--V1--14-11-2016.csv")
escolas <- read.csv2("ESCOLAS--BRUTOPORMUNICIPIO--PRIV--V1--14-11-2016.csv")
turmas <- read.csv2("TURMAS--BRUTOPORMUNICIPIO--PRIV--V1--14-11-2016.csv")

colnames(escolas)[2] <- "munescolas"
colnames(turmas)[2] <- "munturmas"

#ENEM & Escolas
selenem <- escolas$munescolas[match(enem$dfEnem, escolas$munescolas)]
enem <- subset(enem, enem$dfEnem==selenem)
selescolas <- enem$dfEnem[match(escolas$munescolas, enem$dfEnem)]
escolas <- subset(escolas, escolas$munescolas==selescolas)
enem[, 27:101] <- escolas

#ENEMESCOLAS + turmas
selturmas <- enem$dfEnem[match(turmas$munturmas, enem$dfEnem)]
turmas <- subset(turmas, turmas$munturmas==selturmas)
selfinal <- turmas$munturmas[match(enem$dfEnem, turmas$munturmas)]
enem <- subset(enem, enem$dfEnem==selfinal)
enem[, 101:147] <- turmas

#WRITINGS ON THE WAAALLLL
write.csv2(enem, "bancoFinal--PRIVADO--v2--14-11-16.csv")

#write.csv2(enem2, "enem14--1656ROWS--v1--13-11-2016.csv")
#write.csv2(escolas2, "escolas14--165ROWS--v1--13-11-2016.csv")
