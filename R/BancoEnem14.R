library(dplyr)
library(fastmatch)

setwd("~/TCC/Working/Nov2016")
#enem14 <- read.csv2("enem14--v2--10-11-2016.csv")
#enem14 <- read.csv2("enem14Publica--v2--10-11-2016.csv")
#enem14 <- read.csv2("enem14Estadual--v2--10-11-20166.csv")
#enem14 <- read.csv2("enem14Municipal--v2--10-11-2016.csv")
#enem14 <- read.csv2("enem14Federal--v2--10-11-2016.csv")
enem14 <- read.csv2("enem14Privada--v2--10-11-2016.csv")


#BASE----

dfEnem <- as.factor(enem14$COD_MUNICIPIO_RESIDENCIA)
dfEnem <- sort(dfEnem)
dfEnem <- matrix(dfEnem)
dfEnem <- unique(dfEnem)
dfEnem <- data.frame(dfEnem)

levels(as.factor(enem14$ID_DEPENDENCIA_ADM_ESC))
#dfEnem[, 2:5] <- table(enem14$COD_MUNICIPIO_RESIDENCIA, enem14$ID_DEPENDENCIA_ADM_ESC)
dfEnem[, 2:5] <- NA
dfEnem[, 6:7] <- table(enem14$COD_MUNICIPIO_RESIDENCIA, enem14$TP_SEXO)
dfEnem[, 8:13] <- table(enem14$COD_MUNICIPIO_RESIDENCIA, enem14$TP_COR_RACA) #0 1 2 3 4 5
dfEnem[, 14:15] <- table(enem14$COD_MUNICIPIO_RESIDENCIA, enem14$TP_LINGUA)
colnames(dfEnem)[2:15] <- c("ID_ADM_1","ID_ADM_2", "ID_ADM_3", "ID_ADM_4", "TP_SEXO_0", "TP_SEXO_1", "TP_COR_RACA_0",
                        "TP_COR_RACA_1", "TP_COR_RACA_2", "TP_COR_RACA_3","TP_COR_RACA_4", "TP_COR_RACA_5",
                        "TP_LINGUA_0", "TP_LINGUA_1")

mdp <- enem14%>%
  group_by(COD_MUNICIPIO_RESIDENCIA)%>%
  summarise(mean(NOTA_CN), sd(NOTA_CN))
dfEnem[,16:17] <- mdp[,2:3]
mdp <- NULL

mdp <- enem14%>%
  group_by(COD_MUNICIPIO_RESIDENCIA)%>%
  summarise(mean(NOTA_CH), sd(NOTA_CH))
dfEnem[,18:19] <- mdp[,2:3]
mdp <- NULL

mdp <- enem14%>%
  group_by(COD_MUNICIPIO_RESIDENCIA)%>%
  summarise(mean(NOTA_MT), sd(NOTA_MT))
dfEnem[,20:21] <- mdp[,2:3]
mdp <- NULL

mdp <- enem14%>%
  group_by(COD_MUNICIPIO_RESIDENCIA)%>%
  summarise(mean(NOTA_LC), sd(NOTA_LC))
dfEnem[,22:23] <- mdp[,2:3]
mdp <- NULL

mdp <- enem14%>%
  group_by(COD_MUNICIPIO_RESIDENCIA)%>%
  summarise(mean(NU_NOTA_REDACAO), sd(NU_NOTA_REDACAO))
dfEnem[,24:25] <- mdp[,2:3]
mdp <- NULL

colnames(dfEnem)[16:25] <- c("MEAN_NOTA_CN", "SD_NOTA_CN", "MEAN_NOTA_CH", "SD_NOTA_CH", "MEAN_NOTA_MT", "SD_NOTA_MT",
                         "MEAN_NOTA_LC", "SD_NOTA_LC", "MEAN_NOTA_REDACAO", "SD_NOTA_REDACAO")

dfEnem[,26] <- dfEnem$MEAN_NOTA_CN/dfEnem$SD_NOTA_CN
dfEnem[,27] <- dfEnem$MEAN_NOTA_CH/dfEnem$SD_NOTA_CH
dfEnem[,28] <- dfEnem$MEAN_NOTA_MT/dfEnem$SD_NOTA_MT
dfEnem[,29] <- dfEnem$MEAN_NOTA_LC/dfEnem$SD_NOTA_LC
dfEnem[,30] <- dfEnem$MEAN_NOTA_REDACAO/dfEnem$SD_NOTA_REDACAO
colnames(dfEnem)[26:30] <- c("NOTA_CN_INDEX", "NOTA_CH_INDEX", "NOTA_MT_INDEX", "NOTA_LC_INDEX", "NOTA_REDACAO_INDEX")

dfEnem[, 31] <- scale(dfEnem$NOTA_CN_INDEX)
dfEnem[, 32] <- scale(dfEnem$NOTA_CH_INDEX)
dfEnem[, 33] <- scale(dfEnem$NOTA_MT_INDEX)
dfEnem[, 34] <- scale(dfEnem$NOTA_LC_INDEX)
dfEnem[, 35] <- scale(dfEnem$NOTA_REDACAO_INDEX)


#FUNÇÕES----
porc <- function(a, b)
{
  total <- a+b
  return(a*100/total)
}

porc4 <- function(a, b, c, d)
{
  total <- a+b+c+d
  return(a*100/total)
}

porc3 <- function(a, b, c)
{
  total <- a+b+c
  return(a*100/total)
}

porc6 <- function(a, b, c, d, e, f)
{
  total <- a+b+c+d+e+f
  return(a*100/total)
}
#PORCENTAGEM e Z----
fenem <- as.factor(enem14$COD_MUNICIPIO_RESIDENCIA)
fenem <- sort(fenem)
fenem <- matrix(fenem)
fenem <- unique(fenem)
fenem <- data.frame(fenem)

fenem[,2] <- porc4(dfEnem$ID_ADM_1, dfEnem$ID_ADM_2, dfEnem$ID_ADM_3, dfEnem$ID_ADM_4)
fenem[,3] <- porc4(dfEnem$ID_ADM_2, dfEnem$ID_ADM_1, dfEnem$ID_ADM_3, dfEnem$ID_ADM_4)
fenem[,4] <- porc4(dfEnem$ID_ADM_3, dfEnem$ID_ADM_2, dfEnem$ID_ADM_1, dfEnem$ID_ADM_4)
fenem[,5] <- porc4(dfEnem$ID_ADM_4, dfEnem$ID_ADM_2, dfEnem$ID_ADM_3, dfEnem$ID_ADM_1)
fenem[,6] <- porc(dfEnem$TP_SEXO_1, dfEnem$TP_SEXO_0)
fenem[,7] <- porc6(dfEnem$TP_COR_RACA_0, dfEnem$TP_COR_RACA_1, dfEnem$TP_COR_RACA_2, dfEnem$TP_COR_RACA_3, dfEnem$TP_COR_RACA_4, dfEnem$TP_COR_RACA_5)
fenem[,8] <- porc6(dfEnem$TP_COR_RACA_1, dfEnem$TP_COR_RACA_0, dfEnem$TP_COR_RACA_2, dfEnem$TP_COR_RACA_3, dfEnem$TP_COR_RACA_4, dfEnem$TP_COR_RACA_5)
fenem[,9] <- porc6(dfEnem$TP_COR_RACA_2, dfEnem$TP_COR_RACA_1, dfEnem$TP_COR_RACA_0, dfEnem$TP_COR_RACA_3, dfEnem$TP_COR_RACA_4, dfEnem$TP_COR_RACA_5)
fenem[,10] <- porc6(dfEnem$TP_COR_RACA_3, dfEnem$TP_COR_RACA_1, dfEnem$TP_COR_RACA_2, dfEnem$TP_COR_RACA_0, dfEnem$TP_COR_RACA_4, dfEnem$TP_COR_RACA_5)
fenem[,11] <- porc6(dfEnem$TP_COR_RACA_4, dfEnem$TP_COR_RACA_1, dfEnem$TP_COR_RACA_2, dfEnem$TP_COR_RACA_3, dfEnem$TP_COR_RACA_0, dfEnem$TP_COR_RACA_5)
fenem[,12] <- porc6(dfEnem$TP_COR_RACA_5, dfEnem$TP_COR_RACA_1, dfEnem$TP_COR_RACA_2, dfEnem$TP_COR_RACA_3, dfEnem$TP_COR_RACA_4, dfEnem$TP_COR_RACA_0)
fenem[,13] <- porc(dfEnem$TP_LINGUA_1, dfEnem$TP_LINGUA_0)

fenem[, 14] <- scale(na_if(dfEnem$NOTA_CN_INDEX, Inf))
fenem[, 15] <- scale(na_if(dfEnem$NOTA_CH_INDEX, Inf))
fenem[, 16] <- scale(na_if(dfEnem$NOTA_MT_INDEX, Inf))
fenem[, 17] <- scale(na_if(dfEnem$NOTA_LC_INDEX, Inf))
fenem[, 18] <- scale(na_if(dfEnem$NOTA_REDACAO_INDEX, Inf))

colnames(fenem)[1:18] <- c("municipios", "adm1", "adm2", "adm3", "adm4", "sexo1", "raca0", "raca1", "raca2", "raca3", "raca4", "raca5", "lingua1", "znotaCN",
                     "znotaCH", "znotaMT", "znotaLC", "znotaredacao")

#SALVANDO----
#write.csv2(fenem, "enem14--FINAL--v1--10-11-2016.csv")
#write.csv2(fenem, "enem14--Publica--FINAL--v1--11-11-2016.csv")
write.csv2(fenem, "enem14--Privada--FINAL--v1--11-11-2016.csv")
#write.csv2(fenem, "enem14--Federal--v1--11-11-2016.csv")
#write.csv2(fenem, "enem14--Estadual--v1--11-11-2016.csv")
#write.csv2(fenem, "enem14--Municipal--v1--11-11-2016.csv")
