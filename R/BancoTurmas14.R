library(dplyr)
setwd("~/TCC/Working/Nov2016")
#turmas14 <- read.csv2("turmas14--limpo--v3--10-11-2016.csv")
#turmas14 <- read.csv2("turmas14--limpo--v3--10-11-2016.csv")
#turmas14 <- read.csv2("turmas14Publica--v1.1--11-11-2016.csv")
#turmas14 <- read.csv2("turmas14Estadual--v1.1--11-11-2016.csv")
#turmas14 <- read.csv2("turmas14Municipal--v1.1--11-11-2016.csv")
turmas14 <- read.csv2("turmas14Particular--v1.1--11-11-2016.csv")
#turmas14 <- read.csv2("turmas14Federal--v1.1--11-11-2016.csv")

turmas <- as.factor(turmas14$FK_COD_MUNICIPIO)
turmas <- sort(turmas)
turmas <- matrix(turmas)
turmas <- unique(turmas)
turmas <- data.frame(turmas)

#turmas[,2:5] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_DEPENDENCIA_ADM)
turmas[,2:5] <- NA
turmas[,6:7] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_QUIMICA)
turmas[,8:9] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_FISICA)
turmas[,10:11] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_MATEMATICA)
turmas[,12:13] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_BIOLOGIA)
turmas[,14:15] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_LINGUA_LITERAT_PORTUGUESA)
turmas[,16:17] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_LINGUA_LITERAT_INGLES)
turmas[,18:19] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_LINGUA_LITERAT_ESPANHOL)
turmas[,20:21] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_LINGUA_LITERAT_FRANCES)
turmas[,22:23] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_LINGUA_LITERAT_OUTRA)
turmas[,24:25] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_LINGUA_LITERAT_INDIGENA)
turmas[,26:27] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_ARTES)
turmas[,28:29] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_EDUCACAO_FISICA)
turmas[,30:31] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_HISTORIA)
turmas[,32:33] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_GEOGRAFIA)
turmas[,34:35] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_FILOSOFIA)
turmas[,36:37] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_ENSINO_RELIGIOSO)
turmas[, 38:39] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_SOCIOLOGIA)
turmas[,40:41] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_INFORMATICA_COMPUTACAO)
turmas[,42:43] <- table(turmas14$FK_COD_MUNICIPIO, turmas14$ID_DISCIPLINAS_PEDAG)

colnames(turmas)[2:43] <- c("adm1", "adm2", "adm3", "adm4", "quimica0", "quimica1", "fisica0", "fisica1",
                            "mat0", "mat1", "bio0", "bio1", "port0", "port1", "ing0", "ing1", "esp0", "esp1","frances0", "frances1",
                            "outra0", "outra1", "indigena0", "indigena1", "artes0", "artes1", "edfisica0", "edfisica1",
                            "hist0", "hist1", "geo0", "geo1", "filosofia0", "filosofia1", "ensreligioso0", "ensreligioso1",
                            "sociologia0", "sociologia1", "inf0", "inf1", "pedag0", "pedag1")

mdp <- turmas14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NU_DURACAO_TURMA), sd(NU_DURACAO_TURMA))
turmas[,44:45] <- mdp[,2:3]
mdp <- NULL

turmas[,46] <- turmas$media_duracaoturma/turmas$desvio_duracaoturma
colnames(turmas)[44:46] <- c("media_duracaoturma", "desvio_duracaoturma", "duracao_index")

#Funções----
porc4 <- function(a, b, c, d)
{
  total <- a+b+c+d
  return(a*100/total)
}

porc <- function(a, b)
{
  total <- a+b
  return(a*100/total)
}

#Porcentagens e Z----
fturmas <- as.factor(turmas14$FK_COD_MUNICIPIO)
fturmas <- sort(fturmas)
fturmas <- matrix(fturmas)
fturmas <- unique(fturmas)
fturmas <- data.frame(fturmas)

fturmas[,2] <- porc4(turmas$adm1, turmas$adm2, turmas$adm3, turmas$adm4)
fturmas[,3] <- porc4(turmas$adm2, turmas$adm1, turmas$adm3, turmas$adm4)
fturmas[,4] <- porc4(turmas$adm3, turmas$adm2, turmas$adm1, turmas$adm4)
fturmas[,5] <- porc4(turmas$adm4, turmas$adm2, turmas$adm3, turmas$adm1)
fturmas[,6] <- porc(turmas$quimica1, turmas$quimica0)
fturmas[,7] <- porc(turmas$fisica1, turmas$fisica0)
fturmas[,8] <- porc(turmas$mat1, turmas$mat0)
fturmas[,9] <- porc(turmas$bio1, turmas$bio0)
fturmas[,10] <- porc(turmas$port1, turmas$port0)
fturmas[,11] <- porc(turmas$ing1, turmas$ing0)
fturmas[,12] <- porc(turmas$esp1, turmas$esp0)
fturmas[,13] <- porc(turmas$frances1, turmas$frances0)
fturmas[,14] <- porc(turmas$outra1, turmas$outra0)
fturmas[,15] <- porc(turmas$indigena1, turmas$indigena0)
fturmas[,16] <- porc(turmas$artes1, turmas$artes0)
fturmas[,17] <- porc(turmas$edfisica1, turmas$edfisica0)
fturmas[,18] <- porc(turmas$hist1, turmas$hist0)
fturmas[,19] <- porc(turmas$geo1, turmas$geo0)
fturmas[,20] <- porc(turmas$filosofia1, turmas$filosofia0)
fturmas[,21] <- porc(turmas$ensreligioso1, turmas$ensreligioso0)
fturmas[,22] <- porc(turmas$sociologia1, turmas$sociologia0)
fturmas[,23] <- porc(turmas$inf1, turmas$inf0)
fturmas[,24] <- porc(turmas$pedag1, turmas$pedag0)
fturmas[,25] <- scale(na_if(turmas$duracao_index, Inf))

colnames(fturmas)[2:25] <- c("federal", "estadual", "municipal", "particular", "quimica", "fisica", "mat", "bio", "port", "ing", "esp",
                             "frances", "outralingua", "linguaindigena", "artes", "edfisica", "hist", "geo", "filosofia", "ensreligioso",
                             "socilogia", "inf", "pedag", "duracao_turmas")

#write.csv2(fturmas, "turmas14--final--v1--11-11-2016.csv")
#write.csv2(fturmas, "turmas14--Publica--FINAL--v1--11-11-2016.csv")
write.csv2(fturmas, "turmas14--Privada--FINAL--v3--13-11-2016.csv")
#write.csv2(fturmas, "turmas14--Federal--v1--11-11-2016.csv")
#write.csv2(fturmas, "turmas14--Estadual--v1--11-11-2016.csv")
#write.csv2(fturmas, "turmas14--Municipal--v1--11-11-2016.csv")
