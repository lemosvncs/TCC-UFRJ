library(dplyr)
setwd("~/TCC/Working/Nov2016")
#esc14 <- read.csv2("escolas14-limpo-08-11-2016.csv")
#esc14 <- read.csv2("escolas14Publica--v1.1--11-11-2016.csv")
#esc14 <- read.csv2("escolas14Federal--v2--10-11-2016.csv")
#esc14 <- read.csv2("escolas14Estadual--v2--10-11-2016.csv")
#esc14 <- read.csv2("escolas14Municipal--v2--10-11-2016.csv")
esc14 <- read.csv2("escolas14Particular--v1.1--11-11-2016.csv")

escolas <- as.factor(esc14$FK_COD_MUNICIPIO)
escolas <- sort(escolas)
escolas <- matrix(escolas)
escolas <- unique(escolas)
escolas <- data.frame(escolas)

#----

#escolas[, 2:5] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_DEPENDENCIA_ADM)
escolas[, 2:5] <- NA
escolas[, 6:7] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_LOCAL_FUNC_PREDIO_ESCOLAR)
escolas[, 8:9] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_LABORATORIO_INFORMATICA)
escolas[, 10:11] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_LABORATORIO_CIENCIAS)
escolas[, 12:13] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_SALA_ATENDIMENTO_ESPECIAL)
escolas[, 14:15] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_QUADRA_ESPORTES_COBERTA)
escolas[, 16:17] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_QUADRA_ESPORTES_DESCOBERTA)
escolas[, 18:19] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_BIBLIOTECA)
escolas[, 20:21] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_SALA_LEITURA)
escolas[, 22:23] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_DEPENDENCIAS_PNE)
escolas[, 24:25] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_REFEITORIO)
escolas[, 26:27] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_PATIO_COBERTO)
escolas[, 28:29] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_PATIO_DESCOBERTO)
escolas[, 30:31] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_AREA_VERDE)

escolas[, 32:33] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_INTERNET)
escolas[, 34:35] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_BANDA_LARGA)
escolas[, 36:37] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_ALIMENTACAO)
escolas[, 38:39] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_AEE)
escolas[, 40:41] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_MOD_ATIV_COMPLEMENTAR)
escolas[, 42:43] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_MATERIAL_ESP_NAO_UTILIZA)
escolas[, 44:45] <- table(esc14$FK_COD_MUNICIPIO, esc14$ID_PROPOSTA_PEDAG_ALTERNANCIA)
colnames(escolas)[2:45] <- c("adm1", "adm2", "adm3", "adm4", "predio0", "predio1",
                             "labinfo0", "labinfo1", "labciencias0", "labciencias1",
                             "sala_atendimento_esp0", "sala_atendimento_esp1", "quadra_coberta0", "quadra_coberta1",
                             "quadra_descoberta0", "quadra_descoberta1",
                             "biblio0", "biblio1", "sala_leitura0", "sala_leitura1", "pne0", "pne1",
                             "refeitorio0", "refeitorio1", "patio_coberto0", "patio_coberto1", "patio_descoberto0", "patio_descoberto1",
                             "areaverde0", "areaverde1", "internet0", "internet1", "bandalarga0", "bandalarga1",
                             "alimentacao0", "alimentacao1", "aee0", "aee1", "ativcomplementar0", "ativcomplementar1",
                             "materialespnao0", "materialespnao1", "alternancia0", "alternancia1")


#Cada município tem em média tantas salas
mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_SALAS_EXISTENTES), sd(NUM_SALAS_EXISTENTES))
escolas[,46:47] <- mdp[,2:3]
mdp <- NULL

#Cada município utiliza em média tantas salas
mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_SALAS_UTILIZADAS), sd(NUM_SALAS_UTILIZADAS))
escolas[,48:49] <- mdp[,2:3]
mdp <- NULL

#cada município tem em média tantas sals não utilizadas.
mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_SALAS_EXISTENTES-NUM_SALAS_UTILIZADAS), sd(NUM_SALAS_EXISTENTES-NUM_SALAS_UTILIZADAS))
escolas[,50:51] <- mdp[,2:3]
mdp <- NULL

mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_EQUIP_TV), sd(NUM_EQUIP_TV))
escolas[,52:53] <- mdp[,2:3]
mdp <- NULL

mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_EQUIP_DVD), sd(NUM_EQUIP_DVD))
escolas[,54:55] <- mdp[,2:3]
mdp <- NULL

mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_EQUIP_COPIADORA), sd(NUM_EQUIP_COPIADORA))
escolas[,56:57] <- mdp[,2:3]
mdp <- NULL

mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_EQUIP_RETRO), sd(NUM_EQUIP_RETRO))
escolas[,58:59] <- mdp[,2:3]
mdp <- NULL

mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_EQUIP_IMPRESSORA), sd(NUM_EQUIP_IMPRESSORA))
escolas[,60:61] <- mdp[,2:3]
mdp <- NULL

mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_EQUIP_SOM), sd(NUM_EQUIP_SOM))
escolas[,62:63] <- mdp[,2:3]
mdp <- NULL

mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_EQUIP_MULTIMIDIA), sd(NUM_EQUIP_MULTIMIDIA))
escolas[,64:65] <- mdp[,2:3]
mdp <- NULL

mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_COMPUTADORES), sd(NUM_COMPUTADORES))
escolas[,66:67] <- mdp[,2:3]
mdp <- NULL

mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_COMP_ADMINISTRATIVOS), sd(NUM_COMP_ADMINISTRATIVOS))
escolas[,68:69] <- mdp[,2:3]
mdp <- NULL

mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_COMP_ALUNOS), sd(NUM_COMP_ALUNOS))
escolas[,70:71] <- mdp[,2:3]
mdp <- NULL

mdp <- esc14%>%
  group_by(FK_COD_MUNICIPIO)%>%
  summarise(mean(NUM_FUNCIONARIOS), sd(NUM_FUNCIONARIOS))
escolas[,72:73] <- mdp[,2:3]
mdp <- NULL

colnames(escolas)[46:73] <- c("msalasexistentes", "sdsalasexistentes", "msalasutilizadas", "sdutilizadas", "minutilizadas", "sdinutilizadas",
                              "mtv", "sdtv", "mdvd", "sddvd", "mcopiadora", "sdcopiadora", "mretro", "sdretro", "mimpressora", "sdimpressora",
                              "msom", "sdsom", "mmultimidia", "sdmultimidia", "mcomp", "sdcomp", "mcompadm", "sdcompadm",
                              "mcompalunos", "sdcompalunos", "mfuncionarios", "sdfuncionarios")
escolas[,74] <- escolas$msalasexistentes/escolas$sdsalasexistentes
escolas[,75] <- escolas$msalasutilizadas/escolas$sdutilizadas
escolas[,76] <- escolas$minutilizadas/escolas$sdinutilizadas
escolas[,77] <- escolas$mtv/escolas$sdtv
escolas[,78] <- escolas$mdvd/escolas$sddvd
escolas[,79] <- escolas$mcopiadora/escolas$sdcopiadora
escolas[,80] <- escolas$mretro/escolas$sdretro
escolas[,81] <- escolas$mimpressora/escolas$sdimpressora
escolas[,82] <- escolas$msom/escolas$sdsom
escolas[,83] <- escolas$mmultimidia/escolas$sdmultimidia
escolas[,84] <- escolas$mcomp/escolas$sdcomp
escolas[,85] <- escolas$mcompadm/escolas$sdcompadm
escolas[,86] <- escolas$mcompalunos/escolas$sdcompalunos
escolas[,87] <- escolas$mfuncionarios/escolas$sdfuncionarios

colnames(escolas)[74:87] <- c("salasexistentes", "salasutilizadas", "inutilizadas",
                              "tv", "dvd", "copiadora", "retro", "impressora",
                              "som", "multimidia", "comp", "compadm",
                              "compalunos", "funcionarios")

#FORMULAS----
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

#FINAL----
fescolas <- as.factor(esc14$FK_COD_MUNICIPIO)
fescolas <- sort(fescolas)
fescolas <- matrix(fescolas)
fescolas <- unique(fescolas)
fescolas <- data.frame(fescolas)

fescolas[, 2] <- porc4(escolas$adm1, escolas$adm2, escolas$adm3, escolas$adm4)
fescolas[, 3] <- porc4(escolas$adm2, escolas$adm1, escolas$adm3, escolas$adm4)
fescolas[, 4] <- porc4(escolas$adm3, escolas$adm2, escolas$adm1, escolas$adm4)
fescolas[, 5] <- porc4(escolas$adm4, escolas$adm2, escolas$adm3, escolas$adm1)
fescolas[, 6] <- porc(escolas$predio1, escolas$predio0)
fescolas[, 7] <- porc(escolas$labinfo1, escolas$labinfo0)
fescolas[, 8] <- porc(escolas$labciencias1, escolas$labciencias0)
fescolas[, 9] <- porc(escolas$sala_atendimento_esp1, escolas$sala_atendimento_esp0)
fescolas[, 10] <- porc(escolas$quadra_coberta1, escolas$quadra_coberta0)
fescolas[, 11] <- porc(escolas$quadra_descoberta1, escolas$quadra_descoberta0)
fescolas[, 12] <- porc(escolas$areaverde1, escolas$areaverde0)
fescolas[, 13] <- porc(escolas$internet1, escolas$internet0)
fescolas[, 14] <- porc(escolas$bandalarga1, escolas$bandalarga0)
fescolas[, 15] <- porc(escolas$alimentacao1, escolas$alimentacao0)
fescolas[, 16] <- porc(escolas$aee1, escolas$aee0)
fescolas[, 17] <- porc(escolas$ativcomplementar1, escolas$ativcomplementar0)
fescolas[, 18] <- porc(escolas$materialespnao1, escolas$materialespnao0)
fescolas[, 19] <- porc(escolas$alternancia1, escolas$alternancia0)
colnames(fescolas)[2:19] <- c("adm1", "adm2", "adm3", "adm4", "predio", "labinfo", "labciencias", "sala_atendimento_esp",
                           "quadracoberta", "quadradescoberta", "areaverde", "internet", "bandalarga", "alimentacao", "aee",
                           "ativcomplementar", "materialespnao", "alternancia")

fescolas[, 20] <- scale(na_if(escolas$salasexistentes, Inf))
fescolas[, 21] <- scale(na_if(escolas$salasutilizadas, Inf))
fescolas[, 22] <- scale(na_if(escolas$inutilizadas, Inf))
fescolas[, 23] <- scale(na_if(escolas$tv, Inf))
fescolas[, 24] <- scale(na_if(escolas$dvd, Inf))
fescolas[, 25] <- scale(na_if(escolas$copiadora, Inf))
fescolas[, 26] <- scale(na_if(escolas$retro, Inf))
fescolas[, 27] <- scale(na_if(escolas$impressora, Inf))
fescolas[, 28] <- scale(na_if(escolas$som, Inf))
fescolas[, 29] <- scale(na_if(escolas$multimidia, Inf))
fescolas[, 30] <- scale(na_if(escolas$comp, Inf))
fescolas[, 31] <- scale(na_if(escolas$compadm, Inf))
fescolas[, 32] <- scale(na_if(escolas$compalunos, Inf))
fescolas[, 33] <- scale(na_if(escolas$funcionarios, Inf))
colnames(fescolas)[20:33] <- colnames(escolas)[74:87]

#SALVANDO----
#write.csv2(fescolas, "escolas--final--v1--11-11-2016.csv")
#write.csv2(fescolas, "escolas--federal--v1--11-11-2016.csv")
#write.csv2(fescolas, "escolas--municipal--v1--11-11-2016.csv")
#write.csv2(fescolas, "escolas--estadual--v1--11-11-2016.csv")
write.csv2(fescolas, "escolas--particular--FINAL--v1--11-11-2016.csv")
#write.csv2(fescolas, "escolas--publicas--v1--11-11-2016.csv")
