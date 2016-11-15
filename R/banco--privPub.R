priv <- read.csv2("bancoFinal--PUBLICO--v4--14-11-16.csv")
#priv <- read.csv2("bancoFinal--PRIVADO--v2--14-11-16.csv")
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

temp <- as.factor(priv$dfEnem)
temp <- sort(temp)
temp <- matrix(temp)
temp <- unique(temp)
temp <- data.frame(temp)

temp[,2] <- priv$MEAN_NOTA_CN/priv$SD_NOTA_CN
temp[,3] <- priv$MEAN_NOTA_CH/priv$SD_NOTA_CH
temp[,4] <- priv$MEAN_NOTA_MT/priv$SD_NOTA_MT
temp[,5] <- priv$MEAN_NOTA_LC/priv$SD_NOTA_LC
temp[,6] <- priv$MEAN_NOTA_REDACAO/priv$SD_NOTA_REDACAO
colnames(temp)[2:6] <- c("NOTA_CN_INDEX", "NOTA_CH_INDEX", "NOTA_MT_INDEX", "NOTA_LC_INDEX", "NOTA_REDACAO_INDEX")
temp[,7] <- priv$media_duracaoturma/priv$desvio_duracaoturma
temp[,8] <- priv$msalasexistentes/priv$sdsalasexistentes
temp[,9] <- priv$msalasutilizadas/priv$sdutilizadas
temp[,10] <- priv$minutilizadas/priv$sdinutilizadas
temp[,11] <- priv$mtv/priv$sdtv
temp[,12] <- priv$mdvd/priv$sddvd
temp[,13] <- priv$mcopiadora/priv$sdcopiadora
temp[,14] <- priv$mretro/priv$sdretro
temp[,15] <- priv$mimpressora/priv$sdimpressora
temp[,16] <- priv$msom/priv$sdsom
temp[,17] <- priv$mmultimidia/priv$sdmultimidia
temp[,18] <- priv$mcomp/priv$sdcomp
temp[,19] <- priv$mcompadm/priv$sdcompadm
temp[,20] <- priv$mcompalunos/priv$sdcompalunos
temp[,21] <- priv$mfuncionarios/priv$sdfuncionarios
colnames(temp)[7:21] <- c("duracao_turma", "salasexistentes", "salasutilizadas", "inutilizadas",
                              "tv", "dvd", "copiadora", "retro", "impressora",
                              "som", "multimidia", "comp", "compadm",
                              "compalunos", "funcionarios")

#ENEM----
colnames(priv)[3] <- "municipios"
final_priv <- as.factor(priv$municipios)
final_priv <- sort(final_priv)
final_priv <- matrix(final_priv)
final_priv <- unique(final_priv)
final_priv <- data.frame(final_priv)

final_priv[,2] <- NA
final_priv[,3] <- NA
final_priv[,4] <- NA
final_priv[,5] <- NA
final_priv[,6] <- porc(priv$TP_SEXO_1, priv$TP_SEXO_0)
final_priv[,7] <- porc6(priv$TP_COR_RACA_0, priv$TP_COR_RACA_1, priv$TP_COR_RACA_2, priv$TP_COR_RACA_3, priv$TP_COR_RACA_4, priv$TP_COR_RACA_5)
final_priv[,8] <- porc6(priv$TP_COR_RACA_1, priv$TP_COR_RACA_0, priv$TP_COR_RACA_2, priv$TP_COR_RACA_3, priv$TP_COR_RACA_4, priv$TP_COR_RACA_5)
final_priv[,9] <- porc6(priv$TP_COR_RACA_2, priv$TP_COR_RACA_1, priv$TP_COR_RACA_0, priv$TP_COR_RACA_3, priv$TP_COR_RACA_4, priv$TP_COR_RACA_5)
final_priv[,10] <- porc6(priv$TP_COR_RACA_3, priv$TP_COR_RACA_1, priv$TP_COR_RACA_2, priv$TP_COR_RACA_0, priv$TP_COR_RACA_4, priv$TP_COR_RACA_5)
final_priv[,11] <- porc6(priv$TP_COR_RACA_4, priv$TP_COR_RACA_1, priv$TP_COR_RACA_2, priv$TP_COR_RACA_3, priv$TP_COR_RACA_0, priv$TP_COR_RACA_5)
final_priv[,12] <- porc6(priv$TP_COR_RACA_5, priv$TP_COR_RACA_1, priv$TP_COR_RACA_2, priv$TP_COR_RACA_3, priv$TP_COR_RACA_4, priv$TP_COR_RACA_0)
final_priv[,13] <- porc(priv$TP_LINGUA_1, priv$TP_LINGUA_0)

final_priv[, 14] <- scale(na_if(temp$NOTA_CN_INDEX, Inf))
final_priv[, 15] <- scale(na_if(temp$NOTA_CH_INDEX, Inf))
final_priv[, 16] <- scale(na_if(temp$NOTA_MT_INDEX, Inf))
final_priv[, 17] <- scale(na_if(temp$NOTA_LC_INDEX, Inf))
final_priv[, 18] <- scale(na_if(temp$NOTA_REDACAO_INDEX, Inf))

colnames(final_priv)[1:18] <- c("municipiosENEM", "adm1", "adm2", "adm3", "adm4", "sexo1", "raca0", "raca1", "raca2", "raca3", "raca4", "raca5", "lingua1", "znotaCN",
                           "znotaCH", "znotaMT", "znotaLC", "znotaredacao")
#ESCOLAS----

final_priv[, 19] <- NA
final_priv[, 20] <- NA
final_priv[, 21] <- NA
final_priv[, 22] <- NA
final_priv[, 23] <- porc(temp$predio1, temp$predio0)
final_priv[, 24] <- porc(temp$labinfo1, temp$labinfo0)
final_priv[, 25] <- porc(temp$labciencias1, temp$labciencias0)
final_priv[, 26] <- porc(temp$sala_atendimento_esp1, temp$sala_atendimento_esp0)
final_priv[, 27] <- porc(temp$quadra_coberta1, temp$quadra_coberta0)
final_priv[, 28] <- porc(temp$quadra_descoberta1, temp$quadra_descoberta0)
final_priv[, 29] <- porc(temp$areaverde1, temp$areaverde0)
final_priv[, 30] <- porc(temp$internet1, temp$internet0)
final_priv[, 31] <- porc(temp$bandalarga1, temp$bandalarga0)
final_priv[, 32] <- porc(temp$alimentacao1, temp$alimentacao0)
final_priv[, 33] <- porc(temp$aee1, temp$aee0)
final_priv[, 34] <- porc(temp$ativcomplementar1, temp$ativcomplementar0)
final_priv[, 35] <- porc(temp$materialespnao1, temp$materialespnao0)
final_priv[, 36] <- porc(temp$alternancia1, temp$alternancia0)
colnames(final_priv)[19:36] <- c("adm1", "adm2", "adm3", "adm4", "predio", "labinfo", "labciencias", "sala_atendimento_esp",
                              "quadracoberta", "quadradescoberta", "areaverde", "internet", "bandalarga", "alimentacao", "aee",
                              "ativcomplementar", "materialespnao", "alternancia")

final_priv[, 37] <- scale(na_if(temp$salasexistentes, Inf))
final_priv[, 38] <- scale(na_if(temp$salasutilizadas, Inf))
final_priv[, 39] <- scale(na_if(temp$inutilizadas, Inf))
final_priv[, 40] <- scale(na_if(temp$tv, Inf))
final_priv[, 41] <- scale(na_if(temp$dvd, Inf))
final_priv[, 42] <- scale(na_if(temp$copiadora, Inf))
final_priv[, 43] <- scale(na_if(temp$retro, Inf))
final_priv[, 44] <- scale(na_if(temp$impressora, Inf))
final_priv[, 45] <- scale(na_if(temp$som, Inf))
final_priv[, 46] <- scale(na_if(temp$multimidia, Inf))
final_priv[, 47] <- scale(na_if(temp$comp, Inf))
final_priv[, 48] <- scale(na_if(temp$compadm, Inf))
final_priv[, 49] <- scale(na_if(temp$compalunos, Inf))
final_priv[, 50] <- scale(na_if(temp$funcionarios, Inf))
colnames(final_priv)[37:50] <- colnames(temp)[8:21]

#TURMAS----
final_priv[,51] <- porc4(priv$adm1, priv$adm2, priv$adm3, priv$adm4)
final_priv[,52] <- porc4(priv$adm2, priv$adm1, priv$adm3, priv$adm4)
final_priv[,53] <- porc4(priv$adm3, priv$adm2, priv$adm1, priv$adm4)
final_priv[,54] <- porc4(priv$adm4, priv$adm2, priv$adm3, priv$adm1)
final_priv[,55] <- porc(priv$quimica1, priv$quimica0)
final_priv[,56] <- porc(priv$fisica1, priv$fisica0)
final_priv[,57] <- porc(priv$mat1, priv$mat0)
final_priv[,58] <- porc(priv$bio1, priv$bio0)
final_priv[,59] <- porc(priv$port1, priv$port0)
final_priv[,60] <- porc(priv$ing1, priv$ing0)
final_priv[,61] <- porc(priv$esp1, priv$esp0)
final_priv[,62] <- porc(priv$frances1, priv$frances0)
final_priv[,63] <- porc(priv$outra1, priv$outra0)
final_priv[,64] <- porc(priv$indigena1, priv$indigena0)
final_priv[,65] <- porc(priv$artes1, priv$artes0)
final_priv[,66] <- porc(priv$edfisica1, priv$edfisica0)
final_priv[,67] <- porc(priv$hist1, priv$hist0)
final_priv[,68] <- porc(priv$geo1, priv$geo0)
final_priv[,69] <- porc(priv$filosofia1, priv$filosofia0)
final_priv[,70] <- porc(priv$ensreligioso1, priv$ensreligioso0)
final_priv[,71] <- porc(priv$sociologia1, priv$sociologia0)
final_priv[,72] <- porc(priv$inf1, priv$inf0)
final_priv[,73] <- porc(priv$pedag1, priv$pedag0)
final_priv[,74] <- scale(na_if(temp$duracao_turma, Inf))

colnames(final_priv)[51:74] <- c("federal", "estadual", "municipal", "particular", "quimica", "fisica", "mat", "bio", "port", "ing", "esp",
                             "frances", "outralingua", "linguaindigena", "artes", "edfisica", "hist", "geo", "filosofia", "ensreligioso",
                             "socilogia", "inf", "pedag", "duracao_turmas")

#write.csv2(final_priv, "BANCOFINAL--PRIV--V1--14-11-2016.csv")
write.csv2(final_priv, "BANCOFINAL--PUB--V1--14-11-2016.csv")