library(dplyr)
#priv <- read.csv2("[3.a]enem_escolas--Publica--v1--22-11-2016.csv")
priv <- read.csv2("[3.a]enem_escolas--Particular--v1--22-11-2016.csv")

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

final_priv[, 14] <- scale(na_if(priv$NOTA_CN_INDEX, Inf))
final_priv[, 15] <- scale(na_if(priv$NOTA_CH_INDEX, Inf))
final_priv[, 16] <- scale(na_if(priv$NOTA_MT_INDEX, Inf))
final_priv[, 17] <- scale(na_if(priv$NOTA_LC_INDEX, Inf))
final_priv[, 18] <- scale(na_if(priv$NOTA_REDACAO_INDEX, Inf))

colnames(final_priv)[1:18] <- c("municipiosENEM", "adm1", "adm2", "adm3", "adm4", "sexo1", "raca0", "raca1", "raca2", "raca3", "raca4", "raca5", "lingua1", "znotaCN",
                                "znotaCH", "znotaMT", "znotaLC", "znotaredacao")



#TURMAS----
#final_priv[,19] <- porc4(priv$adm1, priv$adm2, priv$adm3, priv$adm4)
#final_priv[,20] <- porc4(priv$adm2, priv$adm1, priv$adm3, priv$adm4)
#final_priv[,21] <- porc4(priv$adm3, priv$adm2, priv$adm1, priv$adm4)
#final_priv[,22] <- porc4(priv$adm4, priv$adm2, priv$adm3, priv$adm1)

final_priv[,19] <- NA
final_priv[,20] <- NA
final_priv[,21] <- NA
final_priv[,22] <- NA
final_priv[,23] <- porc(priv$predio0, priv$predio0)
final_priv[,24] <- porc(priv$labinfo0, priv$labinfo0)
final_priv[,25] <- porc(priv$labciencias0, priv$labciencias0)
final_priv[,26] <- porc(priv$sala_atendimento_esp0, priv$sala_atendimento_esp0)
final_priv[,27] <- porc(priv$quadra_coberta0, priv$quadra_coberta0)
final_priv[,28] <- porc(priv$quadra_descoberta0, priv$quadra_descoberta0)
final_priv[,29] <- porc(priv$biblio1, priv$biblio0)
final_priv[,30] <- porc(priv$sala_leitura1, priv$sala_leitura0)
final_priv[,31] <- porc(priv$pne1, priv$pne0)
final_priv[,32] <- porc(priv$refeitorio1, priv$refeitorio0)
final_priv[,33] <- porc(priv$patio_coberto1, priv$patio_coberto0)
final_priv[,34] <- porc(priv$patio_descoberto1, priv$patio_descoberto0)
final_priv[,35] <- porc(priv$areaverde1, priv$areaverde0)
final_priv[,36] <- porc(priv$internet1, priv$internet0)
final_priv[,37] <- porc(priv$bandalarga1, priv$bandalarga0)
final_priv[,38] <- porc(priv$alimentacao1, priv$alimentacao0)
final_priv[,39] <- porc(priv$aee1, priv$aee0)
final_priv[,40] <- porc(priv$ativcomplementar1, priv$ativcomplementar0)
final_priv[,41] <- porc(priv$materialespnao1, priv$materialespnao0)
final_priv[,42] <- porc(priv$alternancia1, priv$alternancia0)
final_priv[,43] <- scale(na_if(priv$mtv, Inf))
final_priv[,44] <- scale(na_if(priv$tv, Inf))
final_priv[,45] <- scale(na_if(priv$dvd, Inf))
final_priv[,46] <- scale(na_if(priv$retro, Inf))
final_priv[,47] <- scale(na_if(priv$impressora, Inf))
final_priv[,48] <- scale(na_if(priv$som, Inf))
final_priv[,49] <- scale(na_if(priv$multimidia, Inf))
final_priv[,50] <- scale(na_if(priv$comp, Inf))



colnames(final_priv)[19:50] <- c("federal", "estadual", "municipal", "particular", "predio", "labinfo", "labciencias", "sala_atendimento_esp",
                                 "quadra_coberta", "quadra_descoberta", "biblio", "sala_leitura", "pne", "refeitorio", "patio_coberto",
                                 "patio_descoberto", "areaverde", "internet", "bandalarga", "alimentacao", "aee", "ativcomplementar",
                                 "materialespnao", "alternancia", "tv",	"dvd",	"copiadora",	"retro",	"impressora",	"som",
                                 "multimidia",	"comp")

write.csv2(final_priv, "[3.b]ENEM-ESCOLAS--PRIV--V1--15-11-2016.csv")
#write.csv2(final_priv, "[3.b]ENEM-ESCOLAS--PUB--V1--15-11-2016.csv")