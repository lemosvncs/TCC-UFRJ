library(dplyr)
priv <- read.csv2("[3.a]enem_turmas--Publica--v1--15-11-2016.csv")
#priv <- read.csv2("[3.a]enem_turmas--Privada--v1--15-11-2016.csv")
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
final_priv[,19] <- porc4(priv$adm1, priv$adm2, priv$adm3, priv$adm4)
final_priv[,20] <- porc4(priv$adm2, priv$adm1, priv$adm3, priv$adm4)
final_priv[,21] <- porc4(priv$adm3, priv$adm2, priv$adm1, priv$adm4)
final_priv[,22] <- porc4(priv$adm4, priv$adm2, priv$adm3, priv$adm1)
final_priv[,23] <- porc(priv$quimica1, priv$quimica0)
final_priv[,24] <- porc(priv$fisica1, priv$fisica0)
final_priv[,25] <- porc(priv$mat1, priv$mat0)
final_priv[,26] <- porc(priv$bio1, priv$bio0)
final_priv[,27] <- porc(priv$port1, priv$port0)
final_priv[,28] <- porc(priv$ing1, priv$ing0)
final_priv[,29] <- porc(priv$esp1, priv$esp0)
final_priv[,30] <- porc(priv$frances1, priv$frances0)
final_priv[,31] <- porc(priv$outra1, priv$outra0)
final_priv[,32] <- porc(priv$indigena1, priv$indigena0)
final_priv[,33] <- porc(priv$artes1, priv$artes0)
final_priv[,34] <- porc(priv$edfisica1, priv$edfisica0)
final_priv[,35] <- porc(priv$hist1, priv$hist0)
final_priv[,36] <- porc(priv$geo1, priv$geo0)
final_priv[,37] <- porc(priv$filosofia1, priv$filosofia0)
final_priv[,38] <- porc(priv$ensreligioso1, priv$ensreligioso0)
final_priv[,39] <- porc(priv$sociologia1, priv$sociologia0)
final_priv[,40] <- porc(priv$inf1, priv$inf0)
final_priv[,41] <- porc(priv$pedag1, priv$pedag0)
final_priv[,42] <- scale(na_if(priv$duracao_turma, Inf))

colnames(final_priv)[19:42] <- c("federal", "estadual", "municipal", "particular", "quimica", "fisica", "mat", "bio", "port", "ing", "esp",
                             "frances", "outralingua", "linguaindigena", "artes", "edfisica", "hist", "geo", "filosofia", "ensreligioso",
                             "socilogia", "inf", "pedag", "duracao_turmas")

#write.csv2(final_priv, "[3.b]ENEM-TURMAS--PRIV--V1--15-11-2016.csv")
write.csv2(final_priv, "[3.b]ENEM-TURMAS--PUB--V1--15-11-2016.csv")