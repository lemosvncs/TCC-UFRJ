require(fastmatch)
install.packages("fastmatch")
setwd("~/TCC/Dados/Censo Escolar Educacao Basica 2014/DADOS")
turmas14 <- read.table("TURMAS.csv", sep="|", header = T)


#Selecting Cases----
#variavel FK_COD_MOD_ENSINO
#Vou selecionar somente os casos de educacao em modalidade regular,
#excluind os casos de educacao especial EJA
table(turmas14$FK_COD_MOD_ENSINO) #Redundancia, Regular = 2.021.644
turmas14 <- subset(turmas14, FK_COD_MOD_ENSINO==1) #Casos: 2.021.644

#Deixando somente os casos de Ensino Medio
#Refere-se as turmas de Ensino Medio, as turmas de formacao profissional concomitante e mista (concomitante e subsequente)
#Onde isto:
table(turmas14$FK_COD_ETAPA_ENSINO >= 25 & turmas14$FK_COD_ETAPA_ENSINO <= 39)
table(turmas14$FK_COD_ETAPA_ENSINO == 64)
#E igual a isto
table(turmas14$FK_COD_ETAPA_ENSINO >= 25 & turmas14$FK_COD_ETAPA_ENSINO <= 39| turmas14$FK_COD_ETAPA_ENSINO ==  64)

turmas14 <- subset(turmas14, FK_COD_ETAPA_ENSINO >= 25 & FK_COD_ETAPA_ENSINO <= 39 | FK_COD_ETAPA_ENSINO == 64)

table(turmas14$FK_COD_TIPO_TURMA)#Tipos de turmas (294000 nao se aplicam, as outras sao unidades hospitalares, socioeducativas)
table(turmas14$ID_MAIS_EDUCACAO) #55.776 turmas participam do programa Mais Educacao

turmas14 <- read.csv2("turmas14-041016.csv")
#Selecao de variaveis----
colnames(turmas14)
tc <- c("NU_DURACAO_TURMA", "NUM_MATRICULAS", "ID_MAIS_EDUCACAO", "ID_QUIMICA", "ID_FISICA", "ID_MATEMATICA",
       "ID_BIOLOGIA", "ID_LINGUA_LITERAT_PORTUGUESA", "ID_LINGUA_LITERAT_INGLES", "ID_LINGUA_LITERAT_ESPANHOL",
       "ID_LINGUA_LITERAT_FRANCES", "ID_LINGUA_LITERAT_OUTRA", "ID_LINGUA_LITERAT_INDIGENA", "ID_ARTES",                      
       "ID_EDUCACAO_FISICA", "ID_HISTORIA", "ID_GEOGRAFIA", "ID_FILOSOFIA",                  
       "ID_ENSINO_RELIGIOSO", "ID_SOCIOLOGIA", "ID_INFORMATICA_COMPUTACAO", "ID_DISCIPLINAS_PEDAG", "COD_MUNICIPIO")


turmas14s <- data.frame(matrix(ncol = 23, nrow = 294907))
colnames(turmas14s) <- tc

turmas14s[,1:2] <- turmas14[,6:7]
turmas14s[, 3] <- turmas14[, 12]
turmas14s[, 4:7] <- turmas14[, 37:40]
turmas14s[, 8:19] <- turmas14[, 42:53]
turmas14s[, 20:21] <- turmas14[, 55:56]
turmas14s[, 22] <- turmas14[, 61]
turmas14s[, 23] <- turmas14$FK_COD_MUNICIPIO




turmas14$PK_COD_TURMA <- NULL
turmas14$PK_COD_ENTIDADE <- NULL
turmas14$FK_COD_DISTRITO <- NULL
which(colnames(turmas14=="ID_LOCALIZACAO"))
str(turmas14)
fmatch("ID_LOCALIZACAO",names(turmas14))
fmatch("ID_EDUCACAO_INDIGENA",names(turmas14))
turmas14[,64:76] <- NULL

#SAVE----
write.csv2(turmas14s, "turmas14-051016.csv")
write.csv2(turmas14, "turmas14-todasasvar-06-10-2016.csv")
write.csv2(turmas14, "turmas14--09-10-2016.csv")
