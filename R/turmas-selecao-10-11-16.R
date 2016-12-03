setwd("C:/Users/Vini/Documents/TCC/Dados/Censo Escolar Educação Básica 2014/DADOS")
turmas14 <- read.table("TURMAS.csv", sep="|", header = T)

turmas14 <- subset(turmas14, FK_COD_MOD_ENSINO==1)

turmas14 <- subset(turmas14, FK_COD_ETAPA_ENSINO >= 25 & FK_COD_ETAPA_ENSINO <= 39 | FK_COD_ETAPA_ENSINO == 64)

tc <- c("FK_COD_MUNICIPIO", "ID_DEPENDENCIA_ADM", "NU_DURACAO_TURMA", "NUM_MATRICULAS", "ID_QUIMICA", "ID_FISICA", "ID_MATEMATICA",
        "ID_BIOLOGIA", "ID_LINGUA_LITERAT_PORTUGUESA", "ID_LINGUA_LITERAT_INGLES", "ID_LINGUA_LITERAT_ESPANHOL",
        "ID_LINGUA_LITERAT_FRANCES", "ID_LINGUA_LITERAT_OUTRA", "ID_LINGUA_LITERAT_INDIGENA", "ID_ARTES",                      
        "ID_EDUCACAO_FISICA", "ID_HISTORIA", "ID_GEOGRAFIA", "ID_FILOSOFIA",                  
        "ID_ENSINO_RELIGIOSO", "ID_SOCIOLOGIA", "ID_INFORMATICA_COMPUTACAO", "ID_DISCIPLINAS_PEDAG")
turmas14 <- turmas14[tc]

setwd("C:/Users/Vini/Documents/TCC/Working/Nov2016")
write.csv2(turmas14, "turmas14--limpo--v3--10-11-2016.csv")

turmas14Federal <- subset(turmas14, ID_DEPENDENCIA_ADM==1)
turmas14Estadual <- subset(turmas14, ID_DEPENDENCIA_ADM==2)
turmas14Municipal <- subset(turmas14, ID_DEPENDENCIA_ADM==3)
turmas14Particular <- subset(turmas14, ID_DEPENDENCIA_ADM==4)
turmas14Publica <- subset(turmas14, ID_DEPENDENCIA_ADM!=4)
write.csv2(turmas14Federal, "turmas14Federal--v2--10-11-2016.csv")
write.csv2(turmas14Estadual, "turmas14Estadual--v2--10-11-2016.csv")
write.csv2(turmas14Municipal, "turmas14Municipal--10--08-11-2016.csv")
write.csv2(turmas14Particular, "turmas14Particular--v2--10-11-2016.csv")
write.csv2(turmas14Publica, "turmas14Publica--v2--10-11-2016.csv")