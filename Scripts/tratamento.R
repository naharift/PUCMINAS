getwd()
setwd("C://Users//Olnalu//Desktop//Nahari//PUCMINAS//Dados")
list.files()

########################################################
#### TCC PUCMINAS - NAHARI - TRATAMENTO SIM E SINASC ###
########################################################

dataset <- read.csv2("dados_sim_sinasc_2019.csv", sep = ";")
colnames(dataset)
# Considerar que .x é SIM e .y é SINASC

#### Retirar colunas ####
excluir <- c("NOMEMAE.x", "DTNASC", "SEXO.y", "GRAVIDEZ.y", "IDADEMAE.y", "CODINST.x",  "CODESTAB.x",
             "LINHAA", "LINHAB", "LINHAC", "LINHAD", "LINHAII", "ESCMAE.y", "ESCMAE2010", "CODINST.y", "CODANOMAL",
             "CODESTAB.y", "CODMUNNASC", "CODMUNOCOR",  "CODMUNRES", "CAUSABAS_O", "DTOBITO")
dados <- dataset[,!(names(dataset)%in% excluir)]


#### Retirar numero de DN invalido ####
dados$ncharDN <- nchar(dados$NUMERODN)
dados2 <- dados[!(dados$ncharDN != 8 | is.na(dados$ncharDN) | dados$NUMERODN < 0),]

## Utilizar DO como ID
## Retirar NUMERODN e ncharDN
df <- dados2[,!names(dados2) %in% c("NUMERODN","ncharDN")]

#### Causa basica - discretizar ####
df$ncharcausa <- substr(df$CAUSABAS,1,1)
table(df$ncharcausa)
df$ncausa <- ifelse(df$ncharcausa == "A", 1,
                    ifelse(df$ncharcausa == "B", 2,
                    ifelse(df$ncharcausa == "C", 3,
                    ifelse(df$ncharcausa == "D", 4,
                    ifelse(df$ncharcausa == "E", 5,
                    ifelse(df$ncharcausa == "F", 6,
                    ifelse(df$ncharcausa == "G", 7,
                    ifelse(df$ncharcausa == "H", 8,
                    ifelse(df$ncharcausa == "I", 9,
                    ifelse(df$ncharcausa == "J", 10,
                    ifelse(df$ncharcausa == "K", 11,
                    ifelse(df$ncharcausa == "L", 12,
                    ifelse(df$ncharcausa == "M", 13,
                    ifelse(df$ncharcausa == "N", 14,
                    ifelse(df$ncharcausa == "O", 15,
                    ifelse(df$ncharcausa == "P", 16,
                    ifelse(df$ncharcausa == "Q", 17,
                    ifelse(df$ncharcausa == "R", 18,
                    ifelse(df$ncharcausa == "S", 19,
                    ifelse(df$ncharcausa == "T", 20,
                    ifelse(df$ncharcausa == "U", 21,
                    ifelse(df$ncharcausa == "V", 22,
                    ifelse(df$ncharcausa == "W", 23,
                    ifelse(df$ncharcausa == "X", 24,
                    ifelse(df$ncharcausa == "Y", 25,
                    ifelse(df$ncharcausa == "Z", 26,
                    999)
                    )))))))))))))))))))))))))


### Discretizar TPPOS ####
table(df$TPPOS)
df$TPPOS <- ifelse(df$TPPOS  == "S",1,0)


### Organizar idade ####
df$grupo_idade <- ""
df$grupo_idade[which(df$IDADE <=227)] <- "Neonatal Tardia"
df$grupo_idade[which(df$IDADE <=206)] <- "Neonatal Precoce"
df$grupo_idade[which(df$IDADE >= 228)] <- "Pós-neonatal"

df$faixaetaria <- ""
df$faixaetaria[which(df$grupo_idade == "Neonatal Tardia")] <- 1
df$faixaetaria[which(df$grupo_idade == "Neonatal Precoce")] <- 0
df$faixaetaria[which(df$grupo_idade == "Pós-neonatal")] <- 2

table(df$faixaetaria)

### Separar o DF entre racas indigenas e nao-indigenas ####
df$corraca <- ifelse(is.na(df$RACACOR.x), df$RACACOR.y, df$RACACOR.x) #se a raca obito ficou na, entao considerar a de nascimento
df$rc_ind <- "" #1 indigena em branco, excluido (tratar antes se nulo)
df$rc_ind[which(df$corraca == 5)] <- 1
df$rc_ind[which(df$corraca != 5 | is.na(df$corraca))] <- 0

table(df$rc_ind) #26300 e 438
df1 <- df[,!names(df) %in% c("CAUSABAS", "IDADE", "grupo_idade","RACACOR.x", "RACACOR.y", "ncharcausa")]

### EXPORTANDO DF ####
colnames(df1) <- c("SEXO", "GRAVIDEZ", "IDADEMAE", "NUMERODO", "TPPOS", "FONTEINV",  "LOCOCOR",   
                   "ATESTANTE","ESCMAE",  "PESO", "APGAR1", "APGAR5", "IDANOMAL",  "LOCNASC", 
                   "ESTCIVMAE", "RACACORMAE", "QTDGESTANT",  "QTDPARTNOR",  "QTDPARTCES",  "QTDFILVIVO", 
                   "QTDFILMORT",  "GESTACAO",  "CONSULTAS", "CONSPRENAT", "MESPRENAT", "TPAPRESENT",
                   "STTRABPART",  "PARTO", "TPNASCASSI", "TPROBSON",  "PARIDADE",  "KOTELCHUCK", "CAUSABAS", 
                   "IDADE", "RACACOR",   "IND_RC")
library(dplyr)
df_ordenado <- df1 %>% select(NUMERODO, everything())
base_tratada <- write.csv2(df_ordenado, "sim_sinasc_tratada.csv", row.names = F)












