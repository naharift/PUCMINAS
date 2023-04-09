getwd()
setwd("C://Users//Olnalu//Desktop//Nahari//PUCMINAS//Dados")
list.files()

#################################################
#### TCC PUCMINAS - NAHARI - PCA SIM E SINASC ###
#################################################

## PACOTES  ####
library(ggcorrplot)
library('corrr')
library("FactoMineR")
library("factoextra")
library(wesanderson)

## LEITURA DO ARQUIVO
dataset <- read.csv2("sim_sinasc_tratada.csv", sep = ";")

## CHECAGEM DE VALORES NULOS POR COLUNA
colSums(is.na(dataset))/26738*100

## EXCLUSAO DE COLUNAS COM % ALTO DE NULOS
df_pca = na.omit(dataset[,-c(1,5,6,25)])

## PADRONIZANDO
PCAdf <- scale(df_pca)

## CORRELAÇAO
corr_matrix <- cor(PCAdf)
ggcorrplot(corr_matrix)

## COMPONENTES
data.pca <- princomp(corr_matrix)
summary(data.pca)

## CONTRIBUIÇAO POR VARIAVEL
data.pca$loadings[, 1:2]

### Graficos ####
## CORES
cores = wes_palette("Darjeeling1")

## DIMENSOES E VARIANCIA
fviz_eig(data.pca, addlabels = TRUE,title = "PCA SIM-Sinasc", barfill = cores[5], barcolor = cores[5], linecolor ="red") +
   theme_classic()

## GRAFICO DAS VARIAVEIS
fviz_cos2(data.pca, choice = "var", axes = 1:2)
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("yellow", cores[3], cores[1]),
             repel = TRUE, title = "Variáveis - PCA")

## GRAFICO DOS INDIVIDUOS
fviz_pca_ind(data.pca,  col.ind="contrib",  
             repel = TRUE, title = "Indivíduos - PCA") +
  scale_color_gradient2(low="yellow", mid=cores[3],
                        high=cores[2], midpoint=4)
             

## Seleçao de colunas
df <- na.omit(dataset[,c(1,36,17,20,22,10,18,30,31,23, 32, 4, 21, 34,24,19)])
#QTDGESTANT, QTDFILVIVO, GESTACAO,PESO,QTDPARTNOR, 
#TPROBSON,PARIDADE, CONSULTAS,KOTELCHUCK,IDADEMAE,
#QTDFILMORT,IDADE,CONSPRENATA,QTDPARCES

## Exportacao do arquivo
write.csv2(df, "base_final.csv", row.names = F)


wes_palette("Darjeeling1")
