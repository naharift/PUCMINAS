gc()
getwd()
setwd("C://Users//Olnalu//Desktop//Nahari//PUCMINAS//Dados")

list.files()
#################################################
#### TCC PUCMINAS - NAHARI - EDA SIM E SINASC ###
#################################################

### PACOTES ####
library(ggcorrplot)
library('corrr')
library("ggplot2")
library(wesanderson)
library(corrplot)
require(reshape)
library(tidyr)
require(gridExtra)

## LEITURA
dataset <- read.csv2("base_final.csv", sep = ";")

###  ANALISE UNIVARIADA ####
### IND RC ###
prop.table(table(dataset$IND_RC))

### QTD GESTACOES ###
barplot(table(dataset$QTDGESTANT))
boxplot(dataset$QTDGESTANT[which(dataset$QTDGESTANT<40)]~dataset$IND_RC[which(dataset$QTDGESTANT<40)],
        xlab = "Cor;raca", ylab = "Qtd. de Gestacoes", main = "Gestacoes segundo cor;raca")
#incluir outra aba por idade
#organizar os rotulos


### IDADE MAE ###
boxplot(dataset$IDADEMAE[-which(dataset$IDADEMAE > 60)]) #retirando ignorado(99)

### QTD FILHO VIVO ###
barplot(table(dataset$QTDFILVIVO))

### QTD FILHO MORTO ###
barplot(table(dataset$QTDFILMORT))

### GESTACAO ###
barplot(table(dataset$GESTACAO))

### PESO ###
boxplot(dataset$PESO)
summary(dataset$PESO)

### QTD PARTO NORMAL ###
boxplot(dataset$QTDPARTNOR)

### QTD PARTO CESAREA ###
boxplot(dataset$QTDPARTCES)

### CONSULTAS ###
barplot(table(dataset$CONSULTAS))

### CONSPRENAT ###
boxplot(dataset$CONSPRENAT[-which(dataset$CONSPRENAT >50)])

### IDADE ###
barplot(table(dataset$IDADE))

### IDADEMAE ###
boxplot(dataset$IDADEMAE[-which(dataset$IDADEMAE> 65)])

### KOTELCHUCK - PRENATAL ADEQUADO ###
# "KOTELCHUCK" pre natal adequado 1 N?o fez pr?-natal (Campo33=0);
# 2 Inadequado (Campo34>3 ou Campo34<=3 e Campo33<3);
# 3 Intermediario (Campo34<=3 e Campo33 entre 3 e 5);
# 4 Adequado (Campo34<=3 e Campo33=6);
# 5 Mais que adequado (Campo34<=3 e Campo33>=7);
# 6 Nao Classificados (campos 33 ou 34, Nulo ou Ign
barplot(table(dataset$KOTELCHUCK))

### PARIDADE ###
barplot(table(dataset$PARIDADE))

### TPROBSON ###
barplot(table(dataset$TPROBSON))

###  ANALISE BIVARIADA ####

## CORES
pal <- wes_palette("Darjeeling1", 13, type = "continuous")
paleta <- wes_palette("Darjeeling1", 5, type = "discrete")

## IDADE E COR E RACA ##
idade <- cast(dataset, IDADE+ IND_RC~., fun.aggregate = length, value = "NUMERODO")
colnames(idade) <- c('IDADE','IND_RC', 'Valor')

## Proporcao
idade$n = tapply(idade$Valor, idade$IND_RC, FUN=sum)
idade$pct = round((idade$Valor/idade$n), 3)

## Classificacao dos grupos etarios
idade$IDADE[which(idade$IDADE == 0)] <- 'Neonatal precoce'
idade$IDADE[which(idade$IDADE == 1)] <- 'Neonatal tardio'
idade$IDADE[which(idade$IDADE == 2)] <- 'Pós neonatal'

## Grafico Idade vs. raca/cor
ggplot(idade, aes(x=IDADE, y=pct, fill=IDADE, label = scales::percent(pct)))+
  geom_bar(stat='identity')+ 
  scale_fill_manual(values=paleta)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  ggtitle("Mortalidade infantil segundo raça/cor por grupo etário")+
  geom_text(position = position_dodge(width = .9),    
            vjust = -0.4,    
            size = 3) + 
  xlab("")+ ylab("Óbitos infantis (%)") + 
  facet_grid(.~IND_RC)+ theme(legend.position = "bottom", legend.title=element_blank(), 
                                                                   axis.text.x=element_blank(),
                                                                   axis.text.y=element_blank(),
                                                                   axis.ticks.x=element_blank())

## TIPO DE PARTO POR RACA/COR ##

cesarea <- cast(dataset,IND_RC~.,fun.aggregate = sum, value = "QTDPARTCES")
colnames(cesarea) <- c('IND_RC', 'Valor')
cesarea$Tipo = "Parto Cesárea"

pn <- cast(dataset,IND_RC~.,fun.aggregate = sum, value = "QTDPARTNOR")
colnames(pn) <- c('IND_RC', 'Valor')
pn$Tipo = "Parto Vaginal"

## Unindo as infos
partos <- rbind(cesarea,pn)

## Calculando a proporcao
partos$n = tapply(partos$Valor, partos$IND_RC, FUN=sum)
partos$pct = round((partos$Valor/partos$n), 3)

## Grafico Tipo de parto vs. raca/cor
ggplot(partos, aes(x=Tipo, y=pct, fill=Tipo, label = scales::percent(pct)))+
  geom_bar(stat='identity')+ 
  scale_fill_manual(values=paleta)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  ggtitle("Tipo de parto segundo raça/cor")+
  geom_text(position = position_dodge(width = .9),    
            vjust = -0.4,    
            size = 3) + 
  xlab("")+ ylab("Óbitos infantis (%)") + 
  facet_grid(.~IND_RC)+ theme(legend.position = "bottom", legend.title=element_blank(), 
                              axis.text.x=element_blank(),
                              axis.text.y=element_blank(),
                              axis.ticks.x=element_blank())


## QTD DE CONSULTAS PRE NATAL POR RACA/COR ##
consultas <- dataset[which(dataset$CONSPRENAT < 40),] ##99 eh ignorado

ggplot(consultas, aes(x=IND_RC, y=log(CONSPRENAT), fill = IND_RC))+
  geom_boxplot()+ 
  scale_fill_manual(values=paleta)+
  #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  ggtitle("Quantidade de Consultas de Pré-natal segundo raça/cor")+
  xlab("")+ ylab("Quantidade em escala log")+
  stat_summary(fun.y=mean, geom="point", shape=42, size=5, col = paleta[3])+
  theme(legend.position = "none")#, legend.title=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())


## INDICE DE KOTELCHUCK POR RACA/COR ##
kotel <- cast(dataset, KOTELCHUCK+ IND_RC~., fun.aggregate = length, value = "NUMERODO")
colnames(kotel) <- c('Kotel','IND_RC', 'Valor')

## Classificacao
kotel$Kotel[which(kotel$Kotel == 9)] <- 'Não classificado'
kotel$Kotel[which(kotel$Kotel == 1)] <- 'Sem pré-natal'
kotel$Kotel[which(kotel$Kotel == 2)] <- 'Inadequado'
kotel$Kotel[which(kotel$Kotel == 3)] <- 'Intermediário'
kotel$Kotel[which(kotel$Kotel == 4)] <- 'Adequado'
kotel$Kotel[which(kotel$Kotel == 5)] <- 'Mais que adequado'

## Proporcao
kotel$n = tapply(kotel$Valor, kotel$IND_RC, FUN=sum)
kotel$pct = round((kotel$Valor/kotel$n), 3)

## Grafico Indice vs raca/cor
ggplot(kotel, aes(x=Kotel, y=pct, fill=Kotel, label = scales::percent(pct)))+
  geom_bar(stat='identity')+ 
  scale_fill_manual(values=c(rev(pal)[1],pal[1],rev(pal)[2:4], pal[4]))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  ggtitle("Assistência de pré-natal (APNCU) por raça/cor")+
  scale_y_continuous(labels = scales::percent)+
  xlab("")+ ylab("") + 
  geom_text(position = position_dodge(width = .9),    
            vjust = -0.3,    
            size = 3) + 
  facet_grid(.~IND_RC)+ 
  theme(legend.position = "bottom", legend.title=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.x=element_blank())


## PESO POR RACA/COR ##
ggplot(dataset, aes(x=as.factor(as.character(IND_RC)), y=PESO, fill = as.factor(as.character(IND_RC))))+
  geom_boxplot()+ 
  scale_fill_manual(values=paleta)+
  #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  ggtitle("Peso, em gramas, segundo raça/cor")+
  xlab("")+ ylab("Peso recém-nascido (g)")+
  stat_summary(fun.y=mean, geom="point", shape=42, size=5, col = cores[3])+
  theme(legend.position = "none")



## TPROBSON POR RACA COR ##
tprobson <- cast(dataset, TPROBSON+ IND_RC~., fun.aggregate = length, value = "NUMERODO")
colnames(tprobson) <- c('Grupo','IND_RC', 'Valor')

## Proporcao
tprobson[nrow(tprobson)+1,] <- tprobson[nrow(tprobson),]
tprobson[nrow(tprobson)-1,] <- c(11, "Indígena", 1)

## Organizacao da ordem
tprobson$n = rep(tapply(as.numeric(tprobson$Valor), tprobson$IND_RC, FUN=sum), 11)
tprobson$pct = round((as.numeric(tprobson$Valor)/as.numeric(tprobson$n)), 2)

## Grupos como fator
tprobson$Grupo <- factor(tprobson$Grupo, levels=c("1", "2", "3", "4", "5", "6", "7", "8","9", "10", "11"))

## Grafico
ggplot(tprobson, aes(x=Grupo, y=as.numeric(pct), fill=Grupo, label = scales::percent(pct)))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c(pal[1:7], rev(pal[10:13])))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  ggtitle("Grupo de Robson por raça/cor")+
  scale_y_continuous(labels = scales::percent)+
  xlab("")+ ylab("(%)") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.2,    
            size = 3) + 
  facet_grid(.~IND_RC)+ theme(legend.position = "bottom", 
                              axis.text.x=element_blank(), axis.text.y=element_blank(),
                              axis.ticks.x=element_blank())

### VAR NUMERICAS E CORRELACAO ####
numericas <- dataset[,c(3:7,10,12:14,16)]

## Correlacao
res <- cor(numericas)

## Grafico
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

### TESTE DE HIPOTESES ####
##H0: Independência entre as Raca/cor e grupo etario na mortalidade infantil
##H1: Há dependencia entre as variaveis

raca_idade = cast(dataset, IDADE~IND_RC, fun.aggregate = length, value = "NUMERODO")
chisq <- chisq.test(raca_idade)
chisq

## Classificacao dos grupos etarios
raca_idade$IDADE[which(raca_idade$IDADE == 0)] <- 'Neonatal precoce'
raca_idade$IDADE[which(raca_idade$IDADE == 1)] <- 'Neonatal tardio'
raca_idade$IDADE[which(raca_idade$IDADE == 2)] <- 'Pós neonatal'

raca_idade$Tipo = "Freq Observada"
## As frequências esperadas estimadas sob H0 são
esp = as.data.frame(chisq$expected)
esp = round(esp,0)

esp$IDADE = raca_idade$IDADE
esp$Tipo = "Freq Esperada"

idade_raca = rbind(esp,raca_idade)
idade_raca <- gather(idade_raca, key="IND_RC", value="Valor", 1:2)

idade_raca_ind <- idade_raca[which(idade_raca$IND_RC == "Indígena"),]
idade_raca_Nind <- idade_raca[which(idade_raca$IND_RC != "Indígena"),]

## Grafico
ind = ggplot(idade_raca_ind, aes(x=Tipo, y=(Valor), fill=IDADE, label = Valor))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=paleta)+
  #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #ggtitle("Mortalidade infantil observada e esperada por raça/cor")+
  xlab("")+ ylab("Óbitos Infantis") + 
  geom_text(position = position_stack(vjust = 0.5),
            vjust = -0.2,    
            size = 3) + 
  facet_grid(.~IND_RC)+ theme(legend.position = "none", 
                              axis.text.y=element_blank(),
                              axis.ticks.x=element_blank())

Nind = ggplot(idade_raca_Nind, aes(x=Tipo, y=(Valor), fill=IDADE, label = Valor))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=paleta)+
  #scale_x_discrete(guide = guide_axis(n.dodge=2))+
  #ggtitle("Mortalidade infantil observada e esperada por raça/cor")+
  xlab("")+ ylab("") + 
  geom_text(position = position_stack(vjust = 0.5),
            vjust = -0.2,    
            size = 3) + 
  facet_grid(.~IND_RC)+ theme(legend.position = "none", 
                              axis.text.y=element_blank(),
                              axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

grid.arrange(ind, Nind, ncol=2)

