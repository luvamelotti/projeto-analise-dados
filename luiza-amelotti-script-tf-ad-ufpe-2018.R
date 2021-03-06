# Ativando os pacotes exigidos:
if(require(rio) == F) install.packages('rio', dependencies = TRUE); require(rio)
if(require(car) == F) install.packages('car', dependencies = TRUE); require(rio)
if(require(haven) == F) install.packages('haven', dependencies = TRUE); require(haven)
if(require(MASS) == F) install.packages('MASS', dependencies = TRUE); require(MASS)
if(require(pander) == F) install.packages('pander', dependencies = TRUE); require(pander)
if(require(tidyverse) == F) install.packages('tidyverse', dependencies = TRUE); require(tidyverse)
if(require(ggplot2) == F) install.packages('ggplot2', dependencies = TRUE); require(ggplot2)
if(require(dplyr) == F) install.packages('dplyr', dependencies = TRUE); require(dplyr)
if(require(readxl) == F) install.packages('readxl', dependencies = TRUE); require(readxl)
if(require(stats) == F) install.packages('stats', dependencies = TRUE); require(stats)
if(require(lmtest) == F) install.packages('lmtest', dependencies = TRUE); require(lmtest)
if(require(ggpubr) == F) install.packages('ggpubr', dependencies = TRUE); require(ggpubr)
if(require(coefplot) == F) install.packages('coefplot', dependencies = TRUE); require(coefplot)

# AN�LISE INICAL DOS DADOS ####

# Abrindo o banco de dados
luiza_amelotti_bd_tf_ad_ufpe_2018 <- read_excel("C:/Users/Luh/Google Drive/AULAS MESTRADO/2018_2 davi/luiza-amelotti-bd-tf-ad-ufpe-2018.xlsx")
View(luiza_amelotti_bd_tf_ad_ufpe_2018)

# Atribuindo novo nome a base de dados
DATABASE_CORRUPCAO <- luiza_amelotti_bd_tf_ad_ufpe_2018

# Omitindo as c�lulas vazias da base
na.omit(DATABASE_CORRUPCAO)

# Organizando a VD (�ndice de corrup��o - vdem_corr):
Y <- DATABASE_CORRUPCAO$vdem_corr

# Organizando as VIs:
X1 <- (DATABASE_CORRUPCAO$wdi_wip)  # participa��o de mulheres no parlamento
X2 <- DATABASE_CORRUPCAO$wbgi_rle # estado de direito
X3 <- DATABASE_CORRUPCAO$icrg_qog # qualidade do governo

# Analisando a estrutura das vari�veis:
str(DATABASE_CORRUPCAO)
summary(Y)

# observando as estat�sticas descritivas das var�aveis
DATABASE_CORRUPCAO$ht_region <- as.factor(DATABASE_CORRUPCAO$ht_region) 
pander(summary(DATABASE_CORRUPCAO, header = T))

# Rodando a o teste da regress�o
reg <- lm(Y ~ X1 + X2 + X3)
reg
summary(reg)

# Plotando os resultados da regress�o
plot(reg)

# resultado da regress�o
coefplot(reg)

# criando tabela com dados do summary que n�o aparecem no gr�fico
regressao <- data.frame(Observations = c(194),
                        Residual_Std._Error = c(0.1294),
                        "Deleted observations" = c(63),
                        R2 = c(0,8028),
                        Adjusted_R2 = c(0,7981),
                        p_value = c("2.2e-16"))
pander(regressao, caption = "Regression Results")


# Analisando se h� outliers na amostra:
outlierTest(reg) ##INDICA QUE O CASO 85 � OUTLIER

# plotando o resultado do teste de outlier:
qqPlot(reg, main="QQ Plot") #qq plot for studentized resid
##MOSTRA QUE TEM DOIS POSS�VEIS OUTLIERS
## QQPLOT: INDICA��O DE BOM AJUSTE DOS RES�DUOS
leveragePlots(reg) #leverage plots 

# checando o pressuposto da n�o-normalidade:
dist_residuo <- studres(reg) # distribui��o do res�duo
dist_residuo

# histograma dos res�udos:
hist(dist_residuo, freq=FALSE,
     main="Distribui��o dos Res�duos")
xfit <- seq(min(dist_residuo),max(dist_residuo),length=40)
yfit <- dnorm(xfit)
lines(xfit, yfit) 
# EVIDENCIA A NORMALIDADE DOS ERROS 

# checando pressuposto da homocedasticidade:
bptest(reg, varformula = NULL, studentize = TRUE, data = DATABASE_CORRUPCAO()) #p-valor > 0,05 indica heterocedasticidade

#plotando studentized residuals vs. fitted values:
spreadLevelPlot(reg)  

# checando o pressuposto da multicolinearidade:
vif(reg) # varia��o da infla��o dos fatores
sqrt(vif(reg)) > 2 
pander(sqrt(vif(reg)) > 2) # transformando em tabela

# checando o pressuposto da n�o-linearidade:
crPlots(reg)
ceresPlots(reg)

# checando o pressuposto de independencia dos erros:
durbinWatsonTest(reg) # teste para erros autocorrelacionados
# o p-valor < 0,05 indica que a H0 � rejeitada, ou seja, h� correla��o entre os erros

# Fazendo a transforma��o da vari�vel dependente para respeitar o pressuposto da homocedasticidade
# Transforma��o Box-Cox para observar o valor de l�mbida que maximiza a fun��o
trans_var <- boxCox(reg, lam=seq(-1, 1, 1/10), plotit = TRUE)
trans_var
# A fun��o � maximizada � um l�mbda de 0.5

#transformando a vari�vel:
Y_transf <- (DATABASE_CORRUPCAO$vdem_corr^(0.5)-1)/0.5
#reanalisando a regress�o e os pressupostos:
reg2 <- lm(Y_transf ~ X1 + X2 + X3)
reg2
summary(reg2)
# gerando os novos gr�ficos para an�lise da nova regress�o
plot(reg2)

# resulatdo da regress�o
coefplot(reg2)

#Adicionando uma nova vari�vel ao modelo para tentar solucionar a n�o independencia dos erros:
#Necess�rio baixar a base de dados 2
# abrindo os dados da nova var�vel
luiza_amelotti_bd2_tf_ad_ufpe_2018 <- read_excel("C:/Users/Luh/Google Drive/AULAS MESTRADO/2018_2 davi/database/luiza-amelotti-bd2-tf-ad-ufpe-2018.xlsx")
View(luiza_amelotti_bd2_tf_ad_ufpe_2018)

# atribuindo novo nome a base de dados 2 - complementar
undp_hdi <- luiza_amelotti_bd2_tf_ad_ufpe_2018

#agregando a nova vari�vel � base de dados original:
DATABASE_CORRUPCAO2 <- cbind.data.frame(DATABASE_CORRUPCAO, Y_transf, undp_hdi)
View(DATABASE_CORRUPCAO2)
na.omit(DATABASE_CORRUPCAO2)
X4 <- DATABASE_CORRUPCAO2$undp_hdi

#regress�o final com nova VI (X4), exclus�o da vari�vel wbgi_rle (X2) para tentar solucionar a multicolearidade e VD transformada: 
reg_final <- lm(Y_transf ~ X1 + X3 + X4)
reg_final
summary(reg_final)

regressao_final <- data.frame(Observations = c(194),
                        "Deleted observations" = c(65),
                        Residual_Std._Error = c("0,2376"),
                        R2 = c("0,7842"),
                        Adjusted_R2 = c("0,779"),
                        p_value = c("2,2e-16"))
pander(regressao_final, caption = "Final Regression Results")

#plotando os resultados da regress�o
coefplot(reg_final)
plot(reg_final)

#retestando o pressuposto da homocedasticidade:
bptest(reg_final, varformula = NULL, studentize = TRUE, data = DATABASE_CORRUPCAO2())

#retestando o pressuposto da multicolinearidade:
vif(reg_final) # varia��o da infla��o dos fatores
sqrt(vif(reg_final)) > 2 

#retestando o pressuposto da independencia dos erros:
durbinWatsonTest(reg_final)
# O p-valor > 0,05 n�o rejeita a H0, ou seja, confirma a independencia dos erros.

# AN�LISE EXPLORAT�RIA DOS DADOS ####

# observando as estat�sticas descritivas da var�avel (X1) porcentagem de mulheres
pander(summary(DATABASE_CORRUPCAO, header = TRUE))



# Explorando a vari�vel X1 (Participa��o Feminina no Parlamento)

library(haven)
library(ggridges)
library(ggplot2)

# criando gr�fico de densidade para observar, em cada regi�o, qual a concentra��o de part. feminina

DATABASE_CORRUPCAO$ht_region <- (DATABASE_CORRUPCAO$ht_region = factor(DATABASE_CORRUPCAO$ht_region, 
                                                                   labels = c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                                                                              "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                                                                              "South Asia", "The Pacific", "The Caribbean")))

mulheres <- ggplot(DATABASE_CORRUPCAO, aes(x = wdi_wip, y = DATABASE_CORRUPCAO$ht_region , fill = DATABASE_CORRUPCAO$ht_region )) +
  geom_density_ridges(alpha=0.3, bandwidth=2) +
  theme_light() +
  theme(axis.text.x=element_text(hjust = 0)) +
  theme(legend.position = "none") +
  labs(title="Mulheres no Parlamento por Regi�es do Mundo", 
       x="% Mulheres no Parlamento", y="",
       fill = "",
       caption = "Elabora��o Pr�pria com base nos dados do QoG")



# criando um gr�fico de barra de participa��o feminina por regi�o do mundo:
theme_set(theme_bw())
DATABASE_CORRUPCAO$ht_region <- paste(as.character(DATABASE_CORRUPCAO$ht_region))
mulheres2 <- ggplot(data=DATABASE_CORRUPCAO,aes(x=ht_region, y=wdi_wip)) + 
  geom_bar(stat = "identity", aes(fill=factor(ht_region), alpha = 0.9)) +
  scale_fill_hue(c = 40) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  scale_x_discrete(labels=c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                            "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                            "South Asia", "The Pacific", "The Caribbean")) +
  theme(legend.position="none") +
  labs(title="Gr�fico 1 - Mulheres parlamentares por Regi�o", caption = "Elabroa��o Pr�pria com base nos dados do QoG",
       x="",
       y="M�dia mulheres no Parlam.")
mulheres2

# gr�fico plot porcentagem mulher x indice corrup
basic <- ggplot(DATABASE_CORRUPCAO, aes(x=vdem_corr, y=wdi_wip)) + geom_point()
basic

basic+
  ggtitle("�NDICE DE CORRUP��O x % MULHERES NO PARLAMENTO") + 
  xlab("�ndice de corrup��o") +
  ylab("% mulheres no Parlamento") + 
  theme(axis.title = element_text( angle = 45, color="black", size=10, face=2)) +
  theme(axis.title.x = element_text(angle = 0, color = "black", size = 10, face = 2)) +
  theme(axis.text = element_text( angle = 90, color="black", size=8, face=2))

# criando gr�fico de �ndice de qualidade do governo x regi�o
DATABASE_CORRUPCAO$ht_region <- (DATABASE_CORRUPCAO$ht_region = factor(DATABASE_CORRUPCAO$ht_region, 
                                                                       labels = c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                                                                                  "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                                                                                  "South Asia", "The Pacific", "The Caribbean")))

qog <- ggplot(DATABASE_CORRUPCAO, aes(x = DATABASE_CORRUPCAO$icrg_qog, y = DATABASE_CORRUPCAO$ht_region , 
                                      fill = DATABASE_CORRUPCAO$ht_region )) +
  geom_density_ridges(alpha=0.3, bandwidth=0.1) +
  theme_light() +
  theme(axis.text.x=element_text(hjust = 0)) +
  theme(legend.position = "none") +
  labs(title="Qualidade do Governo por Regi�es do Mundo", 
       x="�ndice qualidade do governo", y="",
       fill = "",
       caption = "Elabora��o Pr�pria com base nos dados do QoG")
qog


#criando gr�fico de barra para qualidade do governo por regi�o
theme_set(theme_bw())
DATABASE_CORRUPCAO$ht_region <- paste(as.character(DATABASE_CORRUPCAO$ht_region))
qog2 <- ggplot(data=DATABASE_CORRUPCAO,aes(x=ht_region, y=icrg_qog)) + 
  geom_bar(stat = "identity", aes(fill=factor(ht_region), alpha = 0.9)) +
  scale_fill_hue(c = 40) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  scale_x_discrete(labels=c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                            "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                            "South Asia", "The Pacific", "The Caribbean")) +
  theme(legend.position="none") +
  labs(title="Gr�fico 2 - Qualidade do Governo", caption = "Elabroa��o Pr�pria com base nos dados do QoG",
       x="",
       y="Qualidade do Gov")
qog2

# gr�fico plot qualidade do governo x indice corrup
basic2 <- ggplot(DATABASE_CORRUPCAO, aes(x=vdem_corr, y=wdi_wip)) + geom_point()
basic2

basic2+
  ggtitle("�NDICE DE CORRUP��O x % MULHERES NO PARLAMENTO") + 
  xlab("�ndice de corrup��o") +
  ylab("�ndice de qualidade do governo") + 
  theme(axis.title = element_text( angle = 45, color="black", size=10, face=2)) +
  theme(axis.title.x = element_text(angle = 0, color = "black", size = 10, face = 2)) +
  theme(axis.text = element_text( angle = 90, color="black", size=8, face=2))


# criando gr�fico densidade do �ndice de estado de direito x regi�o
DATABASE_CORRUPCAO$ht_region <- (DATABASE_CORRUPCAO$ht_region = factor(DATABASE_CORRUPCAO$ht_region, 
                                                                       labels = c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                                                                                  "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                                                                                  "South Asia", "The Pacific", "The Caribbean")))

rle2 <- ggplot(DATABASE_CORRUPCAO, aes(x = DATABASE_CORRUPCAO$wbgi_rle, y = DATABASE_CORRUPCAO$ht_region , 
                                      fill = DATABASE_CORRUPCAO$ht_region )) +
  geom_density_ridges(alpha=0.3, bandwidth=0.1) +
  theme_light() +
  theme(axis.text.x=element_text(hjust = 0)) +
  theme(legend.position = "none") +
  labs(title="Estado de Direito por Regi�es do Mundo", 
       x="�ndice Estado de Direito", y="",
       fill = "",
       caption = "Elabora��o Pr�pria com base nos dados do QoG")
rle2


# criando gr�fico de barra do �ndice de estado de direito x regi�o
theme_set(theme_bw())
DATABASE_CORRUPCAO$ht_region <- paste(as.character(DATABASE_CORRUPCAO$ht_region))
rle <- ggplot(data=DATABASE_CORRUPCAO,aes(x=ht_region, y=wbgi_rle)) + 
  geom_bar(stat = "identity", aes(fill=factor(ht_region), alpha = 0.9)) +
  scale_fill_hue(c = 40) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  scale_x_discrete(labels=c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                            "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                            "South Asia", "The Pacific", "The Caribbean")) +
  theme(legend.position="none") +
  labs(title="Gr�fico 3 - Estado de Direito", caption = "Elabroa��o Pr�pria com base nos dados do QoG",
       x="",
       y="�ndice Estado de Direito")
rle

# gr�fico plot Estado de Direito x indice corrup
basic3 <- ggplot(DATABASE_CORRUPCAO, aes(x=vdem_corr, y=wbgi_rle)) + geom_point()
basic3

basic3+
  ggtitle("�NDICE DE CORRUP��O x % MULHERES NO PARLAMENTO") + 
  xlab("�ndice de corrup��o") +
  ylab("�ndice de qualidade do governo") + 
  theme(axis.title = element_text( angle = 45, color="black", size=10, face=2)) +
  theme(axis.title.x = element_text(angle = 0, color = "black", size = 10, face = 2)) +
  theme(axis.text = element_text( angle = 90, color="black", size=8, face=2))


# criando gr�fico de densidade do �ndice de corrup��o x regi�o
DATABASE_CORRUPCAO$ht_region <- (DATABASE_CORRUPCAO$ht_region = factor(DATABASE_CORRUPCAO$ht_region, 
                                                                       labels = c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                                                                                  "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                                                                                  "South Asia", "The Pacific", "The Caribbean")))

corr <- ggplot(DATABASE_CORRUPCAO, aes(x = DATABASE_CORRUPCAO$vdem_corr, y = DATABASE_CORRUPCAO$ht_region , 
                                      fill = DATABASE_CORRUPCAO$ht_region )) +
  geom_density_ridges(alpha=0.3, bandwidth=0.1) +
  theme_light() +
  theme(axis.text.x=element_text(hjust = 0)) +
  theme(legend.position = "none") +
  labs(title="Corrup��o por Regi�es do Mundo", 
       x="�ndice de Corrup��o", y="",
       fill = "",
       caption = "Elabora��o Pr�pria com base nos dados do QoG")
corr


# criando gr�fico de barra do �ndice de corrup��o x regi�o
theme_set(theme_bw())
DATABASE_CORRUPCAO$ht_region <- paste(as.character(DATABASE_CORRUPCAO$ht_region))
corr2 <- ggplot(data=DATABASE_CORRUPCAO,aes(x=ht_region, y=vdem_corr)) + 
  geom_bar(stat = "identity", aes(fill=factor(ht_region), alpha = 0.9)) +
  scale_fill_hue(c = 40) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  scale_x_discrete(labels=c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                            "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                            "South Asia", "The Pacific", "The Caribbean")) +
  theme(legend.position="none") +
  labs(title="Gr�fico 5 - �ndice Corrup��o", caption = "Elabroa��o Pr�pria com base nos dados do QoG",
       x="",
       y="�ndice de Corrup��o")
corr2
# criando gr�fico de IDH x regi�o
DATABASE_CORRUPCAO$ht_region <- (DATABASE_CORRUPCAO$ht_region = factor(DATABASE_CORRUPCAO$ht_region, 
                                                                       labels = c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                                                                                  "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                                                                                  "South Asia", "The Pacific", "The Caribbean")))

idh <- ggplot(DATABASE_CORRUPCAO, aes(x = DATABASE_CORRUPCAO2$undp_hdi, y = DATABASE_CORRUPCAO$ht_region , 
                                      fill = DATABASE_CORRUPCAO$ht_region )) +
  geom_density_ridges(alpha=0.3, bandwidth=0.1) +
  theme_light() +
  theme(axis.text.x=element_text(hjust = 0)) +
  theme(legend.position = "none") +
  labs(title="Qualidade do Governo por Regi�es do Mundo", 
       x="�ndice qualidade do governo", y="",
       fill = "",
       caption = "Elabora��o Pr�pria com base nos dados do QoG")
idh

# criando gr�fico de barra de IDH x regi�o
theme_set(theme_bw())
DATABASE_CORRUPCAO$ht_region <- paste(as.character(DATABASE_CORRUPCAO$ht_region))
idh2 <- ggplot(data=DATABASE_CORRUPCAO,aes(x=ht_region, y=DATABASE_CORRUPCAO2$undp_hdi)) + 
  geom_bar(stat = "identity", aes(fill=factor(ht_region), alpha = 0.9)) +
  scale_fill_hue(c = 40) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  scale_x_discrete(labels=c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                            "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                            "South Asia", "The Pacific", "The Caribbean")) +
  theme(legend.position="none") +
  labs(title="Gr�fico 4 - �ndice de Desenvolvimento Humano", caption = "Elabroa��o Pr�pria com base nos dados do QoG",
       x="",
       y="IDH")
idh2

# gr�fico plot Estado de Direito x indice corrup
basic5 <- ggplot(DATABASE_CORRUPCAO, aes(x=vdem_corr, y=DATABASE_CORRUPCAO2$undp_hdi)) + geom_point()
basic5

basic5+
  ggtitle("�NDICE DE CORRUP��O x % MULHERES NO PARLAMENTO") + 
  xlab("�ndice de corrup��o") +
  ylab("�ndice de qualidade do governo") + 
  theme(axis.title = element_text( angle = 45, color="black", size=10, face=2)) +
  theme(axis.title.x = element_text(angle = 0, color = "black", size = 10, face = 2)) +
  theme(axis.text = element_text( angle = 90, color="black", size=8, face=2))


# colocando os gr�ficos anteriores na mesma janela
ggarrange(mulheres, qog, rle, idh, corr)
