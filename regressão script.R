# Ativando os pacotes exigidos:
library(car)
library(haven)
library(MASS)
library(pander)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(stats)
library(lmtest)
library(ggpubr)
library(coefplot)

# ANÁLISE INICAL DOS DADOS ####

# Abrindo o banco de dados
DATABASE_CORRUPCAO <- read_excel("C:/Users/Luh/Google Drive/AULAS MESTRADO/2018_2 davi/DATABASE_CORRUPCAO.xlsx")
View(DATABASE_CORRUPCAO)

# Omitindo as células vazias da base
na.omit(DATABASE_CORRUPCAO)

# Organizando a VD (índice de corrupção - vdem_corr):
Y <- DATABASE_CORRUPCAO$vdem_corr

# Organizando as VIs:
X1 <- DATABASE_CORRUPCAO$wdi_wip  # participação de mulheres no parlamento
X2 <- DATABASE_CORRUPCAO$wbgi_rle # estado de direito
X3 <- DATABASE_CORRUPCAO$icrg_qog # qualidade do governo

# Analisando a estrutura das variáveis:
str(DATABASE_CORRUPCAO)

# observando as estatísticas descritivas das varíaveis
DATABASE_CORRUPCAO$ht_region <- as.factor(DATABASE_CORRUPCAO$ht_region) 
pander(summary(DATABASE_CORRUPCAO, header = T))

# Rodando a o teste da regressão
reg <- lm(Y ~ X1 + X2 + X3)
reg
summary(reg)

# Plotando os resultados da regressão
plot(reg)

# resultado da regressão
coefplot(reg)

# criando tabela com dados do summary que não aparecem no gráfico
regressao <- data.frame(Observations = c(194),
                        Residual_Std._Error = c(0.1294),
                        "Deleted observations" = c(63),
                        R2 = c(0,8028),
                        Adjusted_R2 = c(0,7981),
                        p_value = c("2.2e-16"))
pander(regressao, caption = "Regression Results")


# Analisando se há outliers na amostra:
outlierTest(reg) ##INDICA QUE O CASO 85 É OUTLIER

# plotando o resultado do teste de outlier:
qqPlot(reg, main="QQ Plot") #qq plot for studentized resid
##MOSTRA QUE TEM DOIS POSSÍVEIS OUTLIERS
## QQPLOT: INDICAÇÃO DE BOM AJUSTE DOS RESÍDUOS
leveragePlots(reg) #leverage plots 

# checando o pressuposto da não-normalidade:
dist_residuo <- studres(reg) # distribuição do resíduo
dist_residuo

# histograma dos resíudos:
hist(dist_residuo, freq=FALSE,
     main="Distribuição dos Resíduos")
xfit <- seq(min(dist_residuo),max(dist_residuo),length=40)
yfit <- dnorm(xfit)
lines(xfit, yfit) 
# EVIDENCIA A NORMALIDADE DOS ERROS 

# checando pressuposto da homocedasticidade:
bptest(reg, varformula = NULL, studentize = TRUE, data = DATABASE_CORRUPCAO()) #p-valor > 0,05 indica heterocedasticidade

#plotando studentized residuals vs. fitted values:
spreadLevelPlot(reg)  

# checando o pressuposto da multicolinearidade:
vif(reg) # variação da inflação dos fatores
sqrt(vif(reg)) > 2 
pander(sqrt(vif(reg)) > 2) # transformando em tabela

# checando o pressuposto da não-linearidade:
crPlots(reg)
ceresPlots(reg)

# checando o pressuposto de independencia dos erros:
durbinWatsonTest(reg) # teste para erros autocorrelacionados
# o p-valor < 0,05 indica que a H0 é rejeitada, ou seja, há correlação entre os erros

# Fazendo a transformação da variável dependente para respeitar o pressuposto da homocedasticidade
# Transformação Box-Cox para observar o valor de lâmbida que maximiza a função
trans_var <- boxCox(reg, lam=seq(-1, 1, 1/10), plotit = TRUE)
trans_var
# A função é maximizada à um lâmbida de 0.5

#transformando a variável:
Y_transf <- (DATABASE_CORRUPCAO$vdem_corr^(0.5)-1)/0.5
#reanalisando a regressão e os pressupostos:
reg2 <- lm(Y_transf ~ X1 + X2 + X3)
reg2
summary(reg2)
# gerando os novos gráficos para análise da nova regressão
plot(reg2)

# resulatdo da regressão
coefplot(reg2)

#Adicionando uma nova variável ao modelo para tentar solucionar a não independencia dos erros:
#Necessário baixar a base de dados 2
# abrindo os dados da nova varável
undp_hdi <- read_excel("C:/Users/Luh/Google Drive/AULAS MESTRADO/2018_2 davi/database/undp_hdi.xlsx")
View(undp_hdi)

#agregando a nova variável à base de dados original:
DATABASE_CORRUPCAO2 <- cbind.data.frame(DATABASE_CORRUPCAO, Y_transf, undp_hdi)
View(DATABASE_CORRUPCAO2)
na.omit(DATABASE_CORRUPCAO2)
X4 <- DATABASE_CORRUPCAO2$undp_hdi

#regressão final com nova VI (X4), exclusão da variável wbgi_rle (X2) para tentar solucionar a multicolearidade e VD transformada: 
reg_final <- lm(Y_transf ~ X1 + X2 + X4)
reg_final
summary(reg_final)

#plotando os resultados da regressão
coefplot(reg_final)
plot(reg_final)

#retestando o pressuposto da homocedasticidade:
bptest(reg_final, varformula = NULL, studentize = TRUE, data = DATABASE_CORRUPCAO2())

#retestando o pressuposto da multicolinearidade:
vif(reg_final) # variação da inflação dos fatores
sqrt(vif(reg_final)) > 2 

#retestando o pressuposto da independencia dos erros:
durbinWatsonTest(reg_final)
# O p-valor > 0,05 não rejeita a H0, ou seja, confirma a independencia dos erros.

# ANÁLISE EXPLORATÓRIA DOS DADOS ####

# observando as estatísticas descritivas da varíavel (X1) porcentagem de mulheres
pander(summary(DATABASE_CORRUPCAO, header = TRUE))



# Explorando a variável X1 (Participação Feminina no Parlamento)

library(haven)
library(ggridges)
library(ggplot2)

# criando gráfico de densidade para observar, em cada região, qual a concentração de part. feminina

DATABASE_CORRUPCAO$ht_region <- (DATABASE_CORRUPCAO$ht_region = factor(DATABASE_CORRUPCAO$ht_region, 
                                                                   labels = c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                                                                              "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                                                                              "South Asia", "The Pacific", "The Caribbean")))

mulheres <- ggplot(DATABASE_CORRUPCAO, aes(x = wdi_wip, y = DATABASE_CORRUPCAO$ht_region , fill = DATABASE_CORRUPCAO$ht_region )) +
  geom_density_ridges(alpha=0.3, bandwidth=2) +
  theme_light() +
  theme(axis.text.x=element_text(hjust = 0)) +
  theme(legend.position = "none") +
  labs(title="Mulheres no Parlamento por Regiões do Mundo", 
       x="% Mulheres no Parlamento", y="",
       fill = "",
       caption = "Elaboração Própria com base nos dados do QoG")



# criando um gráfico de barra de participação feminina por região do mundo:
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
  labs(title="Gráfico 1 - Mulheres parlamentares por Região", caption = "Elabroação Própria com base nos dados do QoG",
       x="",
       y="Média mulheres no Parlam.")
mulheres2

# gráfico plot porcentagem mulher x indice corrup
basic <- ggplot(DATABASE_CORRUPCAO, aes(x=vdem_corr, y=wdi_wip)) + geom_point()
basic

basic+
  ggtitle("ÍNDICE DE CORRUPÇÃO x % MULHERES NO PARLAMENTO") + 
  xlab("índice de corrupção") +
  ylab("% mulheres no Parlamento") + 
  theme(axis.title = element_text( angle = 45, color="black", size=10, face=2)) +
  theme(axis.title.x = element_text(angle = 0, color = "black", size = 10, face = 2)) +
  theme(axis.text = element_text( angle = 90, color="black", size=8, face=2))

# criando gráfico de índice de qualidade do governo x região
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
  labs(title="Qualidade do Governo por Regiões do Mundo", 
       x="índice qualidade do governo", y="",
       fill = "",
       caption = "Elaboração Própria com base nos dados do QoG")
qog


#criando gráfico de barra para qualidade do governo por região
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
  labs(title="Gráfico 2 - Qualidade do Governo", caption = "Elabroação Própria com base nos dados do QoG",
       x="",
       y="Qualidade do Gov")
qog2

# gráfico plot qualidade do governo x indice corrup
basic2 <- ggplot(DATABASE_CORRUPCAO, aes(x=vdem_corr, y=wdi_wip)) + geom_point()
basic2

basic2+
  ggtitle("ÍNDICE DE CORRUPÇÃO x % MULHERES NO PARLAMENTO") + 
  xlab("índice de corrupção") +
  ylab("índice de qualidade do governo") + 
  theme(axis.title = element_text( angle = 45, color="black", size=10, face=2)) +
  theme(axis.title.x = element_text(angle = 0, color = "black", size = 10, face = 2)) +
  theme(axis.text = element_text( angle = 90, color="black", size=8, face=2))


# criando gráfico densidade do índice de estado de direito x região
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
  labs(title="Estado de Direito por Regiões do Mundo", 
       x="índice Estado de Direito", y="",
       fill = "",
       caption = "Elaboração Própria com base nos dados do QoG")
rle2


# criando gráfico de barra do índice de estado de direito x região
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
  labs(title="Gráfico 3 - Estado de Direito", caption = "Elabroação Própria com base nos dados do QoG",
       x="",
       y="ìndice Estado de Direito")
rle

# gráfico plot Estado de Direito x indice corrup
basic3 <- ggplot(DATABASE_CORRUPCAO, aes(x=vdem_corr, y=wbgi_rle)) + geom_point()
basic3

basic3+
  ggtitle("ÍNDICE DE CORRUPÇÃO x % MULHERES NO PARLAMENTO") + 
  xlab("índice de corrupção") +
  ylab("índice de qualidade do governo") + 
  theme(axis.title = element_text( angle = 45, color="black", size=10, face=2)) +
  theme(axis.title.x = element_text(angle = 0, color = "black", size = 10, face = 2)) +
  theme(axis.text = element_text( angle = 90, color="black", size=8, face=2))


# criando gráfico de índice de corrupção x região
theme_set(theme_bw())
DATABASE_CORRUPCAO$ht_region <- paste(as.character(DATABASE_CORRUPCAO$ht_region))
corr <- ggplot(data=DATABASE_CORRUPCAO,aes(x=ht_region, y=vdem_corr)) + 
  geom_bar(stat = "identity", aes(fill=factor(ht_region), alpha = 0.9)) +
  scale_fill_hue(c = 40) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  scale_x_discrete(labels=c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                            "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                            "South Asia", "The Pacific", "The Caribbean")) +
  theme(legend.position="none") +
  labs(title="Gráfico 5 - Índice Corrupção", caption = "Elabroação Própria com base nos dados do QoG",
       x="",
       y="ìndice de Corrupção")
corr
# criando gráfico de IDH x região
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
  labs(title="Qualidade do Governo por Regiões do Mundo", 
       x="índice qualidade do governo", y="",
       fill = "",
       caption = "Elaboração Própria com base nos dados do QoG")
idh

# criando gráfico de barra de IDH x região
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
  labs(title="Gráfico 4 - Índice de Desenvolvimento Humano", caption = "Elabroação Própria com base nos dados do QoG",
       x="",
       y="IDH")
idh2

# gráfico plot Estado de Direito x indice corrup
basic5 <- ggplot(DATABASE_CORRUPCAO, aes(x=vdem_corr, y=DATABASE_CORRUPCAO2$undp_hdi)) + geom_point()
basic5

basic5+
  ggtitle("ÍNDICE DE CORRUPÇÃO x % MULHERES NO PARLAMENTO") + 
  xlab("índice de corrupção") +
  ylab("índice de qualidade do governo") + 
  theme(axis.title = element_text( angle = 45, color="black", size=10, face=2)) +
  theme(axis.title.x = element_text(angle = 0, color = "black", size = 10, face = 2)) +
  theme(axis.text = element_text( angle = 90, color="black", size=8, face=2))


# colocando os gráficos anteriores na mesma janela
ggarrange(mulheres, qog, rle, idh, corr)
