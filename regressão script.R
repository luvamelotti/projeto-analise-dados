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

# Rodando a o teste da regressão
reg <- lm(Y ~ X1 + X2 + X3)
reg
summary(reg)

# Plotando os resultados da regressão
plot(reg)

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
DATABASE_CORRUPCAO$vdem_corr <- (DATABASE_CORRUPCAO$vdem_corr^(0.5)-1)/0.5
#reanalisando a regressão e os pressupostos:
reg2 <- lm(DATABASE_CORRUPCAO$vdem_corr ~ X1 + X2 + X3)
reg2
# gerando os novos gráficos para análise da nova regressão
plot(reg2)



# ANÁLISE EXPLORATÓRIA DOS DADOS ####

# Explorando a variável Y1 (Participação Feminina no Parlamento)
# criando um gráfico de barra de participação feminina por região do mundo:
theme_set(theme_bw())
DATABASE_CORRUPCAO$ht_region <- paste(as.character(DATABASE_CORRUPCAO$ht_region))
ggplot(data=DATABASE_CORRUPCAO,aes(x=ht_region, fill=wdi_wip)) + 
  geom_bar(aes(fill=factor(ht_region), alpha = 0.8)) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  scale_x_discrete(labels=c("Eastern Europe and post Soviet Union", "Latin America", "North Africa & the Middle East",
                            "Sub-Saharan Africa","Western Europe and North America","East Asia", "South-East Asia", 
                            "South Asia", "The Pacific", "The Caribbean")) +
  theme(legend.position="none") +
  labs(title="Grafico", caption = "Elabroação Própria com base nos dados do QoG",
       x="",
       y="Media mulheres no parlamento")

