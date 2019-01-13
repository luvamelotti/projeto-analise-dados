library(car)
library(haven)
library(MASS)
library(pander)
library(tidyverse)
library(ggplot2)


# Abrindo o banco de dados
library(readxl)
regressão <- read_excel("C:/Users/Luh/Google Drive/AULAS MESTRADO/2018_2 davi/regressão.xlsx")
View(regressão)

# Omitindo as células vazias da base
na.omit(regressão)

# Organizando a VD (índice de corrupção - vdem_corr):
X <- regressão$vdem_corr

# Organizando as VIs:
Y1 <- regressão$wdi_wip  # participação de mulheres no parlamento
Y2 <- regressão$wbgi_rle # qualidade do governo
Y3 <- regressão$icrg_qog # estado de direito

# Analisando a estrutura das variáveis:
str(regressão)

# Rodando a o teste da regressão
reg <- lm(X ~ Y1 + Y2 + Y3)
summary(reg)

# Plotando os resultados da regressão
plot(reg)

# Analisando se há outliers na amostra:
outlierTest(reg)

# plotando o resultado do teste de outlier:
qqPlot(reg, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(reg) #leverage plots #GOSTEI MAIS

# checando o pressuposto da não-normalidade:
library(MASS) # ativando o pacote
dist_residuo <- studres(reg) # distribuição do resíduo

# histograma dos resíudos:
hist(dist_residuo, freq=FALSE,
     main="Distribuição dos Resíduos")
xfit <- seq(min(dist_residuo),max(dist_residuo),length=40)
yfit <- dnorm(xfit)
lines(xfit, yfit) 

# checando pressuposto da homocedasticidade:
ncvTest(reg)

#plotando studentized residuals vs. fitted values:
spreadLevelPlot(reg)  

# checando o pressuposto da multicolinearidade:
vif(reg) # variação da inflação dos fatores
sqrt(vif(reg)) > 2 # problem?

# checando o pressuposto da não-linearidade:
crPlots(reg)
ceresPlots(reg)

# checando o pressuposto de não-independencia dos erros:
durbinWatsonTest(reg) # teste para erros autocorrelacionados

