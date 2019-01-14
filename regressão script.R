# Ativando os pacotes exigidos:
library(car)
library(haven)
library(MASS)
library(pander)
library(tidyverse)
library(ggplot2)
library(dplyr)

# ANÁLISE INICAL DOS DADOS ####

# Abrindo o banco de dados
library(readxl)
DATABASE_CORRUPÇÃO <- read_excel("C:/Users/Luh/Google Drive/AULAS MESTRADO/2018_2 davi/DATABASE_CORRUPÇÃO.xlsx")
View(DATABASE_CORRUPÇÃO)

# Omitindo as células vazias da base
na.omit(DATABASE_CORRUPÇÃO)

# Organizando a VD (índice de corrupção - vdem_corr):
X <- DATABASE_CORRUPÇÃO$vdem_corr

# Organizando as VIs:
Y1 <- DATABASE_CORRUPÇÃO$wdi_wip  # participação de mulheres no parlamento
Y2 <- DATABASE_CORRUPÇÃO$wbgi_rle # estado de direito
Y3 <- DATABASE_CORRUPÇÃO$icrg_qog # qualidade do governo

# Analisando a estrutura das variáveis:
str(DATABASE_CORRUPÇÃO)

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

# ANÁLISE EXPLORATÓRIA DOS DADOS ####

# Explorando a variável Y1 (Participação Feminina no Parlamento)
tab_complet <- aggregate(wdi_wip ~ ht_region, data = DATABASE_CORRUPÇÃO, FUN = mean)
View(tab_complet)
region <- as.factor(tab_complet$ht_region)

médiafem <- c(20.514286, 26.010000, 13.180000, 21.197959, 30.485185, 15.800000, 
           18.745454, 16.162500, 4.041667, 16.238462)
baseregion <- c("Eastern Europe and post Soviet Union", "Latin America", "North Africa and 
            the Middle East", "Sub-Saharan Africa", "Western Europe and North America", 
                "East Asia", "South-East Asia", "South Asia", 
                "The Pacific", "The Caribbean")

barplot(médiafem, names.arg = baseregion, col = "purple",
        main = "Gráfico 1 - Média de Mulheres no Parlamento por Região",
        cex.axis = 1, las = 2, cex.names = 0.7)



