# Projeto People Analytics - Quais Fatores Mais Causam Atritos no Ambiente de Trabalho?

# Leia a definição do projeto no Capítulo 8 do curso.

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("~/Dropbox/DSA/Business-Analytics2.0/Cap08")
getwd()

# Imports
library(caret)
library(ggplot2)
library(gridExtra)
library(data.table)
library(car)
library(caTools)
library(corrplot)
library(rpart)
library(rpart.plot)

# Carregando o dataset
dados_rh <- fread('dados/dataset.csv')
dim(dados_rh)
View(dados_rh)
str(dados_rh)
summary(dados_rh)

##### Limpeza e Transformação ##### 

# Transformando variáveis categóricas para o tipo fator
View(dados_rh)
dados_rh$Attrition                <- as.factor(dados_rh$Attrition)
dados_rh$BusinessTravel           <- as.factor(dados_rh$BusinessTravel)
dados_rh$Department               <- as.factor(dados_rh$Department)
dados_rh$Education                <- as.factor(dados_rh$Education)
dados_rh$EducationField           <- as.factor(dados_rh$EducationField)
dados_rh$'Employee Source'        <- as.factor(dados_rh$'Employee Source')
dados_rh$EnvironmentSatisfaction  <- as.factor(dados_rh$EnvironmentSatisfaction)
dados_rh$Gender                   <- as.factor(dados_rh$Gender)
dados_rh$JobInvolvement           <- as.factor(dados_rh$JobInvolvement)
dados_rh$JobLevel                 <- as.factor(dados_rh$JobLevel)
dados_rh$JobRole                  <- as.factor(dados_rh$JobRole)
dados_rh$JobSatisfaction          <- as.factor(dados_rh$JobSatisfaction)
dados_rh$MaritalStatus            <- as.factor(dados_rh$MaritalStatus)
dados_rh$OverTime                 <- as.factor(dados_rh$OverTime)
dados_rh$PerformanceRating        <- as.factor(dados_rh$PerformanceRating)
dados_rh$RelationshipSatisfaction <- as.factor(dados_rh$RelationshipSatisfaction)
dados_rh$StockOptionLevel         <- as.factor(dados_rh$StockOptionLevel)
dados_rh$WorkLifeBalance          <- as.factor(dados_rh$WorkLifeBalance)
str(dados_rh)

# Transformando variáveis numéricas para o tipo inteiro
View(dados_rh)
dados_rh$DistanceFromHome  <- as.integer(dados_rh$DistanceFromHome)
dados_rh$MonthlyIncome     <- as.integer(dados_rh$MonthlyIncome)
dados_rh$PercentSalaryHike <- as.integer(dados_rh$PercentSalaryHike)

# Drop dos níveis de fatores com 0 count
dados <- droplevels(dados_rh)
str(dados_rh)
summary(dados_rh)
View(dados_rh)

##### Engenharia de Atributos ##### 

# Criamos uma coluna de anos anteriores de experiência para visualizar melhor o 
# perfil de experiência do funcionário.
dados_rh$PriorYearsOfExperience <- dados_rh$TotalWorkingYears - dados_rh$YearsAtCompany
View(dados_rh)

# A estabilidade no emprego (job tenure) é a medida do tempo que um funcionário está empregado 
# por seu empregador atual. A estabilidade no emprego de um funcionário é muito importante e 
# muitas vezes os empregadores consideram a estabilidade no emprego um critério para a contratação 
# de novos funcionários. A permanência no emprego pode ser longa ou curta.

# Criamos um novo recurso de estabilidade média para traçar o perfil de permanência média 
# dos funcionários em empresas anteriores.
dados_rh$AverageTenure <- dados_rh$PriorYearsOfExperience / dados_rh$NumCompaniesWorked
View(dados_rh)

# A estabilidade média produz valores como Inf devido à natureza de sua derivação
# Substituímos para zero.
summary(dados_rh$AverageTenure)
dados_rh$AverageTenure[!is.finite(dados_rh$AverageTenure)] <- 0
summary(dados_rh$AverageTenure)
View(dados_rh)

# Analisamos e dividimos os dados como base na coluna Termination, que indica se 
# o funcionário foi desligado da empresa.
dados_rh_1 <- dados_rh[dados_rh$Attrition != 'Termination']
dados_rh_1 <- droplevels(dados_rh_1)
dim(dados_rh_1)
summary(dados_rh_1)

# Mesmo filtro anterior, mas agora por demissão voluntária
dados_rh_2 <- dados_rh[dados_rh$Attrition != 'Voluntary Resignation']
dados_rh_2 <-droplevels(dados_rh_2)
dim(dados_rh_2)  
summary(dados_rh_2)

##### Análise Exploratória ##### 

# Plots de análise univariada
ggplot(dados_rh) + geom_bar(aes(x = Gender))
ggplot(dados_rh) + geom_density(aes(x = Age))
ggplot(dados_rh) + geom_bar(aes(x = Attrition))
ggplot(dados_rh) + geom_bar(aes(x = Department))
ggplot(dados_rh) + geom_bar(aes(x = JobRole))
ggplot(dados_rh) + geom_bar(aes(x = Education)) + facet_grid(~EducationField)

# Multiplot Grid
p.TotalWorkingYears       <- ggplot(dados_rh) + geom_density(aes(TotalWorkingYears))
p.YearsAtCompany          <- ggplot(dados_rh) + geom_density(aes(YearsAtCompany))
p.YearsSinceLastPromotion <- ggplot(dados_rh) + geom_density(aes(YearsSinceLastPromotion))
p.YearsWithCurrManager    <- ggplot(dados_rh) + geom_density(aes(YearsWithCurrManager))
p.YearsInCurrentRole      <- ggplot(dados_rh) + geom_density(aes(YearsInCurrentRole))
p.PriorYearsOfExperience  <- ggplot(dados_rh) + geom_density(aes(PriorYearsOfExperience))

# Organiza no grid
grid.arrange(p.TotalWorkingYears, 
             p.YearsAtCompany, 
             p.YearsSinceLastPromotion, 
             p.YearsWithCurrManager, 
             p.YearsInCurrentRole, 
             p.PriorYearsOfExperience, 
             nrow = 2, 
             ncol = 3)

# Tempo de experiência anterior
# Vamos descobrir a proporção de funcionários com menos de alguns anos de experiência 
# (valores escolhidos: 1, 3, 5, 7, 10 anos)
length(which(dados_rh$PriorYearsOfExperience < 1)) / length(dados_rh$PriorYearsOfExperience)  
length(which(dados_rh$PriorYearsOfExperience < 3)) / length(dados_rh$PriorYearsOfExperience)   
length(which(dados_rh$PriorYearsOfExperience < 5)) / length(dados_rh$PriorYearsOfExperience)   
length(which(dados_rh$PriorYearsOfExperience < 7)) / length(dados_rh$PriorYearsOfExperience)   
length(which(dados_rh$PriorYearsOfExperience < 10)) / length(dados_rh$PriorYearsOfExperience)  

# Exemplo de insight:
# 58% dos funcionários têm menos de 3 anos de experiência de trabalho antes de entrar na IBM
# Possíveis problemas: conjuntos de habilidades subdesenvolvidos, base de jovens funcionários, 
# mentalidade de "trabalho" imatura.

# Idade
length(which(dados_rh$Age < 30)) / length(dados_rh$Age)

# Exemplo de insight:
# Apenas 22% dos funcionários têm menos de 30 anos, a base de funcionários não é exatamente 
# tão jovem como o esperado.

# # Educação
summary(dados_rh$Education)
length(which(dados_rh$Education == 3)) / length(dados_rh$Education)
length(which(dados_rh$Education == 4)) / length(dados_rh$Education)

# Exemplo de insight:
# Cerca de 39% dos funcionários são graduados e 27% realizaram o mestrado.
# A busca pelo ensino superior pode ter levado a uma diminuição da experiência de trabalho.

# Boxplot mostrando a distribuição do salário mensal para todos os 4 níveis 
# de satisfação no trabalho de 1-4
ggplot(data = subset(dados_rh, !is.na(JobSatisfaction)), aes(JobSatisfaction, MonthlyIncome)) + 
  geom_boxplot()

# Exemplo de Insight
# Não há sinais óbvios de que um salário mais alto leva a uma maior satisfação no trabalho

# Correlação
cor(dados_rh$TotalWorkingYears, dados_rh$YearsAtCompany,          use = "complete.obs")
cor(dados_rh$YearsAtCompany,    dados_rh$YearsInCurrentRole,      use = "complete.obs")
cor(dados_rh$YearsAtCompany,    dados_rh$YearsSinceLastPromotion, use = "complete.obs")
cor(dados_rh$YearsAtCompany,    dados_rh$YearsWithCurrManager,    use = "complete.obs")
cor(dados_rh$TotalWorkingYears, dados_rh$MonthlyIncome,           use = "complete.obs")
cor(dados_rh$YearsAtCompany,    dados_rh$MonthlyIncome,           use = "complete.obs")  

# Scatterplots
ggplot(dados_rh) + geom_point(aes(TotalWorkingYears, MonthlyIncome))
ggplot(dados_rh) + geom_point(aes(YearsAtCompany, MonthlyIncome))

# Vamos investigar a relação do equilíbrio entre vida pessoal e profissional e renda mensal
ggplot(data = subset(dados_rh, !is.na(WorkLifeBalance)), aes(WorkLifeBalance, MonthlyIncome)) + 
  geom_boxplot()

# Exemplo de insight
# Os funcionários que avaliaram o equilíbrio entre vida profissional e pessoal igual a 1 também têm renda média mensal 
# significativamente mais baixa.
# Baixo equilíbrio entre vida profissional e baixo salário? Um problema que o departamento de RH precisa examinar.

# Verificando a diferença salarial entre homens e mulheres.
ggplot(data = subset(dados_rh, !is.na(Gender)), aes(Gender, MonthlyIncome, fill = Gender)) +
  geom_boxplot() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)) +
  labs(x = "Gender", y = "Monthly Income", title = "Salário Mensal Entre Gêneros") +
  coord_flip()

# Exemplo de insight
# Não há sinais de discriminação de gênero; na verdade, as mulheres ganham um pouco mais, em média, 
# desconsiderando todos os outros fatores.

# Função
ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, MonthlyIncome)) +
  ggtitle("Salário Mensal Por Função")

ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, AgeStartedWorking)) +
  ggtitle("Idade Que Iniciou na Função")

ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, Age)) +
  ggtitle("Idade Por Função")

ggplot(data = subset(dados_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, YearsAtCompany)) +
  ggtitle("Tempo de Empresa (em anos)")

ggplot(data = na.omit(dados_rh)) + geom_bar(aes(JobRole, fill = Education), position = "fill") +
  ggtitle("Nível de Educação Por Função") + 
  ylab("Proportion")

# Plots de análise multivariada para variáveis normalmente usadas durante o processo de contratação
ggplot(data = dados_rh_1) + 
  geom_bar(aes(x = Education , fill = Attrition), position = 'fill') + 
  facet_grid(.~Department)

ggplot(data = dados_rh_1) + 
  geom_bar(aes(x = Education , fill = Attrition), position = 'fill') + 
  facet_grid(.~JobRole)

ggplot(data = dados_rh_1) + 
  geom_bar(aes(x = EducationField , fill = Attrition), position = 'fill') + 
  facet_grid(.~JobRole) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0))

# Plots de análise multivariada para variáveis normalmente usadas após o processo de contratação
ggplot(dados_rh_1) + geom_bar(aes(x = Age, fill = Attrition), position = 'fill') 
ggplot(dados_rh_1) + geom_bar(aes(x = Department, fill = Attrition), position = 'fill') 
ggplot(dados_rh_1) + geom_bar(aes(x = DistanceFromHome, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(x = `Employee Source`, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(x = JobRole, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(x = MaritalStatus, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(x = AverageTenure, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(x = Education, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(x = EducationField, fill = Attrition),position ='fill')
ggplot(dados_rh_1) + geom_bar(aes(x = Gender, fill = Attrition), position = 'fill')

# Plots de análise multivariada entre algumas variáveis e o status do funcionário
ggplot(dados_rh_1) + geom_boxplot(aes(Attrition, MonthlyIncome))
ggplot(dados_rh_1) + geom_boxplot(aes(Attrition, PercentSalaryHike))
ggplot(dados_rh_1) + geom_bar(aes(TrainingTimesLastYear, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(BusinessTravel, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(OverTime, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(StockOptionLevel, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(EnvironmentSatisfaction, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(JobSatisfaction, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(JobInvolvement, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(RelationshipSatisfaction, fill = Attrition), position = 'fill')
ggplot(dados_rh_1) + geom_bar(aes(WorkLifeBalance, fill = Attrition), position = 'fill')

##### Modelagem Preditiva ##### 

# Vamos concentrar nosso trabalho em tentar ajudar o RH a recrutar melhor visando evitar atritos 
# e, consequentemente, demissões.

# Criaremos 5 versões do modelo e para cada um vamos explorar as opções e interpretar o resultado.

# Primeira versão do modelo com algumas variáveis
?glm
modelo_v1 <- glm(Attrition ~ Age + Department + DistanceFromHome + `Employee Source` + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience + Gender + 
                   Education + EducationField, 
                 family = binomial, 
                 data = dados_rh)

summary(modelo_v1)
?vif
vif(modelo_v1)

# Vamos dividir os dados em treino e teste. Vamos trabalhar com os dados sem registros de demitidos.
set.seed(2004)
index_treino <- sample.split(Y = dados_rh_1$Attrition, SplitRatio = 0.7)
dados_rh_1_treino <- subset(dados_rh_1, train = T)
dados_rh_1_teste <- subset(dados_rh_1, train = F)

# Segunda versão do modelo com dados de treino
modelo_v2 <- glm(Attrition ~ Age + Department + DistanceFromHome + `Employee Source` + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience + Gender + 
                   Education + EducationField, 
                 family = binomial, 
                 data = dados_rh_1_treino)

summary(modelo_v2)
vif(modelo_v2)

# Previsões
threshold <- 0.5
previsoes_v2 <- predict(modelo_v2, type = 'response', newdata = dados_rh_1_teste)
previsoes_finais_v2 <- ifelse(previsoes_v2 > threshold, 'Voluntary Resignation', 'Current employee')
table(dados_rh_1_teste$Attrition, previsoes_finais_v2)

# Terceira versão do modelo com dados de treino e sem variáveis de educação
modelo_v3 <- glm(Attrition ~ Age + Department + DistanceFromHome + `Employee Source` + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience + Gender, 
                 family = binomial, 
                 data = dados_rh_1_treino)

summary(modelo_v3)
vif(modelo_v3)

# Previsões
threshold <- 0.5
previsoes_v3 <- predict(modelo_v3, type = 'response', newdata = dados_rh_1_teste)
previsoes_finais_v3 <- ifelse(previsoes_v3 > threshold, 'Voluntary Resignation', 'Current employee')
table(dados_rh_1_teste$Attrition, previsoes_finais_v3)

# Quarta versão do modelo com dados de treino e sem variáveis de educação e genero
modelo_v4 <- glm(Attrition ~ Age + Department + DistanceFromHome + `Employee Source` + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience, 
                 family = binomial, 
                 data = dados_rh_1_treino)

summary(modelo_v4)
vif(modelo_v4)

# Previsões
threshold <- 0.5
previsoes_v4 <- predict(modelo_v4, type = 'response', newdata = dados_rh_1_teste)
previsoes_finais_v4 <- ifelse(previsoes_v4 > threshold, 'Voluntary Resignation', 'Current employee')
table(dados_rh_1_teste$Attrition, previsoes_finais_v4)

# Quinta versão do modelo com dados de treino e sem variáveis de educação, genero e outro algoritmo
?rpart
modelo_v5 <- rpart(Attrition ~ Age + Department + DistanceFromHome + JobRole + MaritalStatus + 
                     AverageTenure + PriorYearsOfExperience, 
                   method = "class", 
                   control = rpart.control(minsplit = 500, cp = 0),
                   data = dados_rh_1_treino)

summary(modelo_v5)
rpart.plot(modelo_v5)


# Fim

