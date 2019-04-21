#Importa a base de dados em uma variavel
sales <- read.csv("C:/Users/ivosm/Documents/r/Regressao Logistica/SALES_CALL_CENTER_3.csv", header=T)
#Importa os dados do Github
#sales <- read.csv(url("https://raw.githubusercontent.com/ivosmatos/CallCenterRLM/master/Dataset/SALES_CALL_CENTER_3.csv"), header=T)
#Mostra as primeira 5 linhas da tabela criada
head(sales)
#----------------------------------------------------------------------------------------------------------#
#Tansformando o texto em numerico
#----------------------------------------------------------------------------------------------------------#
#Mostra as variaveis texto que existe
table(sales$ESTADO_CIVIL)
#Inclui um campo na tabela e transforma o texto em numerico
sales <- sales %>%
  mutate(ESTADO_CIVIL_2 = case_when(ESTADO_CIVIL=="CASADO" ~ 1, 
                                    ESTADO_CIVIL=="DESQUITADO" ~ 2,
                                    ESTADO_CIVIL=="INDETERMINADO" ~ 3,
                                    ESTADO_CIVIL=="NAO INFORMADO" ~ 4,
                                    ESTADO_CIVIL=="SOLTEIRO" ~ 5,
                                    ESTADO_CIVIL=="VIUVO" ~ 6))
#Mostra como ficou a nova coluna criada com os numeros
table(sales$ESTADO_CIVIL_2)

#Mostra as variaveis texto que existe
table(sales$SEXO)
#Inclui um campo na tabela e transforma o texto em numerico
sales <- sales %>%
  mutate(SEXO_2 = case_when(SEXO=="F" ~ 1, 
                            SEXO=="M" ~ 2,
                            SEXO=="I" ~ 3))
#Mostra como ficou a nova coluna criada com os numeros
table(sales$SEXO_2)

#Mostra as variaveis texto que existe
table(sales$REGIAO)
#Inclui um campo na tabela e transforma o texto em numerico
sales <- sales %>%
  mutate(REGIAO_2 = case_when(REGIAO=="CENTRO_OESTE" ~ 1, 
                              REGIAO=="NORDESTE" ~ 2,
                              REGIAO=="NORTE" ~ 3,
                              REGIAO=="SUDESTE" ~ 4,
                              REGIAO=="SUL" ~ 5))
#Mostra como ficou a nova coluna criada com os numeros
table(sales$REGIAO_2)

#----------------------------------------------------------------------------------------------------------#
#Verificar os campos que são numericos
str(sales)
#----------------------------------------------------------------------------------------------------------#
#Variavel com as quantidade de venda e não venda
y_act <- sales$STATUS_VENDA
#----------------------------------------------------------------------------------------------------------#

#Primeiro modelo com todas as variaveis numericas
lrm1 <- glm(STATUS_VENDA ~ DIA_VENCIMENTO_FATURA+
                           MEDIA_FATURA+
                           IDADE+
                           ESTADO_CIVIL_2+
                           SEXO_2+
                           TEMPO_RELACIONAMENTO_MESES+
                           REGIAO_2, family="binomial", data = sales)
#resultado o modelo
summary(lrm1)

#Aplicando o Predict para verificar o resultado
pred1 <- predict(lrm1, newdata = sales, type = "response")
#Transforma o valor em 1 ou 0 gerado no predict
y_pred_num1 <- ifelse(pred1 > 0.5, 1, 0)
#Cria uma matriz para verificar se o resultado esta igual ao valores reais
y_pred1 <- factor(y_pred_num1, levels=c(0, 1))
#Compara os valores entre o Preditc e a tabela real
table(sales$STATUS_VENDA,y_pred1)
#Valor de acerto do modelo
mean(y_pred1 == y_act)

#Ultimo modelo onde foi alcançado o melhor acerto. Foi utilizada apenas dois preditores
lrm6 <- glm(STATUS_VENDA ~ MEDIA_FATURA+
                           TEMPO_RELACIONAMENTO_MESES, family="binomial", data = sales)
#resultado o modelo
summary(lrm6)

#Aplicando o Predict para verificar o resultado
pred6 <- predict(lrm6, newdata = sales, type = "response")
#Transforma o valor em 1 ou 0 gerado no predict
y_pred_num6 <- ifelse(pred6 > 0.5, 1, 0)
#Cria uma matriz para verificar se o resultado esta igual ao valores reais
y_pred6 <- factor(y_pred_num6, levels=c(0, 1))
#Compara os valores entre o Preditc e a tabela real
table(sales$STATUS_VENDA,y_pred6)
#Valor de acerto do modelo
mean(y_pred6 == y_act)

#Criando um campo na tabela onde sera mostratado a propabilidade de venda ou não venda
sales$prop <- predict(lrm6, newdata = sales, type = "response")

#Realizando o Ranking dos registros
sales <- sales %>%
  mutate(score = case_when(between(prop,0.10,0.199) ~ 1,
                           between(prop,0.20,0.299) ~ 2,
                           between(prop,0.30,0.399) ~ 3,
                           between(prop,0.40,0.499) ~ 4, 
                           between(prop,0.50,0.599) ~ 5, 
                           between(prop,0.60,0.699) ~ 6, 
                           between(prop,0.70,0.799) ~ 7,
                           between(prop,0.80,0.899) ~ 8,
                           between(prop,0.90,0.999) ~ 9,
                           between(prop,1.00,1.999) ~ 10))
#Resultado do ranking
table(sales$score)
