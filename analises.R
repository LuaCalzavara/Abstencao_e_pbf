## Mexendo na base tsePBFabst
library(ggplot2)
library(stargazer)
library(data.table)
library(ggfortify)
library(tidyverse)
library(janitor)
library(Hmisc)



tsePBFabst <- fread("tsePBFabst.csv")
names(tsePBFabst)
tsePBFabst <- tsePBFabst[,-c(1)] #retirando a  coluna V1

names(juntada)
names(tsePBFabst)

#### estatísticas ####

stargazer(tsePBFabst, summary = T, type = "text", out = "summary.txt")


#desvio-padrão de média de receitamédia por MN
sd(tsePBFabst$total_receita)
sd(tsePBFabst$media_receita)
sd(tsePBFabst$abstFEMININA)

regiao <- tsePBFabst %>% 
  group_by(REGIAO) %>% 
  summarise(Média_de_beneficiários_por_região= mean(total_beneficiarios)) %>% 
  arrange(-Média_de_beneficiários_por_região)
stargazer(regiao, type = "text", out = "regiao.txt") 

tsePBFabst %>% 
  tabyl(REGIAO)

regCasos <- tsePBFabst %>% 
  tabyl(REGIAO,total_beneficiarios) 




# densidade das variáveis numéricas

hist.data.frame(tsePBFabst)


## correlação 

tsePBFabst %>% 
  select(x = media_receita, y = abstFEMININA) %>% 
  cor(.)

tsePBFabst %>% 
  ggplot(aes(x = media_receita, y = abstFEMININA)) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Correlação entre média de receita por usuário
       e abstenção feminina", x = "Média de receita por usuário",
       y = "Abstenção feminina")


tsePBFabst %>% 
  select(x = total_beneficiarios, y = abstFEMININA) %>% 
  cor(.)

tsePBFabst %>% 
  ggplot(aes(x = total_beneficiarios, y = abstFEMININA)) +
  geom_point()+
  geom_smooth(method = "lm")

tsePBFabst %>% 
  select(x = media_receita, y = perc_abstencao) %>% 
  cor(.)

# correlação mais forte(0.2)
tsePBFabst %>% 
  ggplot(aes(x = media_receita, y = perc_abstencao)) +
  geom_point()+
  geom_smooth(method = "lm")

#### REGRESSÃO ####

# 4) Estimar 3 regressões lineares multivariadas com, ao menos, 3 
# preditores cada. Desses 3 modelos, pelo menos 1 deverá ter 
# uma interação.
names(tsePBFabst)

# Modelo 1 = abstsFeminina explicada por receitaMédia de PBF(dos Municpios)
modelo1 <- lm(log(abstFEMININA) ~ log(media_receita) + REGIAO + log(total_aptos), data = tsePBFabst)


# explicando a porcentagem total de abst por perc.media de receita de BF de Municipio,
# controlado por região e quantidade de beneficiarios do bolsa familia
modelo2 <- lm(log(perc_abstencao) ~ log(total_beneficiarios) +log(media_receita) + REGIAO,  data = tsePBFabst)

## explicar o percAbstTOTAL pela abstFEMININA + qntdd de PBF nos municipio,
#controlado por região, interagindo abstFEM com a qntd de PBF
modelo3 <- lm(log(perc_abstencao)  ~ log(abstFEMININA) + log(total_beneficiarios) + REGIAO +  log(abstFEMININA):log(total_beneficiarios), 
              data = tsePBFabst)

stargazer(modelo1, modelo2, modelo3, type = "text", 
          title = "Comparação de coeficientes entre os 3 modelos",
          out = "Comp_modelos.docx")








## modelo 4 com uma região definida
names(tsePBFabst)
modelo4 <- lm(log(perc_abstencao)  ~ log(abstFEMININA) + log(total_beneficiarios) + SG_UF +  log(abstFEMININA):log(total_beneficiarios), 
              data = tsePBFabst)
stargazer(modelo4, type="text")
plot(modelo4)

plot(modelo3)


região <- tsePBFabst %>% 
  group_by(REGIAO) %>% 
  summarise(Média_de_beneficiários_por_região= mean(total_beneficiarios)) %>% 
  arrange(-Média_de_beneficiários_por_região) 

região$Média_de_beneficiários_por_região <- round(região$Média_de_beneficiários_por_região, digits = 2)

stargazer(região, type = "text", summary = F, out = "regiao.txt")


regiao_abstFeminina <- tsePBFabst %>% 
  group_by(REGIAO) %>% 
  summarise(Abstencao_Feminina = mean(percAbsT_Mulher)) 
regiao_abstFeminina$Abstencao_Feminina <- round(regiao_abstFeminina$Abstencao_Feminina, digits = 2)

stargazer(regiao_abstFeminina, type = "text", summary = F, out = "regiao_abstFeminina.txt") 

plot(modelo2)

library(stargazer)
stargazer(modelo2, type = "html", out = "modelo2.html")
stargazer(modelo1, type = "html", out = "modelo1.html")
stargazer(modelo3, type = "html", out = "modelo3.html")
