library(tidyverse)
library(data.table)

## rm acento. Função criada por Andre Felix
convert_special_char <- function(x){
  lstSpecialChar <- list(c('[aàáâäãåæ]', 'a'),
                         c('[eèéêë]', 'e'),
                         c('[iìíîï]', 'i'),
                         c('[oòóôöõø]', 'o'),
                         c('[uùúûü]', 'u'),
                         c('[ñ]', 'n'),
                         c('[ç]', 'c')
  )
  x <- tolower(x)
  for (i in 1:length(lstSpecialChar)) {
    x <- gsub(unlist(lstSpecialChar[i])[1], unlist(lstSpecialChar[i])[2], x)
  }
  x <- toupper(x)
}

## Base completa perfil abst TSE

tseTODO<- fread("perfil_comparecimento_abstencao_2018.csv", encoding = "Latin-1")


summary(tseTODO)
head(tseTODO, 10)
tail(tseTODO)

#tirando horario, ano de elição
tse1<- tseTODO[,-c(1,2,3,4,6,8)]
names(tse1)

## filtrar TSE POR GENERO

base_tseMulher <- tse1 %>% filter(DS_GENERO =="FEMININO")
names(base_tseMulher)
base_tseMulher <- base_tseMulher[,-c(4,6,8,10)]

rm(tse1)

glimpse(base_tseMulher)
write.csv(base_tseMulher, "tseMulher.csv")


###### PBF, selecionando as colunas desejadas e renomeando-as
# tirando frequencia para somar por municipio.

rm(pbf)
pbf <- fread("/home/luana/Link para IESP/2019_discp/Lego II/trab 2/bF/201809_BolsaFamilia_Pagamentos.csv", 
                 encoding = "Latin-1")
head(pbf)
bolsaFM <-subset(pbf, select =c("UF", "NOME MUNICÍPIO", "NIS FAVORECIDO", "VALOR PARCELA")) %>% 
          rename( SG_UF = UF,
          NM_MUNICIPIO = "NOME MUNICÍPIO",
          NIS_FAVORECIDO = "NIS FAVORECIDO",
          VR_PARCELA = "VALOR PARCELA")
rm(pbf)
bolsaFM$NM_MUNICIPIO <- convert_special_char(bolsaFM$NM_MUNICIPIO)

write.csv(bolsaFM, "bolsaFM.csv")

bolsaFamilia <- fread("bolsaFM.csv")
bolsaFamilia$VR_PARCELA <- as.numeric(gsub(",", ".", bolsaFamilia$VR_PARCELA))
glimpse(bolsaFamilia)

#group_by() de municipio e tirando as DS_
baseMunicipio <- base_tseMulher %>% 
  group_by(NM_MUNICIPIO) %>% 
  mutate(total_abstencao=sum(QT_ABSTENCAO)) %>% 
  mutate(total_aptos=sum(QT_APTOS)) %>% 
  mutate(perc_abstencao = total_abstencao/total_aptos)

head(baseMunicipio)
rm(baseMunicipio)
rm(base_tseMulher)
baseMunicipio<- baseMunicipio[,-c(4,6,8,10)]


names(baseMunicipio)
# bolsaFamilia aggreate
bolsaFM <- aggregate(bolsaFM,
          by = list(bolsaFM$NM_MUNICIPIO, bolsaFM$SG_UF),
          FUN= length)
head(bolsaFM)
glimpse(bolsaFamilia)
rm(bolsaFM)

# criando novas colunas com o total de receita,
#total de favorecidos e a média disso por município
base_PBF <- bolsaFamilia %>% 
  group_by(NM_MUNICIPIO) %>% 
  mutate(total_receita=sum(VR_PARCELA)) %>% 
  mutate(total_quantidade=length(NM_MUNICIPIO)) %>% 
  filter(!duplicated(NM_MUNICIPIO)) %>% 
  #filter(!duplicated(SG_UF))
  mutate(perc_media = total_receita/total_quantidade)

glimpse(bolsaFamilia)
rm(bolsaFamilia)
bolsaFamilia$NIS_FAVORECIDO <- NULL
names(base_PBF)
write.csv2(base_PBF, "basePBFcomMediaporMunicipio.csv")

base_PBF$NM_MUNICIPIO <- convert_special_char(base_PBF$NM_MUNICIPIO)



# MUDAR NOMES
paste(names(bolsaFM), collapse = "','")  

names(bolsaFM) <- c('NM_MUNICIPIO','SG_UF','N','n', 'n2')

names(bolsaFM)
bolsaFM <- bolsaFM[,-c(4,5)]

baseMunicipio$NM_MUNICIPIO <- convert_special_char(baseMunicipio$NM_MUNICIPIO)



#juntar tse com PBF por nm_municipio
rm(tse_pbf)
tse_pbf <- merge(baseMunicipio,
                 bolsaFM,
                 by = c("NM_MUNICIPIO", "SG_UF"),
                 all.x = T)

names(baseMunicipio)
names(bolsaFM)
head(tse_pbf)


write.csv(tse_pbf, "tse_pbf.csv")


#### AGRUPAR POR ESCOLARIDADE (** depois fazer por genero)
names(tse1)
tseESCOLARIDADE <- tse1%>% 
  group_by(CD_GRAU_ESCOLARIDADE) %>% 
  mutate(total_abstencao=sum(QT_ABSTENCAO)) %>% 
  mutate(total_aptos=sum(QT_APTOS)) %>% 
  #filter(!duplicated(cod_ib)) %>% 
  mutate(abst_ESCOLARIDADE = total_abstencao/total_aptos)

head(tseESCOLARIDADE)

##### TSE mulher por abstenção

tseMulher <- fread("/home/luana/Link para IESP/2019_discp/Lego II/trab 2/tseMulher.csv")

tse <- tseMulher %>% 
  group_by(NM_MUNICIPIO) %>% 
  mutate(tt_abstencao=sum(QT_ABSTENCAO)) %>% 
  mutate(tt_aptos=sum(QT_APTOS)) %>% 
  filter(!duplicated(NM_MUNICIPIO)) %>% 
  mutate(perc_abstencao = tt_abstencao/tt_aptos)

rm(tseMulher)
names(tse)
tse<- tse[,-c(1,5,7,9,11,)]
head(tse)
tse_limpo <- tse[,-c(7,8,9,10,11,12,13)]
write.csv2(tse, "tseMulhercomTTabst.csv")
write.csv2(tse_limpo, "tseMulher_limpo.csv")
names(tse_limpo)

### tse_limpoMulheres com pbf merde
names(base_PBF)
base_PBF$NM_MUNICIPIO <- convert_special_char(base_PBF$NM_MUNICIPIO)
tse_limpo$NM_MUNICIPIO <- convert_special_char(tse_limpo$NM_MUNICIPIO)

juntadaMulherTSE <- merge.data.frame(base_PBF, tse_limpo,
                                    by= "NM_MUNICIPIO", all = T)
juntTSEna <- merge.data.frame(base_PBF, tse_limpo,
                              by= "NM_MUNICIPIO", all = F)
summary(juntTSEna)

names(juntadaMulherTSE)
juntadaMulherTSE <- juntadaMulherTSE %>% 
  rename(SG_UF = SG_UF.x)

juntadaMulherTSE$V1 <- NULL
juntadaMulherTSE$SG_UF.y <- NULL
juntadaMulherTSE$VR_PARCELA <- NULL
names(juntadaMulherTSE)

write.csv2(juntadaMulherTSE, "juntadaMulherTSE.csv")

juntTSEna <- juntTSEna %>% 
  rename(SG_UF = SG_UF.x)

juntTSEna$V1 <- NULL
juntTSEna$SG_UF.y <- NULL
juntTSEna$VR_PARCELA <- NULL

write.csv2(juntTSEna, "juntTSEsemNA.csv")

