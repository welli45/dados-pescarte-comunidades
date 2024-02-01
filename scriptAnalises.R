###############################################################################
########### - Grupo de pesquisa: Análise de redes comunitárias ################
###############################################################################

# pacotes utilizados
if(!require(tidyverse)){install.packages('tidyverse')};library(tidyverse) 
if(!require(knitr)){install.packages('knitr')}; library(knitr)
if(!require(kableExtra)){install.packages('kableExtra')}; library(kableExtra)
if(!require(writexl)){install.packages('writexl')};library(writexl)

options(scipen = 999)

# Comunidades
comunidades <- c("Tocos", "Ponta Grossa dos Fidalgos", "Farol de São Tomé")

# setores censitarios

# Comunidade de Tocos
setoresTocos <- c('330100975000013','330100975000012','330100975000001','330100975000002',
                  '330100975000003','330100975000004','330100975000005','330100975000014')

# Comunidade de Ponta Grossa dos Fidalgos
setoresPontaGrossa <- c('330100975000009','330100975000008')

# Comunidade de Farol de São Tome
setoresFarol <- c('330100950000005','330100950000004','330100950000006','330100950000008',
                  '330100950000007','330100950000009','330100950000010','330100950000003',
                  '330100940000009','330100940000008','330100940000013','330100940000012',
                  '330100940000025','330100940000015','330100940000007','330100940000014',
                  '330100940000016','330100940000027','330100940000010')
##############################################################################################
# Arquivo basico - informações gerais dos domicílios
basicoRJ <- read_delim("Base informaçoes setores2010 universo RJ/CSV/Basico_RJ.csv",
                       delim = ";",locale = locale(decimal_mark = ",",encoding = "WINDOWS-1252")) |> 
  select(-'...34')

# Informacoes sobre a quantidade de domicilios particulares permanentes, 
# população residente em domicílios particulares permanentes e Valor do 
# rendimento nominal médio mensal das pessoas responsáveis por domicílios 
# particulares permanentes (com e sem rendimento)

# comunidad de Tocos
tocosBasico <- basicoRJ |> 
  filter(Cod_setor %in% c(setoresTocos)) |> 
  select('Cod_setor','V001','V002','V003','V005') |> 
  rename('domPartiPerm'='V001', 'moradoresDom' = 'V002', 'medMorDom'='V003',
         'rendMedNomDom' = 'V005')

write_xlsx(tocosBasico,"tocosBasico.xlsx")

tocosBasicosum <- tocosBasico |> 
  select(-Cod_setor,-medMorDom) |> 
  summarise_all(funs(sum))


kable(tocosBasico, caption = "Tocos: renda por número de moradores") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)


# Comunidade de Farol de São Tomé
farolBasico <- basicoRJ |>
  filter(Cod_setor %in% c(setoresFarol)) |> 
  select('Cod_setor','V001','V002','V003','V005') |> 
  rename('domPartiPerm'='V001', 'moradoresDom' = 'V002', 'medMorDom'='V003',
         'rendMedNomDom' = 'V005')

write_xlsx(farolBasico, 'farolBasico.xlsx')

farolBasicosum <- farolBasico |> 
  select(-Cod_setor, -medMorDom) |> 
  summarise_all(funs(sum))

kable(farolBasico, caption = "Farol de São Tomé: renda por número de moradores") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

# Comunidade de Ponta Grossa dos Fidalgos
pontaGrossaBasico <- basicoRJ |> 
  filter(Cod_setor %in% c(setoresPontaGrossa)) |> 
  select('Cod_setor','V001','V002','V003','V005') |> 
  rename('domPartiPerm'='V001', 'moradoresDom' = 'V002', 'medMorDom'='V003',
         'rendMedNomDom' = 'V005')

write_xlsx(pontaGrossaBasico,"pontaGrossaBasico.xlsx")

pontaGrossaBasicosum <- pontaGrossaBasico |> 
  select(-Cod_setor, -medMorDom) |> 
  summarise_all(funs(sum))

kable(pontaGrossaBasico, caption = "Ponta Grossa dos Fidalgos: renda por número de moradores") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

# Tabela geral
kable(basicoTotal <- full_join(
  tocosBasicosum, farolBasicosum, by = c(
    'domPartiPerm', 'moradoresDom', 'rendMedNomDom')) |> 
  full_join(pontaGrossaBasicosum) |> 
  transform(Comunidades = comunidades, medMorDom = moradoresDom/domPartiPerm) |> 
  select(4,1,2,5,3), caption = "Informações de renda e número de moradores por Comunidades") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)
  
write_xlsx(basicoTotal,"basicoTotal.xlsx")

###############################################################################################

# Cor ou Raça
pessoa03 <- read_delim("Base informaçoes setores2010 universo RJ/CSV/Pessoa03_RJ.csv",
                       delim = ";",locale = locale(decimal_mark = ",",encoding = "WINDOWS-1252"))

# comunidade de Tocos
kable(tocosPessoa01 <- pessoa03 |> 
  filter(Cod_setor %in% c(setoresTocos)) |> 
  select(Cod_setor,V001,V002,V003,V004,V005,V006) |> 
    transform(V001 = as.numeric(V001),V002 = as.numeric(V002),V003 = as.numeric(V003),
              V004 = as.numeric(V004), V005 = as.numeric(V005), V006 = as.numeric(V006)) |> 
  rename(moradoresDom=V001,branco=V002,preta=V003,amarela=V004,
         parda=V005,indigena=V006),caption = "Tocos: Qtd de moradores por Cor/Raça") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

write_xlsx(tocosPessoa01,"tocosPessoa03.xlsx")

tocosPessoa01sum <- tocosPessoa01 |> 
  select(-Cod_setor) |> 
  summarise_all(funs(sum))

# Comunidade de Farol de São Tomé
kable(farolPessoa01 <- pessoa03 |>
  filter(Cod_setor %in% c(setoresFarol)) |> 
  select(Cod_setor,V001,V002,V003,V004,V005,V006) |> 
    transform(V001 = as.numeric(V001),V002 = as.numeric(V002),V003 = as.numeric(V003),
              V004 = as.numeric(V004), V005 = as.numeric(V005), V006 = as.numeric(V006)) |> 
  rename(moradoresDom=V001,branco=V002,preta=V003,amarela=V004,
         parda=V005,indigena=V006), caption = "Farol de São Tomé: Qtd de moradores por Cor/Raça") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

write_xlsx(farolPessoa01,  'farolPessoa01.xlsx')

farolPessoa01SUM <- farolPessoa01 |> 
  select(-Cod_setor) |> 
  summarise_all(funs(sum))

# Comunidade de Ponta Grossa dos Fidalgos
kable(pontaGrossaPessoa01 <- pessoa03 |>
  filter(Cod_setor %in% c(setoresPontaGrossa)) |>
  select(Cod_setor,V001,V002,V003,V004,V005,V006) |>
    transform(V001 = as.numeric(V001),V002 = as.numeric(V002),V003 = as.numeric(V003),
              V004 = as.numeric(V004), V005 = as.numeric(V005), V006 = as.numeric(V006)) |> 
  rename(moradoresDom=V001,branco=V002,preta=V003,amarela=V004,
         parda=V005,indigena=V006),caption = "Ponta Grossa dos Fidalgos: Qtd de moradores por Cor/Raça") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

write_xlsx(pontaGrossaPessoa01,'pontaGrossaPessoa01.xlsx')

pontaGrossaPessoa01Sum <- pontaGrossaPessoa01 |> 
  select(-Cod_setor) |> 
  summarise_all(funs(sum))


kable(corRacaTotal <- full_join(
  tocosPessoa01sum, farolPessoa01SUM, by = c(
    'moradoresDom', "branco",'preta', 'amarela','parda','indigena')) |> 
    full_join(pontaGrossaPessoa01Sum) |> 
    transform(Comunidades = comunidades) |> 
    select(7,1,3,2,4,5,6), caption = "Qtd de moradores por Cor/Raça por comunidade") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

write_xlsx(corRacaTotal,'corRacaTotal.xlsx')
##################################################################################################

# número de domicilios por numero de moradores 
domicilio01 <- read_delim("Base informaçoes setores2010 universo RJ/CSV/Domicilio01_RJ.csv",
                          delim = ";",locale = locale(decimal_mark = ",",encoding = "WINDOWS-1252"))
# comunidade de Tocos
kable(tocosdomicilio01 <- domicilio01 |> 
  filter(Cod_setor %in% c(setoresTocos)) |> 
  select(Cod_setor,V001, V050:V059) |> 
    transform(V001=as.numeric(V001), V050=as.numeric(V050), V051=as.numeric(V051), V052=as.numeric(V052), 
              V053=as.numeric(V053), V054=as.numeric(V054), V055=as.numeric(V055), V056=as.numeric(V056), 
              V057=as.numeric(V057),V058=as.numeric(V058), V059=as.numeric(V059)) |> 
  rename(moradoresDom=V001,morador01=V050,morador02=V051,morador03=V052,morador04=V053,morador05=V054,
         morador06=V055,morador07=V056, morador08=V057, morador09=V058, morador10mais=V059),
  caption = "Tocos: número de domicílios por número de moradores") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

write_xlsx(tocosdomicilio01,'tocosdomicilio01.xlsx')

tocosdomicilio01sum <- tocosdomicilio01 |> 
  select(-Cod_setor) |> 
  summarise_all(funs(sum))

# Comunidade de Farol de São Tomé
kable(faroldomicilio01 <- domicilio01 |>
  filter(Cod_setor %in% c(setoresFarol)) |> 
  select(Cod_setor,V001, V050:V059) |> 
    transform(V001=as.numeric(V001), V050=as.numeric(V050), V051=as.numeric(V051), 
              V052=as.numeric(V052),V053=as.numeric(V053), V054=as.numeric(V054), 
              V055=as.numeric(V055), V056=as.numeric(V056),V057=as.numeric(V057),V058=as.numeric(V058), 
              V059=as.numeric(V059)) |> 
  rename(moradoresDom=V001,morador01=V050,morador02=V051,morador03=V052,morador04=V053,morador05=V054,
         morador06=V055,morador07=V056, morador08=V057, morador09=V058, morador10mais=V059),
caption = "Farol de São Tomé: número de moradores por domicílio") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

write_xlsx(faroldomicilio01,'faroldomicilio01.xlsx')

faroldomicilio01sum <- faroldomicilio01 |> 
  select(-Cod_setor) |> 
  summarise_all(funs(sum))

# Comunidade de Ponta Grossa dos Fidalgos
kable(pontaGrossadomicilio01 <- domicilio01 |>
  filter(Cod_setor %in% c(setoresPontaGrossa)) |>
  select(Cod_setor, V001, V050:V059) |> 
    transform(V001=as.numeric(V001), V050=as.numeric(V050), V051=as.numeric(V051), 
              V052=as.numeric(V052),V053=as.numeric(V053), V054=as.numeric(V054), 
              V055=as.numeric(V055), V056=as.numeric(V056),V057=as.numeric(V057),V058=as.numeric(V058), 
              V059=as.numeric(V059)) |>
  rename(moradoresDom=V001,morador01=V050,morador02=V051,morador03=V052,morador04=V053,morador05=V054,
         morador06=V055,morador07=V056, morador08=V057, morador09=V058, morador10mais=V059), 
  caption = "Ponta Grossa dos Fidalgos: número de de moradores por domicílio",) |>
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

write_xlsx(pontaGrossadomicilio01,'pontaGrossadomicilio01.xlsx')


pontaGrossadomicilio01sum <- pontaGrossadomicilio01 |> 
  select(-Cod_setor) |> 
  summarise_all(funs(sum))


kable(domTotal <- full_join(
  tocosdomicilio01sum, faroldomicilio01sum, by = c('moradoresDom','morador01','morador02','morador03','morador04','morador05',
    'morador06','morador07', 'morador08', 'morador09', 'morador10mais')) |> 
    full_join(pontaGrossadomicilio01sum) |> 
    transform(Comunidades = comunidades) |>  
  select(12,1:11),caption = "Número de moradores por domicílio") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

write_xlsx(domTotal,'domTotal.xlsx')

######################################################################################

# Pessoa Responsável
responsavelSexoFemi <- read_delim("Base informaçoes setores2010 universo RJ/CSV/Responsavel01_Rj.csv",
                          delim = ";",locale = locale(decimal_mark = ",",encoding = "WINDOWS-1252"))
responsavelSexoMasc <- read_delim("Base informaçoes setores2010 universo RJ/CSV/Responsavel02_Rj.csv",
                                 delim = ";",locale = locale(decimal_mark = ",",encoding = "WINDOWS-1252"))

# comunidade de Tocos
respFemiTocos <- responsavelSexoFemi |>
  filter(Cod_setor %in% c(setoresTocos)) |> 
  select(Cod_setor, V001) |> 
  rename(Feminino = V001)

respMascTocos <- responsavelSexoMasc |>
  filter(Cod_setor %in% c(setoresTocos)) |> 
  select(Cod_setor, V001,V109) |> 
  rename(Masculino = V109, NumPessoasResp=V001) |> 
  transform(Masculino=as.numeric(Masculino))


kable(tocosSexo <- inner_join(respFemiTocos,respMascTocos, by="Cod_setor") |> 
  select(Cod_setor,NumPessoasResp,Feminino,Masculino),
  caption = "Tocos: número de pessoas Responsáveis por sexo") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

write_xlsx(tocosSexo,'tocosSexo.xlsx')

tocosSexoSum <- tocosSexo |> 
  select(-Cod_setor) |> 
  summarise_all(funs(sum))

# Comunidade de Farol de São Tomé
respFemiFarol <- responsavelSexoFemi |>
  filter(Cod_setor %in% c(setoresFarol)) |> 
  select(Cod_setor, V001) |> 
  rename(Feminino = V001)

respMascFarol <- responsavelSexoMasc |>
  filter(Cod_setor %in% c(setoresFarol)) |> 
  select(Cod_setor, V001,V109) |> 
  rename(Masculino = V109, NumPessoasResp=V001) |> 
  transform(Masculino=as.numeric(Masculino))

kable(farolSexo <- inner_join(respFemiFarol,respMascFarol, by="Cod_setor") |> 
  select(Cod_setor,NumPessoasResp,Feminino,Masculino),
  caption = "Farol de São Tomé: número de pessoas Responsáveis por sexo") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

write_xlsx(farolSexo,'farolSexo.xlsx')

farolSexoSum <- farolSexo |> 
  select(-Cod_setor) |> 
  summarise_all(funs(sum))

# Comunidade de Ponta Grossa dos Fidalgos
respFemiPontaGrossa <- responsavelSexoFemi |>
  filter(Cod_setor %in% c(setoresPontaGrossa)) |> 
  select(Cod_setor, V001) |> 
  rename(Feminino = V001)

respMascPontaGrossa <- responsavelSexoMasc |>
  filter(Cod_setor %in% c(setoresPontaGrossa)) |> 
  select(Cod_setor, V001,V109) |> 
  rename(Masculino = V109, NumPessoasResp=V001) |> 
  transform(Masculino = as.numeric(Masculino))

kable(PontaGrossaSexo <- inner_join(respMascPontaGrossa,respFemiPontaGrossa, by="Cod_setor") |> 
  select(Cod_setor,NumPessoasResp,Feminino,Masculino),
  caption = "Ponta Grossa dos Fidalgos: número de pessoas Responsáveis por sexo") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)

write_xlsx(PontaGrossaSexo,'PontaGrossaSexo.xlsx')

pontaGrossaSexoSum <- PontaGrossaSexo |> 
  select(-Cod_setor) |> 
  summarise_all(funs(sum))

# tabela de totais por comunidades
tabSexoTotal <- full_join(
  tocosSexoSum, farolSexoSum, by = c(
    'Masculino', 'Feminino', 'NumPessoasResp')) |>
  full_join(pontaGrossaSexoSum) |> 
  transform(Comunidades = comunidades) |>
  select(4,1,2,3)

write_xlsx(tabSexoTotal,"tabSexoTotal.xlsx")

kable(tabSexoTotal,caption = "Numero de pessoas responsáveis por sexo e comunidades") |> 
  add_footnote("Fonte: IBGE - CENSO 2010") |> 
  kable_styling(full_width = F)




