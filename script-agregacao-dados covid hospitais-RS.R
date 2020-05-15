


# http://ti.saude.rs.gov.br/covid19/export_seplag/
# usuario = seplag
# senha = s3pl4gcovid19


library(stringi)
library(dplyr)
library(readxl)


files <- list.files("C:/Users/Fernanda/Downloads", full.names=T, pattern="csv")

id.file <- files[grep('SES_COVID19_', files)][which(grepl(as.character(Sys.Date()), files[grep('SES_COVID19_', files)]))]


## DADOS RS

df.rs <- read.csv(id.file, head=TRUE)
dim(df.rs)
df.rs$CLASSIFICACAO <- as.character(df.rs$CLASSIFICACAO)

df.rs <- df.rs[which(df.rs$CLASSIFICACAO=="RT-PCR"),]
head(df.rs)

df.rs$DT_COLETA <- as.Date(as.character(df.rs$DT_COLETA), format="%d/%m/%y")
df.rs$DT_OBITO  <- as.Date(as.character(df.rs$DT_OBITO), format="%d/%m/%y")
df.rs$DT_CONFIRMACAO <- as.Date(as.character(df.rs$DT_CONFIRMACAO), format="%d/%m/%y")

var <- "DT_COLETA"
names(df.rs)[which(names(df.rs)==var)] <- "DATA_CASOS"

df.rs$OBITO_NUM <- 0
df.rs$OBITO_NUM[which(df.rs$OBITO=="Sim")] <- 1
table(df.rs$OBITO_NUM)

df.rs$CASOS_NUM <- 1
table(df.rs$CASOS_NUM)


df.rmpa <- read.csv2("C:\\Users\\Fernanda\\Desktop\\Curso de Introdução ao R\\bases\\RMPA_municipios.csv", head=TRUE)
head(df.rmpa)
#Encoding(df.rmpa$Região.Metropolitana) <- "UTF-8"

df.rs$NM_MUNICIPIO <- as.character(df.rs$NM_MUNICIPIO)
df.rmpa$Região.Metropolitana <- as.character(df.rmpa$Região.Metropolitana)

df.rs$REGIAO <- "DEMAIS" 
df.rs$REGIAO[which(sapply(1:length(df.rs$NM_MUNICIPIO), function(x)
  sum(df.rmpa$Região.Metropolitana %in% df.rs$NM_MUNICIPIO[x])) == 1)] <- "RMPA"
table(df.rs$REGIAO)


dia.min <- min(c(df.rs$DATA_CASOS,df.rs$DT_OBITO), na.rm=TRUE)
dia.max <- max(c(df.rs$DATA_CASOS,df.rs$DT_OBITO), na.rm=TRUE)

seq.dias <- seq(as.Date(dia.min), as.Date(dia.max),"day")
length(seq.dias)

datas.ausentes <- seq.dias[which(!(seq.dias %in% unique(c(df.rs$DATA_CASOS, df.rs$DT_OBITO[!is.na(df.rs$DT_OBITO)]))))]


# Agregando dados RS

df.rs.casos <- df.rs %>% 
  group_by(DATA_CASOS) %>% 
  summarise(Frequency = sum(CASOS_NUM))

df.rs.mortes <- df.rs[!is.na(df.rs$DT_OBITO),] %>% 
  group_by(DT_OBITO, OBITO) %>% 
  summarise(Frequency = sum(OBITO_NUM))


df.rs.merge <- merge(df.rs.casos, df.rs.mortes[,-2], by.x="DATA_CASOS", by.y="DT_OBITO", all=TRUE)
df.rs.merge[is.na(df.rs.merge)] <- 0

names(df.rs.merge) <- c("Data", "Numero de casos", "Numero de mortes")
vars <- names(df.rs.merge) 


df.rs.merge[nrow(df.rs.merge)+length(datas.ausentes),] <- NA
tail(df.rs.merge)
df.rs.merge$Data[is.na(df.rs.merge$Data)] <- datas.ausentes


df.rs.merge <- data.frame(cbind(df.rs.merge, rep("RS", dim(df.rs.merge)[1])))
names(df.rs.merge) <- c(vars, "Regiao")
dim(df.rs.merge)

df.rs.merge <- df.rs.merge[order(df.rs.merge$Data),]
head(df.rs.merge)

df.rs.merge[is.na(df.rs.merge)] <- 0
df.rs.merge$`Numero de casos acumulados` <- cumsum(df.rs.merge$`Numero de casos`)
df.rs.merge$`Numero de mortes acumuladas` <- cumsum(df.rs.merge$`Numero de mortes`)


df.rs.merge <- df.rs.merge[, c("Data", "Regiao", "Numero de casos", "Numero de mortes", "Numero de casos acumulados", 
                               "Numero de mortes acumuladas")]


# Agregando dados PORTO ALEGRE

df.poa.casos <- df.rs %>% 
  filter(NM_MUNICIPIO == "Porto Alegre") %>%
  group_by(DATA_CASOS) %>% 
  summarise(Frequency = sum(CASOS_NUM)) %>%
  mutate(Regiao = rep("Porto Alegre", n()))


df.poa.mortes <- df.rs[!is.na(df.rs$DT_OBITO),] %>% 
  filter(NM_MUNICIPIO == "Porto Alegre") %>%
  group_by(DT_OBITO, OBITO) %>% 
  summarise(Frequency = sum(OBITO_NUM)) %>%
  mutate(Regiao = rep("Porto Alegre", n()))


df.poa.merge <- merge(df.poa.casos, df.poa.mortes[,-2], by.x="DATA_CASOS", by.y="DT_OBITO", all=TRUE)
dim(df.poa.merge)
names(df.poa.merge) <- c("Data", "Numero de casos", "Regiao", "Numero de mortes", "Drop")


df.rs.merge$Data[!(df.rs.merge$Data %in% df.poa.merge$Data)]

df.poa.merge[nrow(df.poa.merge)+length(which(!(df.rs.merge$Data %in% df.poa.merge$Data))),] <- NA
df.poa.merge$Data[is.na(df.poa.merge$Data)] <- df.rs.merge$Data[!(df.rs.merge$Data %in% df.poa.merge$Data)]
df.poa.merge$Regiao[is.na(df.poa.merge$Regiao)] <- "Porto Alegre"

df.poa.merge <- df.poa.merge[order(df.poa.merge$Data),]

df.poa.merge[is.na(df.poa.merge)] <- 0

df.poa.merge$`Numero de casos acumulados` <- cumsum(df.poa.merge$`Numero de casos`)
df.poa.merge$`Numero de mortes acumuladas` <- cumsum(df.poa.merge$`Numero de mortes`)

df.poa.merge <- df.poa.merge[, c("Data", "Regiao", "Numero de casos", "Numero de mortes", "Numero de casos acumulados",  
                                 "Numero de mortes acumuladas")]
head(df.poa.merge)
dim(df.poa.merge)
dim(df.rs.merge)



# Agregando dados RMPA 

df.rmpa.casos <- df.rs %>% 
  filter(REGIAO == "RMPA") %>%
  group_by(DATA_CASOS) %>% 
  summarise(Frequency = sum(CASOS_NUM)) %>%
  mutate(Regiao = rep("RMPA", n()))


df.rmpa.mortes <- df.rs[!is.na(df.rs$DT_OBITO),] %>% 
  filter(REGIAO == "RMPA") %>%
  group_by(DT_OBITO, OBITO) %>% 
  summarise(Frequency = sum(OBITO_NUM)) %>%
  mutate(Regiao = rep("RMPA", n()))


df.rmpa.merge <- merge(df.rmpa.casos, df.rmpa.mortes[,-2], by.x="DATA_CASOS", by.y="DT_OBITO", all=TRUE)
dim(df.rmpa.merge)
names(df.rmpa.merge) <- c("Data", "Numero de casos", "Regiao", "Numero de mortes", "Drop")

df.rs.merge$Data[!(df.rs.merge$Data %in% df.rmpa.merge$Data)]

df.rmpa.merge[nrow(df.rmpa.merge)+length(which(!(df.rs.merge$Data %in% df.rmpa.merge$Data))),] <- NA
df.rmpa.merge$Data[is.na(df.rmpa.merge$Data)] <- df.rs.merge$Data[!(df.rs.merge$Data %in% df.rmpa.merge$Data)]
df.rmpa.merge$Regiao[is.na(df.rmpa.merge$Regiao)] <- "RMPA"

df.rmpa.merge <- df.rmpa.merge[order(df.rmpa.merge$Data), ]

df.rmpa.merge[is.na(df.rmpa.merge)] <- 0

df.rmpa.merge$`Numero de casos acumulados` <- cumsum(df.rmpa.merge$`Numero de casos`)
df.rmpa.merge$`Numero de mortes acumuladas` <- cumsum(df.rmpa.merge$`Numero de mortes`)

df.rmpa.merge <- df.rmpa.merge[, c("Data", "Regiao", "Numero de casos", "Numero de mortes", "Numero de casos acumulados",  
                                   "Numero de mortes acumuladas")]
head(df.rmpa.merge)
dim(df.rmpa.merge)
dim(df.rs.merge)
dim(df.poa.merge)



## DADOS HOSPITALARES ##
df.hosp <- read.table("C:/Users/Fernanda/Downloads/Hospitais_Periodo_Total_08-05.txt", header=TRUE, 
                      sep=";", encoding="UTF-8")
head(df.hosp)


list.data <- lapply(1:dim(df.hosp)[1], function(x)
  strsplit(as.character(df.hosp$Data.da.Última.Atualização.Considerada), " ")[[x]][1]
)

df.hosp$Data <- as.Date(unlist(list.data))

dias <- unique(df.hosp$Data)[order(unique(df.hosp$Data))]

# verificando se há duplicacao do codigo cnes nas datas
table(sapply(1:length(dias), function(x)
  sum(duplicated(df.hosp$Cod.CNES[which(df.hosp$Data==dias[x])]))))

vars.select <- c("CONFIRMADOS.UTI", "SUSPEITOS.E.CONFIRMADOS.EM.UTI", "TOTAL.PACIENTES.UTI", "LEITOS.UTI.LIVRES", 
          "CONFIRMADOS.FORA.DA.UTI", "SUSPEITOS.E.CONFIRMADOS.FORA.DA.UTI", 
          "CONFIRMADOS.UTI.PEDIÁTRICOS", "SUSPEITOS.E.CONFIRMADOS.EM.UTI.PEDIÁTRICO", 
          "CONFIRMADOS.FORA.DA.UTI.PEDIÁTRICO", "SUSPEITOS.E.CONFIRMADOS.FORA.DA.UTI.PEDIÁTRICO")

head(df.hosp[which(df.hosp$Data==dias[1]), vars.select])


list.dias <- lapply(1:length(dias), function(x)
  data.frame(matrix(c(as.character(dias[x]), "RS", apply(df.hosp[which(df.hosp$Data==dias[x]), vars.select], 2, sum)), nrow=1,
                    dimnames = list(c(),c("DATA", "REGIAO", vars.select))), stringsAsFactors=FALSE)
)

df.dias.RS <- do.call(rbind, list.dias)
class(df.dias.RS)
head(df.dias.RS)

df.dias.RS$CONFIRMADOS.UTI.COVID <- 
  (as.numeric(df.dias.RS$CONFIRMADOS.UTI) + as.numeric(df.dias.RS$CONFIRMADOS.UTI.PEDIÁTRICOS))

df.dias.RS$CONFIRMADOS.CLINICOS.COVID <- 
  (as.numeric(df.dias.RS$CONFIRMADOS.FORA.DA.UTI) + as.numeric(df.dias.RS$CONFIRMADOS.FORA.DA.UTI.PEDIÁTRICO))



df.hosp$Município <- as.character(df.hosp$Município)

df.hosp.POA <- df.hosp[which(df.hosp$Município=="Porto Alegre"),]
head(df.hosp.POA)

list.dias.POA <- lapply(1:length(dias), function(x)
  data.frame(matrix(c(as.character(dias[x]), "Porto Alegre", apply(df.hosp.POA[which(df.hosp.POA$Data==dias[x]), vars.select], 2, sum)), nrow=1,
                    dimnames = list(c(),c("DATA", "REGIAO", vars.select))), stringsAsFactors=FALSE)
)

df.dias.POA <- do.call(rbind, list.dias.POA)
class(df.dias.POA)
head(df.dias.POA)

df.dias.POA$CONFIRMADOS.UTI.COVID <- 
  (as.numeric(df.dias.POA$CONFIRMADOS.UTI) + as.numeric(df.dias.POA$CONFIRMADOS.UTI.PEDIÁTRICOS))

df.dias.POA$CONFIRMADOS.CLINICOS.COVID <- 
  (as.numeric(df.dias.POA$CONFIRMADOS.FORA.DA.UTI) + as.numeric(df.dias.POA$CONFIRMADOS.FORA.DA.UTI.PEDIÁTRICO))



dim(df.rmpa)
head(df.rmpa)
# número de municipios da RMPA que possuem casos confirmados
#table(sapply(lapply(df.rmpa$Região.Metropolitana, function(x)
#  which(df.hosp$Município==x)), length)!=0)

lins.rmpa <- unlist(lapply(df.rmpa$Região.Metropolitana, function(x)
which(df.hosp$Município==x)))

df.hosp.RMPA <- df.hosp[lins.rmpa,]
head(df.hosp.RMPA)
#table(df.hosp.RMPA$Município)


list.dias.RMPA <- lapply(1:length(dias), function(x)
  data.frame(matrix(c(as.character(dias[x]), "RMPA", apply(df.hosp.RMPA[which(df.hosp.RMPA$Data==dias[x]), vars.select], 2, sum)), nrow=1,
                    dimnames = list(c(),c("DATA", "REGIAO", vars.select))), stringsAsFactors=FALSE)
)

df.dias.RMPA <- do.call(rbind, list.dias.RMPA)
class(df.dias.RMPA)
head(df.dias.RMPA)

df.dias.RMPA$CONFIRMADOS.UTI.COVID <- 
  (as.numeric(df.dias.RMPA$CONFIRMADOS.UTI) + as.numeric(df.dias.RMPA$CONFIRMADOS.UTI.PEDIÁTRICOS))

df.dias.RMPA$CONFIRMADOS.CLINICOS.COVID <- 
  (as.numeric(df.dias.RMPA$CONFIRMADOS.FORA.DA.UTI) + as.numeric(df.dias.RMPA$CONFIRMADOS.FORA.DA.UTI.PEDIÁTRICO))



head(df.rs.merge)

df.merge.RS <- merge(df.rs.merge, df.dias.RS, by.x = c("Data", "Regiao"), by.y = c("DATA", "REGIAO"), all.x=TRUE)
#View(df.merge.RS)

df.merge.POA <- merge(df.poa.merge, df.dias.POA, by.x = c("Data", "Regiao"), by.y = c("DATA", "REGIAO"), all.x=TRUE)
#View(df.merge.POA)

df.merge.RMPA <- merge(df.rmpa.merge, df.dias.RMPA, by.x = c("Data", "Regiao"), by.y = c("DATA", "REGIAO"), all.x=TRUE)
#View(df.merge.RMPA)


df.total <- data.frame(rbind(df.merge.RS, df.merge.POA, df.merge.RMPA))
df.total[is.na(df.total)] <- ""
#View(df.total)
dim(df.total)

names(df.total) <- gsub('\\.', ' ', names(df.total))

df.total <- df.total[,c("Data", "Regiao", "Numero de casos", "Numero de mortes", "Numero de casos acumulados", 
  "Numero de mortes acumuladas", "CONFIRMADOS UTI COVID", "CONFIRMADOS CLINICOS COVID",
  "CONFIRMADOS UTI", "SUSPEITOS E CONFIRMADOS EM UTI", 
  "TOTAL PACIENTES UTI", "LEITOS UTI LIVRES", "CONFIRMADOS FORA DA UTI", 
  "SUSPEITOS E CONFIRMADOS FORA DA UTI", "CONFIRMADOS UTI PEDIÁTRICOS", 
  "SUSPEITOS E CONFIRMADOS EM UTI PEDIÁTRICO", "CONFIRMADOS FORA DA UTI PEDIÁTRICO", 
  "SUSPEITOS E CONFIRMADOS FORA DA UTI PEDIÁTRICO")]


write.csv2(df.total, "Dados.Dt_Coleta-Hospitais.csv", 
           row.names=FALSE, fileEncoding = "UTF-8", na="")




## DADOS HOSPITALARES POR REGIAO ##

df.reg <- data.frame(read_excel("C:\\Users\\Fernanda\\Desktop\\Curso de Introdução ao R\\bases\\mapa_municipios_20regioes_24042020.xlsx", 
                                sheet = 1, col_names = TRUE))
head(df.reg)

df.nome.reg <- read.csv2("C:\\Users\\Fernanda\\Desktop\\Curso de Introdução ao R\\bases\\Nome Regiao Covid.csv", 
                         header=TRUE, stringsAsFactors=FALSE)
head(df.nome.reg)

df.nome.reg$Regiao.COVID <- stringi::stri_trim(df.nome.reg$Regiao.COVID)
df.nome.reg$Nome.Regiao  <- gsub("_", " R", gsub("r_", "R", paste0(df.nome.reg$Regiao.COVID, " - ", df.nome.reg$Reg)))



regions <- unique(df.reg$vinculo_20_regioes)

list.munic <- lapply(1:length(regions), function(x)
  df.reg$muni[which(df.reg$vinculo_20_regioes==regions[x])])

length(regions)
length(list.munic)


df.rs$REGIAO.COVID <- ""

eval(parse(text = paste0("df.rs$REGIAO.COVID[which(df.rs$NM_MUNICIPIO %in% list.munic[[",
       1:length(regions), "]])] <- regions[", 1:length(regions), "]")))

sum(table(df.rs$REGIAO.COVID))
dim(df.rs)
head(df.rs)


list.cases.in <- lapply(1:length(regions), function(x)
  df.rs[which(df.rs$REGIAO.COVID==regions[x]),] %>% 
    group_by(DATA_CASOS) %>% 
    arrange(DATA_CASOS) %>%
    summarise(casos = sum(CASOS_NUM)) 
)
#seq.dias 
list.cases.out <- lapply(1:length(regions), function(x)
data.frame(matrix(cbind(as.character(seq.dias[!(seq.dias %in% list.cases.in[[x]]$DATA_CASOS)]),
rep(0, sum(!(seq.dias %in% list.cases.in[[x]]$DATA_CASOS)))), ncol=2, 
dimnames = list(c(),c("DATA", "casos"))), stringsAsFactors=FALSE)
)

list.cases <- lapply(1:length(list.cases.in), function(x)
bind_rows(
  list.cases.in[[x]], list.cases.out[[x]] %>% as_tibble() %>%
    mutate(DATA_CASOS = as.Date(as.character(DATA))) %>%
    mutate(casos = as.numeric(casos)) %>%
    select(-1))  %>% 
  arrange(DATA_CASOS) %>%
  #summarise(casos = sum(CASOS_NUM)) %>%
  mutate(cum_casos=cumsum(casos)) %>%
  mutate(REGIAO.COVID = rep(regions[x]))
)

class(list.cases)
df.cases <- data.frame(plyr::ldply(list.cases, rbind)) %>% as_tibble()
head(df.cases)



list.dead.in <- lapply(1:length(regions), function(x)
  df.rs[which(df.rs$REGIAO.COVID==regions[x]),] %>% 
    group_by(DATA_CASOS) %>% 
    arrange(DATA_CASOS) %>%
    summarise(mortes = sum(OBITO_NUM)) 
)
#seq.dias 
list.dead.out <- lapply(1:length(regions), function(x)
  data.frame(matrix(cbind(as.character(seq.dias[!(seq.dias %in% list.dead.in[[x]]$DATA_CASOS)]),
                          rep(0, sum(!(seq.dias %in% list.dead.in[[x]]$DATA_CASOS)))), ncol=2, 
                    dimnames = list(c(),c("DATA", "mortes"))), stringsAsFactors=FALSE)
)

list.dead <- lapply(1:length(list.dead.in), function(x)
  bind_rows(
    list.dead.in[[x]], list.dead.out[[x]] %>% as_tibble() %>%
      mutate(DATA_CASOS = as.Date(as.character(DATA))) %>%
      mutate(mortes = as.numeric(mortes)) %>%
      select(-1))  %>% 
    arrange(DATA_CASOS) %>%
    #summarise(casos = sum(CASOS_NUM)) %>%
    mutate(cum_mortes=cumsum(mortes)) %>%
    mutate(REGIAO.COVID = rep(regions[x]))
)

class(list.dead)
df.dead <- data.frame(plyr::ldply(list.dead, rbind)) %>% as_tibble()
head(df.dead)


df.cases.dead <- full_join(df.cases, df.dead, by=c("DATA_CASOS","REGIAO.COVID")) 
head(df.cases.dead)

#View(df.cases.dead[which(df.cases.dead$REGIAO.COVID=="r_22"),])



## AGREGANDO DADOS DOS HOSPITAIS NAS REGIOES ##
head(df.hosp)

vars.reg <- c("Data", "CONFIRMADOS.UTI", "CONFIRMADOS.FORA.DA.UTI", 
              "CONFIRMADOS.UTI.PEDIÁTRICOS", "CONFIRMADOS.FORA.DA.UTI.PEDIÁTRICO")

list.hosp <- lapply(1:length(list.munic), function(x)
  df.hosp[which(df.hosp$Município %in% list.munic[[x]]), vars.reg]
)
#View(df.hosp[which(df.hosp$Município %in% list.munic[[9]]), vars.reg])

df.list.hosp.aux <- lapply(1:length(list.munic), function(x) 
list.hosp[[x]] %>% 
  group_by(Data) %>% 
  arrange(Data) %>%
  summarise(CONFIRMADOS.UTI = sum(CONFIRMADOS.UTI),
            CONFIRMADOS.UTI.PEDIÁTRICOS = sum(CONFIRMADOS.UTI.PEDIÁTRICOS),
            CONFIRMADOS.FORA.DA.UTI = sum(CONFIRMADOS.FORA.DA.UTI),
            CONFIRMADOS.FORA.DA.UTI.PEDIÁTRICO = sum(CONFIRMADOS.FORA.DA.UTI.PEDIÁTRICO)) %>%
  mutate(CONFIRMADOS.UTI.COVID = CONFIRMADOS.UTI+CONFIRMADOS.UTI.PEDIÁTRICOS) %>%
  mutate(CONFIRMADOS.FORA.UTI.COVID = CONFIRMADOS.FORA.DA.UTI+CONFIRMADOS.FORA.DA.UTI.PEDIÁTRICO) %>%
  mutate(REGIAO.COVID = rep(regions[x]))
)

df.list.hosp.aux[[1]]
class(df.list.hosp.aux)

df.list.hosp <- data.frame(plyr::ldply(df.list.hosp.aux, rbind)) %>% as_tibble() %>% rename(DATA_CASOS = Data) %>% arrange(DATA_CASOS)
dim(df.list.hosp)
head(df.list.hosp)


df.cases.dead.hosp <- full_join(df.cases.dead, df.list.hosp, by=c("DATA_CASOS","REGIAO.COVID")) 
dim(df.cases.dead)
dim(df.cases.dead.hosp)
head(df.cases.dead.hosp)
tail(df.cases.dead.hosp)

df.cases.dead.hosp$recovered <- NA

names(df.cases.dead.hosp)

df.cases.dead.hosp.final <- df.cases.dead.hosp[,c("DATA_CASOS", "cum_casos", "cum_mortes", "CONFIRMADOS.FORA.UTI.COVID", 
                                                  "CONFIRMADOS.UTI.COVID", "recovered", "REGIAO.COVID")]
head(df.cases.dead.hosp.final)

names(df.cases.dead.hosp.final) <- c("time", "cases", "deaths", "hospitalized", "icu", "recovered", "REGIAO.COVID")


df.regions.cases.dead.hosp.final <- split.data.frame(df.cases.dead.hosp.final, df.cases.dead.hosp.final$REGIAO.COVID)
class(df.regions.cases.dead.hosp.final)
length(df.regions.cases.dead.hosp.final)

sapply(df.regions.cases.dead.hosp.final, dim)


names(df.regions.cases.dead.hosp.final)[1]

sapply(1:length(df.regions.cases.dead.hosp.final), function(x)
  eval(parse(text = paste0("write.csv2(df.regions.cases.dead.hosp.final[[", x, 
                            "]][,-7], 'data/Dados.Dt_Coleta-Hospitais-", 
       names(df.regions.cases.dead.hosp.final)[x], ".csv', na='', row.names=FALSE, fileEncoding = 'UTF-8')")))
)


head(df.total)
names(df.total)

names(df.total)[c(1,5,6,8,7)]

## Dados Neherlab RS ##
df.regiao.rs <- df.total[which(df.total$Regiao=="RS"), names(df.total)[c(1,5,6,8,7)]]
names(df.regiao.rs) <- c("time", "cases", "deaths", "hospitalized", "icu")
df.regiao.rs$recovered <- ""
head(df.regiao.rs)

write.csv2(df.regiao.rs, 'data/Dados.Dt_Coleta-Hospitais-RS.csv', 
           na='', row.names=FALSE, fileEncoding = 'UTF-8')


## Dados Neherlab POA ##
df.regiao.poa <- df.total[which(df.total$Regiao=="Porto Alegre"), names(df.total)[c(1,5,6,8,7)]]
names(df.regiao.poa) <- c("time", "cases", "deaths", "hospitalized", "icu")
df.regiao.poa$recovered <- ""
head(df.regiao.poa)

write.csv2(df.regiao.poa, 'data/Dados.Dt_Coleta-Hospitais-Porto_Alegre.csv', 
           na='', row.names=FALSE, fileEncoding = 'UTF-8')



