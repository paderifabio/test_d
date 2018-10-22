library(data.table)
library(dplyr)

getwd()
rm(list = ls())
# df <- fread("C:/Users/fpaderi/Desktop/AppImmobiliri/dataset_finale.csv")
df <- fread("~/Desktop/Deloitte/AppImmobiliari/dataset_finale_1sem2018.csv")
df <- as_tibble(df)
df
names(df)

df$Periodo %>% table()
x2 = ifelse(df$Periodo == "1-2016", "I Semestre 2016",
       ifelse(df$Periodo == "2-2016", "II Semestre 2016",
              ifelse(df$Periodo == "1-2017", "I Semestre 2017",
                     ifelse(df$Periodo == "2-2017", "II Semestre 2017", 
                            ifelse(df$Periodo == "1-2018", "I Semestre 2018", df$Periodo)))))


df$Periodo <- x2


df$ID = 1:nrow(df)

df %>% 
  select(ID, everything()) -> df

data.table::fwrite(df, file = "dataset_finale.csv", row.names = FALSE) ## Dati generali

df <- data.table::fread("dataset_finale.csv")
head(df)

df %>% 
  group_by(Regione) %>% 
  count(Stato) %>% 
  filter(Regione == "MOLISE")

df %>% 
  group_by(Stato) %>% 
  summarise(mean = mean(Compr_min), mean2 = mean(Compr_max))


table(df$Descr_Tipologia)


df$Rapp_loc_comp <- ifelse(df$Rapp_loc_comp == 0, NA, df$Rapp_loc_comp)

library(magrittr)

df[11:17] %<>% 
  na_if(.,0)


data.table::fwrite(df, "dataset_finale_noMiss.csv")

# df <- readxl::read_xlsx("C:/Users/fpaderi/Desktop/AppImmobiliri/dataset_finale.xlsx")


df1 <- df %>% 
  filter(Comune_descrizione == "MILANO")


df1 %>% 
  filter(Zona_Descr == "TABACCHI, SARFATTI, CREMA") %>% 
  filter(Stato == "OTTIMO") %>% 
  filter(Descr_Tipologia == "Negozi") # primo beccato

df1 %>% 
  filter(Zona_Descr == "PORTA NUOVA") %>% 
  filter(Stato == "OTTIMO") %>% 
  filter(Descr_Tipologia == "Uffici strutturati") # secondo beccato

df1 %>% 
  filter(Zona_Descr == "MUSOCCO, CERTOSA, EXPO, C.NA MERLATA" ) %>% 
  filter(Stato == "OTTIMO") %>% 
  filter(Descr_Tipologia == "Negozi") # terzo beccato

680258
680198
680417


names(df1) 
names(df1) <- paste(names(df1), ".Milano", sep = "")
df1
head(df1)

df1 %>% 
  filter(ID.Milano %in% c(680417, 680198, 680258)) -> dati_aggiunti

df1 %>% 
  filter(ID.Milano != 680417) %>% 
  filter(ID.Milano != 680198) %>% 
  filter(ID.Milano != 680258) -> dati_Milano


# writexl::write_xlsx(df1, "C:/Users/fpaderi/Desktop/dataset_MILANO_ISem2018.xlsx")
writexl::write_xlsx(dati_Milano, "~/Desktop/Deloitte/AppImmobiliari/dataset_MILANO_ISem2018.xlsx")


### Dati milano sistemazione ####


df <- dati_Milano


#### Mi accerto che le righe del dataset coincidano per ognuno dei periodi ####

df %>% 
  filter(Periodo.Milano == "I Semestre 2016") -> primo2016
df %>% 
  filter(Periodo.Milano == "II Semestre 2016") -> secondo2016
df %>% 
  filter(Periodo.Milano == "I Semestre 2017") -> primo2017
df %>% 
  filter(Periodo.Milano == "II Semestre 2017") -> secondo2017
df %>% 
  filter(Periodo.Milano == "I Semestre 2018") -> primo2018

primo2016$ID <- 1:nrow(primo2016)
secondo2016$ID <- 1:nrow(secondo2016)
primo2017$ID <- 1:nrow(primo2017)
secondo2017$ID <- 1:nrow(secondo2017)
primo2018$ID <- 1:nrow(primo2018)

library(compare)

primo1 <- primo2016 %>% 
  select(Area_territoriale.Milano:Stato.Milano, ID)
secondo1 <- secondo2016 %>% 
  select(Area_territoriale.Milano:Stato.Milano, ID)
primo2 <- primo2017 %>% 
  select(Area_territoriale.Milano:Stato.Milano, ID)
secondo2 <- secondo2017 %>% 
  select(Area_territoriale.Milano:Stato.Milano, ID)
primo3 <- primo2018 %>% 
  select(Area_territoriale.Milano:Stato.Milano, ID)

comparison <- compare::compare(primo1, secondo1,primo2, secondo2,primo3, allowAll = TRUE)
comparison$result

#### Sono speculari! ####

primo2016 %<>% 
  select(ID, everything())
secondo2017 %<>% 
  select(ID, everything())
primo2017 %<>% 
  select(ID, everything())
secondo2016 %<>% 
  select(ID, everything())
primo2018 %<>% 
  select(ID, everything())

primo2016 %>% 
  bind_rows(secondo2016, primo2017, secondo2017, primo2018) -> mydf

mydf
rm(list = c("comparison", "primo1", "primo2"))
rm(list = c("primo2016", "primo2017", "secondo2017", "secondo2016", "secondo1", "secondo2"))

##### dataset final mydf ######
names(mydf)
mydf %>% 
  group_by(Periodo.Milano) %>% 
  summarise(Compravendita = mean(Avg_Comp.Milano))

levels(mydf$Periodo.Milano)
pp <- factor(mydf$Periodo.Milano, levels = c("I Semestre 2016","II Semestre 2016","I Semestre 2017","II Semestre 2017", "I Semestre 2018"), ordered = TRUE)

mydf %<>% 
  mutate(Periodo.Milano = pp)

library(ggplot2)
mydf %>% 
  group_by(Periodo.Milano) %>% 
  summarise(Compravendita = mean(Avg_Comp.Milano)) %>% 
  ggplot(aes(x = Periodo.Milano, y = Compravendita)) + 
  geom_point() + geom_line(aes(group = 1))


save(df, mydf, file = "~/Documents/Github/test_d/imDat.RData")

















       