#### AI ####

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


