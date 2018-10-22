#### AI ####

df <- readxl::read_excel("C:/Users/fpaderi/Desktop/dataset_MILANO.xlsx")

library(dplyr)
library(magrittr)
library(ggplot2)

df <- as_tibble(df)
head(df)
glimpse(df)

df <- df %>% 
  mutate_if(is.character, as.factor)

glimpse(df)

#### Mi accerto che le righe del dataset coincidano per ognuno dei periodi ####

df %>% 
  filter(Periodo.Milano == "I Semestre 2016") -> primo2016
df %>% 
  filter(Periodo.Milano == "II Semestre 2016") -> secondo2016
df %>% 
  filter(Periodo.Milano == "I Semestre 2017") -> primo2017
df %>% 
  filter(Periodo.Milano == "II Semestre 2017") -> secondo2017

primo2016$ID <- 1:nrow(primo2016)
secondo2016$ID <- 1:nrow(secondo2016)
primo2017$ID <- 1:nrow(primo2017)
secondo2017$ID <- 1:nrow(secondo2017)

library(compare)

primo1 <- primo2016 %>% 
  select(Area_territoriale.Milano:Stato.Milano, ID)
secondo1 <- secondo2016 %>% 
  select(Area_territoriale.Milano:Stato.Milano, ID)
primo2 <- primo2017 %>% 
  select(Area_territoriale.Milano:Stato.Milano, ID)
secondo2 <- secondo2017 %>% 
  select(Area_territoriale.Milano:Stato.Milano, ID)


comparison <- compare::compare(primo1, secondo1,primo2, secondo2, allowAll = TRUE)
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


primo2016 %>% 
  bind_rows(secondo2016, primo2017, secondo2017) -> mydf


rm(list = c("comparison", "primo1", "primo2"))
rm(list = c("primo2016", "primo2017", "secondo2017", "secondo2016", "secondo1", "secondo2"))

##### dataset final mydf ######
names(mydf)
mydf %>% 
  group_by(Periodo.Milano) %>% 
  summarise(Compravendita = mean(Avg_Comp.Milano))

levels(mydf$Periodo.Milano)
pp <- factor(mydf$Periodo.Milano, levels = c("I Semestre 2016","II Semestre 2016","I Semestre 2017","II Semestre 2017"), ordered = TRUE)

mydf %<>% 
  mutate(Periodo.Milano = pp)

mydf %>% 
  group_by(Periodo.Milano) %>% 
  summarise(Compravendita = mean(Avg_Comp.Milano)) %>% 
  ggplot(aes(x = Periodo.Milano, y = Compravendita)) + 
  geom_point() + geom_line(aes(group = 1))
names(mydf)

model <- aov(Avg_Comp.Milano ~ Periodo.Milano +Error(factor(ID)), data = mydf)
print(summary(model))
obtF <- summary(model)$"Error: Within"[[1]][[4]][1]
obtF

summary(model)


##### Trying multilevel ####

library(gridExtra)
library(lme4)
library(sjPlot)

#### inizio in Mac


save(df, mydf, file = "imDat.RData")
