### Lavoro su MAC #### 

library(dplyr)
library(ggplot2)
library(magrittr)
library(forecast)
library(arm)
library(broom)
library(purrr)

load("imDat.RData")

mydf <- as_tibble(mydf)
mydf$ID <- as.factor(mydf$ID)
str(mydf)

mydf$Periodo.Milano <- factor(mydf$Periodo.Milano, ordered = FALSE)

m0 <- lm(Avg_Comp.Milano ~ 1, data = mydf)
boxplot(residuals(m0)~mydf$ID,horizontal=TRUE)

m1 <- lm(Avg_Comp.Milano ~ ID -1, data=mydf)
summary(m1)
boxplot(residuals(m1)~mydf$ID,horizontal=TRUE)

m2 <- lmer(Avg_Comp.Milano ~ (1|ID),data=mydf)
fixef(m2)
ranef(m2)
coef(m2)

m3 <- lmer(Avg_Comp.Milano ~ Zona_Descr.Milano + (1|ID), data = mydf)
fixef(m3)
ranef(m3)
coef(m3)$ID
summary(m3)


######## Trend Compravendita ###########

mydf %>% 
  dplyr::select(ID, Periodo.Milano, Avg_Comp.Milano) %>% 
  reshape2::dcast(ID~Periodo.Milano) -> df_trend

df_trend <- as_tibble(df_trend)

names(df_trend) <- make.names(names(df_trend))

df_trend %>% 
  dplyr::mutate(media.2016 = (I.Semestre.2016+II.Semestre.2016)/2,
         media.2017 = (I.Semestre.2017+II.Semestre.2017)/2) %>% 
  dplyr::mutate(trend_annuale = ifelse(media.2016 == media.2017, "Stabile", 
                                ifelse(media.2016 > media.2017, "Negativo", "Positivo"))) %>% 
  dplyr::mutate(trend_semestrale = ifelse(II.Semestre.2017 == I.Semestre.2017, "Stabile", 
                                          ifelse(II.Semestre.2017 > I.Semestre.2017, "Positivo", "Negativo"))) %>% 
  reshape2::melt(id.var = c("ID", "media.2016", "media.2017","trend_annuale", "trend_semestrale")) %>% 
  arrange(ID) -> df_trend_long

df_trend_long %<>% 
  dplyr::select(-value)
df_trend_long <- as_tibble(df_trend_long)
names(df_trend_long)[6] <- "Periodo.Milano"
x2 <- names(df_trend_long)

mydf$Periodo.Milano <- gsub(" ", ".", mydf$Periodo.Milano)

mydf$Periodo.Milano <- as.factor(mydf$Periodo.Milano)

levels(mydf$Periodo.Milano)
levels(df_trend_long$Periodo.Milano)

mydf %<>% 
  inner_join(df_trend_long, by = c("ID", "Periodo.Milano")) %>% 
  arrange(ID) 

mydf$Periodo.Milano <- as.factor(mydf$Periodo.Milano)

mydf %>% 
  select(-Area_territoriale.Milano, -Regione.Milano, -Prov.Milano,
         -Comune_descrizione.Milano, -Fascia.Milano)


######### Trend Locazione ###########


mydf %>% 
  dplyr::select(ID, Periodo.Milano, Avg_Loc.Milano) %>% 
  reshape2::dcast(ID~Periodo.Milano) -> df_trend_loc

df_trend_loc <- as_tibble(df_trend_loc)

names(df_trend_loc) <- make.names(names(df_trend_loc))

df_trend_loc %>% 
  dplyr::mutate(media.2016 = (I.Semestre.2016+II.Semestre.2016)/2,
                media.2017 = (I.Semestre.2017+II.Semestre.2017)/2) %>% 
  dplyr::mutate(trend_annuale = ifelse(media.2016 == media.2017, "Stabile", 
                                       ifelse(media.2016 > media.2017, "Negativo", "Positivo"))) %>% 
  dplyr::mutate(trend_semestrale = ifelse(II.Semestre.2017 == I.Semestre.2017, "Stabile", 
                                          ifelse(II.Semestre.2017 > I.Semestre.2017, "Positivo", "Negativo"))) %>% 
  reshape2::melt(id.var = c("ID", "media.2016", "media.2017","trend_annuale", "trend_semestrale")) %>% 
  arrange(ID) -> df_trend_long_loc

df_trend_long_loc %<>% 
  dplyr::select(-value)
df_trend_long_loc <- as_tibble(df_trend_long_loc)
names(df_trend_long_loc)[6] <- "Periodo.Milano"


names(df_trend_long_loc) <- c("ID", "loc.media.2016", "loc.media.2017", "loc.trend_annuale", "loc_trend_semestrale", "Periodo.Milano")


mydf %<>% 
  inner_join(df_trend_long_loc, by = c("ID", "Periodo.Milano")) %>% 
  arrange(ID) 

mydf

mydf %>% 
  select(-Area_territoriale.Milano, -Regione.Milano, -Prov.Milano,
         -Comune_descrizione.Milano, -Fascia.Milano)

mydf %>% 
  select(ID, ID.Milano, Periodo.Milano, Avg_Comp.Milano, Avg_Loc.Milano, media.2016, loc.media.2016, loc.media.2017)


save(mydf, file = "trend_set.RData")

load("trend_set.RData")

rm(list = ls())

new_df

save(new_df, file = "trend_set2.RData")

mydf
writexl::write_xlsx(new_df, path = "trend_set2.xlsx")
