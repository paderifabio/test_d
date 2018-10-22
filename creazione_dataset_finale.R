library(data.table)
library(dplyr)

getwd()

df <- fread("C:/Users/fpaderi/Desktop/AppImmobiliri/dataset_finale.csv")

df <- as_tibble(df)
df
names(df)

df$Periodo %>% as.factor()
x2 = ifelse(df$Periodo == "1-2016", "I Semestre 2016",
       ifelse(df$Periodo == "2-2016", "II Semestre 2016",
              ifelse(df$Periodo == "1-2017", "I Semestre 2017",
                     ifelse(df$Periodo == "2-2017", "II Semestre 2017", df$Periodo))))

df$Periodo <- x2


df$ID = 1:nrow(df)

df %>% 
  select(ID, everything()) -> df

data.table::fwrite(df, file = "dataset_finale.csv", row.names = FALSE)
getwd()
dir()
setwd("C:/Users/fpaderi/Desktop/AppImmobiliri")
dir()
df <- data.table::fread("dataset_finale.csv")

install.packages("sf")

library(sf)
st_read("Abruzzo.kml")

df %>% 
  select(Avg_Comp) %>% 
  arrange(Avg_Comp) %>% 
  head()

sum(df$Avg_Comp == 0)
range(df$Avg_Comp)

df %>% 
  group_by(Regione) %>% 
  count(Stato) %>% 
  filter(Regione == "MOLISE")
df %>% 
  group_by(Stato) %>% 
  summarise(mean = mean(Compr_min), mean2 = mean(Compr_max))
names(df)

table(df$Descr_Tipologia)
table(Comun)
names(df)

df %>% 
  filter(Comune_descrizione == "MILANO") %>% 
  summarise(mean = mean(Avg_Loc))


df %>% 
  group_by(Periodo) %>% 
  summarise(mean = mean(Avg_Comp, na.rm = TRUE))
df %>% 
  group_by(Periodo)


df %>% 
  as_tibble()

mean(df$Avg_Comp)
range(df$Avg_Comp)
dim(df)              
median(df$Avg_Comp)

unique(df$Avg_Comp)






df <- as_tibble(df)

df

install.packages("writexl")




df$Rapp_loc_comp <- ifelse(df$Rapp_loc_comp == 0, NA, df$Rapp_loc_comp)


df[,11:17][df[, 11:17] == 0]

library(magrittr)

df[11:17] %<>% 
  na_if(.,0)

sum(df$Avg_Loc == 0)       



writexl::write_xlsx(df, "dataset_finale.xlsx")



df %>% 
  group_by(Periodo) %>% 
  summarise(mean = mean(Avg_Comp, na.rm = TRUE))

df %>% 
  filter(Comune_descrizione == "CAPRI") %>% 
  group_by(Stato) %>% 
  summarise(min = mean(Compr_max))

df <- readxl::read_xlsx("C:/Users/fpaderi/Desktop/AppImmobiliri/dataset_finale.xlsx")

df <- as_tibble(df)
df1 <- df %>% 
  filter(Comune_descrizione == "MILANO")

names(df1) 
names(df1) <- paste(names(df1), ".Milano", sep = "")
df1


writexl::write_xlsx(df1, "C:/Users/fpaderi/Desktop/dataset_MILANO.xlsx")


library(geojsonR)

# INPUT IS A PATH TO A FILE

file_js = FROM_GeoJson(url_file_string = file.choose())

file_js

str(file_js$features)















       