
# Einlesen der benötigten Libraries ---------------------------------------

library(dplyr)
library(tidyr)
library(tidyr)
library(labelled)
library(ggplot2)
library(readxl)
library(haven)


# Importieren der relevanten Datensätze -----------------------------------

# Die relevanten Datensätze importieren (Bader)

SHP_Covid_USER <- read_sav("C:/Users/badrl/OneDrive - ZHAW/Desktop/Data_SPSS/Data_SPSS/SHP-Data-SHP-3-W1-SPSS/Wichtig Praktikum 2/SHP_Covid_USER.sav")
SHP_SO <- read_sav("C:/Users/badrl/OneDrive - ZHAW/Desktop/Data_SPSS/Data_SPSS/SHP-Data-SHP-3-W1-SPSS/Wichtig Praktikum 2/SHP-Data-WA-SPSS/SHP_SO.sav")
SHP19_H_USER <- read_sav("C:/Users/badrl/OneDrive - ZHAW/Desktop/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W21_2019/SHP19_H_USER.sav")
SHP19_P_USER <- read_sav("C:/Users/badrl/OneDrive - ZHAW/Desktop/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W21_2019/SHP19_P_USER.sav")
SHP18_P_USER <- read_sav("C:/Users/badrl/OneDrive - ZHAW/Desktop/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W20_2018/SHP18_P_USER.sav")
SHP17_P_USER <- read_sav("C:/Users/badrl/OneDrive - ZHAW/Desktop/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W19_2017/SHP17_P_USER.sav")
SHP16_P_USER <- read_sav("C:/Users/badrl/OneDrive - ZHAW/Desktop/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W18_2016/SHP16_P_USER.sav")
SHP15_P_USER <- read_sav("C:/Users/badrl/OneDrive - ZHAW/Desktop/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W17_2015/SHP15_P_USER.sav")


# Die relevanten Datensätze importieren (Nevio)

# SHP_Covid_USER <- read_sav("C:/Users/nevio/OneDrive - ZHAW/Dokumente/Data Processing with R/swissubase_932_7_0/Data_SPSS/Data_SPSS/SHP_Covid_SPSS/SHP_Covid_USER.sav")
# SHP_SO <- read_sav("C:/Users/nevio/OneDrive - ZHAW/Dokumente/Data Processing with R/swissubase_932_7_0/Data_SPSS/Data_SPSS/SHP-Data-WA-SPSS/SHP_SO.sav")
# SHP19_H_USER <- read_sav("C:/Users/nevio/OneDrive - ZHAW/Dokumente/Data Processing with R/swissubase_932_7_0/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W21_2019/SHP19_H_USER.sav")
# SHP19_P_USER <- read_sav("C:/Users/nevio/OneDrive - ZHAW/Dokumente/Data Processing with R/swissubase_932_7_0/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W21_2019/SHP19_P_USER.sav")
# SHP18_P_USER <- read_sav("C:/Users/nevio/OneDrive - ZHAW/Dokumente/Data Processing with R/swissubase_932_7_0/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W20_2018/SHP18_P_USER.sav")
# SHP17_P_USER <- read_sav("C:/Users/nevio/OneDrive - ZHAW/Dokumente/Data Processing with R/swissubase_932_7_0/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W19_2017/SHP17_P_USER.sav")
# SHP16_P_USER <- read_sav("C:/Users/nevio/OneDrive - ZHAW/Dokumente/Data Processing with R/swissubase_932_7_0/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W18_2016/SHP16_P_USER.sav")
# SHP15_P_USER <- read_sav("C:/Users/nevio/OneDrive - ZHAW/Dokumente/Data Processing with R/swissubase_932_7_0/Data_SPSS/Data_SPSS/SHP-Data-W1-W21-SPSS/W17_2015/SHP15_P_USER.sav")

# Erstellen des neuen Data-Frames -----------------------------------------

# selektieren der benötigten Spalten von SHP_COVID_USER
SHP_Covid_USER |> 
  select(IDPERS,C20PC44,C20SEX,C20D29,C20W01,C20W02A,C20W02b,C20W03A,
         C20W03B,C20W03C,C20C01)-> Covid2020

# selektieren der benötigten Spalten von SHP_19_P_User
SHP19_P_USER |> 
  select(IDPERS,IDHOUS19,P19C44,AGE19,EDUCAT19,P19D29,I19UNEY)-> P19

# selektieren der benötigten Spalten von SHP_19_H_User
SHP19_H_USER |> 
  select(IDHOUS19,CANTON19,COM2_19,HLDCEN19,I19HTYN,NBPERS19)-> H19

# selektieren der benötigten Spalten von SHP_18_P_User
SHP18_P_USER |> 
  select(IDPERS,I18UNEY)-> P18

# selektieren der benötigten Spalten von SHP_18_P_User
SHP17_P_USER |> 
  select(IDPERS,I17UNEY)-> P17

# selektieren der benötigten Spalten von SHP_18_P_User
SHP16_P_USER |> 
  select(IDPERS,I16UNEY)-> P16

# selektieren der benötigten Spalten von SHP_18_P_User
SHP15_P_USER |> 
  select(IDPERS,I15UNEY)-> P15


# selektieren der benötigten Spalten von SHP_SO
SHP_SO |> 
  select(IDPERS,`P$$O58`)-> SO

# Zusammenführen der separaten Data-Frames in einen grossen Datensatz 

# Covid2020 und P19 mit einem left join zusammenführen
data<-merge(x = Covid2020, y = P19, by.x = "IDPERS",by.y="IDPERS", all.x = TRUE) 

# Der neue Datensatz und H19 mit einem left join zusammenführen
data<- merge(x = data, y = H19, by.x = "IDHOUS19", by.y = "IDHOUS19", all.x = TRUE)

# Der neue Datensatz und SO mit einem left join zusammenführen
data <- merge(x = data, y = SO, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)

# Der neue Datensatz und P18 mit einem left join zusammenführen
data <- merge(x = data, y = P18, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)

# Der neue Datensatz und P17 mit einem left join zusammenführen
data <- merge(x = data, y = P17, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)

# Der neue Datensatz und P16 mit einem left join zusammenführen
data <- merge(x = data, y = P16, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)

# Der neue Datensatz und P15 mit einem left join zusammenführen
data <- merge(x = data, y = P15, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)

View(data)
df<- data
df<- 
# Daten-Bereinigung ------------------------------------------------------


# Die Variable "Änderung der Lebenszufriedenheit" bearbeiten 
data |> 
  filter(C20PC44>=0 & P19C44 >=0) |> 
  mutate(LZ_änderung = C20PC44 - P19C44) |> 
  mutate(LZ_änderung = case_when(
    LZ_änderung >0 ~ 'besser',
    LZ_änderung <0 ~ 'schlechter',
    LZ_änderung == 0 ~ 'gleich'),
    LZ_änderung = factor(LZ_änderung, levels=c('schlechter','gleich','besser'),ordered = T))-> data

# Die Variable "Geschlecht" bearbeiten
data |> 
  mutate(Geschlecht = case_when(
    C20SEX ==1 ~ 'Mann',
    C20SEX == 2 ~ 'Frau' ))-> data

# Die Variable "Alter" bearbeiten 
data |> 
  rename(Alter = AGE19) |> 
  filter(Alter >= 18)->data

# Die Variable "Kanton" bearbeiten 
data |> 
  rename(Kanton = CANTON19)->data

# Die Variable "Gemeindetyp" bearbeiten 
data |> 
  rename(Gemeindetyp = COM2_19) -> data 

# Die Variable "Höchstes Ausbildungszertifikat" bearbeiten 
data |> 
  rename(Höchstes_Ausbildungszertifikat = EDUCAT19)-> data 

# Die Variable "Beziehungsstatus" bearbeiten 
data$C20D29 [data$C20D29 == 2 ]<- 1
data |> 
  filter(C20D29 > 0) |>
  mutate(beziehungstatus = case_when(
    C20D29 == 1 ~ 'in Beziehung',
    C20D29 == 3 ~ 'nicht in Beziehung '
  ))-> data

# Die Variable "Änderung des Beziehungsstatus von 2019 auf 2020" bearbeiten 
data$P19D29[data$P19D29==2]<- 1
data |> 
  filter((C20D29 == 1  |C20D29 == 3 ) & (P19D29 == 1  |P19D29 == 3)) ->data 
data |> 
  mutate(BeziehungsstatusÄnderung = P19D29 - C20D29) |> 
  mutate(BeziehungsstatusÄnderung = case_when(BeziehungsstatusÄnderung == 0 ~ "keine Änderung",
                                              BeziehungsstatusÄnderung == -2 ~ "Trennung",
                                              BeziehungsstatusÄnderung == 2 ~ "neu in Beziehung"))->data 

# Die Variable "Kurzfristige Änderung der Arbeitssituation nach Pandemiebeginn" bearbeiten 
data |> 
  mutate("Kurzfristige Änderung Arbeitssituation"= case_when((C20W01 == 4 & 
                                                                I19UNEY <= 0 | C20W02A == 1 | C20W02b == 1 | C20W03A == 1 | C20W03B == 1 | C20W03C == 1  ~ "Ja"), TRUE ~ "Nein")) |> 
  filter(C20W01 >0)-> data 


# Die Variable "Corona Infektion" bearbeiten 
data |> 
  mutate('Covid_infektion' = case_when( C20C01 == 2 ~ "Ja",  C20C01 == 1 ~ "Nein",  C20C01 == 3 ~ "Nein",  C20C01 == 4 ~ "Nein",  C20C01 == 5 ~ "Nein",  C20C01 ==6 ~ "Nein" ))-> data 

# Die Variable "Haushaltstyp" bearbeiten 
data |> 
  rename(Haushaltstyp = HLDCEN19)-> data 

# Die Variable "Einkommen pro Kopf" bearbeiten
data |> 
  filter(NBPERS19 > 0) |> 
  mutate(EinkommenProKopf = I19HTYN/NBPERS19)->data

# Die Variable "Finanzielle Probleme" in Jugend bearbeiten
data |> 
  filter(`P$$O58` > 0) |> 
  mutate("Finanzielle Probleme in Jugend" = case_when(`P$$O58` == 1 ~ "Ja",`P$$O58` == 2 ~ "Nein" ))->data

# Die Variable "War Person mindestens einmal Arbeitslos zwischen 2015 und 2019" bearbeiten 
data |> 
  mutate(Arbeitslos_15_19 = I19UNEY+I18UNEY+I17UNEY+I16UNEY+I15UNEY) |> 
  mutate('Arbeitslos_15_19' = case_when(
    Arbeitslos_15_19  == 0 ~ 'Nein',
    Arbeitslos_15_19 > 0  ~ 'Ja' ))->data

# Die 14 Variablen vom Datensatz behalten und alle Anderen löschen 
data |> 
  select(IDPERS,IDHOUS19,Alter,Höchstes_Ausbildungszertifikat,
         Kanton,Gemeindetyp,Haushaltstyp,Geschlecht, beziehungstatus, BeziehungsstatusÄnderung,
         `Kurzfristige Änderung Arbeitssituation`,Covid_infektion,EinkommenProKopf,`Finanzielle Probleme in Jugend`, Arbeitslos_15_19,LZ_änderung)->data


# Ergänzen des Data-Frames mit eigenen Einflussfaktoren -------------------


# 3 Zusätzliche Variablen hinzufügen
# Selektieren der gewünschten Variablen aus dem Datensatz
SHP19_H_USER |> 
  select(IDHOUS19,H19H27, H19H23)-> data1
SHP19_P_USER |> 
  select(IDPERS,IDHOUS19,P19L01)->data2

# Mergen mit unserem Hauptdatensatz
data <- merge(x = data, y = data1, by.x = "IDHOUS19", by.y = "IDHOUS19", all.x = TRUE)
data <- merge(x = data, y = data2, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)
data<- data[,-19]

# Umbenennung der Spalte
names(data)[names(data) == 'P19L01'] <- 'Erkrankung,Unfall'
names(data)[names(data) == 'H19H27'] <- 'Unterkunftsprobleme:Verschmutzung...'
names(data)[names(data) == 'H19H23'] <- 'Unterkunft zu klein '

# Ergänzen mit einer neuen ja/nein Spalte pro Variable
data |> 
  mutate(`Erkrankung,Unfall`= case_when(
    `Erkrankung,Unfall`==1 ~ 'ja',
    `Erkrankung,Unfall`==2 ~ ' nein'
  ))-> data

data |> 
  mutate(`Unterkunftsprobleme:Verschmutzung...`= case_when(
    `Unterkunftsprobleme:Verschmutzung...`==1 ~ 'ja',
    `Unterkunftsprobleme:Verschmutzung...`==2 ~ ' nein'
  ))-> data
data |> 
  mutate(`Unterkunft zu klein `= case_when(
    `Unterkunft zu klein `==1 ~ 'ja',
    `Unterkunft zu klein `==2 ~ ' nein'
  ))-> data


# Speichern des erarbeiteten Data-Frames  ---------------------------------


#write.table(data, file = "C:/Users/badrl/OneDrive - ZHAW/Dokumente/GitHub/Praktikum-2/Praktikum_Data_Processing2.txt", sep = ",",
#row.names = TRUE, col.names = TRUE)

#write.csv(data, file = "C:/Users/badrl/OneDrive - ZHAW/Dokumente/GitHub/Praktikum-2/Praktikum_Data_Processing2.csv",
#row.names = TRUE)


# Prüfen der Datenqualität ------------------------------------------------

# BFS Datensatz einlesen und bearbeiten 
BFS_2 <- read_excel("C:/Users/badrl/OneDrive - ZHAW/Desktop/Studium/2.Semester/Data Processing with R/BFS.2.xlsx")

# Bearbeiten der Altersspalte, so dass nur noch zwei Ziffern für die Altersangaben drinstehen.
View(BFS_2)
BFS_2$Alter[BFS_2$Alter == '100 Jahre und mehr']<- '100'
BFS_2$Alter<- substr(BFS_2$Alter,1,2)
BFS_2$Alter[BFS_2$Alter == '10']<- '100'

pivot_longer(BFS_2, cols = 3:28, names_to ="Kanton",
             values_to = "Population")-> BFS_2

# Erstellen eines neuen Data-Frames 
BFS_2 |> 
  group_by(Alter) |> 
  summarise(Population = sum(Population))->b
b<- as.data.frame(b)
b$Alter<-as.numeric(b$Alter)

#
data |> 
  count(Alter)->a

a<- as_tibble(a)

colors<-c('BFS' = 'blue', 'SHP' = 'red')
ggplot() +
  geom_line(data = b, aes(x = Alter, y = Population/sum(Population),colour = 'BFS'))+
  geom_line(data = a, aes(x = Alter, y = n/sum(n),colour='SHP'))+
  labs(x = "Alter",
       y = "(%)",
       color= "Source")+
  scale_color_manual(values = colors)

BFS_2 |> 
  group_by(Kanton) |> 
  summarise(Population = sum(Population))->b2
b2<- as.data.frame(b2)
b2$Kanton<- substr(b2$Kanton,1,2)

data |> 
  count(Kanton)->a2

a2<- as_tibble(a2)
typeof(a2$Kanton)
a2$Kanton<- as.character(a2$Kanton)
a2$Kanton[a2$Kanton == 1]<-'AG'
a2$Kanton[a2$Kanton == 2]<-'AI'
a2$Kanton[a2$Kanton == 3]<-'AR'
a2$Kanton[a2$Kanton == 4]<-'BE'
a2$Kanton[a2$Kanton == 5]<-'BL'
a2$Kanton[a2$Kanton == 6]<-'BS'
a2$Kanton[a2$Kanton == 7]<-'FR'
a2$Kanton[a2$Kanton == 8]<-'GE'
a2$Kanton[a2$Kanton == 9]<-'GL'
a2$Kanton[a2$Kanton == 10]<-'GR'
a2$Kanton[a2$Kanton == 11]<-'JU'
a2$Kanton[a2$Kanton == 12]<-'LU'
a2$Kanton[a2$Kanton == 13]<-'NE'
a2$Kanton[a2$Kanton == 14]<-'NW'
a2$Kanton[a2$Kanton == 15]<-'OW'
a2$Kanton[a2$Kanton == 16]<-'SG'
a2$Kanton[a2$Kanton == 17]<-'SH'
a2$Kanton[a2$Kanton == 18]<-'SO'
a2$Kanton[a2$Kanton == 19]<-'SZ'
a2$Kanton[a2$Kanton == 20]<-'TG'
a2$Kanton[a2$Kanton == 21]<-'TI'
a2$Kanton[a2$Kanton == 22]<-'UR'
a2$Kanton[a2$Kanton == 23]<-'VD'
a2$Kanton[a2$Kanton == 24]<-'VS'
a2$Kanton[a2$Kanton == 25]<-'ZG'
a2$Kanton[a2$Kanton == 26]<-'ZH'

b2<-as.data.frame(b2)
a2<-as.data.frame(a2)

ggplot(fill=colors) +
  geom_line(data = b2, aes(x = Kanton, y = Population/sum(Population),group =1, col = 'BFS'))+
  geom_line(data = a2, aes(x = Kanton, y = n/sum(n), group=1,col='SHP'))+
  labs(x = "Kanton",
       y = "(%)",
       color = "Source") +
  scale_color_manual(values = colors)


BFS_2 |> 
  group_by(Geschlecht) |> 
  summarise(Population = sum(Population))->b3
b3<- as.data.frame(b3)


data |> 
  count(Geschlecht)->a3

a3<- as_tibble(a3)
ggplot() +
  geom_line(data = b3, aes(x = Geschlecht, y = Population/sum(Population), group=1, col= 'BFS'))+
  geom_line(data = a3, aes(x = Geschlecht, y = n/sum(n),group = 1,col='SHP'))+
  ylim(0.0,0.7)+
  labs(x = "Sex",
       y = "(%)",
       color = "Legend") +
  scale_color_manual(values = colors)



# Darstellungen zum Datensatz ---------------------------------------------

# univariate Darstellung 
data|> 
  ggplot(aes(x=LZ_änderung,fill=LZ_änderung))+
  geom_bar()

table(data$LZ_änderung)/length(data$LZ_änderung)

data |> 
  ggplot(aes(x=Geschlecht,fill=Geschlecht))+
  geom_bar()

table(data$Geschlecht)/length(data$Geschlecht)

data |> 
  ggplot(aes(x=Alter))+
  geom_histogram(binwidth=10, xlim =c(25,100), bins = 7,col='red')


data |> 
  filter(!is.na(Covid_infektion)) |> 
  ggplot(aes(x=Covid_infektion, fill= Covid_infektion))+
  geom_bar()

table(data$Covid_infektion)

data |> 
  ggplot(aes(x=beziehungstatus))+
  geom_bar(alpha=0.5,col='red')

data |> 
  filter(!is.na(`Unterkunftsprobleme:Verschmutzung...`)) |>
  ggplot(aes(x=`Unterkunftsprobleme:Verschmutzung...`))+
  geom_bar(alpha=0.5,col='red')

data |> 
  filter(!is.na(`Erkrankung,Unfall`)) |>
  ggplot(aes(x=`Erkrankung,Unfall`))+
  geom_bar(alpha=0.5,col='red')


# Bivariate Darstellung:
data |> 
  ggplot(aes(x=LZ_änderung,y=log10( EinkommenProKopf),fill=LZ_änderung))+
  geom_boxplot()

data |> 
  ggplot(aes(x=Geschlecht,fill=LZ_änderung))+
  geom_bar(position = 'dodge')

prop.table(table(df$LZ_änderung,df$Geschlecht))


data$P19L01<- as.character(data$`Erkrankung,Unfall`)
data |> 
  filter(!is.na(`Erkrankung,Unfall`)) |>
  ggplot(aes(LZ_änderung,fill=`Erkrankung,Unfall`))+
  geom_bar(position = 'dodge')

data |> 
  filter(!is.na(beziehungstatus)) |>
  ggplot(aes(LZ_änderung,fill=beziehungstatus))+
  geom_bar(position = 'dodge')

prop.table(table(data$LZ_änderung,data$BeziehungsstatusÄnderung))











