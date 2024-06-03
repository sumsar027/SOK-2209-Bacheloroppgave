# ulike filer/datasett som er tatt med i koden vil være tilgjengelige i github repo "https://github.com/sumsar027/SOK-2209-Bacheloroppgave"

# setter r-studio til norsk språk 
Sys.setlocale(locale='no_NB.utf8')

# laster ned pakker 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(jtools)
library(gglorenz)
library(ineq)
library(lubridate)
library(scales)
library(readr)
library(PxWebApiData)
library(rjstat)
library(httr)
library(car)
library(sjPlot)
library(ggeffects)
library(ggrepel)
library(gridExtra) # for å vise grafer sammen
library(naniar)# for NA https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html

# bruker noen spesielle farger i grafene for ggplot; https://sape.inf.usi.ch/quick-reference/ggplot2/colour
# mattefunksjoner er hentet fra https://rpruim.github.io/s341/S19/from-class/MathinRmd.html


################################################################################


# Grafer og tabeller for oppgaven


################################################################################


############################ Regioner i Norge ##################################

# https://www.ssb.no/statbank/table/09817/tableViewLayout1/

# henter datasettet ved bruk av api spørring
url1 <- "https://data.ssb.no/api/v0/no/table/09817/"

data <- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:FylkerAlle",
        "values": [
          "31",
          "32",
          "30",
          "01",
          "02",
          "03",
          "34",
          "04",
          "05",
          "33",
          "06",
          "39",
          "40",
          "38",
          "07",
          "08",
          "42",
          "09",
          "10",
          "11",
          "46",
          "12",
          "13",
          "14",
          "15",
          "50",
          "16",
          "17",
          "18",
          "55",
          "56",
          "54",
          "19",
          "20",
          "21",
          "22",
          "23",
          "25",
          "26",
          "88",
          "99"
        ]
      }
    },
    {
      "code": "InnvandrKat",
      "selection": {
        "filter": "item",
        "values": [
          "B"
        ]
      }
    },
    {
      "code": "Landbakgrunn",
      "selection": {
        "filter": "item",
        "values": [
          "999"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "AndelBefolkning"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2023"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'


d.tmp <- POST(url1 , body = data, encode = "json", verbose())

ssbinnvandring1 <- fromJSONstat(content(d.tmp, "text"))

# fjerner kolonner
ssbinnvandring1 <- ssbinnvandring1[, -c(2, 3, 4, 5)]

# endrer navn på regionene
nye_navn <- c("Viken", "Vestfold og Telemark", "Trøndelag", 
              "Nordland", "Troms og Finmark")
rad_indekser <- c(3, 14, 26, 29, 32)
ssbinnvandring1$region[rad_indekser] <- nye_navn

# fjerner rader med verdien 0
ssbinnvandring1 <- ssbinnvandring1[ssbinnvandring1$value != 0, ]

# definerer fargeskala 
color_scale <- scale_fill_gradient(low = "deepskyblue", high = "darkblue")

# lager barplot
ggplot(data = ssbinnvandring1, aes(x = region, y = value, fill = value)) +
  geom_bar(stat = "identity") +
  color_scale + # legger til fargeskalaen
  labs(x = "Region", y = "Andel innvandrere", 
       title = "Andel innvandrere av total populasjon i norske regioner", 
       subtitle = "År 2023", 
       caption = "Kilde: https://www.ssb.no/statbank/table/09817/tableViewLayout1/") + # legger til navn og titler
  scale_y_continuous(limits = c(0, 30),labels = function(x) paste0(x, "%")) + #legger til prosent på y aksen
  geom_text(aes(label = value), 
            position = position_dodge(width = 5), 
            vjust = 0) + # legger til tall for hver bar
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) #Snur tekst og sentrerer titler

############################ Innvandring #######################################

# innvandring over tid
# https://www.ssb.no/statbank/table/05182/tableViewLayout1/

# henter datasettet ved bruk av api spørring
url <- "https://data.ssb.no/api/v0/no/table/05182/"

data <- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:Landet",
        "values": [
          "0"
        ]
      }
    },
    {
      "code": "InnvandrKat",
      "selection": {
        "filter": "item",
        "values": [
          "2"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'


d.tmp <- POST(url , body = data, encode = "json", verbose())

ssbinnvandring <- fromJSONstat(content(d.tmp, "text"))

# lager plot med linjer og punkter 
ggplot(ssbinnvandring, aes(x = år, y = value, group = 1)) +
  geom_line(colour = "deepskyblue") +
  geom_point(colour = "deepskyblue4") + 
  labs(x = "Årstall", y = "Antall innvandrere", 
       title = "Personer innvandret til Norge", 
       subtitle = "1970-2024", 
       caption = "Kilde: https://www.ssb.no/statbank/table/05182/tableViewLayout1/") + # legger til navn og titler
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) +# Sentrerer titler
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # snur x akse labels
  scale_y_continuous(breaks = seq(0, 1000000, by = 100000), labels = comma)


########################Innvandring over tid og grunn###########################

# innvandring over tid
# https://www.ssb.no/statbank/table/09817/tableViewLayout1/

# henter datasettet ved bruk av api spørring
url <- "https://data.ssb.no/api/v0/no/table/09817/"

data <- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:Landet",
        "values": [
          "0"
        ]
      }
    },
    {
      "code": "InnvandrKat",
      "selection": {
        "filter": "item",
        "values": [
          "B",
          "C"
        ]
      }
    },
    {
      "code": "Landbakgrunn",
      "selection": {
        "filter": "item",
        "values": [
          "999"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'


d.tmp <- POST(url , body = data, encode = "json", verbose())

ssbinnvandring2 <- fromJSONstat(content(d.tmp, "text"))

# fjerner landbakgrunn og region, setter opp underkategorier som kolonner
# lager lineplot for de forskjellige variablene
ssbinnvandring2 <- ssbinnvandring2 %>% 
  select(-c(region, landbakgrunn)) %>% 
  pivot_wider(names_from = c(innvandringskategori, statistikkvariabel), 
              values_from = value) %>% 
  rename("Antall_Innvandrere" = "Innvandrere_Personer", 
         "Andel_Innvandrere" = "Innvandrere_Andel av befolkningen (prosent)", 
         "Norskfødte_Innvandrere" = "Norskfødte med innvandrerforeldre_Personer", 
         "Andel_Norskfødte_Innvandrere" = "Norskfødte med innvandrerforeldre_Andel av befolkningen (prosent)")

ssbinnvandring2$år <- as.numeric(ssbinnvandring2$år)

# endrer til normalform
options(scipen = 999)

# lager plot med linjer og punkter 
figur1 <- ggplot(ssbinnvandring2, aes(x=år)) +
  geom_line(aes(y = Antall_Innvandrere), color = "springgreen2") +
  geom_point(aes(y = Antall_Innvandrere), color = "green4") +
  geom_line(aes(y = Norskfødte_Innvandrere), color = "deepskyblue3") +
  geom_point(aes(y = Norskfødte_Innvandrere), color = "dodgerblue4") + 
  scale_x_continuous(breaks = unique(ssbinnvandring2$år)) +
  scale_y_continuous(breaks = seq(0, 931081, by = 100000)) + 
  labs(x = "Årstall", y = "Antall Personer", 
       title = "Antall Innvandrere og Norskfødte Innvandrere", 
       subtitle = "Grønn = Antall Innvandrere, Blå = Norskfødte Innvandrere", 
       caption = "Kilde: https://www.ssb.no/statbank/table/09817/tableViewLayout1/") + # legger til navn og titler
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #Snur x-akse punkter
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) # Sentrerer titler 

figur1

# innvandringsgrunn 
# https://www.ssb.no/statbank/table/08348

# henter datasettet ved bruk av api spørring
url1 <- "https://data.ssb.no/api/v0/no/table/08348/"

data1 <- '{
  "query": [
    {
      "code": "Innvandringsgrunn",
      "selection": {
        "filter": "item",
        "values": [
          "01",
          "02",
          "03",
          "04",
          "05",
          "06"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "1990-2022"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'


d.tmp1 <- POST(url1 , body = data1, encode = "json", verbose())

ssbinnvandring3 <- fromJSONstat(content(d.tmp1, "text"))

# definerer fargeskala 
color_scale <- scale_fill_gradient(low = "deepskyblue", high = "darkblue")

# lager barplot
figur2 <- ggplot(ssbinnvandring3, aes(x=innvandringsgrunn, y=value, fill=value)) +
  geom_bar(stat="identity") +
  color_scale + # fargeskala
  labs(x = "Årstall", y = "Antall Personer", 
       title = "Antall Innvandrere basert på grunn", 
       subtitle = "Innvandrere i alt fra 1990-2022", 
       caption = "https://www.ssb.no/statbank/table/08348/tableViewLayout1/") +
  # legger til navn og titler
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + # sentrerer titler
  scale_y_continuous(labels = label_comma()) # endrer fra scientific til normal

# viser figur 1 og 2 ved siden av hverandre, går ann å endre størrelser hvis nødvendig
figur2

#################################Tillit i Europa################################

# henting av datasett "Our World in Data, Trust in Europa" 
trusteuropa <- read.csv("C:\\Users\\Eier\\Downloads\\Data_for_bacheloroppgave\\interpersonal-trust-in-europe.csv")

# lager ggplot for alle land
ggplot(trusteuropa, aes(x = Trust.in.others..Eurostat..2015.., y = Code,
                        fill = Code)) +
  geom_bar(stat = "identity", width = 0.5) +
  color_scale + # farger fra tidligere 
  scale_fill_manual("Norden", values = c("NOR" = "deepskyblue", "SWE" = "deepskyblue", "DNK" = "deepskyblue",
                                         "FIN" = "deepskyblue", "ISL" = "deepskyblue")) + 
  labs(x = "Sosial tillit", y = "Land", title = "Tillit til andre, Europa") +
  geom_text(aes(label = Trust.in.others..Eurostat..2015..), 
            hjust = -1, nudge_x = -.3) +
  coord_cartesian(clip = "off") + # labels blir ikke kuttet 
  theme_minimal()


###############################Utdanning og alder ##############################

url <- "https://data.ssb.no/api/v0/no/table/09430/"

data <- '{
  "query": [
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "999",
          "16-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-49",
          "50-59",
          "60-66",
          "067+"
        ]
      }
    },
    {
      "code": "UtdNivaa",
      "selection": {
        "filter": "item",
        "values": [
          "04a"
        ]
      }
    },
    {
      "code": "InnvandrKat",
      "selection": {
        "filter": "item",
        "values": [
          "01",
          "90"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Person"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'


d.tmp <- POST(url , body = data, encode = "json", verbose())

utdanningalder <- fromJSONstat(content(d.tmp, "text"))

# fjerner radene med "16-19år", "20-24år" og "Alder i alt"
utdanningalder <- utdanningalder[-(which(utdanningalder$alder %in% c("16-19 år", "20-24 år", "Alder i alt"))),]

# filtrerer for en befolkningskategori
utdanningalderi <- utdanningalder %>% 
  filter(innvandringskategori == "Innvandrere")

# fjerner innvandring
utdanningalder <- utdanningalder[-(which(utdanningalder$innvandringskategori %in% "Innvandrere")),]

# plotter for uten innvandring
ggplot(utdanningalder, aes(x = år, y = value, fill = alder, colour = alder, group = alder)) +
  geom_line(size = 0.55) +
  labs(x = "Årstall", y = "Antall personer", 
       title = "Personer i høyere utdanning", 
       subtitle = "1980-2022", 
       caption = "Kilde: https://www.ssb.no/statbank/table/09430/tableViewLayout1/") + # legger til navn og titler
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) +# Sentrerer titler
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks = seq(0, 1000000, by = 10000), labels = comma)

# plotter for bare innvandring
ggplot(utdanningalderi, aes(x = år, y = value, fill = alder, colour = alder, group = alder)) +
  geom_line(size = 0.55) +
  labs(x = "Årstall", y = "Antall personer", 
       title = "Personer i høyere utdanning; Innvandrere", 
       subtitle = "1980-2022", 
       caption = "Kilde: https://www.ssb.no/statbank/table/09430/tableViewLayout1/") + # legger til navn og titler
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) +# Sentrerer titler
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks = seq(0, 1000000, by = 10000), labels = comma)




################################################################################


# OLS-MODELLEL OG ANALYSE


################################################################################



# henting av datasett "Norsk medborgerpanel runde 26"
df <- read.csv("C:\\Users\\Eier\\Downloads\\2249\\Norsk medborgerpanel runde 26, 2023\\NSD3127.csv", encoding="UTF-8")

# starter med å endre navn for enklere visuell forståelse
df <- df %>% rename(holdninger = r26_pcldi, tillit = r26_pccoo, 
                    aldersgruppe = r26P5_1, utdanningsnivå = r26P4_1,
                    arbeidstilstand = r26_bgday, inntekt = r26k2_bginc, 
                    fylke = r26P3, kjønn = r26P1, valg_parti = r26k2_pcpar,
                    sivilstatus = r26_bgciv, samfunnsø = r26k2_pcecs,
                    vektpolitikk = r26_pcpin, trygghet = r26_pcsaf, 
                    politisk = r26k2_bglrs, tillitting = r26_pccos, 
                    flykning = r26k2_dvras, samfunnø1 = r26_pcecz,
                    
                    # innvandringsvariabler
                    innvandring1 = w01_k29, innvandring2 = w03_r3k29, 
                    innvandring3 = r4k29, innvandring4 = r5k29, 
                    innvandring5 = r11bk29, innvandring6 = r14bk29, 
                    innvandring7 = r16bk29, innvandring8 = r18bk29, 
                    innvandring9 = r22bk29, innvandring10 = r25_bgimm)


# fjerner alle som er innvandrere, beholder 1, 97 og 98 for alle innvandrer variabler

df <- df %>%
  filter(if_else(innvandring1 %in% c(1, 97, 98), TRUE, FALSE) &
           if_else(innvandring2 %in% c(1, 97, 98), TRUE, FALSE) &
           if_else(innvandring3 %in% c(1, 97, 98), TRUE, FALSE) &
           if_else(innvandring4 %in% c(1, 97, 98), TRUE, FALSE) &
           if_else(innvandring5 %in% c(1, 97, 98), TRUE, FALSE) &
           if_else(innvandring6 %in% c(1, 97, 98), TRUE, FALSE) &
           if_else(innvandring7 %in% c(1, 97, 98), TRUE, FALSE) &
           if_else(innvandring8 %in% c(1, 97, 98), TRUE, FALSE) &
           if_else(innvandring9 %in% c(1, 97, 98), TRUE, FALSE) &
           if_else(innvandring10 %in% c(1, 97, 98), TRUE, FALSE))

# fjerner rader med verdi 97 og 98 fra holdninger til innvandrere og tillit

df <- subset(df, !(holdninger %in% c(97, 98)))

# kontinuerlige variabler er tillit, trygghet, samfunnsøkonomi


# henter ut variablene som skal brukes

df = select(df, holdninger, tillit, aldersgruppe, kjønn, fylke, trygghet, 
                  utdanningsnivå, inntekt, samfunnsø, arbeidstilstand, sivilstatus, politisk)


# bytter ut 97 og 98 med na i kontinuerlige variabler

library(naniar)

df <- df %>%
  replace_with_na(replace = list(tillit = c(97,98),
                                 trygghet = c(97, 98),
                                 samfunnsø = c(97, 98)))

# endrer tallene til svaralternativene og grupperer

olsdf <- df %>%
  
  # fylke
  mutate(fylke = case_when(
    fylke %in% c(3, 11, 30) ~ 1,
    fylke %in% c(38, 42, 46, 54) ~ 2,
    fylke %in% c(15, 18, 34, 50) ~ 3)) %>% 
  
  # inntekt
  mutate(inntekt = case_when(
    inntekt %in% c(97, 98) ~ NA_real_,
      inntekt %in% c(1, 2, 3, 4) ~ 1,
      inntekt %in% 5 ~ 2,
      inntekt %in% c(6, 7, 8) ~ 3)) %>% 
  
  # aldersgruppe 
  mutate(aldersgruppe = case_when(
    aldersgruppe %in% c(7) ~ 1,
    aldersgruppe %in% c(6, 5, 4) ~ 2,
    aldersgruppe %in% c(3, 2, 1) ~ 3)) %>% 
  
  # arbeidstilstand
  mutate(arbeidstilstand = case_when(
    arbeidstilstand %in% c(97, 98) ~ NA_real_,
    arbeidstilstand %in% c(1, 2, 3, 4, 5) ~ 0,
    arbeidstilstand %in% c(6, 7, 8, 9, 10, 11, 12) ~ 1)) %>% 
  
  # sivilstatus
  mutate(sivilstatus = case_when(
    sivilstatus %in% c(97, 98) ~ NA_real_,
    sivilstatus %in% c(1, 2, 5, 6, 7) ~ 0,
    sivilstatus %in% c(3, 4) ~ 1)) %>% 
  
  # politisk side
  mutate(politisk = case_when(
    politisk %in% c(0, 1, 2, 3) ~ 1,
    politisk %in% c(4, 5, 6) ~ 2,
    politisk %in% c(7, 8, 9, 10) ~ 3))

# kjønnsdummy (1 for mann, 0 for kvinne)
olsdf$kjønn <- ifelse(olsdf$kjønn == 1, 1, 0)


# gir navn og setter som faktor, ekskluderer 97 og 98
olsdf <- olsdf %>%
  
  # utdanning
  mutate(utdanningsnivå = factor(utdanningsnivå, 
                            levels = c(1, 2, 3),
                            labels = c("Grunnskole", "Videregående", "Høyere"),
                            exclude = 97)) %>%
  
  # fylke/kontakt
  mutate(kontakt = factor(fylke,
                        levels = c(1, 2, 3),
                        labels = c("Lav", "Middels", "Høy"))) %>% 
  
  # inntekt
  mutate(inntekt = factor(inntekt, 
                          levels = c(1, 2, 3),
                          labels = c("Lav", "Median", "Høy"),
                          exclude = c(97, 98))) %>%
  
  # aldersgruppe
  mutate(aldersgruppe = factor(aldersgruppe,
                                levels = c(1, 2, 3),
                                labels = c("Yngre", "Middelaldrer", "Eldre"))) %>% 
  
  # politisk 
  mutate(politisk = factor(politisk, 
                           levels = c(1, 2, 3),
                           labels = c("Venstre", "Midten", "Høyre"))) %>% 
  
  # faktor
  mutate(utdanningsnivå = fct_relevel(utdanningsnivå, "Videregående"),
         kontakt = fct_relevel(kontakt, "Middels"),
         inntekt = fct_relevel(inntekt, "Median"),
         aldersgruppe = fct_relevel(aldersgruppe, "Middelaldrer"),
         politisk = fct_relevel(politisk, "Midten"))

# fjerner gamle kolonner
olsdf = subset(olsdf, select = -c(fylke))

# setter ikke navn, men setter som factor
olsdf <- olsdf %>% 
  mutate(
    arbeidstilstand = factor(arbeidstilstand, levels = c(0, 1), 
                             exclude = c(97, 98))) %>% 
  mutate(
    kjønn = factor(kjønn, levels = c(0, 1))) %>% 
  mutate(
    sivilstatus = factor(sivilstatus, levels = c(0, 1),
                         exclude = c(97, 98)))


##############################OSL MODELL########################################

# modell likning 1
olsmodell1 <- lm(holdninger ~ tillit + aldersgruppe, data = olsdf)

# modell likning 2
olsmodell2 <- lm(holdninger ~ tillit + aldersgruppe + kjønn + arbeidstilstand + 
                   sivilstatus + inntekt + samfunnsø + 
                   utdanningsnivå + kontakt + trygghet + politisk, data = olsdf)

# modell likning 3
olsmodell3 <- lm(holdninger ~ tillit + aldersgruppe + kjønn + arbeidstilstand + 
                   sivilstatus + inntekt + samfunnsø + 
                   utdanningsnivå + kontakt + trygghet + politisk + 
                   tillit*aldersgruppe, data = olsdf)

# sjekker for multikollinaritet forhold
vif(olsmodell2)


plot(olsmodell2, which = 2, main = "Model Fit")

summary(olsmodell2)

# https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html
# https://strengejacke.github.io/sjPlot/articles/table_css.html
tab_model(olsmodell1, olsmodell2, p.style = "stars", digits = 4, show.se = TRUE,  
          # setter navn
          pred.labels = c("Intercept", "Sosial tillit", "Yngre alder", "Eldre alder",
                          "Kjønn mann", "Arbeidsløs", "Gift eller samboer", "Lav inntekt", 
                          "Høy inntekt", "Samfunnsøkonomi", "Grunnskole", "Høyere", "Lav kontakt", "Høy kontakt",
                          "Trygghet Nærområdet", "Politisk venstre", "Politisk høyre"),
          dv.labels = c("Modell 1", "Modell 2"),
          string.pred = "Koeffisient",
          string.p = "P-Verdi",
          string.est = "Estimat",
          string.ci = "Konfindensintervall",
          string.se = "Std Error",
          # endrer farge
          CSS = list(
            css.depvarhead = 'color: red;',
            css.centeralign = 'text-align: left;',
            css.firsttablecol = 'font-weight: bold;',
            css.summary = 'color: blue;')
)


############################ Grafiske resultater ###############################
# https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html 

plot_model(olsmodell2, type = "pred", terms = c("tillit", "aldersgruppe"),
           title = "Resultat for Holdninger og Sosial tillit",
           legend.title = "Aldersgruppe",
           axis.title = c("Sosial tillit", "Holdninger"))

plot_model(olsmodell3, type = "pred", terms = c("tillit", "aldersgruppe"),
           title = "Resultat for Holdninger og Sosial tillit",
           legend.title = "Aldersgruppe",
           axis.title = c("Sosial tillit", "Holdninger"))


######################### Holdninger og aldersgrupper ##########################

# finner gjennomsnitt
mean <- olsdf %>% 
  group_by(aldersgruppe) %>% 
  summarize(meanv = mean(holdninger))

# tar bort den ene raden
mean <- mean[-c(4), ]

# plotter
olsdf %>% 
  na.omit() %>% 
  ggplot(aes(x = holdninger, fill = aldersgruppe)) +
  geom_bar() +
  facet_wrap(~aldersgruppe) + #splitter opp grafene
  labs(x = "Holdninger til innvandrere", y = "Antall personer", 
       title = "Holdninger til innvandrere per aldersgruppe") +
  geom_vline(data = mean, aes(xintercept=meanv)) +
  theme_bw()


olsdf %>% 
  na.omit() %>% 
  ggplot(aes(x = arbeidstilstand, fill = aldersgruppe)) +
  geom_bar() +
  facet_wrap(~aldersgruppe) +
  labs(x = "Arbeids tilstand", y = "Antall personer", 
       title = "Arbeids tilstand per aldersgruppe",
       subtitle = "0 = sysselsatt, 1 = arbeidsløs") +
  theme_bw()

olsdf %>% 
  na.omit() %>% 
  ggplot(aes(x = politisk, fill = aldersgruppe)) +
  geom_bar() +
  facet_wrap(~aldersgruppe) +
  labs(x = "Politisk plassering", y = "Antall personer", 
       title = "Politisk plassering per aldersgruppe") +
  theme_bw()


################################################################################


# DESKRIPTIV STATISTIKK


################################################################################

# endrer tallene til svaralternativene for fylke til en kontinuerlig skala

tabell <- df %>%
  mutate(fylke = case_when(
    fylke == 3 ~ 1,
    fylke == 11 ~ 2,
    fylke == 15 ~ 3,
    fylke == 18 ~ 4,
    fylke == 30 ~ 5, 
    fylke == 34 ~ 6,
    fylke == 38 ~ 7,
    fylke == 42 ~ 8, 
    fylke == 46 ~ 9,
    fylke == 50 ~ 10,
    fylke == 54 ~ 11
  ))

# tillitsvariabelen starter på null, dette må endres til en og de følgende svaralternativene må flyttes opp et nivå

tabell <- tabell %>% 
  mutate(tillit = case_when(
    tillit == 0 ~ 1,
    tillit == 1 ~ 2,
    tillit == 2 ~ 3, 
    tillit == 3 ~ 4,
    tillit == 4 ~ 5, 
    tillit == 5 ~ 6, 
    tillit == 6 ~ 7,
    tillit == 7 ~ 8,
    tillit == 8 ~ 9,
    tillit == 9 ~ 10, 
    tillit == 10 ~ 11
  ))

# summariser tabellen 
tabell <- tabell %>%
  summarise_all(~ table(factor(., levels = c(1:13)))) %>%
  mutate(svar_alternativer = paste(1:13)) %>%
  select(svar_alternativer, everything())

# https://stackoverflow.com/questions/73726177/create-color-bars-by-column-and-group-in-rmarkdown-presentation
# https://kcuilla.github.io/reactablefmtr/articles/reactablefmtr_cookbook.html

# henter pakker
library(reactable)
library(reactablefmtr)

# grupperer
df1 <- group_by(tabell, svar_alternativer)

# lager tabell med visuelle målinger
reactable(df1, 
          theme = clean(),
          pagination = FALSE,
          columns = list(
            holdninger = colDef(
              name = "Holdninger",
              align = "left", 
              cell = data_bars(
                data = df1,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            ),
            tillit = colDef(
              name = "Tillit",
              align = "left",
              cell = data_bars(
                data = df1,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            ),
            aldersgruppe = colDef(
              name = "Aldersgruppe",
              align = "left", 
              cell = data_bars(
                data = df1,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            ),
            kjønn = colDef(
              name = "Kjønn",
              align = "left", 
              cell = data_bars(
                data = df1,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            ),
            fylke = colDef(
              name = "Fylke",
              align = "left", 
              cell = data_bars(
                data = df1,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            ),
            trygghet = colDef(
              name = "Trygghet", 
              align = "left", 
              cell = data_bars(
                data = df1,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            ),
            utdanningsnivå = colDef(
              name = "Utdanningsnivå",
              align = "left", 
              cell = data_bars(
                data = df1,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            ),
            inntekt = colDef(
              name = "Inntekt",
              align = "left", 
              cell = data_bars(
                data = df1,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            ),
            samfunnsø = colDef(
              name = "Samfunnsøkonomi",
              align = "left",
              cell = data_bars(
                data = df1,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            ),
            arbeidstilstand = colDef(
              name = "Arbeidstilstand",
              align = "left", 
              cell = data_bars(
                data = df1,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            ),
            sivilstatus = colDef(
              name = "Sivilstatus", 
              align = "left", 
              cell = data_bars(
                data = df1,,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            ),
            politisk = colDef(
              name = "Politisk",
              align = "left", 
              cell = data_bars(
                data = df1,
                background = '#F1F1F1',
                min_value = 0,
                max_value = 5000,
                text_position = 'outside-end',
                round_edges = TRUE
              )
            )
          )
        ) %>% 
  add_title(
    title = 'Andel av totale observasjoner'
  )



################################# Summary ######################################

# fjerner 97 og 98 verdier
df2 <- df %>%
  replace_with_na(replace = list(utdanningsnivå = 97,
                                 inntekt = 97,
                                 arbeidstilstand = 97,
                                 sivilstatus = c(97, 98))) %>% 
  subset(select = -c(fylke) )

# endrer tallene til svaralternativene og grupperer

df2 <- df2 %>%
  
  # inntekt
  mutate(inntekt = case_when(
    inntekt %in% c(97, 98) ~ NA_real_,
    inntekt %in% c(1, 2, 3, 4) ~ 1,
    inntekt %in% 5 ~ 2,
    inntekt %in% c(6, 7, 8) ~ 3)) %>% 
  
  # aldersgruppe 
  mutate(aldersgruppe = case_when(
    aldersgruppe %in% c(7) ~ 1,
    aldersgruppe %in% c(6, 5, 4) ~ 2,
    aldersgruppe %in% c(3, 2, 1) ~ 3)) %>% 
  
  # arbeidstilstand
  mutate(arbeidstilstand = case_when(
    arbeidstilstand %in% c(97, 98) ~ NA_real_,
    arbeidstilstand %in% c(1, 2, 3, 4, 5) ~ 0,
    arbeidstilstand %in% c(6, 7, 8, 9, 10, 11, 12) ~ 1)) %>% 
  
  # sivilstatus
  mutate(sivilstatus = case_when(
    sivilstatus %in% c(97, 98) ~ NA_real_,
    sivilstatus %in% c(1, 2, 5, 6, 7) ~ 0,
    sivilstatus %in% c(3, 4) ~ 1)) %>% 
  
  # politisk side
  mutate(politisk = case_when(
    politisk %in% c(0, 1, 2, 3) ~ 1,
    politisk %in% c(4, 5, 6) ~ 2,
    politisk %in% c(7, 8, 9, 10) ~ 3))

# kjønnsdummy (1 for mann, 0 for kvinne)
df2$kjønn <- ifelse(df2$kjønn == 1, 1, 0)

library(vtable)

# setter navn
labs <- c("Holdninger",
          "Tillit",
          "Aldersgruppe",
          "Kjønn",
          "Tryghhet",
          "Utdanningsnivå",
          "Inntekt",
          "Samfunnsøkonomi",
          "Arbeidstilstand",
          "Sivilstatus",
          "Politisk")

# lager summary tabell 
st(df2, labels = labs, 
  summ = c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
  digits = 4,
  summ.names = list(
    c('Antall observasjoner','Gjennomsnitt','Standardavvik',
      'Minimum','Maksimum')))

# predikerer verdiene fra ols modellen
pred <- ggpredict(olsmodell1, terms = c("tillit", "aldersgruppe"))

# plotter predikerte verdier
ggplot(pred, aes(x = x, y = predicted, color = group)) +
  geom_point(size = 2.5) +
  geom_line(linetype = "dashed") +
  labs(title = "Forventede verdier for holdninger basert på tillit og aldersgruppe",
       x = "Tillit",
       y = "Holdninger",
       color = "Aldersgruppe") +
  theme_light()




