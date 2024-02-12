# Ulike datasett som er tatt med i oppgaven er lagt inn i github repo "https://github.com/sumsar027/SOK-2209-Bacheloroppgave"

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Setter rstudio norske bokstaver
Sys.setlocale(locale="no_NO")

# Legger inn working directory 
setwd("C:\\Users\\Eier\\Downloads\\Data for bacheloroppgave")

# Nedlastning av pakker
  library(ggplot2)
  library(tidyverse)
  library(dplyr)
  library(gglorenz)
  library(ineq)
  library(lubridate)
  library(scales)
  library(readr)
  library(PxWebApiData)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Henting av datasett "Our World in Data, Trust in Europa" og "Our World in Data, Trust in Government"
trusteuropa <- read.csv("C:\\Users\\Eier\\Downloads\\Data for bacheloroppgave\\interpersonal-trust-in-europe.csv")
trustgovernment <- read.csv("C:\\Users\\Eier\\Downloads\\Data for bacheloroppgave\\oecd-average-trust-in-governments.csv")


# Henter ut land vi skal ha 

valgt_trusteuropa <- trusteuropa[trusteuropa$Code %in% c('NOR','SWE','DNK','FIN','FRA','DEU','ITA','POL','BGR'), ]

ggplot(valgt_trusteuropa, aes(x = Code, y = Trust.in.others..Eurostat..2015.. , fill = Code)) +
  geom_bar(stat = "identity") +
  labs(x = "Land", y = "Prosent", title = "Tillit til andre, Europa") +
  theme_minimal()

ggplot(trustgovernment, aes(x = Year, y = trust_government)) +
  geom_line() +
  labs(x = "År", y = "Prosent", title = "Tillit til Myndigheter i gjennomsnitt") +
  scale_x_continuous(labels = number_format(accuracy = 1)) +
  theme_minimal()

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Henting av datasett "Norsk medborgerpanel runde 26"
df <- read.csv("C:\\Users\\Eier\\Downloads\\2249\\Norsk medborgerpanel runde 26, 2023\\NSD3127.csv")
df

# Starter med å endre navn for enklere visuell forståelse
df <- df %>% rename(sivilstatus = r26_bgciv, type_området = r26k2_bgurb, høyest_utdanning = r26P4_1,
                      antall_barn = r26k2_bgchi, kjønn = r26P1, landsdel = r26P2, fylke = r26P3, 
                        fødselsår = r26P5_1, brutto_inntekt = r26k2_bginc, type_arbeid = r26_bgsct, 
                          tillit = r26_pccoo, politisk_tillit = r26_pccop, tillit_stortinget = r26_pccos, 
                            tillit_media = r26_pccom, vurdering_regjering = r26_pcsag,
                              vurdering_egenøkonomi = r26_pceco, interesse_politikk = r26_pcpin,
                                vurdering_livet = r26_pcsal, investering_utdanning = r26k2_powpr_2, 
                                  valg_parti = r26k2_pcpar, vurdering_demokrati = r26k2_pcsad, 
                                    trygghet_nær = r26_pcsaf, mening_innvandrere_bo = r26_pcimm, 
                                      mening_innvandrere = r26_pcldi,
                    
                    innvandring1 = w01_k29, innvandring2 = w03_r3k29, innvandring3 = r4k29, innvandring4 = r5k29,
                    innvandring5 = r11bk29, innvandring6 = r14bk29, innvandring7 = r16bk29, innvandring8 = r18bk29,
                    innvandring9 = r22bk29, innvandring10 = r25_bgimm)


# Fjerner rader med verdi 97 og 98 fra tillitsvariabelen

df <- subset(df, !(tillit_stortinget %in% c(97, 98)))

# Lager nytt datasett med de variablene vi skal bruke for en enklere visualisering 

valgt_df = select(df, kjønn, fødselsår, sivilstatus, innvandring1, landsdel, fylke, høyest_utdanning,
                    brutto_inntekt, type_arbeid, vurdering_egenøkonomi, tillit, politisk_tillit, 
                      tillit_stortinget, tillit_media, vurdering_regjering, interesse_politikk, 
                        valg_parti, vurdering_demokrati, vurdering_livet, investering_utdanning, 
                          trygghet_nær, mening_innvandrere_bo, mening_innvandrere)



# Lager en visuell beskrivelse av utdanning ved bruk av barplot 

par(mar=c(6,3,1,1))

utdanning1 <- 
  barplot(table(valgt_df$høyest_utdanning), 
          names.arg=c("Ingen/Grunnskole","VGS","Universitet/høgskole","Ikke svart"),
          las=2, 
          main="Høyeste utdanningsnivå",
          ylab="Antall",
          border="lightblue",
          col=valgt_df$høyest_utdanning)

# Lager en visuell beskrivelse av fødselsår

fødselsår1 <- 
  barplot(table(valgt_df$fødselsår),
          names.arg=c("1939_og_før", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990_og_etter"),
                      las = 2,
                      main = "Fødselsår",
                      ylab = "Antall",
                      border = "lightblue",
                      col = valgt_df$fødselsår)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Emperi for å finne referansegruppene 


# Gjennomgang av variabler, referansegruppe;

# Utdanning: Videregående eller høyere 
# Inntekt: 150-300 
# Sivilstatus: Ikke gift
# Kjønn: Kvinne
# Innvandring: Ikke innvandrer
# Investering utdanning: Samme som nå


summary(valgt_df)
table(valgt_df$fødselsår)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Lager dummy variabler, bruker kodebok for å se svaralternativer

# Dummy for kjønn 
valgt_df$mann <- ifelse(valgt_df$kjønn == 1, 1, 0)

# Dummy for alder 
valgt_df <- valgt_df %>%
  mutate(aldersgruppe = case_when(
    fødselsår == 1 ~ '1939_og_før',
    fødselsår == 2 ~ '1940-1949',
    fødselsår == 3 ~ '1950-1959',
    fødselsår == 4 ~ '1960-1969',
    fødselsår == 5 ~ '1970-1979',
    fødselsår == 6 ~ '1980-1989',
    TRUE ~ '1990_og_etter'
  )) %>%
  mutate(aldersgruppe = factor(aldersgruppe, levels = 
    c('1939_og_før', '1940-1949', '1950-1959', '1960-1969', '1970-1979', '1980-1989', '1990_og_etter')))

# Dummy for sivilstatus

# blir gift eller ikke der de andre alternativene blir satt til 0
# Ser at variabelen har 97 og 98 som må bli fjernet 

valgt_df <- valgt_df %>%
  mutate(gift_samboer = case_when(
    sivilstatus %in% c(97, 98) ~ NA_real_,
    sivilstatus %in% c(1, 2, 5, 6, 7) ~ 0,
    TRUE ~ 1
  )) 

# Dummy for utdanningsnivå, #mulig at det er bedre å bruke den andre utdanningsvariabelen

valgt_df <- valgt_df %>%
  mutate(utdanning = factor(høyest_utdanning, 
                            levels = c(1, 2, 3),
                            labels = c("Grunnskole", "Videregående", "Høyere"),
                            exclude = 97)) %>%
  mutate(utdanning = fct_relevel(utdanning, "høyere")) # sammenligningsnivå

# Dummy for investering i utdanning

valgt_df <- valgt_df %>% 
  mutate(investering_utdanningD = factor(investering_utdanning,
                            levels = c(1, 2, 3, 4, 5, 6),
                            labels = c("Mye mindre", "Litt mindre", "Det samme", 
                                       "Litt mer", "Mye mer", "Vet ikke"),
                            exclude = c(97, 98))) %>% 
  mutate(investering_utdanningD = fct_relevel(investering_utdanningD, "Det samme")) # sammenligningsnivå

# Dummy for inntektsnivå 

valgt_df <- valgt_df %>% 
  mutate(inntekt = factor(brutto_inntekt,
                          levels = c(1,2,3,4,5,6,7,8),
                          labels = c("150_og_under", "150-300", "300-400", "400-500", 
                                     "500-600", "600-700", "700-1000", "1000_og_mer"),
                          exclude = c(97,98))) %>% 
  mutate(inntekt = fct_relevel(inntekt, "300-400")) # sammenligningsnivå



# Henter data for innvandring fra andre runder siden innvandringsstatusen endrer seg ikke over tid

innvandring_data <- df %>%
  dplyr::select(innvandring1, innvandring2, innvandring3, innvandring4, innvandring5, innvandring6, 
innvandring7, innvandring8, innvandring9, innvandring10)

# Kanskje prøve å gjøre om alle 97 og 98 til NA verdier før man setter opp dummy 
innvandring_data <- innvandring_data %>% 
  mutate()

# Litt usikker her, alle blir NA 
innvandring_data <- innvandring_data %>%
  mutate(innvandring = case_when(
    innvandring1 %in% c(97, 98) ~ NA_real_, 
    innvandring2 %in% c(97, 98) ~ NA_real_, 
    innvandring3 %in% c(97, 98) ~ NA_real_, 
    innvandring4 %in% c(97, 98) ~ NA_real_, 
    innvandring5 %in% c(97, 98) ~ NA_real_, 
    innvandring6 %in% c(97, 98) ~ NA_real_, 
    innvandring7 %in% c(97, 98) ~ NA_real_, 
    innvandring8 %in% c(97, 98) ~ NA_real_, 
    innvandring9 %in% c(97, 98) ~ NA_real_, 
    innvandring10 %in% c(97, 98) ~ NA_real_, 
    
    innvandring1 %in% c(1, 3, 4, 5) ~ 0,
    innvandring2 %in% c(1, 3, 4, 5) ~ 0,
    innvandring3 %in% c(1, 3, 4, 5) ~ 0,
    innvandring4 %in% c(1, 3, 4, 5) ~ 0,
    innvandring5 %in% c(1, 3, 4, 5) ~ 0,
    innvandring6 %in% c(1, 3, 4, 5) ~ 0,
    innvandring7 %in% c(1, 3, 4, 5) ~ 0,
    innvandring8 %in% c(1, 3, 4, 5) ~ 0,
    innvandring9 %in% c(1, 3, 4, 5) ~ 0,
    innvandring10 %in% c(1, 3, 4, 5) ~ 0,

    TRUE ~ 1
  ))



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Deskriptiv statistikk

# (Må fikse innvandring, ikke tatt med her)

rdf <- valgt_df %>% 
  dplyr::select(tillit_stortinget, mann, aldersgruppe, gift_samboer, utdanning, 
                investering_utdanningD, inntekt)

saveRDS(rdf, "rdf.rds")

rdf <- readRDS("rdf.rds")

# Beregner gjennomsnitt og standardavvik til tillit_stortinget
tillit_gj <- mean(rdf$tillit_stortinget, na.rm = TRUE)
tillit_sd <- sd(rdf$tillit_stortinget, na.rm = TRUE)


cat("tillit - gjennomsnitt:", tillit_gj, "\n")
cat("tillit standard avvik:", tillit_sd, "\n")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Regresjonsanalyse

rsa <- lm(tillit_stortinget ~ aldersgruppe + utdanning + mann + utdanning + 
            investering_utdanningD + inntekt + gift_samboer, data = rdf)

summary(rsa)

plot(rsa, main = "Model fit")
