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
  library(naniar) # For å sette NA verdier, https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html

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

# Henting av datasett "Trust in government" fra OECD (2024)

oecdtrust <- read.csv("C:\\Users\\Eier\\Downloads\\Data for bacheloroppgave\\DP_LIVE_13022024235619732.csv")

valgt_oecdtrust <- oecdtrust[oecdtrust$ï..LOCATION %in% c('NOR','SWE','DNK','FIN','FRA','DEU','ITA','POL','BGR'), ]

ggplot(valgt_oecdtrust, aes(x = factor(TIME), y = Value, fill = ï..LOCATION)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "År", y = "Verdi", title = "Verdi per år for hvert land") +
  theme_minimal() +
  theme(legend.position = "top")

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
                                      mening_innvandrere = r26_pcldi, jobb_tilstand = r26_bgday, 
                                        statsborgerskap = w01_k28, 
                    
                    innvandring1 = w01_k29, innvandring2 = w03_r3k29, innvandring3 = r4k29, innvandring4 = r5k29,
                    innvandring5 = r11bk29, innvandring6 = r14bk29, innvandring7 = r16bk29, innvandring8 = r18bk29,
                    innvandring9 = r22bk29, innvandring10 = r25_bgimm)


# Fjerner rader med verdi 97 og 98 fra tillitsvariabelen

df <- subset(df, !(tillit_stortinget %in% c(97, 98)))

# Lager nytt datasett med de variablene vi skal bruke for en enklere visualisering 

valgt_df = select(df, kjønn, fødselsår, sivilstatus, statsborgerskap, innvandring1, landsdel, fylke, 
                    høyest_utdanning, brutto_inntekt, type_arbeid, jobb_tilstand, vurdering_egenøkonomi, 
                      tillit, politisk_tillit, tillit_stortinget, tillit_media, vurdering_regjering, 
                        interesse_politikk, valg_parti, vurdering_demokrati, vurdering_livet, 
                          investering_utdanning, trygghet_nær, mening_innvandrere_bo, mening_innvandrere)



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Emperi for å finne referansegruppene 

# Endrer alle 97 og 98 verdier til NAs

empiri <- valgt_df %>% replace_with_na_all(condition = ~.x == 97)
empiri <- empiri %>% replace_with_na_all(condition = ~.x == 98)

# Ser gjennom en oppsummering av alle variablene
summary(empiri)

# Går gjennom antall observasjoner for de forskjellige svaralternativene 
# for hver variabel i datasettet

antall_observasjoner <- function(valgt_df) {
  for (col in names(valgt_df)) {
    cat("Variabel:", col, "\n")
    cat("Antall svaralternativer:", length(unique(valgt_df[[col]])), "\n")
    cat("Antall observasjoner:\n")
    print(table(valgt_df[[col]]))
    cat("\n")
  }}

antall_observasjoner(valgt_df)

# Gjennomgang av variabler, referansegruppe;

# Utdanning: Høyere (3)
# Inntekt: 500-600 (5)
# Sivilstatus: Gift (4)
# Kjønn: Kvinne (2)
# Innvandring: Ikke innvandrer (1)
# Statsborgerskap: Norsk statsborgerskap (1)
# Fødselsår: 30-40 (6)
# Jobb tilstand: Lønnet fulltid (1)
# Vurdering økonomi: God (2)
# Interesse politikk: Interresert (2)
# Vurdering demokrati: Tilfreds (2)
# Vurdering regjering: Tilfreds (2)
# Vurdering livet: Tilfreds (2)
# Trygghet nærområde siste årene: Ingen endring (3)
# Mening innvandrere bosetter seg: Verken fordel eller ulempe (4)
# Mening innvandrere: Verken misliker eller liker (4) 
# Investering utdanning: Bare NA, tas ikke med


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Lager dummy variabler, bruker kodebok for å se svaralternativer
# Mulig at det er bedre å gjøre flere om til slik sivilstatus er gjort,
# for enklere visning i regresjonsmodellene


# Dummy for utdanningsnivå 
#mulig at det er bedre å bruke den andre utdanningsvariabelen

valgt_df <- valgt_df %>%
  mutate(utdanning = factor(høyest_utdanning, 
                            levels = c(1, 2, 3),
                            labels = c("Grunnskole", "Videregående", "Høyere"),
                            exclude = 97)) %>%
  mutate(utdanning = fct_relevel(utdanning, "Høyere")) # sammenligningsnivå


# Dummy for inntektsnivå 

valgt_df <- valgt_df %>% 
  mutate(inntekt = factor(brutto_inntekt,
                          levels = c(1,2,3,4,5,6,7,8),
                          labels = c("150_og_under", "150-300", "300-400", "400-500", 
                                     "500-600", "600-700", "700-1000", "1000_og_mer"),
                          exclude = c(97,98))) %>% 
  mutate(inntekt = fct_relevel(inntekt, "500-600")) # sammenligningsnivå


# Dummy for kjønn 
valgt_df$mann <- ifelse(valgt_df$kjønn == 1, 1, 0)


# Dummy for sivilstatus
# blir gift eller ikke der de andre alternativene blir satt til 0 

valgt_df <- valgt_df %>%
  mutate(gift_samboer = case_when(
    sivilstatus %in% c(97, 98) ~ NA_real_,
    sivilstatus %in% c(1, 2, 5, 6, 7) ~ 0,
    TRUE ~ 1
  )) 


# Dummy for innvandrer


# Dummy statsborgerskap

valgt_df$borgerskap <- ifelse(valgt_df$statsborgerskap == 2, 2, 0)


# Dummy for fødselsår 
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


# Dummy for jobb tilstand

valgt_df <- valgt_df %>% 
  mutate(arbeids_tilstand = factor(jobb_tilstand,
                                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                         labels = c("Lønnet fulltid", "Lønnet deltid 30", "Lønnet deltid u30", 
                                                    "Selvdrivende", "Student", "Arbeidsledig søkende",
                                                    "Arbedisledig ikke-søkende", "Ufør", "Pensjonert",
                                                    "Militæret", "Hjemmeværende", "Annet"),
                                         exclude = c(97, 98))) %>% 
  mutate(arbeids_tilstand = fct_relevel(arbeids_tilstand, "Lønnet fulltid")) # sammenligningsnivå


# Dummy for vurdering egen økonomi

valgt_df <- valgt_df %>% 
  mutate(egenøkonomi = factor(vurdering_egenøkonomi,
                                   levels = c(1, 2, 3, 4, 5, 6, 7),
                                   labels = c("Svært god", "God", "Noe god", "Verken god eller dårlig", 
                                              "Noe dårlig", "Dårlig", "Svært dårlig"),
                                   exclude = c(97, 98))) %>% 
  mutate(egenøkonomi = fct_relevel(egenøkonomi, "God")) # sammenligningsnivå


# Dummy for interesse politikk 

valgt_df <- valgt_df %>% 
  mutate(politikk = factor(interesse_politikk,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("Svært interessert", "Interessert", "Noe interresert", 
                                         "Lite interessert", "Ikke interessert"),
                              exclude = c(97, 98))) %>% 
  mutate(politikk = fct_relevel(politikk, "Interessert")) # sammenligningsnivå


# Dummy for vurdering av det norske demokratiet

valgt_df <- valgt_df %>% 
  mutate(demokrati = factor(vurdering_demokrati,
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("Svært tilfreds", "Tilfreds", "Noe tilfreds", 
                                      "Lite tilfreds", "Ikke tilfreds"),
                           exclude = c(97, 98))) %>% 
  mutate(demokrati = fct_relevel(demokrati, "Tilfreds")) # sammenligningsnivå


# Dummy for vurdering av den norske regjering

valgt_df <- valgt_df %>% 
  mutate(regjering = factor(vurdering_regjering,
                            levels = c(1, 2, 3, 4, 5),
                            labels = c("Svært tilfreds", "Tilfreds", "Noe tilfreds", 
                                       "Lite tilfreds", "Ikke tilfreds"),
                            exclude = c(97, 98))) %>% 
  mutate(regjering = fct_relevel(regjering, "Tilfreds")) # sammenligningsnivå


# Dummy for vurdering av livet

valgt_df <- valgt_df %>% 
  mutate(livet = factor(vurdering_livet,
                            levels = c(1, 2, 3, 4, 5),
                            labels = c("Svært tilfreds", "Tilfreds", "Noe tilfreds", 
                                       "Lite tilfreds", "Ikke tilfreds"),
                            exclude = c(97, 98))) %>% 
  mutate(livet = fct_relevel(livet, "Tilfreds")) # sammenligningsnivå


# Dummy for trygghet i nærområdet de siste årene

valgt_df <- valgt_df %>% 
  mutate(trygghet = factor(trygghet_nær,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("Mye tryggere", "Noe tryggere", "Ingen endring", 
                                   "Noe mer utrygt", "Mye mer utrygt"),
                        exclude = c(97, 98))) %>% 
  mutate(trygghet = fct_relevel(trygghet, "Ingen endring")) # sammenligningsnivå



# Dummy for mening om at innvandrere bosetter seg

valgt_df <- valgt_df %>% 
  mutate(innvandrere_bosetting = factor(mening_innvandrere_bo,
                           levels = c(1, 2, 3, 4, 5, 6, 7),
                           labels = c("Svært stor fordel", "Stor fordel", "En viss fordel", 
                                      "Verken en fordel eller ulempe", "En viss ulempe",
                                      "Stor ulempe", "Svært stor ulempe"),
                           exclude = c(97, 98))) %>% 
  mutate(innvandrere_bosetting = fct_relevel(innvandrere_bosetting, "Verken en fordel eller ulempe")) # sammenligningsnivå


# Dummy for mening om innvandrere 

valgt_df <- valgt_df %>% 
  mutate(innvandrere = factor(mening_innvandrere,
                                        levels = c(1, 2, 3, 4, 5, 6, 7),
                                        labels = c("Svært stor fordel", "Stor fordel", "En viss fordel", 
                                                   "Verken en fordel eller ulempe", "En viss ulempe",
                                                   "Stor ulempe", "Svært stor ulempe"),
                                        exclude = c(97, 98))) %>% 
  mutate(innvandrere = fct_relevel(innvandrere, "Verken en fordel eller ulempe")) # sammenligningsnivå

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


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

# Innvandring ikke tatt med, må fikses

reduced_data <- valgt_df %>%
  dplyr::select(tillit_stortinget, utdanning, inntekt, gift_samboer, mann, borgerskap,
                aldersgruppe, arbeids_tilstand, egenøkonomi, politikk, demokrati, 
                regjering, livet, trygghet, innvandrere_bosetting, innvandrere)

saveRDS(reduced_data, "reduced_data.rds")

reduced_data <- readRDS("reduced_data.rds")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Regresjonsmodell og analyse
# Tenker at det er best å ha med to forskjellige modeller

rsa <- lm(tillit_stortinget ~ utdanning + inntekt + gift_samboer + mann + 
            borgerskap + aldersgruppe + arbeids_tilstand + egenøkonomi +
            politikk + demokrati + regjering + livet + trygghet +
            innvandrere_bosetting + innvandrere, data = reduced_data)

summary(rsa)

plot(rsa, main = "Model fit")

