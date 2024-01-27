# Ulike datasett som er tatt med i oppgaven er lagt inn i github repo "https://github.com/sumsar027/SOK-2209-Bacheloroppgave"

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Setter rstudio norske bokstaver
Sys.setlocale(locale="no_NO")

# Nedlastning av pakker
  library(ggplot2)
  library(tidyverse)
  library(dplyr)
  library(gglorenz)
  library(ineq)

  # Pakker for kart av Europa
    library(rnaturalearth)
    library(sf)
    library(wbstats)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Henting av datasett "Our World Data, Trust in Europa"
trusteuropa <- read.csv("C:\\Users\\Eier\\Downloads\\interpersonal-trust-in-europe.csv")

# Henter ut land vi skal ha 

valgt_trusteuropa <- trusteuropa[trusteuropa$Code %in% c('NOR','SWE','DNK','FIN','FRA','DEU','ITA','POL','BGR'), ]

ggplot(valgt_trusteuropa, aes(x = Code, y = Trust.in.others..Eurostat..2015.. , fill = Code)) +
  geom_bar(stat = "identity") +
  labs(x = "Land", y = "Prosent", title = "Tillit til andre, Europa") +
  theme_minimal()

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Henting av datasett "Norsk medborgerpanel runde 26"
df <- read.csv("C:\\Users\\Eier\\Downloads\\2249\\Norsk medborgerpanel runde 26, 2023\\NSD3127.csv")
df

# Starter med å endre navn for enklere visuell forståelse
df <- df %>% rename(sivilstatus = r26_bgciv, type_området = r26k2_bgurb, høyest_utdanning = r26P4_2,
                      antall_barn = r26k2_bgchi, kjønn = r26P1, landsdel = r26P2, fylke = r26P3, 
                        fødselsår = r26P5_1, brutto_inntekt = r26k2_bginc, type_arbeid = r26_bgsct, 
                          tillit = r26_pccoo, politisk_tillit = r26_pccop, tillit_stortinget = r26_pccos, 
                            tillit_media = r26_pccom, vurdering_regjering = r26_pcsag,
                              vurdering_egenøkonomi = r26_pceco, interesse_politikk = r26_pcpin,
                                vurdering_livet = r26_pcsal, investering_utdanning = r26k2_powpr_2, 
                                  valg_parti = r26k2_pcpar, vurdering_demokrati = r26k2_pcsad)

# Lager nytt datasett med de variablene vi skal bruke for en enklere visualisering 

valgt_df = select(df, kjønn, fødselsår, sivilstatus, landsdel, fylke, høyest_utdanning,
                    brutto_inntekt, type_arbeid, vurdering_egenøkonomi, tillit, politisk_tillit, 
                      tillit_stortinget, tillit_media, vurdering_regjering, interesse_politikk, 
                        valg_parti, vurdering_demokrati, vurdering_livet, investering_utdanning)

# Lager en visuell beskrivelse av utdanning ved bruk av barplot 

par(mar=c(6,3,1,1))

utdanning1 <- 
  barplot(table(valgt_df$høyest_utdanning), 
          names.arg=c("Ingen","Grunnskole","VGS","VGS yrkesfaglig",
                        "VGS påbygg", "Universitet/høgskole", "Høgskole 3-4",
                          "Universitet 3-4", "Høgskole 5-6", "Universitet 5-6", "Forskernivå",
                            "Annet", "Ikke svart"),
          las=2, 
          main="Høyeste utdanningsnivå",
          ylab="Antall",
          border="lightblue",
          col=valgt_df$høyest_utdanning)

# Lager en visuell beskrivelse av 
