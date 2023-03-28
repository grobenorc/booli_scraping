---
title: "booli_scraping_inladdning_Data"
author: "xxx"
date: "`r Sys.Date()`"
output: html_document
---

---
title: "booli_scraping_inladdning_Data"
author: "Claes"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

setwd("C:/Users/claes/OneDrive/R programmering/booli_scraping")

library(tidyverse)
library(rvest)
library(stringr)
library(fuzzyjoin)
library(xml2)
library(httr)



```

## Länkar

Inleder med att skapa url-länkar får repsektive sida på hemnet.

```{r}
  lank_huvud_manga <- function(Antal_sidor){
  lankar <<- paste0("https://www.hemnet.se/salda/bostader?page=", 1:Antal_sidor)
  }
# lank_huvud_manga(25)
```

## Inläsning av data

Vi behöver sedan läsa in alla olika element vilket sparas i en lista. En (1) sida är 50 stycken objekt/annonser.

```{r}
sold_list <- function(Antal_sidor){
  lankar <- lank_huvud_manga(Antal_sidor)
  url <- lapply(lankar, function(href) {
    page <- read_html(href)
    links <- unlist(html_attr(html_nodes(page, "a"), "href"))
    })
#  urls <<- unlist(url)
  annons_lankar <- unlist(lapply(url, function(x) x[22:56]))
  urls <<- paste0("https://www.booli.se", annons_lankar)
}
sold_list(50) # 35 objekt per lista / sida

```

```{r}
sold_list(50) # Här läses alltså 50 sidor in
```

## Extrahering av data

Här extraheras all data från länkarna genererade ovan

```{r}
list_data <- lapply(urls, function(url) {
  
  # Läser fårst in sidan
  page <- read_html(url)
  
  # Läser sedan in respektive variabler
  datum <- page %>% html_node("div:contains('Såld eller borttagen') + div") %>% html_text(trim = TRUE)
  
  adress <- page %>% html_nodes("div._2epd7:nth-child(2) h1") %>% html_text() %>% unlist()
  
  slutpris <- page %>% html_nodes("div._2epd7:nth-child(3) h2") %>% html_text() %>% unlist()
  
  utgangspris <- page %>% html_node("div:contains('Utropspris') + div") %>% html_text(trim = TRUE)
  
  bostadstyp <- page %>% html_node("div:contains('Bostadstyp') + div") %>% html_text(trim = TRUE)
 
  antal_rum <- page %>% html_node("div._2IyrD div._3XAuT._10w08 h4._1544W._10w08:nth-child(1)") %>% html_text() %>% str_extract(",\\s*\\d+") %>% str_extract("\\d+")
 
  boarea <- page %>% html_node("div._2IyrD div._3XAuT._10w08 h4._1544W._10w08:nth-child(1)") %>% html_text() %>% str_extract("\\d+([.,]\\d+)?") %>% str_replace(",", ".")
 
  tomtarea <- page %>% html_node("div:contains('Tomtstorlek') + div") %>% html_text(trim = TRUE) %>% str_extract("\\d+([.,]\\d+)?") %>% str_replace(",", ".")

  vaning <- page %>% html_node("div:contains('Våning') + div") %>% html_text(trim = TRUE)

  byggar <- page %>% html_node("div:contains('Byggår') + div") %>% html_text(trim = TRUE)

  driftkostnad <- page %>% html_node("div:contains('Driftskostnad') + div") %>% html_text(trim = TRUE)

  avgift <- page %>% html_node("div:contains('Avgift') + div") %>% html_text(trim = TRUE)

  omrade <- page %>% html_nodes("div._2epd7:nth-child(5) h4._1544W") %>% html_text() %>% strsplit(",\\s+") %>% unlist() %>% tail(-1)

  kommun <- page %>% html_node("div._1uxTS a:nth-child(2)") %>% html_text()

  lan <- page %>% html_node("div._1uxTS a:nth-child(1)") %>% html_text()

  dagar_pa_booli <- page %>% html_node("div:contains('Dagar på Booli') + div") %>% html_text(trim = TRUE)
  
  # Här returneras all data 
  return(paste("datum:", datum,
               "adress:", adress,
               "slutpris:", slutpris,
               "utgangspris:", utgangspris,
               "bostadstyp:", bostadstyp,
               "omrade:", omrade,
               "antal_rum:", antal_rum,
               "boarea:", boarea,
               "tomtarea:", tomtarea,
               "byggar:", byggar,
               "avgift_man:", avgift,
               "driftkostnad_man:", driftkostnad,
               "kommun:", kommun,
               "lan:", lan))

})
```

## Sortering av data

Här sorteras datan upp till en dataframe. Formattering gårs även i form av:

```{r}
data_frame_data <- function(data = list_data){
  list_data <- list_data
      # Hämtar här in alla de respektive variablerna som skall vara med
      datum <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("datum:", x) + nchar("datum:"), last = regexpr("adress:", x) - 2)
      }))
      
      adress <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("adress:", x) + nchar("adress:"), last = regexpr("slutpris:", x) - 2)
      }))
      
      slutpris <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("slutpris:", x) + nchar("slutpris:"), last = regexpr("utgangspris:", x) - 2)
      }))
      
      utgangspris <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("utgangspris:", x) + nchar("utgangspris:"), last = regexpr("bostadstyp:", x) - 2)
      }))
      
      bostadstyp <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("bostadstyp:", x) + nchar("bostadstyp:"), last = regexpr("omrade:", x) - 2)
      }))
      
      omrade <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("omrade:", x) + nchar("omrade:"), last = regexpr("antal_rum:", x) - 2)
      }))
      
      antal_rum <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("antal_rum:", x) + nchar("antal_rum:"), last = regexpr("boarea:", x) - 2)
      }))
      
      boarea <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("boarea:", x) + nchar("boarea:"), last = regexpr("tomtarea:", x) - 2)
      }))
      
      tomtarea <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("tomtarea:", x) + nchar("tomtarea:"), last = regexpr("byggar:", x) - 2)
      }))
      
      byggar <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("byggar:", x) + nchar("byggar:"), last = regexpr("avgift_man:", x) - 2)
      }))
      
      avgift_man <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("avgift_man:", x) + nchar("avgift_man:"), last = regexpr("driftkostnad_man:", x) - 2)
      }))
      
      driftkostnad_man <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("driftkostnad_man:", x) + nchar("driftkostnad_man:"), last = regexpr("kommun:", x) - 2)
      }))
      
      kommun <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("kommun:", x) + nchar("kommun:"), last = regexpr("lan:", x) - 2)
      }))
     
     lan <- unlist(lapply(list_data, function(x) {
        substring(x, first = regexpr("lan:", x) + nchar("lan:")+1)
      }))
     
  # Sätter här in alla variablerna i respektive kolumner
  df <- data.frame(datum = datum,
                   adress = adress, 
                   omrade = omrade,
                   slutpris = slutpris,
                   utgangspris = utgangspris,
                   bostadstyp = bostadstyp,
                   antal_rum = antal_rum,
                   boarea = boarea,
                   tomtarea = tomtarea,
                   byggar = byggar,
                   avgift_man = avgift_man,
                   driftkostnad_man = driftkostnad_man,
                   kommun = kommun,
                   lan = lan)
  return(df)
}
```

Skapar sedan variabeln, alltså data-framen med:

```{r}
raw_data <- data_frame_data()
head(raw_data, n = 8)
```

## Processerar data:
# Formatering:
- Datum formateras
- Adress trimmas av blanksteg
- Område trimmas av blanksteg
- Slutpris formateras till numerisk
- Utgångspris formateras till numerisk
- Bostadstyp trimmas av blanksteg
- Antal rum formateras till numerisk
- Boarea formateras till numerisk
- Tomtarea formateras till numerisk
- Byggår formateras till numerisk
- Avgift/mån formateras till numerisk
- Driftkostnas/mån formateras till numerisk

# Beräkning:
- Fårändring utgångspris mot slutpris
- Pris kvadratmeter
- Avgift per månad per kvadratmeter
- Driftkostnad per månad per kvadratmeter
- Andel boarea av tomtarea

```{r}
processed_data <- raw_data %>%
  mutate(
    datum = as.Date(trimws(datum), format = "%Y-%m-%d"),
    adress = adress %>% trimws(),
    omrade = omrade %>% trimws(),
    slutpris = as.numeric(gsub("[^0-9]", "", slutpris, perl = TRUE)),
    utgangspris = as.numeric(gsub("[^0-9]", "", utgangspris, perl = TRUE)),
    bostadstyp = bostadstyp %>% trimws(),
    antal_rum = as.numeric(gsub("[^0-9]", "", antal_rum, perl = TRUE)),
    boarea = as.numeric(gsub("[^0-9]", "", boarea, perl = TRUE)),
    tomtarea = as.numeric(gsub("[^0-9]", "", tomtarea, perl = TRUE)),
    byggar = as.numeric(trimws(byggar)),
    avgift_man = as.numeric(gsub("[^0-9]", "", avgift_man, perl = TRUE)),
    driftkostnad_man = as.numeric(gsub("[^0-9]", "", driftkostnad_man, perl = TRUE)),
    kommun = kommun %>% trimws(),
    lan = str_remove(lan, "län") %>% str_remove("s "),
    forandring_utgangspris = slutpris - utgangspris,
    pris_kvadratmeter = slutpris / boarea,
    avgift_man_kvadratmeter = avgift_man / boarea,
    driftkostnad_man_kvadratmeter = driftkostnad_man / boarea,
    andel_boarea_tomtarea = boarea / tomtarea
    )
```


## Sparar data till CSV-format

```{r pressure, echo=FALSE}
dir.create(paste0(getwd(), "/dat")) # Här skapas alltså en datamap i wd()
write_csv(processed_data, paste0(getwd(), "/dat/raw_data_uppdaterad_", today(), "_", dim(processed_data)[1], "_objekt"))
```







Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
