---
title: "booli_scraping_EDA_markdown"
author: "xxxx"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r grundinstallningar}

# Sätter working directory till en mapp
setwd("C:/Users/xxxx/OneDrive/R programmering/booli_scraping")

# Ladda in paket
library(Rmisc)
library(tidyverse)
library(readr)
library(forcats)
library(ggpubr)
library(scales)

```


# R Inladdning data
Inleder med att ladda in data genererad i tidigare fil där den har scrapeats från Booli. 

```{r raw_data}
 raw_data <- read_csv("dat/raw_data_uppdaterad_2023-03-26_1750_objekt")

 # Kollar strukturen / summering
 summary(raw_data)
```

## Per bostadstyp


```{r Bostadstyp, echo=FALSE}
raw_data %>%
  dplyr::count(bostadstyp) %>%
  arrange(desc(n)) %>%
  mutate(bostadstyp = if_else(row_number() <= 5, as.character(bostadstyp), "Övriga")) %>%
  ggplot(aes(x = reorder(bostadstyp, -n), y = n, fill = bostadstyp)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) + 
  labs(title = "Antal sålda per bostadstyp", x = "Bostadstyp", y = "Antal", fill = "Bostadstyp") + 
  theme_bw()
```


## Per län

**Här kommer vi undersöka indelat per län.**

- Antal sålda objekt per län
- Slutpris per län
- Kvadratmeterpris per län



```{r Län, echo=FALSE}
raw_data %>%
  count(lan) %>%
  arrange(desc(n)) %>%
  mutate(lan = if_else(row_number() <= 5, as.character(lan), "Övriga")) %>%
  ggplot(aes(x = reorder(lan, -n), y = n, fill = lan)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) + 
  labs(title = "Antal sålda per län", x = "Län", y = "Antal", fill = "Län")
```

```{r Län, slutpris historam}
  raw_data %>%
    filter(!is.na(slutpris)) %>%
    filter( slutpris < quantile(raw_data$slutpris, na.rm = TRUE, probs = 0.99) ) %>% 
      mutate(lan = fct_lump(lan, n = 4, other_level = "Övriga")) %>%
       ggplot(aes(x = slutpris, fill = lan)) +
       geom_histogram(binwidth = 100000) +
       facet_wrap(~lan, ncol = 3) +
       labs(title = "Slutpris fördelat på län", x = "Slutpris", y = "Antal", fill = "Län") +
       scale_fill_discrete(name = "Län", labels = c(levels(raw_data$lan)[1:4], "Övriga")) +
       theme(legend.position = "none")
```

```{r Län, slutpris stolpdiagram lägenheter}
  raw_data %>%
    filter(!is.na(slutpris)) %>%
    filter( slutpris < quantile(raw_data$slutpris, na.rm = TRUE, probs = 0.99) & bostadstyp == "Lägenhet" ) %>% 
      mutate(lan = fct_lump(lan, n = 4, other_level = "Övriga")) %>%
       ggplot(aes(x = slutpris, fill = lan)) +
       geom_histogram(binwidth = 100000) +
       facet_wrap(~lan, ncol = 3) +
       labs(title = "Slutpris fördelat på län", x = "Slutpris", y = "Antal", fill = "Län") +
       scale_fill_discrete(name = "Län", labels = c(levels(raw_data$lan)[1:4], "Övriga")) +
       theme(legend.position = "none")
```


```{r Län, slutpris mot kvadratmeterpris (punktdiagram) }
  raw_data %>%
   # filter(!is.na(slutpris)) %>%
    filter(pris_kvadratmeter < quantile(pris_kvadratmeter, na.rm = TRUE, probs = 0.975) & bostadstyp == "Lägenhet") %>% 
    mutate(lan = fct_lump(lan, n = 4, other_level = "Övriga")) %>%
       ggplot(aes(x = pris_kvadratmeter, y = slutpris, color = lan)) +
       geom_point() +
       scale_y_continuous(labels = dollar_format(prefix = "", suffix = "kr/mÂ²", big.mark = " ")) +
       scale_x_continuous(labels = dollar_format(prefix = "", suffix = "kr/mÂ²", big.mark = " ")) +
       labs(title = "Slutpris mot kvadratmeterpris grupperat per län", x = "Pris/kvadratmeter", y = "Slutpris", fill = "Län") +
       scale_fill_discrete(name = "Län", labels = c(levels(raw_data$lan)[1:4], "Övriga")) +
       theme(legend.position = "right")

```

```{r Län, slutpris mot kvadratmeterpris - Stockholm (punktdiagram) }
  raw_data %>%
    filter(kommun == "Stockholm") %>%
    filter(pris_kvadratmeter < 250000 & bostadstyp == "Lägenhet") %>% 
    mutate(omrade = fct_lump(omrade, n = 4, other_level = "Övriga")) %>%
       ggplot(aes(x = pris_kvadratmeter, y = slutpris, color = omrade)) +
       geom_point() +
       scale_y_continuous(labels = dollar_format(prefix = "", suffix = "kr/mÂ²", big.mark = " ")) +
       scale_x_continuous(labels = dollar_format(prefix = "", suffix = "kr/mÂ²", big.mark = " ")) +
       labs(title = "Slutpris mot kvadratmeterpris grupperat per län", subtitle = "Stockholms kommun", x = "Pris/kvadratmeter", y = "Slutpris", fill = "Län") +
       scale_fill_discrete(name = "Län", labels = c(levels(raw_data$lan)[1:4], "Övriga")) +
       theme(legend.position = "right")

```





```{r Län, slutpris mot kvadratmeterpris (boxplot) }
  raw_data %>%
    # filter(!is.na(slutpris)) %>%
    filter(pris_kvadratmeter < quantile(pris_kvadratmeter, na.rm = TRUE, probs = 0.975)) %>% 
    mutate(lan = fct_lump(lan, n = 4, other_level = "Övriga")) %>%
    ggplot(aes(x = reorder(lan, -pris_kvadratmeter), y = pris_kvadratmeter, color = lan)) +
    geom_boxplot() +
    scale_y_continuous(labels = dollar_format(prefix = "", suffix = "kr/mÂ²", big.mark = " ")) +
    labs(title = "Pris/kvadratmeter per län", x = "Slutpris", y = "Pris/kvadratmeter", fill = "Län") +
    scale_fill_discrete(name = "Län", labels = c(levels(raw_data$lan)[1:5], "Övriga")) +
    theme(legend.position = "right")
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

