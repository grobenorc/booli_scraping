---
title: "booli_scraping_statistical_analysis_markdown"
author: "xxxx"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r grundinstallningar}

# SÃ¤tter working directory till en mapp
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
Inleder med att ladda in data genererad i tidigare fil dÃ¤r den har scrapeats frÃ¥n Booli. 

```{r raw_data}
 raw_data <- read_csv("dat/raw_data_uppdaterad_2023-03-26_1750_objekt")

 # Kollar strukturen / summering
 summary(raw_data)
```

# MedelvÃ¤rden / genomsnittsberÃ¤kningar

**HÃ¤r kommer skattningar av olika medelvÃ¤rden gÃ¶ras **

- $\text{Skattning av } \hat{\mu}_{slutpris} = \bar{x} \pm t/z \cdot \frac{s}{\sqrt{n}}$
- $\text{Skattning av } \hat{\mu}_{kvadratmeterpris} = \bar{x} \pm t/z \cdot \frac{s}{\sqrt{n}}$
- $\text{Skattning av } \hat{\pi}_{lÃ¤n} = p_{lÃ¤n} \pm  \sqrt{ \frac{p_{lÃ¤n}(1-p_{lÃ¤n})}{n}}, \qquad \text{dÃ¤r p = stickprovsandel}$
- $\text{Skattning av } \hat{\pi}_{lÃ¤n:bostadstyp} = p_{lÃ¤n:bostadstyp} \pm  \sqrt{ \frac{p_{lÃ¤n:bostadstyp}(1-p_{lÃ¤n:bostadstyp})}{n}}, \qquad \text{dÃ¤r p = stickprovsandel}$

<br> # Detta Ã¤r syntax fÃ¶r att gÃ¶ra en linebreak i html_document
\\bigskip


#### Skattning av medelvÃ¤rdet av slutpris per lÃ¤n, med konfidensintervall dÃ¤r $\alpha = 0.05$

```{r LÃ¤n: plot medelvÃ¤rde slutpris}
raw_data %>%
  group_by(lan = fct_lump(lan, n = 5, other_level = "Ãvriga")) %>%
  summarise(mean_slutpris = mean(slutpris, na.rm = TRUE), 
            sd_slutpris = sd(slutpris, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd_slutpris / sqrt(n),
         lower_ci = mean_slutpris - qt(1 - 0.05 / 2, n - 1) * se,
         upper_ci = mean_slutpris + qt(1 - 0.05 / 2, n - 1) * se) %>%
  arrange(desc(mean_slutpris)) %>%
  ggplot(aes(x = reorder(lan, -mean_slutpris), y = mean_slutpris, color = lan)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "kr", big.mark = " ")) +
  labs(title = "Genomsnitt slutpris per lÃ¤n", subtitle = "KonfidensnivÃ¥ 95%",
       x = "LÃ¤n",
       y = "Genomsnitt slutpris") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")
```


HÃ¤r kan vi alltsÃ¥ se att Stockholm Ã¤r statistisk sÃ¤kerstÃ¤llt det dyraste lÃ¤net att kÃ¶pa en bostad i. AnmÃ¤rkningsvÃ¤rt Ã¤r att vi inte kan pÃ¥visa en statistiskt sÃ¤kerstÃ¤lld skillnad mellan varken GÃ¶teborg, MalmÃ¶ och SkÃ¥ne. DÃ¤remot kan vi gÃ¶ra detta mellan GÃ¶teborg gentemot VÃ¤stra GÃ¶taland och de Ãvriga. 





```{r LÃ¤n: plot medelvÃ¤rde kvadratmeterpris}
raw_data %>%
  group_by(lan = fct_lump(lan, n = 5, other_level = "Ãvriga")) %>%
  summarise(mean_kvadratmeterpris = mean(pris_kvadratmeter, na.rm = TRUE), 
            sd_slutpris = sd(pris_kvadratmeter, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd_slutpris / sqrt(n),
         lower_ci = mean_kvadratmeterpris - qt(1 - 0.05 / 2, n - 1) * se,
         upper_ci = mean_kvadratmeterpris + qt(1 - 0.05 / 2, n - 1) * se) %>%
  arrange(desc(mean_kvadratmeterpris)) %>%
  ggplot(aes(x = reorder(lan, -mean_kvadratmeterpris), y = mean_kvadratmeterpris, color = lan)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "kr", big.mark = " ")) +
  labs(title = "Genomsnitt kvadratmeterpris per lÃ¤n", subtitle = "KonfidensnivÃ¥ 95%",
       x = "LÃ¤n",
       y = "Genomsnitt kvadratmeterpris") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")
```




```{r Bostadstyp: plot medelvÃ¤rde vÃ¤rdeutveckling per bostadstyp}
raw_data %>%
  group_by(bostadstyp = fct_lump(bostadstyp, n = 5, other_level = "Ãvriga")) %>%
  summarise(mean_forandring_utgangspris = mean(forandring_utgangspris, na.rm = TRUE), 
            sd_forandring_utgangspris = sd(forandring_utgangspris, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd_forandring_utgangspris / sqrt(n),
         lower_ci = mean_forandring_utgangspris - qt(1 - 0.05 / 2, n - 1) * se,
         upper_ci = mean_forandring_utgangspris + qt(1 - 0.05 / 2, n - 1) * se) %>%
  arrange(desc(mean_forandring_utgangspris)) %>%
  ggplot(aes(x = reorder(bostadstyp, -mean_forandring_utgangspris), y = mean_forandring_utgangspris, color = bostadstyp)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "kr", big.mark = " ")) +
  labs(title = "Genomsnitt fÃ¶rÃ¤ndring utgÃ¥ngspris - slutpris", subtitle = "KonfidensnivÃ¥ 95%",
       x = "Bostadstyp",
       y = "Genomsnitt fÃ¶rÃ¤ndring") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")

```

```{r LÃ¤n: plot medelvÃ¤rde vÃ¤rdeutveckling per lÃ¤n}
raw_data %>%
  group_by(lan = fct_lump(lan, n = 5, other_level = "Ãvriga")) %>%
  summarise(mean_forandring_utgangspris = mean(forandring_utgangspris, na.rm = TRUE), 
            sd_forandring_utgangspris = sd(forandring_utgangspris, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd_forandring_utgangspris / sqrt(n),
         lower_ci = mean_forandring_utgangspris - qt(1 - 0.05 / 2, n - 1) * se,
         upper_ci = mean_forandring_utgangspris + qt(1 - 0.05 / 2, n - 1) * se) %>%
  arrange(desc(mean_forandring_utgangspris)) %>%
  ggplot(aes(x = reorder(lan, -mean_forandring_utgangspris), y = mean_forandring_utgangspris, color = lan)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "kr", big.mark = " ")) +
  labs(title = "Genomsnitt fÃ¶rÃ¤ndring utgÃ¥ngspris - slutpris", subtitle = "KonfidensnivÃ¥ 95%",
       x = "LÃ¤n",
       y = "Genomsnitt fÃ¶rÃ¤ndring") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")

```


HÃ¤r gÃ¥r att notera ett par olika saker:
- Det Ã¤r statistsiskt sÃ¤kerstÃ¤llt att Stockholm har den stÃ¶rsta fÃ¶rÃ¤ndringen frÃ¥n utgÃ¥ngspris till slutpris.
- Det Ã¤r statistsiskt sÃ¤kerstÃ¤llt att det i genomsnitt Ã¤r **hÃ¶gre** slutpris Ã¤n i Stockholm.
- Det Ã¤r statistsiskt sÃ¤kerstÃ¤llt att det i genomsnitt Ã¤r **lÃ¤gre** slutpris Ã¤n utgÃ¥ngspris i; VÃ¤stra GÃ¶taland, MalmÃ¶, SkÃ¥ne. 




```{r LÃ¤n: plot medelvÃ¤rde procentuell vÃ¤rdeutveckling per lÃ¤n}
raw_data %>%
  group_by(lan = fct_lump(lan, n = 5, other_level = "Ãvriga")) %>%
  summarise(mean_forandring_utgangspris = mean(forandring_utgangspris/slutpris, na.rm = TRUE), 
            sd_forandring_utgangspris = sd(forandring_utgangspris/slutpris, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd_forandring_utgangspris / sqrt(n),
         lower_ci = mean_forandring_utgangspris - qt(1 - 0.05 / 2, n - 1) * se,
         upper_ci = mean_forandring_utgangspris + qt(1 - 0.05 / 2, n - 1) * se) %>%
  arrange(desc(mean_forandring_utgangspris)) %>%
  ggplot(aes(x = reorder(lan, -mean_forandring_utgangspris), y = mean_forandring_utgangspris, color = lan)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(yintercept = 0) + 
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "%", big.mark = " ")) +
  labs(title = "Genomsnitt procentuell fÃ¶rÃ¤ndring utgÃ¥ngspris - slutpris", subtitle = "KonfidensnivÃ¥ 95%",
       x = "LÃ¤n",
       y = "Genomsnitt fÃ¶rÃ¤ndring") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")

```






```{r Rum: plot medelvÃ¤rde slutpris}

raw_data %>% filter(bostadstyp == "LÃ¤genhet") %>% 
  group_by(antal_rum = fct_lump(as.character(antal_rum), n = 5, other_level = "Ãvriga")) %>%
  summarise(mean_slutpris = mean(slutpris, na.rm = TRUE), 
            sd_slutpris = sd(slutpris, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd_slutpris / sqrt(n),
         lower_ci = mean_slutpris - qt(1 - 0.05 / 2, n - 1) * se,
         upper_ci = mean_slutpris + qt(1 - 0.05 / 2, n - 1) * se) %>%
  arrange(desc(mean_slutpris)) %>%
  ggplot(aes(x = reorder(antal_rum, -mean_slutpris), y = mean_slutpris, color = antal_rum)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(yintercept = 0) + 
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "kr", big.mark = " ")) +
  labs(title = "Genomsnitt procentuell fÃ¶rÃ¤ndring utgÃ¥ngspris - slutpris", subtitle = "KonfidensnivÃ¥ 95%",
       x = "LÃ¤n",
       y = "Genomsnitt fÃ¶rÃ¤ndring") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")

```

