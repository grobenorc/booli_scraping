---
title: "booli_scraping_statistical_analysis_markdown"
author: "Claes"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r grundinstallningar}

# Sätter working directory till en mapp
setwd("C:/Users/claes/OneDrive/R programmering/booli_scraping")

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

# Medelvärden / genomsnittsberäkningar

**Här kommer skattningar av olika medelvärden göras **

- $\text{Skattning av } \hat{\mu}_{slutpris} = \bar{x} \pm t/z \cdot \frac{s}{\sqrt{n}}$
- $\text{Skattning av } \hat{\mu}_{kvadratmeterpris} = \bar{x} \pm t/z \cdot \frac{s}{\sqrt{n}}$
- $\text{Skattning av } \hat{\pi}_{län} = p_{län} \pm  \sqrt{ \frac{p_{län}(1-p_{län})}{n}}, \qquad \text{där p = stickprovsandel}$
- $\text{Skattning av } \hat{\pi}_{län:bostadstyp} = p_{län:bostadstyp} \pm  \sqrt{ \frac{p_{län:bostadstyp}(1-p_{län:bostadstyp})}{n}}, \qquad \text{där p = stickprovsandel}$

<br> # Detta är syntax för att göra en linebreak i html_document
\\bigskip


#### Skattning av medelvärdet av slutpris per län, med konfidensintervall där $\alpha = 0.05$

```{r Län: plot medelvärde slutpris}
raw_data %>%
  group_by(lan = fct_lump(lan, n = 5, other_level = "Övriga")) %>%
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
  labs(title = "Genomsnitt slutpris per län", subtitle = "Konfidensnivå 95%",
       x = "Län",
       y = "Genomsnitt slutpris") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")
```


Här kan vi alltså se att Stockholm är statistisk säkerställt det dyraste länet att köpa en bostad i. Anmärkningsvärt är att vi inte kan påvisa en statistiskt säkerställd skillnad mellan varken Göteborg, Malmö och Skåne. Däremot kan vi göra detta mellan Göteborg gentemot Västra Götaland och de Övriga. 





```{r Län: plot medelvärde kvadratmeterpris}
raw_data %>%
  group_by(lan = fct_lump(lan, n = 5, other_level = "Övriga")) %>%
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
  labs(title = "Genomsnitt kvadratmeterpris per län", subtitle = "Konfidensnivå 95%",
       x = "Län",
       y = "Genomsnitt kvadratmeterpris") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")
```




```{r Bostadstyp: plot medelvärde värdeutveckling per bostadstyp}
raw_data %>%
  group_by(bostadstyp = fct_lump(bostadstyp, n = 5, other_level = "Övriga")) %>%
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
  labs(title = "Genomsnitt förändring utgångspris - slutpris", subtitle = "Konfidensnivå 95%",
       x = "Bostadstyp",
       y = "Genomsnitt förändring") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")

```

```{r Län: plot medelvärde värdeutveckling per län}
raw_data %>%
  group_by(lan = fct_lump(lan, n = 5, other_level = "Övriga")) %>%
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
  labs(title = "Genomsnitt förändring utgångspris - slutpris", subtitle = "Konfidensnivå 95%",
       x = "Län",
       y = "Genomsnitt förändring") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")

```


Här går att notera ett par olika saker:
- Det är statistsiskt säkerställt att Stockholm har den största förändringen från utgångspris till slutpris.
- Det är statistsiskt säkerställt att det i genomsnitt är **högre** slutpris än i Stockholm.
- Det är statistsiskt säkerställt att det i genomsnitt är **lägre** slutpris än utgångspris i; Västra Götaland, Malmö, Skåne. 




```{r Län: plot medelvärde procentuell värdeutveckling per län}
raw_data %>%
  group_by(lan = fct_lump(lan, n = 5, other_level = "Övriga")) %>%
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
  labs(title = "Genomsnitt procentuell förändring utgångspris - slutpris", subtitle = "Konfidensnivå 95%",
       x = "Län",
       y = "Genomsnitt förändring") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")

```






```{r Rum: plot medelvärde slutpris}

raw_data %>% filter(bostadstyp == "Lägenhet") %>% 
  group_by(antal_rum = fct_lump(as.character(antal_rum), n = 5, other_level = "Övriga")) %>%
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
  labs(title = "Genomsnitt procentuell förändring utgångspris - slutpris", subtitle = "Konfidensnivå 95%",
       x = "Län",
       y = "Genomsnitt förändring") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none")

```









Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.



