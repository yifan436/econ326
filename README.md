# econ326
```{r pressure, echo=FALSE}
library(cancensus)
library(tidyverse)
library(knitr)
library(dplyr)
library(haven)
```
```{r}
setwd(">_<")
load("data_transit.rda")
load("data_culture.rda")
load("data_art.rda")

str(census_data_culture)
str(census_data_art)
str(census_data_stops)

merged_data <- census_data_culture %>%
  mutate(
    art_within_500 = census_data_art$art_within_500,
    stops_contained = census_data_stops$stops_contained
  )
```

```{r pressure, echo=FALSE}
census_data_culture %>%
  summarise(variable = "Income",
    max = max(v_CA21_906..Median.total.income.of.household.in.2020...., na.rm = TRUE),
    min = min(v_CA21_906..Median.total.income.of.household.in.2020...., na.rm = TRUE),
    mean = mean(v_CA21_906..Median.total.income.of.household.in.2020...., na.rm = TRUE),
    sd = sd(v_CA21_906..Median.total.income.of.household.in.2020...., na.rm = TRUE)
  )
census_data_art %>%
  summarise(variable = "distance",
    max = max(art_within_500, na.rm = TRUE),
    min = min(art_within_500, na.rm = TRUE),
    mean = mean(art_within_500, na.rm = TRUE),
    sd = sd(art_within_500, na.rm = TRUE)
  )
census_data_stops %>%
  summarise(variable = "stops",
    max = max(stops_contained),
    min = min(stops_contained),
    mean = mean(stops_contained),
    sd = sd(stops_contained)
  )

census_data_culture %>%
  summarise(variable = "population",
    max = max(Population, na.rm = TRUE),
    min = min(Population, na.rm = TRUE),
    mean = mean(Population, na.rm = TRUE),
    sd = sd(Population, na.rm = TRUE)
  )
```

```{r}
regression1 <- lm(v_CA21_906..Median.total.income.of.household.in.2020.... ~ art_within_500, data = merged_data)
summary(regression1)

regression2 <- lm(v_CA21_906..Median.total.income.of.household.in.2020.... ~ stops_contained, data = merged_data)
summary(regression2)

regression3 <- lm(v_CA21_906..Median.total.income.of.household.in.2020.... ~  Population, data = merged_data)
summary(regression3)
```


```{r}
install.packages("stargazer")
library(stargazer)
stargazer(regression1, regression2, regression3, title="Comparison of Regression Results",
          align = TRUE, type="text", keep.stat = c("n","rsq"))
```

```{r}
multiple_model_1 <- lm(data = merged_data, v_CA21_906..Median.total.income.of.household.in.2020....  ~ art_within_500 + stops_contained)

summary(multiple_model_1)
```
