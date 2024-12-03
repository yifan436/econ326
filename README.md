# econ326
```{r pressure, echo=FALSE}
library(cancensus)
library(tidyverse)
library(knitr)
library(dplyr)
library(haven)
```
setwd(">_<")
load("data_transit.rda")
load("data_culture.rda")
load("data_art.rda")
```{r}
str(census_data_culture)
str(census_data_art)
str(census_data_stops)

merged_data <- census_data_culture %>%
  mutate(
    art_within_500 = census_data_art$art_within_500,
    stops_contained = census_data_stops$stops_contained
  )
```

```{r}
merged_data <- merged_data %>%
  drop_na(v_CA21_906..Median.total.income.of.household.in.2020...., 
          art_within_500, 
          stops_contained, 
          Population)
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
