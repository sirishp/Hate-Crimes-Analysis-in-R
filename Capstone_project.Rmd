---
title: "Capstone_project"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, eval=FALSE, echo=FALSE}
install.packages('tinytex')
tinytex::install_tinytex()
```
# Executive Summary:

Hate crimes in the United States has been a concern over the years and one of the major reason for it is 'Racism'. The data provided by the FBI shows that there are other factors to be considered to prove hate crimes and those factors range from lack of education to non-citizens making more money. We have considered the factors such as white population, adults with only a High School degree, non-citizen population, median household income,unemployment, income inequality based on GINI index to gain a deeper insight of the problem.

# Background:

A hate crime is a “criminal offense against a person or property motivated in whole or in part by an offender's bias against a race, religion, disability, sexual orientation, ethnicity, gender, or gender identity.”(Federal Bureau of Investigation.(2017). Hate crimes). Based on the FBI collected data over the hata crimes, FiveThirtyEight had performed an analysis on the hate crimes data for the year of 2016.This analysis is based on the FiveThirtyEight report taking in factors such as white population, adults with only a High School degree, non-citizen population, median household income,unemployment, income inequality based on GINI index a couple of additional state-level variables like male population and number of disable people are considered to map and plot the analysis.

I had taken the 2016 census and the hate crimes data of the year 2017 to perform my analysis. One major setback is that only half the population reports against hate crimes, and to normalize the data; the crimes reported for 100K people is considered for the analysis. Multi-variate regression is performed to see which variables play a significant role in hate crimes across the country.




```{r Loading Libraries, message=FALSE, warning=FALSE,echo= FALSE}
library(tidyverse)
library(lubridate)
library(tidycensus)
library(jsonlite)
library(ggplot2)
library(sf)
library(stargazer)
library(readxl)
library(tinytex)
```



```{r DATA reading, message = FALSE, warning=FALSE,echo= FALSE}
hate_data<- read_excel("Table_12_Agency_Hate_Crime_Reporting_by_State_2017.xls", skip = 2)
```

```{r Census DATA, message = FALSE, warning=FALSE,echo= FALSE}

census_data<-load_variables(2016,"acs5")
hate_data<- hate_data %>%
  select(c(-6,-7,-8)) %>%
  slice(2:nrow(hate_data))

```


```{r Changing the column name,message=FALSE, warning=FALSE,echo= FALSE}

#hate_data = hate_data[-1,]

colnames(hate_data)[colnames(hate_data)=="Participating state"] <- "State"
#colnames(hate_data)[colnames(hate_data)=="Total number of incidents reported"] <- "Hate_report"
```

```{r Kaiser Data Loading, message=FALSE, warning=FALSE,echo= FALSE}

kaiser_data<- read_excel('raw_data.xls')
kaiser_data = kaiser_data[-1,]
```

```{r Modifying the Kaiser dataset, message=FALSE,warning=FALSE,echo= FALSE}

# CHANGING FIRST ROW TO COLUMN HEADER
colnames(kaiser_data)= kaiser_data[1, ]
kaiser_data<- kaiser_data %>%
  select(c(-5))
kaiser_data = kaiser_data[-1,]

```

```{r , message=FALSE,warning=FALSE,echo= FALSE}
# Removing the first row of the dataset (i.e United States row)
kaiser_data= kaiser_data[-1,]

```

```{r,echo= FALSE}
# Changing the name of the column in Kaiser dataset

colnames(kaiser_data)[colnames(kaiser_data)=="Location"] <- "State"

```

```{r Loading GINI index,message=FALSE, warning=FALSE,echo= FALSE}

Gini<- read_excel('ACS_17_1YR_B19083_with_ann.xls')

```

```{r,echo= FALSE}
# Changing the first row of the GINI dataset to column header and removing it as a row.

colnames(Gini)= Gini[1, ]
Gini=Gini[-1,]

# rename - colnames(data)[colnames(data)=="old_name"] <- "new_name"
colnames(Gini)[colnames(Gini)=="Geography"] <- "State"

```

```{r Loading State Level Data, message= FALSE, warning= FALSE,echo= FALSE}

state_data<-get_acs(
  geography = "state",
  variables = c(median_income="B06011_001", unemployed_pop="B27011_014", white="B02001_002", total_pop="B01003_001", pop_High_school="B06009_003", below_poverty="B17020_001", white_below_poverty="B17020A_001",disability="C18108_007",male="B01001_002"),
  year = 2016,
  geometry = TRUE,
  shift_geo = TRUE
)

colnames(state_data)[colnames(state_data)=="NAME"] <- "State"

state_data <-state_data%>%
  select(-moe)%>%
  spread(variable,estimate)
```

```{r, message=FALSE,warning=FALSE,echo= FALSE}
#Joining the hate_data to state_data.
state_data<- left_join(state_data,hate_data,by="State")

```

```{r,echo= FALSE}
# Joining the kasier_data to state data 
state_data<- left_join (state_data,kaiser_data,by="State")

```

```{r,echo= FALSE}
#Joining the Gini data to state_data.

state_data <- left_join(state_data,Gini,by="State")
```

```{r,echo= FALSE}
#colnames(state_data)[colnames(state_data)=='`Total number of incidents reported`'] <- "Hate_reports"
state_data<- state_data%>%
  rename(Hate_reports=`Total
number of
incidents
reported`)

state_data<- state_data %>%
  rename(estimate_gini=`Estimate; Gini Index`)

state_data<- state_data %>%
  rename(moe_gini=`Margin of Error; Gini Index`)
```

```{r,echo= FALSE}
state_data<- state_data%>%
  rename(pop_covered=`Population
covered`)

```

```{r,echo= FALSE}
# Converting to numeric values

state_data$Hate_reports <- as.numeric(state_data$Hate_reports)
state_data$pop_covered <- as.numeric(state_data$pop_covered) 
state_data$`Non-Citizen` <- as.numeric(state_data$`Non-Citizen`)
state_data$estimate_gini <- as.numeric(state_data$estimate_gini)

```

```{r mtate_data,warning=FALSE, message=FALSE,echo=FALSE}
state_data <- state_data %>%
  mutate(Pct_pop_unemployed=(unemployed_pop/total_pop)*100)%>%
  mutate(Pct_pop_HSdegree=(pop_High_school/total_pop)*100)%>%
  mutate(Pct_white_below_poverty=(white_below_poverty/total_pop)*100)%>%
  mutate(Pct_not_white=(1-(white/total_pop))*100)%>%
  mutate(avg_annual_hatecrimes_per_100k=(Hate_reports*100000/pop_covered))%>%
  mutate(pct_male = male/total_pop)%>%
  mutate(pct_disability= disability/total_pop)

```


# Analysis:

Multi-variate regression is performed to see which variables play a significant role in hate crimes across the country. In the regression analysis, I have choosen the variables in the first model based on the FiveThirtyEight analysis.Whereas in the second model & third model I have added the variables male population and percent of disable people to see the trend. To normalize the data, we have taken it per 100K. Maps and Non-map plots are conisidered for a better interpretation of data. I have plotted the annual hate crimes per 100K of the states over a map plot to know which states have a higher count of hate crimes. I had also plotted the average hate crimes to the percentage of disable people as it is one of the most significant variable in the regression table. Another plot shows the average hate crimes against Income Inequality as the trend shows that it plays a significant role in the hatecrimes. The last plot shows the change in average number of hate crimes reported per 100K over the years 2008 to 2017.


# Task2: Regression Analysis
```{r reg_model,message=FALSE, warning=FALSE, results='asis',echo=FALSE}
 
model_1<- lm(avg_annual_hatecrimes_per_100k ~ Pct_pop_unemployed+ Pct_pop_HSdegree+Pct_white_below_poverty+Pct_not_white+ median_income, data=state_data )

model_2<- lm(avg_annual_hatecrimes_per_100k ~ Pct_pop_unemployed +Pct_pop_HSdegree+Pct_white_below_poverty+Pct_not_white+ median_income + `Non-Citizen` + pct_male , data=state_data)

model_3<- lm(avg_annual_hatecrimes_per_100k ~ Pct_pop_unemployed +Pct_pop_HSdegree+Pct_white_below_poverty+Pct_not_white+ median_income + `Non-Citizen` + pct_male +pct_disability , data=state_data)

model_4<- lm(avg_annual_hatecrimes_per_100k ~ Pct_pop_unemployed +Pct_pop_HSdegree+Pct_white_below_poverty*median_income+Pct_not_white+ median_income + `Non-Citizen` + estimate_gini +pct_male + pct_disability, data=state_data)

stargazer(model_1,model_2,model_3,model_4 ,type= "latex")
```

```{r plots,echo= FALSE}
#plot(model_1)
#plot(model_2)
#plot(model_3)
#plot(model_4)
```

```{r,echo= FALSE}
#plot(state_data)
```
# Mapping the States data
```{r maps_plot, warning=FALSE,message=FALSE, echo=FALSE}
state_data <- st_transform(state_data, 2163)

ggplot(state_data)+
  geom_sf(
    data = state_data,
    aes(fill = avg_annual_hatecrimes_per_100k), color =NA
  ) +
  theme_void()+
  scale_fill_distiller (
    name = "Avg Hatecrimes per 100K",
    palette = "Reds",
    limits = c(0,50),
    breaks = seq(0,50,10),
    labels = seq(0,50,10),
    direction = 1
  )+
  scale_color_brewer(palette = "Reds")+
  theme(legend.title = element_text(colour="Red", size=10, 
                                      face="italic"))+
  ggtitle("Hate Crime Rates")


```

```{r Non_Map_plot,echo= FALSE,results='hide',warning=FALSE,message=FALSE}
ggplot(state_data)+
  geom_point(aes(x=pct_disability, y = avg_annual_hatecrimes_per_100k))+
  labs(
    x= "Percentage of disable population",
    y="Average Hate Crimes (Per 100k population)"
  )+
  scale_y_continuous(limits=c(0,10))+
  ggtitle("Disability vs Hate Crimes")+
  theme_bw()

```

```{r,echo= FALSE,results='hide',warning=FALSE,message=FALSE}
ggplot(state_data)+
  geom_point(aes(x=estimate_gini, y = avg_annual_hatecrimes_per_100k))+
  labs(
    x= "Income Inequality (Estimate Gini Index)",
    y= "Average Hate Crimes (Per 100k population)"
  )+
  scale_y_continuous(limits=c(0,10))+
  ggtitle("Income Inequality vs Hate Crimes")+
  theme_bw()


```

```{r,echo= FALSE, results='hide',warning=FALSE,message=FALSE}
fbi<- tibble()
for(file in list.files(pattern="_HC_data.xls")){
  x<-read_excel(file)
  fbi<- bind_rows(fbi,x[3,5])
}

fbi<-fbi%>%
rename(Total_num_of_incidents_reported=...5)

```

```{r,echo= FALSE, results='hide',warning=FALSE,message=FALSE}
fbi<- fbi%>%
  mutate(year=c(2008:2017))
```

```{r,echo= FALSE, results='hide',warning=FALSE,message=FALSE}
fbi1<- tibble()
for(file in list.files(pattern="_HC_data.xls")){
  x<-read_excel(file)
  fbi1<- bind_rows(fbi1,x[3,3])
}

fbi1<-fbi1%>%
rename(Population=...3)

fbi1<- fbi1%>%
  mutate(year=c(2008:2017))
```

```{r,echo= FALSE,results='hide',warning=FALSE,message=FALSE}

fbi<-fbi%>%
  bind_cols(fbi1)%>%
  select(-year1)

```

```{r,echo= FALSE,results='hide',warning=FALSE,message=FALSE}
fbi$Total_num_of_incidents_reported <- as.numeric(fbi$Total_num_of_incidents_reported)
fbi$Population <- as.numeric(fbi$Population)

fbi<-fbi%>%
  mutate(reports_per_100k=(Total_num_of_incidents_reported*100000)/Population)

```

```{r Plotting reports per 100k against years, echo= FALSE,results='hide',warning=FALSE,message=FALSE}

ggplot(fbi)+
  geom_path(aes(y=reports_per_100k, x = year),group=1,colour="Blue")+
  labs(
    y= "Crime reports per 100K",
    x="Years"
  )+
  scale_x_continuous(breaks = seq(2008,2017,1))+
  ggtitle("Crime reports vs Years")+
  theme_bw()

```

```{r, echo= FALSE,results='hide',warning=FALSE,message=FALSE}
library(ggplot2)
library(gganimate)
library(gapminder)
library(ggrepel)
library(devtools)
devtools::install_github('thomasp85/gganimate')
```
# GG-Animation
```{r, eval=FALSE,echo= FALSE, results='hide',warning=FALSE,message=FALSE}
theme_set(theme_bw())
p <- ggplot(fbi,
            aes(
              x = Total_num_of_incidents_reported,
              y = reports_per_100k,
              size = Population
              )
            ) +
  geom_point() +
  geom_label(aes(label = round(year, 0)), nudge_x = 0.05, size = 6) +
  transition_time(year) +
  labs(title = "Year: {frame_time}") + 
  scale_x_log10()

animate(p)
```

# Conclusion:

Hate crimes are positively correlated with disabilty and income inequality. As the disability and income inequality increases the hate crimes tend to increase as well. Therefore, we had to reduce the income inequality index to significantly reduce the hate crimes.


# References:
1. Federal Bureau of Investigation. (2017). Hate crimes. Washington, DC: U. S. Department of Justice. Retrieved from https://www.fbi.gov/about-us/investigate/civilrights/hate_crimes/overview
2. https://fivethirtyeight.com/features/higher-rates-of-hate-crimes-are-tied-to-income-inequality/
3. https://www.splcenter.org/20180415/hate-crimes-explained#collection
