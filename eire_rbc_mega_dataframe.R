library(tidyverse)
library(lubridate)
library(csodata)
library(eurostat)





#---------------Retrieving Y from CSO NAQ05-----------------------------------

#Retrieving inflation and seasonally adjusted, non-distorted output 
#quarterly time series data for Irish economy
ntl_acc_data <- cso_get_data('NAQ05') 

View(ntl_acc_data)


#Extracting relevant rows (note data is wideform)
real_gni_star <- ntl_acc_data |>
  filter(str_detect(Statistic, 'Constant Market Prices \\(Seasonally Adjusted\\)') & Sector == 'Modified Final Domestic Demand') 
         
View(real_gni_star)       


#Making data longform
real_gni_star <- real_gni_star |>
  pivot_longer(
    cols = !c(Statistic, Sector),
    names_to = 'date',
    values_to = 'real_gni_star'
  ) |>
  select(
    date,
    real_gni_star
  )

View(real_gni_star)



#---------------Retrieving C from CSO NAQ05---------------------------------------

#...
real_consumption_data <- ntl_acc_data |>
  filter(
    Sector == 'Personal Expenditure on Consumer Goods and Services'
    &
    str_detect(Statistic, 'Constant Market Prices \\(Seasonally Adjusted\\)')
  )

View(real_consumption_data)


#...
real_consumption_data <- real_consumption_data |>
  pivot_longer(
    cols = !c(Statistic, Sector),
    names_to = 'date',
    values_to = 'real_consumption'
  ) |>
  select(
    date,
    real_consumption
  )

View(real_consumption_data)



#---------------Retrieving I from CSO NAQ05---------------------------------------

#...
real_investment_data <- ntl_acc_data |>
  filter(
    Sector == 'Modified Gross Domestic Fixed Capital Formation'
    &
      str_detect(Statistic, 'Constant Market Prices \\(Seasonally Adjusted\\)')
  )

View(real_investment_data)


#...
real_investment_data <- real_investment_data |>
  pivot_longer(
    cols = !c(Statistic, Sector),
    names_to = 'date',
    values_to = 'real_investment'
  ) |>
  select(
    date,
    real_investment
  )

View(real_investment_data)



#---------------Retrieving L from CSO QLF18---------------------------------------

#Note data only goes back to 1998-Q1; units is in millions of hours 
#per week
labor_data <- cso_get_data('QLF36')

View(labor_data)


#...
agg_hourly_labor_data <- labor_data |>
  filter(
    NACE.Rev.2.Economic.Sector == 'All NACE economic sectors'
  )

View(agg_hourly_labor_data)


#...
agg_hourly_labor_data <- agg_hourly_labor_data |>
  pivot_longer(
    cols = !c(Statistic, NACE.Rev.2.Economic.Sector),
    names_to = 'date',
    values_to = 'agg_labor_hours'
  ) |>
  select(
    date,
    agg_labor_hours
  )

View(agg_hourly_labor_data)




