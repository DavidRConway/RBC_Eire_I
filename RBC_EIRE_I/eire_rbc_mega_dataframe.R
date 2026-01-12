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
gni_star <- ntl_acc_data |>
  filter(
    str_detect(Statistic, 'Constant Market Prices \\(Seasonally Adjusted\\)') 
    |
    str_detect(Statistic, 'Current Market Prices \\(Seasonally Adjusted\\)')) |>
  filter(
    Sector == 'Modified Final Domestic Demand'
  ) 
         
View(gni_star)         


#Making data longform
gni_star <- gni_star |>
  pivot_longer(
    cols = !c(Statistic, Sector),
    names_to = 'date',
    values_to = 'gni_star'
  ) |>
  select(
    Statistic,
    date,
    gni_star,
  ) 

View(gni_star)


#...
real_gni_star <- gni_star |>
  filter(
    str_detect(Statistic, 'Constant') 
  ) |>
  rename(
    real_gni_star = gni_star
  ) |>
  select(
    date,
    real_gni_star
  )

View(real_gni_star)


#...
nom_gni_star <- gni_star |>
  filter(
    str_detect(Statistic, 'Current') 
  ) |>
  rename(
    nom_gni_star = gni_star
  ) |>
  select(
    date,
    nom_gni_star
  )

View(nom_gni_star)



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



#---------------Retrieving nominal W_t x L_t from CSO NAQ08------------------------

#real wage comp only goes back to 2010
nom_agg_worker_comp_data <- cso_get_data('NAQ08') 

View(nom_agg_worker_comp_data)


#...
nom_agg_worker_comp_data <- nom_agg_worker_comp_data |>
  filter(
    Statistic == 'Compensation of Employees at Current Prices (Seasonally Adjusted)'
    &
    NACE.Rev..2.Sector == 'All Sectors'
    &
    Item == 'Compensation of employees (D.1)'
  )

View(nom_agg_worker_comp_data)


#...
nom_agg_worker_comp_data <- nom_agg_worker_comp_data |>
  pivot_longer(
    cols = !c(Statistic, NACE.Rev..2.Sector, Item),
    names_to = 'date',
    values_to = 'nom_agg_worker_comp'
  ) |>
  select(
    date,
    nom_agg_worker_comp
  )

View(nom_agg_worker_comp_data)




#---------------Retrieving nominal W_t x L_t from CSO CSA02------------------------

#...
cptl_stock_data <- cso_get_data('CSA02') 

View(cptl_stock_data)

real_cptl_stock_data <- cptl_stock_data |>
  filter(
    Statistic == 'Net Capital Stock Held at End of Year at Constant Prices'
    &
    Fixed.Asset == 'All fixed assets'
    &
    Industry.Sector.NACE.Rev.2 == 'All NACE economic sectors'
  ) |>
  pivot_longer(
    cols = !c(Statistic, Fixed.Asset, Industry.Sector.NACE.Rev.2),
    names_to = 'date',
    values_to = 'real_net_EOY_cap_stock'
  ) |>
  select(
    date,
    real_net_EOY_cap_stock
  ) |>
  filter(
    date >= 1994
  )

View(real_cptl_stock_data)


real_cptl_stock_adj <- real_cptl_stock_data |>
  mutate(
    date = as.numeric(date)
  ) |>
  mutate(
    date = date + 1
  ) |>
  mutate(
    date = as.character(date)
  ) |>
  mutate(
    date = paste0(date, 'Q1')
  ) |>
  mutate(
    real_net_EOY_cap_stock = real_net_EOY_cap_stock/1000
  )

View(real_cptl_stock_adj)


#---------------Retrieving price index from CSO NAQ03------------------------

#...
price_index <- cso_get_data('CPM02')

view(price_index)




#-------------Merging macro time series into single mega data frame--------------------

#...
merger <- list(
  real_gni_star,
  nom_gni_star,
  real_consumption_data,
  real_investment_data,
  agg_hourly_labor_data,
  nom_agg_worker_comp_data,
  real_cptl_stock_adj
)


#constructing mega data frame
merged_data <- merger |>
  reduce(full_join, by = 'date')


#construction gdp deflator + obtaining real wage data
merged_data <- merged_data |>
  mutate(
    gni_deflator = nom_gni_star/real_gni_star,
  ) |>
  mutate(
    real_agg_worker_comp = nom_agg_worker_comp/gni_deflator
  ) |>
  mutate(
    real_wage = real_agg_worker_comp/agg_labor_hours
  ) |>
  mutate(
    real_quarterly_cptl = c(312074, rep(NA, n() - 1))
  ) 

View(merged_data)d
  

#...
merged_data <- merged_data |>
  mutate(
    real_quarterly_cptl = accumulate(
      real_investment[-n()],
      ~ .y + (1 - 0.006)*.x,
      .init = first(real_quarterly_cptl)
    )
  )

View(merged_data)


merged_data <- merged_data |>
  mutate(
    real_interest_rate = (real_gni_star - real_wage*agg_labor_hours)/real_quarterly_cptl
  )


merged_data <- merged_data |>
  mutate(
    tfp = real_gni_star/((real_quarterly_cptl**0.46)*(agg_labor_hours**(1 - 0.46)))
  )

View(merged_data)



#---------PLOTTING------------------
plot_frame <- merged_data |>
  pivot_longer(
    cols = !date,
    names_to = 'macro_series',
    values_to = 'value'
  )

View(plot_frame)

plot_frame |>
  ggplot(aes(x = date, y = value, color = macro_series, group = macro_series)) +
  geom_line(na.rm = TRUE)


plot_frame |>
  filter(macro_series == 'real_wage') |>
  ggplot(aes(x = date, y = value)) +
  geom_point(na.rm = TRUE)


plot_frame |>
  filter(macro_series == 'real_interest_rate') |>
  ggplot(aes(x = date, y = value, group = 1)) +
  geom_line(na.rm = TRUE) 

























