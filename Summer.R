library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(lubridate)
data <- read.csv("sales_with_rate.csv")
data12 <- read_excel("HistoricSalesExport.xlsx")
clean <- data %>% 
  mutate(age = 2025-year_built, year = year(date_of_sale), date_of_sale=as.Date(date_of_sale)) %>% 
  filter(sale_price!= 0 & finished_sq_ft<5000 & year >= 2018)
  

clean %>%
  filter(age < 250) %>%
  mutate(age_group = cut(age,
                         breaks = c(0, 25, 50, 100, 125, 150, 175, 200, Inf),
                         labels = c("0-25", "25-50", "50-100", "100-125",
                                    "125-150", "150-175", "175-200", "old_houses"),
                         include.lowest = TRUE)) %>%
  group_by(age_group) %>%
  summarise(avg_price = mean(sale_price, na.rm = TRUE)) %>%
  ggplot(aes(x = age_group, y = avg_price)) +
  geom_col()+
  labs(title = "Average Sale Price by Age Group",
       x = "Age Group of House",
       y = "Average Sale Price") +
  theme_minimal()
# House size
clean %>% 
  
  mutate(area = cut(finished_sq_ft,
                         breaks = c(0, 5000, 10000, 15000, 20000, 25000),
                         labels = c("0-5000", "5000-10000", "10000-15000", "15000-20000",
                                    "20000-25000"),
                         include.lowest = TRUE)) %>%
  group_by(area) %>%
  summarise(avg_price = mean(sale_price, na.rm = TRUE), sum_price= sum(log(sale_price), na.rm=T)) %>%
  ggplot(aes(x = area, y = avg_price)) +
  geom_col()+
  labs(title = "Average Sale Price by Area Group",
       x = "Area Group of House",
       y = "Average Sale Price") +
  theme_minimal()

clean %>% 
  filter(year %in% c(2018, 2020)) %>% 
  mutate(area = cut(finished_sq_ft,
                    breaks = c(0, 1000, 2000, 3000, 4000, 5000),
                    labels = c("0-1000", "1000-2000", "2000-3000", "3000-4000",
                               "4000-5000"),
                    include.lowest = TRUE)) %>%
  group_by(area,year) %>% 
  summarise(avg_price = mean(sale_price, na.rm=T), avg_rate = mean(annual_rate, na.rm=T),.groups = 'drop') %>% 
  ggplot(aes(area, avg_price, fill=factor(year)))+
  geom_col(position="dodge")+
  labs(x="Finished area", y="Average Price", fill="Year", title = "Finished area of the property vs Price")

sum <- 80297
clean %>% 
  group_by(year) %>% 
  summarise(count=n(), avg_rate=mean(annual_rate,na.rm=T)) %>% 
  mutate( percentage = count / sum(count)*100)%>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = percentage, color = "Percentage of purchases"), size = 1.2) +
  geom_line(aes(y = avg_rate, color = "Average Mortgage Rate"), size = 1.2) +
  labs( title = "Annual Mortgage and Sales Count Over Years", x = "Year", y ='Percentage', colour = 'Percentage of sale \n vs Annual Mortgage') +
  theme_minimal()

library(data.table)
unemployment <- read_excel("OHHAMI1URN.xlsx", sheet = "Monthly") %>% 
  mutate(date=as.Date(observation_date))
setDT(unemployment)
setDT(clean)
setkey(unemployment, date)
setkey(clean, date_of_sale)
sales <- unemployment[clean, roll=TRUE, on=.(date=date_of_sale)]

sales_clean <- sales %>% 
  rename(unemploy_rate= OHHAMI1URN)
sales_clean %>% 
  group_by(year) %>% 
  summarise(count=n(), avg_rate=mean(annual_rate,na.rm=T), avg_unemploy=mean(unemploy_rate, na.rm=T)) %>% View()
  mutate( percentage = count / sum(count)*100)%>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = percentage, color = "Percentage of sale"), size = 1.2) +
  #geom_line(aes(y = avg_rate, color = "Average Rate"), size = 1.2) +
  geom_line(aes(y=avg_unemploy, color = "Average Unemployment Rate"), size=1.2)+
  labs( x = "Year", y ='Percentage', title = 'Percentage of sale vs Unemployment rate', colour='')
  

lm_model <- lm(sale_price ~ finished_sq_ft + annual_rate+ unemploy_rate, data = sales_clean)
summary(lm_model)

write.csv(sales_clean,"sales_clean2.csv",row.names = F)

sales_clean %>% 
  
#### ####
sales_clean <- read.csv("sales_clean2.csv")
library(patchwork)
library(ggplot2)
library(patchwork) # Or use library(gridExtra)

# Plot 1: sale_price vs finished_sq_ft
p1 <- ggplot(sales_clean, aes(x = finished_sq_ft, y = sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Sale Price vs Finished Sq Ft", x = "Finished Sq Ft", y = "Sale Price")

# Plot 2: sale_price vs annual_rate
p2 <- ggplot(sales_clean, aes(x = annual_rate, y = sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  labs(title = "Sale Price vs Annual Rate", x = "Annual Mortgage Rate", y = "Sale Price")

# Plot 3: sale_price vs unemploy_rate
p3 <- ggplot(sales_clean, aes(x = unemploy_rate, y = sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Sale Price vs Unemployment Rate", x = "Unemployment Rate", y = "Sale Price")

p1 + p2 + p3  
library(data.table)

total_listing <- read_excel('TOTLISCOU39061.xlsx', sheet = "Monthly") %>% 
  mutate(date=as.Date(observation_date),
         year=year(date)) %>% 
  filter(year>=2018)

setDT(total_listing)
setDT(sales_clean)
sales_clean[, date := as.Date(clean_date)]
setkey(total_listing, date)
combined_sales <- total_listing[sales_clean,roll=TRUE, on=.(date)]
combined_sales <- combined_sales %>% 
                rename(total_availa = TOTLISCOU39061)


lm(sale_price~total_availa, data=combined_sales)
combined_sales %>% 
  mutate( clean_date = as.Date(clean_date),
          year_month = format(clean_date, "%Y-%m"),
          total_availa = as.numeric(total_availa)) %>% 
  group_by(year_month) %>% 
  summarise(total_purchase = n(), total_listing = mean(total_availa,na.rm=T)) %>% 
  mutate(year_month = as.Date(paste0(year_month, "-01"))) %>%
  ggplot(aes(x=year_month))+
  geom_line(aes(y = total_purchase, color = 'Total purchases of properties'), size=1.2)+
  geom_line(aes(y=total_listing, color = 'Total avaiable properties'), linewidth =1.2)+
  labs(title = "Supply vs Demand", x ="Year", y = 'Count of records', color = 'Supply vs Demand')

unemploy <-  combined_sales %>%
  mutate(
    clean_date = as.Date(clean_date),
    year_month = format(clean_date, "%Y-%m"),
    total_availa = as.numeric(total_availa)
  ) %>%
  group_by(year_month) %>%
  summarise(
    count = n(),
    avg_price = mean(sale_price, na.rm = TRUE),
    avg_unemploy = mean(unemploy_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year_month = as.Date(paste0(year_month, "-01"))) %>%  # Convert to Date
  ggplot(aes(x = year_month, y = avg_unemploy)) +
  geom_line(color ='forestgreen', linewidth = 1.2) + 
  labs(
    x = "Month/Year",
    y = "Average rate of unemployment",
    title = "Unemployment Rate Over Time"
  ) +
  theme_minimal()

price <- combined_sales %>%
  mutate(
    clean_date = as.Date(clean_date),
    year_month = format(clean_date, "%Y-%m"),
    total_availa = as.numeric(total_availa)
  ) %>%
  group_by(year_month) %>%
  summarise(
    count = n(),
    avg_price = mean(sale_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year_month = as.Date(paste0(year_month, "-01"))) %>%  # Convert to Date
  ggplot(aes(x = year_month, y = log(avg_price))) +
  geom_line(linewidth = 1.2, color = 'magenta4')+ 
  labs(
    x = "Month/Year",
    y = "Log of average price",
    title = "Log of average price Over Time"
  ) +
  theme_minimal()
library(patchwork)
  price|unemploy
  
  model <- lm(sale_price ~ unemploy_rate + annual_rate + total_availa + finished_sq_ft, data = combined_sales)
  summary(model)
library(tidyverse)  
library(httr)       
library(rvest)      
library(lubridate)  
library(magrittr) 
set_config(user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"))
mortgage_rate <- read_html("https://data.cincinnati.com/mortgage-rates/")

date <- mortgage_rate %>% 
  html_elements("a.black.bold") %>% 
  html_text2()

monthly_payment <- mortgage_rate %>% 
  html_elements("tr") %>%     # Select rows
  html_elements("td:nth-child(2)") %>%  # Second <td> contains price
  html_text2()
monthly_payment <- monthly_payment[-1]

monthly_rate <- mortgage_rate %>% 
  html_elements("tr") %>%     # Select rows
  html_elements("td.thr.bold") %>%  # Second <td> contains price
  html_text2() %>% 
  discard(~ .x == "" | is.na(.x)) %>% 
  as.numeric() %>%                # Convert to numeric
  keep(~ .x >= 1)  

monthly_rate_15yrs <- mortgage_rate %>% 
  html_elements("tr") %>%     # Select rows
  html_elements("td.fif.bold") %>%  # Second <td> contains price
  html_text2() %>% 
  discard(~ .x == "" | is.na(.x)) %>% 
  as.numeric() %>%                # Convert to numeric
  keep(~ .x >= 2)  
view(monthly_rate_15yrs)

mortgage <- data.frame(date, monthly_payment, monthly_rate,monthly_rate_15yrs)
write.csv(mortgage, "mortgage.csv", row.names = FALSE)

####Total listing Hamilton county####
library(reaclxl)
listing <- read_excel("TOTLISCOU39061.xlsx", sheet = 'Monthly')
listing %>% 
  mutate(date= as.Date(observation_date)) %>% 
  ggplot(aes(x = date, y = TOTLISCOU39061)) +
  geom_line() +
  labs(title = "Total listing houses in Hamilton County",
       x = "Date",
       y = "Value") +
  theme_minimal()

####Median price ####
median_listing_price <- read_excel("MELIPRMMCOUNTY39061.xlsx", sheet = 'Monthly')
median_listing_persqr <- read_excel("MEDLISPRIPERSQUFEE39061.xlsx",sheet = "Monthly") 
median_listing_persqr%>% 
  mutate(date= as.Date(observation_date)) %>% 
  ggplot(aes(x = date, y = MEDLISPRIPERSQUFEE39061)) +
  geom_line() +
  labs(title = "Median listing price per square feet",
       x = "Date",
       y = "Value") +
  theme_minimal()

