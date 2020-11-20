library(tidyverse)
library(dplyr)

library(rvest)

#Scrape UST yields
web_ust <- read_html("https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield")
tbls <- html_nodes(web_ust, "table")
head(tbls)



ust <- web_ust %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)%>%
  .[[2]]

#format dates
ust$Date <- as.Date(ust$Date, format = "%m/%d/%y")



#only last date
df <- ust %>%
  filter(Date == max(as.Date(Date)))

df <- df %>%
  pivot_longer(-Date,
               names_to = "term",
               values_to = "yield")


#str_remove_all(df, "[ mo]")


df <- df %>%
  mutate(t1 = as.numeric(gsub("([0-9]+).*$", "\\1", df$term)))%>%
  mutate(maturity = case_when(
    grepl("yr", df$term, fixed = TRUE) ~ (t1 *365 / 365),
    grepl("mo", df$term, fixed= TRUE) ~ (t1 * 30 /365)
    
  ))%>%
  select(-t1)%>%
  filter(term != "2 mo")%>%
  mutate(bond = 1:n())


df_3 <- df %>% 
  group_by(Date)%>%
  mutate(dummy = 1L) %>% 
  inner_join(., ., by = "dummy", suffix=c("_short", "_long")) %>% 
  select(-dummy) %>% 
#filter(bond_short < bond_long) %>%
  filter(Date_short == Date_long)

#%>%
 # filter(bond_short <= bond_long)


df_3 <- mutate(df_3, ttm_new_bond = (maturity_long  - maturity_short))



day_count <- 360

calculate_forward_rate <- function(maturity_short, yield_short, maturity_long, yield_long, day_count){
  
  short_bond <- (1+yield_short/100)^(maturity_short/day_count)
  long_bond <- (1+yield_long/100)^(maturity_long/day_count)
  days_between <- (maturity_long - maturity_short)
  forward_rate <- ((long_bond/short_bond)^(360/days_between)-1)*100
  return(round(forward_rate, digits=2))  
}

# calculate_forward_rate(maturity_short, yield_short, maturity_long, yield_long, day_count)


df_3 <- df_3 %>%
  mutate(forward_rate = calculate_forward_rate(
    maturity_short,
    yield_short,
    maturity_long,
    yield_long,
    day_count))

#Create a yield_diff. how much more/less the yield must be when its time to buy the subsequent bond       
df_3 <- df_3 %>%
  mutate(yield_diff = if_else(bond_short == bond_long, NA_real_, forward_rate - yield_short))

#create a slope
df_3 <- df_3 %>%
  mutate(slope = if_else(bond_short == bond_long, NA_real_, yield_long - yield_short))


###Test 
 df_3 <- df_3 %>%
 mutate(yield_diff = ifelse(bond_short > bond_long, "", yield_diff))%>%
   mutate(forward_rate = ifelse(bond_short > bond_long, "", forward_rate))%>%
   mutate(slope = ifelse(bond_short > bond_long, "", slope))
  


###Obs continue here
#i = 2
#create matrix with forward rates, run 10 times
###
MatrixA <- matrix(data = 1:11, nrow = 11, ncol = 0) 
for(i in c(1:11)){
  df <- df_3%>%
    filter(bond_short==i)
  MatrixA <- cbind(MatrixA, df$forward_rate)
  
}

Names <- c("1m", "3m", "6m", "12m", "2y", "3y", "5y", "7y", "10y", "20y", "30y")
colnames(MatrixA) <- Names   
rownames(MatrixA) <- Names






###
#create matrix with yield diff, run 10 times
### THis states how much more / yield you need on the new bond then the first
MatrixB <- matrix(data = 1:11, nrow = 11, ncol = 0)  
for(i in c(1:11)){
  df <- df_3%>%
    filter(bond_short==i)
  MatrixB <- cbind(MatrixB, df$yield_diff)
}

Names <- c("1m", "3m", "6m", "12m", "2y", "3y", "5y", "7y", "10y", "20y", "30y")
colnames(MatrixB) <- Names   
rownames(MatrixB) <- Names


###
#Create a matrix with curve slope
###
MatrixC <- matrix(data = 1:11, nrow = 11, ncol = 0)  
for(i in c(1:11)){
  df <- df_3%>%
    filter(bond_short==i)
  MatrixC <- cbind(MatrixC, df$slope)
}

Names <- c("1m", "3m", "6m", "12m", "2y", "3y", "5y", "7y", "10y", "20y", "30y")
colnames(MatrixC) <- Names   
rownames(MatrixC) <- Names
