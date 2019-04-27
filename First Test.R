library(tidyverse)
library(rvest)
library(shiny)
library(httr)
library(Quandl)

#setting the key for the API
Quandl.api_key("akHYXf6xiWgjt6itcz4X")

planodat <- as_tibble(Quandl("ZILLOW/C72_ZHVIAH")) %>% select(Date, "Plano"=Value)
bostondat <- as_tibble(Quandl("ZILLOW/C22_ZHVIAH")) %>% select(Date, "Boston"=Value)
chicagodat <- as_tibble(Quandl("ZILLOW/C3_ZHVIAH")) %>% select(Date, "Chicago"=Value)
esterodat <- as_tibble(Quandl("ZILLOW/C2246_ZHVIAH")) %>% select(Date, "Estero"=Value)
usdat <- as_tibble(Quandl("ZILLOW/M1_ZHVIAH")) %>% select(Date, "US"=Value)
arlingtondat <- as_tibble(Quandl("ZILLOW/CO3401_ZHVIAH")) %>% select(Date, "Arlington" =Value)


dat1 <- full_join(planodat, bostondat, by=c("Date"))
dat2 <- full_join(chicagodat, esterodat, by=c("Date"))
dat3 <- full_join(usdat, arlingtondat, by=c("Date"))
dat4<- full_join(dat1, dat2, by=c("Date"))
dat <- full_join(dat4, dat3, by=c("Date")) %>% arrange(Date)
dat

dat <- cbind(dat[,1], (dat[,-1]/1000))

#Using a loop to build separate tibbles for each indexed year

for(i in 1996:2018){
  x <- ifelse(i==1996, 1, ((i-1996)*12)-2)
  
  dat %>%
    mutate(Plano = Plano/Plano[x],
           Boston = Boston/Boston[x],
           Chicago = Chicago/Chicago[x],
           Estero = Estero/Estero[x],
           US = US/US[x],
           Arlington = Arlington/Arlington[x]) %>%
    gather(key="City", value="Value", Boston, Chicago, Arlington, Estero, Plano, US) %>%
    arrange(., Date) %>%
    assign(paste0("dat", i), ., envir = .GlobalEnv)
}

#also using the gather function for the non-indexed data
dattrue <- dat %>%
  gather(key="City", value="Value", Boston, Chicago, Arlington, Estero, Plano, US) %>%
  arrange(Date)

#Creating the U.S.-adjusted data
datadjust <- dat %>% mutate(Plano = Plano/US,
                            Boston = Boston/US,
                            Chicago = Chicago/US,
                            Estero = Estero/US,
                            Arlington = Arlington/US,
                            US=US/US) %>%
  select(Date, Arlington, Boston, Chicago, Estero, Plano, US) 

datadjusttrue <-datadjust %>% gather(key="City", value="Value", Boston, Chicago, Arlington, Estero, Plano, US) %>%
  arrange(., Date)

#Creating the tibbles for each year for hte U.S.-adjusted data
for(i in 1996:2018){
  x <- ifelse(i==1996, 1, ((i-1996)*12)-2)
  
  datadjust %>%
    mutate(Plano = Plano/Plano[x],
           Boston = Boston/Boston[x],
           Chicago = Chicago/Chicago[x],
           Estero = Estero/Estero[x],
           Arlington = Arlington/Arlington[x],
           US=US/US[x]) %>%
    gather(key="City", value="Value", Boston, Chicago, Arlington, Estero, Plano, US) %>%
    arrange(., Date) %>%
    assign(paste0("datadjust", i), ., envir = .GlobalEnv)
}

#creating a vector to manually assign colors later in the visualization 
cols <- c("Boston" = "orangered2", 
          "Chicago"= "gold2", 
          "Plano"= "dodgerblue3", 
          "Arlington"="magenta3", 
          "Estero" = "steelblue1",
          "US"="black")
names <- c("Boston"="Boston, MA", "Chicago"= "Chicago, IL", 
           "Plano"= "Plano, TX" , "Arlington" = "Arlington, VA", 
           "Estero"= "Estero, FL", "US"= "U.S. Average")
