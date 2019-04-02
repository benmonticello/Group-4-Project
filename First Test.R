library(tidyverse)
library(rvest)
library(shiny)
library(httr)
library(Quandl)

#setting the key for the API
Quandl.api_key("akHYXf6xiWgjt6itcz4X")

newyorkdat <- as_tibble(Quandl("ZILLOW/C1_ZHVIAH")) %>% select(Date, "NewYork"=Value)
bostondat <- as_tibble(Quandl("ZILLOW/C16_ZHVIAH")) %>% select(Date, "Boston"=Value)
chicagodat <- as_tibble(Quandl("ZILLOW/C3_ZHVIAH")) %>% select(Date, "Chicago"=Value)
losangelesdat <- as_tibble(Quandl("ZILLOW/C2_ZHVIAH")) %>% select(Date, "LosAngeles"=Value)
phillydat <- as_tibble(Quandl("ZILLOW/C4_ZHVIAH")) %>% select(Date, "Philadelphia"=Value)
dcdat <- as_tibble(Quandl("ZILLOW/C18_ZHVIAH")) %>% select(Date, "DC" =Value)

dat1 <- full_join(newyorkdat, bostondat, by=c("Date"))
dat2 <- full_join(chicagodat, losangelesdat, by=c("Date"))
dat3 <- full_join(phillydat, dcdat, by=c("Date"))
dat4<- full_join(dat1, dat2, by=c("Date"))
dat <- full_join(dat4, dat3, by=c("Date")) %>% arrange(Date)
dat

dat[, -1] <- dat[, -1]/1000 #scaling everything (except dates) down by 1000


#Using a loop to build different tibbles for each indexed year

for(i in 1996:2018){
  x <- ifelse(i==1996, 1, ((i-1996)*12)-2)
  
  dat %>%
    mutate(NewYork = NewYork/NewYork[x],
           Boston = Boston/Boston[x],
           Chicago = Chicago/Chicago[x],
           LosAngeles = LosAngeles/LosAngeles[x],
           Philadelphia = Philadelphia/Philadelphia[x],
           DC = DC/DC[x]) %>%
    gather(key="City", value="Value", Boston, Chicago, DC, LosAngeles, NewYork, Philadelphia) %>%
    arrange(., Date) %>%
    assign(paste0("dat", i), ., envir = .GlobalEnv)
}

#also using a gather function for the non-indexed data
dattrue <- dat %>%
  gather(key="City", value="Value", Boston, Chicago, DC, LosAngeles, NewYork, Philadelphia) %>%
  arrange(Date)
dattrue
