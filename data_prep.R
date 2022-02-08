library(tidyverse)
library(lubridate)
library(egg)

evap <- read.csv("./data/evap_raw.csv")
evap_ctrl <- read.csv("./data/evap_control.csv")

evap_all <- evap %>% select(date:evpRate_g.mm2.s) %>%
  filter(paperID != "") %>%
  bind_rows(
    evap_ctrl %>% select(date:evpRate_g.mm2.s) %>%
      filter(startTime != "") %>%
      rename(paperID = ID)
  ) %>%
  mutate(startTime = as.POSIXlt(paste(date, startTime, sep = ":"), 
                             format = "%m/%d/%Y:%H:%M:%S")) %>%
  mutate(endTime = as.POSIXlt(paste(date, endTime, sep = ":"), 
                             format = "%m/%d/%Y:%H:%M:%S")) %>%
  rowwise() %>%
  mutate(date = mean(c(startTime, endTime))) %>% ungroup() %>%
  mutate(startDecT = hour(startTime) + minute(startTime)/60 + second(startTime)/3600)%>%
  mutate(endDecT = hour(endTime) + minute(endTime)/60 + second(endTime)/3600) %>%
  mutate(evapRate_mg = evpRate_g.mm2.s/1000)

ggplot(evap_all, aes(date, evapRate_mg, color = factor(plotID))) + geom_point()
