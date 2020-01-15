library(tidyverse)

setwd('C:\\Users\\megha\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')

survey<-read.csv("2018 Homeowner_Survey Data_081219.csv")%>%
  filter(House_ID!="")

div<-read.csv("All_Front_Back_Diversity.csv")

invasive<-read.csv("Invasive_Richness.csv")
