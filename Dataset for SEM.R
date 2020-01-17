library(tidyverse)

setwd('C:\\Users\\megha\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')
setwd('C:\\Users\\mavolio2\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')

survey<-read.csv("2018 Homeowner_Survey Data_081219.csv")

survey2<-survey%>%
  filter(House_ID!="")%>%
  separate(House_ID, into=c("Nb", "House"), sep="_")%>%
  mutate(Nb=as.integer(Nb), House=as.integer(House))%>%
  rename(yard.sat=A1, 
         imp.native=A4,
         imp.var=A5,
         imp.biod=A6,
         imp.fl.color=A702,
         imp.fl.type=A703,
         imp.tr.sp=A709,
         imp.orn=A710,
         time.yard=C101.1,
         freq.weed=C201,
         use.pest=C301,
         use.fert=C601,
         use.herb.pest=C701,
         water.use.lawn=C1001,
         water.use.orn=C1002,
         water.use.tree=C1003,
         water.use.food=C1004,
         buy.ann=C401,
         ann.buy.freq=C402,
         buy.per=C403,
         per.buy.freq=C404,
         buy.tree=C405,
         tree.buy.freq=C406,
         buy.shrub=C407,
         shrub.buy.freq=C408,
         pay.lawn.care=C5)%>%
  select(Nb, House, yard.sat, imp.native, imp.var, imp.biod, imp.fl.color, imp.fl.type, imp.tr.sp, imp.orn, time.yard, freq.weed, use.pest, use.fert, use.herb.pest, water.use.lawn, water.use.orn, water.use.tree, water.use.food, buy.ann, ann.buy.freq, buy.per, per.buy.freq, buy.tree, tree.buy.freq, buy.shrub, shrub.buy.freq, pay.lawn.care)
  

div_fb<-read.csv("All_Front_Back_Diversity.csv")

div_ave<-div%>%
  group_by()

invasive<-read.csv("Invasive_Richness.csv")

SEM_div_FB<-div_fb%>%
  left_join(survey2)

SEM_div_all

