library(tidyverse)

setwd('C:\\Users\\megha\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')
setwd('C:\\Users\\mavolio2\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')

survey<-read.csv("2018 Homeowner_Survey Data_081219.csv")

survey2<-survey%>%
  filter(House_ID!="")%>%
  separate(House_ID, into=c("Nb", "House"), sep="_")%>%
  mutate(Nb=as.integer(Nb), House=as.integer(House))%>%
  rename(imp.native=A4,
         imp.var=A5,
         imp.biod=A6,
         imp.fl.color=A702,
         imp.fl.type=A703,
         imp.tr.sp=A709,
         imp.orn=A710,
         imp.t.shade=B101,
         imp.t.wind=B105,
         imp.t.vis=B202,
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
         pay.lawn.care=C5, 
         remove.lf=C801)%>%
  select(Nb, House, imp.native, imp.var, imp.biod, imp.fl.color, imp.fl.type, imp.tr.sp, imp.orn, imp.t.shade,imp.t.wind, imp.t.vis, time.yard, freq.weed, use.pest, use.fert, use.herb.pest, water.use.lawn, water.use.orn, water.use.tree, water.use.food, buy.ann, ann.buy.freq, buy.per, per.buy.freq, buy.tree, tree.buy.freq, buy.shrub, shrub.buy.freq, pay.lawn.care,remove.lf)

yardarea<-read.csv("YardArea.csv")
  

div_fb<-read.csv("All_Front_Back_Diversity.csv")
div_all<-read.csv("All_Diversity_House.csv")

div_ave<-div%>%
  group_by()

invasive<-read.csv("Invasive_Richness.csv")

SEM_div_FB<-div_fb%>%
  left_join(survey2)
SEM_div_all

SEM_practive_lawn<-div_all%>%
  left_join(survey2)%>%
  left_join(yardarea)%>%
  select(HOME_VALUE, time.yard, freq.weed, use.fert, use.herb.pest, water.use.lawn, pay.lawn.care,l.rich, leven)%>%
  mutate(freq.weed2=ifelse(freq.weed=="No answer", 0, as.numeric(as.character(freq.weed))))%>%
  mutate(use.fert2=ifelse(use.fert=="No answer"&pay.lawn.care==1, 1, ifelse(use.fert=="No answer"&pay.lawn.care==0, 0, ifelse(use.fert=="No answer"&pay.lawn.care=="No answer",0, as.numeric(as.character(use.fert))))))%>%
  mutate(use.herb.pest2=ifelse(use.herb.pest=="No answer"&pay.lawn.care==1, 1, ifelse(use.herb.pest=="No answer"&pay.lawn.care==0, 0, ifelse(use.herb.pest=="No answer"&pay.lawn.care=="No answer",0, as.numeric(as.character(use.herb.pest))))))%>%
  filter(time.yard!="No answer")%>%
  filter(water.use.lawn!="No answer"&water.use.lawn!="no answer"&water.use.lawn!="not sure")%>%
  select(-freq.weed, -use.fert, -use.herb.pest, -pay.lawn.care)%>%
  na.omit

write.csv(SEM_practive_lawn, "SEM_Lawn_Model.csv")
  
