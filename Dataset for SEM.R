library(tidyverse)

setwd('C:\\Users\\megha\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')
setwd('C:\\Users\\mavolio2\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')

survey<-read.csv("2018 Homeowner_Survey Data_081219.csv")
yardarea<-read.csv("YardArea.csv")%>%
  select(Nb, House, HOME_VALUE)
#div_fb<-read.csv("All_Front_Back_Diversity.csv")
div_all<-read.csv("All_Diversity_House.csv")
invasive<-read.csv("Invasive_Richness.csv")

test<-survey%>%
  select(House_ID, D2)

survey2<-survey%>%
  filter(House_ID!="")%>%
  separate(House_ID, into=c("Nb", "House"), sep="_")%>%
  mutate(Nb=as.integer(Nb), House=as.integer(House))%>%
  mutate(imp.native=ifelse(A4=="No answer", mean(as.numeric(A4)), A4),
         imp.var=A5,
         imp.biod=ifelse(A6=="No answer", mean(as.numeric(A6)), A6),
         imp.fl.color=ifelse(A702=="No answer", mean(as.numeric(A702)), A702),
         imp.fl.type=ifelse(A703=="No answer", mean(as.numeric(A703)), A703),
         imp.tr.sp=ifelse(A709=="No answer", mean(as.numeric(A709)), A709),
         imp.orn=ifelse(A710=="No answer",mean(as.numeric(A710)), A710),
         imp.t.shade=ifelse(B101=="No answer",mean(as.numeric(B101)), B101), #importance of shade
         imp.t.fruit=ifelse(B102=="No answer", mean(as.numeric(B102)), B102),
         imp.t.flower=ifelse(B103=="No answer",mean(as.numeric(B103)), B103),
         imp.t.litter=ifelse(B201=="No answer",mean(as.numeric(B201)), B201),
         imp.t.fallcolor=ifelse(B810=="No answer",mean(as.numeric(B810)), B810), #choose tree with that trait
         imp.t.contrast=ifelse(B817=="No answer",mean(as.numeric(B817)), B817),
         time.yard=ifelse(C101.1=="No answer", mean(as.numeric(C101.1)), C101.1),
         freq.weed=ifelse(C201=="No answer", mean(as.numeric(C201)), C201),
         use.pest=ifelse(C301=="No answer", 1, C301),
         use.herb.pest=ifelse(C701=="No answer"&C5==1, 1, ifelse(C701=="No answer"&C5==0, 1, ifelse(C701=="No answer"&C5=="No answer",1, as.numeric(C701)))),
         use.fert=ifelse(C601=="No answer"&C5==1, 2, ifelse(C601=="No answer"&C5==0, 1, ifelse(C601=="No answer"&C5=="No answer",1, as.numeric(C601)))),
         water.use.lawn=ifelse(C1001=="No answer"|C1001=="no answer"|C1001=="not sure", mean(as.numeric(C1001)),C1001),
         water.use.orn=ifelse(C1002=="No answer"|C1002=="no answer", mean(as.numeric(C1002)),C1002),
         water.use.tree=ifelse(C1003=="No answer"|C1003=="no answer", mean(as.numeric(C1003)), ifelse(C1003=="n/a", 0, C1003)),
         water.use.food=ifelse(C1004=="No answer"|C1004=="no answer", mean(as.numeric(C1004)), ifelse(C1004=="n/a", 0, C1004)),
         water.yard=water.use.lawn+water.use.tree+water.use.orn+water.use.food,
         #buy.ann=C401,
         ann.buy.freq=ifelse(C402=="No answer", mean(as.numeric(C402)), C402),
         #buy.per=C403,
         per.buy.freq=ifelse(C404=="No answer", mean(as.numeric(C404)), C404),
         #buy.tree=C405,
         tree.buy.freq=ifelse(C406=="No answer"|C406=="No anser", mean(as.numeric(C406)), C406),
         #buy.shrub=C407,
         shrub.buy.freq=ifelse(C408=="No answer", mean(as.numeric(C408)), C408),
         buy.plants=ann.buy.freq+per.buy.freq+tree.buy.freq+shrub.buy.freq,
         pay.lawn.care=ifelse(C5=="No answer", 1, C5), 
         remove.lf=ifelse(C801=="No answer", mean(as.numeric(C801)), C801),
         yrs.address=ifelse(D2=="No answer", mean(as.numeric(D2)), D2))%>%
  select(Nb, House, imp.native, imp.var, imp.biod, imp.fl.color, imp.fl.type, imp.tr.sp, imp.orn, imp.t.shade,imp.t.fruit, imp.t.flower, imp.t.litter, imp.t.fallcolor, imp.t.contrast, time.yard, freq.weed, use.pest, use.fert, use.herb.pest, water.use.lawn, water.use.orn, water.use.tree, water.use.food, water.yard, ann.buy.freq, per.buy.freq, tree.buy.freq, shrub.buy.freq, buy.plants, pay.lawn.care, remove.lf, yrs.address)

SEM_div_all<-div_all%>%
  left_join(survey2)%>%
  left_join(yardarea)

# SEM_practive_lawn<-div_all%>%
#   left_join(survey2)%>%
#   left_join(yardarea)%>%
#   select(HOME_VALUE, time.yard, freq.weed, use.fert, use.herb.pest, water.use.lawn, pay.lawn.care,l.rich, leven)%>%
#   mutate(freq.weed2=ifelse(freq.weed=="No answer", 0, as.numeric(as.character(freq.weed))))%>%
#   mutate(use.fert2=ifelse(use.fert=="No answer"&pay.lawn.care==1, 1, ifelse(use.fert=="No answer"&pay.lawn.care==0, 0, ifelse(use.fert=="No answer"&pay.lawn.care=="No answer",0, as.numeric(as.character(use.fert))))))%>%
#   mutate(use.herb.pest2=ifelse(use.herb.pest=="No answer"&pay.lawn.care==1, 1, ifelse(use.herb.pest=="No answer"&pay.lawn.care==0, 0, ifelse(use.herb.pest=="No answer"&pay.lawn.care=="No answer",0, as.numeric(as.character(use.herb.pest))))))%>%
#   filter(time.yard!="No answer")%>%
#   filter(water.use.lawn!="No answer"&water.use.lawn!="no answer"&water.use.lawn!="not sure")%>%
#   select(-freq.weed, -use.fert, -use.herb.pest, -pay.lawn.care)%>%
#   na.omit

write.csv(SEM_div_all, "SEM_alldata.csv")
lawn<-SEM_div_all%>%
  na.omit()#not all yards had lawns

write.csv(lawn, "SEM_lawn_data.csv")
