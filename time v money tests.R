setwd('C:\\Users\\megha\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')
setwd('~/Dropbox/BES Research/Time V Money/Data/Cleaned Data')

#WD for Allie
setwd("C:\\Users\\ablanch4\\Dropbox\\Time V Money\\Data\\Cleaned Data\\")# put path here

library(tidyverse)
library(codyn)
library(gridExtra)
theme_set(theme_bw(20))

NB<-read.csv("NB_Codes.csv")%>%
  select(Nb, Style, med_age, nb_inc, med_inc,size )%>%
  rename(money=nb_inc)
lawn<-read.csv("Lawn Quadrats_Balt18.csv")
trees<-read.csv("Trees_Balt18_FB_clean.csv")
floral<-read.csv("Floral_Data_Balt18_clean.csv")
survey<-read.csv("2018 Homeowner_Survey Data_081219.csv")%>%
  filter(House_ID!="")
yardarea<-read.csv("YardArea.csv")%>%
  rename(homeid=House_ID)%>%
  mutate(homeid=as.character(homeid))

#testing life stage categories
ls<-yardarea%>%
  left_join(NB)

summary(aov(pct_65_older~Style*money, data=ls))
summary(aov(MEDHINC_CY~Style*money, data=ls))


ls_toplot_style<-ls%>%
  group_by(Style)%>%
  summarise(retired=mean(pct_65_older),
            sdr=sd(pct_65_older),
            n=length(pct_65_older))%>%
  mutate(ser=sdr/sqrt(n))
      
         
ggplot(data=ls_toplot_style, aes(x = Style, y = retired))+
  geom_bar(stat = "identity", position = position_dodge(0.9), fill="darkorange3")+
  geom_errorbar(aes(ymin=retired-ser, ymax=retired+ser),
              size=1, width=0.5, position=position_dodge(.9))+
  ylab("% 65 or older")+
  xlab("Life Stage")+
  scale_x_discrete(labels=c("Middle Ground", "Senior Styles"))

ls_toplot_money<-ls%>%
  group_by(money)%>%
  summarise(inc=mean(MEDHINC_CY),
            sdm=sd(MEDHINC_CY),
            n=length(MEDHINC_CY))%>%
  mutate(sem=sdm/sqrt(n))

ggplot(data=ls_toplot_money, aes(x = money, y = inc))+
  geom_bar(stat = "identity", position = position_dodge(0.9),fill="dodgerblue4")+
  geom_errorbar(aes(ymin=inc-sem, ymax=inc+sem),
              position=position_dodge(.9), size=1, width = 0.5)+
  ylab("Median Income")+
  xlab("Income")+
  scale_x_discrete(labels=c("Middle", "High"), limits=c("mid", "high"))

##lawns
lawn2<-lawn%>%
  select(NB, House, Species.combined, F1, F2, B1, B2)%>%
  unique()%>%
  filter(Species.combined!="Dead grass"&Species.combined!="No Lawn")%>%
  mutate(Front = (F1+ F2)/2, Back = (B1+B2)/2)%>%
  select(-F1, -F2, -B1, -B2)%>%
  gather(Location, Cover, Front:Back)%>%
  mutate(homeid_loc=paste(NB, House, Location, sep="_"))


lawn_rich<-community_structure(lawn2, abundance.var="Cover", replicate.var = "homeid_loc")%>%
  separate(homeid_loc, into=c("NB", "House", "Location"))%>%
  mutate(Nb=as.integer(NB),
         House=as.integer(House),
         leven=log(Evar))

lawn_analysis<-lawn_rich%>%
  left_join(yardarea)%>%
  left_join(NB)

hist(lawn_rich$richness)
hist(log(lawn_rich$Evar))

#how many species?
length(unique(lawn2$Species.combined))#subtract 10 for unknowns

#ANCOVA
summary(aov(richness~Style*money*Location+yard_area, data=lawn_analysis))
#nothing with richness

summary(aov(leven~Style*money*Location+yard_area, data=lawn_analysis))

#interaction with money and location for evenness

lawn_toplot<-lawn_analysis%>%
  summarise(even=mean(Evar,na.rm = T),
            sd=sd(Evar, na.rm = T),
            n=length(even))%>%
  mutate(se=sd/sqrt(n))

ggplot(data=lawn_toplot, aes(x = 0.5, y = even))+
  geom_bar(stat = "identity", position = position_dodge(0.9), fill="green4")+
  geom_errorbar(aes(ymin=even-se, ymax=even+se),
                width=.5, size=1,  position=position_dodge(.9))+
  ylab("Lawn Evenness")+
  xlab("")+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

lawn_toplotrich<-lawn_analysis%>%
  summarise(rich=mean(richness,na.rm = T),
            sd=sd(richness, na.rm = T),
            n=length(richness))%>%
  mutate(se=sd/sqrt(n))

ggplot(data=lawn_toplotrich, aes(x=0.5, y = rich))+
  geom_bar(stat = "identity", position = position_dodge(0.9), fill="skyblue")+
  geom_errorbar(aes(ymin=rich-se, ymax=rich+se),
                width=.5, size=1,  position=position_dodge(.9))+
  ylab("Lawn Richness")+
  xlab("")+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


#trees
#this is not working with FB data in there
# notrees_stem<-trees%>%
#   filter(Tree.species=="no trees")%>%
#   mutate(number=0)%>%
#   select(NB, House, number)%>%
#   unique()


# this is not workign with FB
# stems<-trees2%>%
#   group_by(NB, House)%>%
#   summarise(number=sum(present))%>%
#   bind_rows(notrees_stem)%>%
#   rename(Nb=NB)%>%
#   left_join(yardarea)%>%
#   mutate(stemarea=(number/yard_area),
#          lstemarea=log(stemarea+1),
#          lstems=log(number+1))


#looking into FB and number of trees
notrees_stemFB<-trees%>%
  filter(Tree.species=="no trees")%>%
  mutate(number=0)%>%
  select(NB, House, number, Front.Back)

stems2<-trees%>%
  filter(Tree.species!="no trees")%>%
  group_by(NB, House, Tree.species, Front.Back)%>%
  summarize(present=length(DBH1))%>%
  mutate(homeid=paste(NB, House, sep="_"))%>%
  group_by(NB, House, Front.Back)%>%
  summarise(number=sum(present))%>%
  bind_rows(notrees_stemFB)%>%
  rename(Nb=NB)%>%
  left_join(yardarea)%>%
  left_join(NB)%>%
  mutate(lstems=log(number+1))

summary(aov(lstems~Style*money*Front.Back+yard_area, data=stems2))
#effect of money, yard area and front/back, but no interactions.

ggplot(data=stems2, aes(x=yard_area, y = lstems, color=money))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("Middle", "High"), limits=c("mid", "high"))+
  ylab("Log(Num. Trees)")+
  xlab("Yard Area")

p1<-ggplot(data=stems2, aes(x=yard_area, y = number, color=Front.Back))+
  geom_point(size=5, alpha=0.5)+
  geom_smooth(method = "lm", se=T, size = 2, aes(color=Front.Back))+
  scale_color_manual(name="Location", values=c("firebrick", "goldenrod"), labels=c("Back", "Front"))+
  ylab("Num. Trees")+
  xlab("Yard Area")
p2<-ggplot(data=stems2, aes(x=yard_area, y = lstems, color=Front.Back))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Location", values=c("firebrick", "goldenrod"), labels=c("Back", "Front"))+
  ylab("Log(Num. Trees)")+
  xlab("Yard Area")
p3<-ggplot(data=stems2, aes(x=yard_area, y = number, color=Front.Back))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Location", values=c("firebrick", "goldenrod"), labels=c("Back", "Front"))+
  ylab("Log(Num. Trees)")+
  xlab("Yard Area")+
  scale_y_continuous(trans="log10")

grid.arrange(p1,p2,p3)

##tree richness
notrees_rich<-trees%>%
  filter(Tree.species=="no trees")%>%
  mutate(richness=0)%>%
  select(NB, House, richness, Front.Back)%>%
  rename(Nb=NB)

trees2<-trees%>%
  filter(Tree.species!="no trees")%>%
  mutate(homeid=paste(NB, House, Front.Back, sep="::"))%>%
  group_by(homeid, Tree.species)%>%
  summarize(abund=length(Tree.species))

length(unique(trees2$Tree.species))#subtract 6 for unknowns

tree_rich<-community_structure(trees2, abundance.var="abund", replicate.var="homeid")%>%
  separate(homeid, into=c("NB", "House", "Front.Back"))%>%
  select(-Evar)%>%
  mutate(Nb=as.integer(NB), House=as.integer(House))%>%
  bind_rows(notrees_rich)%>%
  left_join(yardarea)%>%
  left_join(NB)%>%
  mutate(lrich=log(richness+1))

hist(tree_rich$lrich)

#ANCOVA
summary(aov(lrich~Style*money*Front.Back+yard_area, data=tree_rich))

ggplot(data=tree_rich, aes(x=yard_area, y = lrich, color=money))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("Middle", "High"), limits=c("mid","high"))+
  ylab("Log(Tree Richness)")+
  xlab("Yard Area")

ggplot(data=tree_rich, aes(x=yard_area, y = lrich, color=Front.Back))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Location", values=c("firebrick", "goldenrod"), labels=c("Back", "Front"))+
  ylab("Log(Tree Richness)")+
  xlab("Yard Area")

mostcommon<-trees2%>%
  ungroup%>%
  mutate(species=tolower(Tree.species))%>%
  group_by(species)%>%
  summarize(num=length(species))


###tree DBH
notrees_dhb<-trees%>%
  filter(Tree.species=="no trees")%>%
  mutate(DBH=0)%>%
  select(NB, House, DBH, Tree.species, Front.Back)

dbh_onetrunk<-trees%>%
  filter(is.na(DBH2), !is.na(DBH1))%>%
  rename(DBH=DBH1)%>%
  select(NB, House, Front.Back, Tree.species, DBH)

dbh_multiple<-trees%>%
  filter(!is.na(DBH2))%>%
  group_by(NB, House, Front.Back, Tree.species)%>%
  select(-notes, -dbh.unit)%>%
  mutate(rep=rank(DBH1, ties.method = "random"))%>%
  group_by(NB, House, Front.Back, Tree.species, rep)%>%
  gather(trunk, measure, DBH1:DBH6)%>%
  na.omit()%>%
  mutate(square=measure*measure)%>%
  ungroup()%>%
  group_by(NB, House, Front.Back, Tree.species, rep)%>%
  summarize(DBHsum=sum(square))%>%
  mutate(DBH=sqrt(DBHsum))%>%
  select(-DBHsum, -rep)

DBHdata<-dbh_multiple%>%
  bind_rows(dbh_onetrunk)%>%
  bind_rows(notrees_dhb)%>%
  group_by(NB, House, Front.Back)%>%
  summarise(DBH=sum(DBH))%>%
  rename(Nb=NB)%>%
  left_join(yardarea)%>%
  left_join(NB)%>%
  mutate(ldbh=log(DBH+1))

hist(DBHdata$ldbh)

#ANCOVA
summary(aov(ldbh~Style*money*Front.Back+yard_area, data=DBHdata))

ggplot(data=DBHdata, aes(x=yard_area, y = ldbh, color=money))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("Middle", "High"), limits=c("mid", "high"))+
  ylab("Log(Tree DBH)")+
  xlab("Yard Area")

ggplot(data=DBHdata, aes(x=yard_area, y = ldbh, color=Front.Back))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Location", values=c("firebrick", "goldenrod"), labels=c("Back", "Front"))+
  ylab("Log(Tree DBH)")+
  xlab("Yard Area")

dbh_toplot<-DBHdata%>%
  group_by(Style, Front.Back)%>%
  summarise(mean=mean(ldbh,na.rm = T),
            sd=sd(ldbh, na.rm = T),
            n=length(ldbh))%>%
  mutate(se=sd/sqrt(n))

ggplot(data=dbh_toplot, aes(x = Style, y = mean, fill=Front.Back))+
  geom_bar(stat = "identity", position = position_dodge(0.9))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,  position=position_dodge(.9))+
  ylab("Log(Tree DBH)")+
  xlab("Life Stage")+
  scale_x_discrete(labels=c("Middle Ground", "Senior Styles"))+
  scale_fill_brewer("Location", palette="Dark2", labels=c("Back", "Front"))



##floral
floral2<-floral%>% 
  filter(Flower.Width..cm.!="", Genus!="NoFlowers")%>%
  mutate(numflowers=X.F.stems*ave_flower_perstem,
         area=as.numeric(as.character(Flower.Width..cm.))*as.numeric(as.character(Flower.Length..cm.))*numflowers)%>%
  group_by(House_ID, Genus, Front.Back)%>%
  summarize(nplants=sum(X..F.plants),
            nflowers=sum(numflowers),
            areatot=sum(area))

noflowers_num<-floral%>%
  filter(Genus=="NoFlowers")%>%
  mutate(nplants=0,
         nflowers=0,
         areatot=0)%>%
  select(House_ID, Front.Back, nplants, nflowers, areatot)

flowernum<-floral2%>%
  group_by(House_ID, Front.Back)%>%
  summarise(nplants=sum(nplants),
            nflowers=sum(nflowers),
            areatot=sum(areatot))%>%
  bind_rows(noflowers_num)%>%
  separate(House_ID, into=c("NB", "House"))%>%
  mutate(Nb=as.integer(NB),
         House=as.integer(House))%>%
  left_join(yardarea)%>%
  left_join(NB)%>%
  mutate(lnumplants=log(nplants+1),
         lnumflowers=log(nflowers+1),
         larea=log(areatot+1))

hist(flowernum$larea)

#Ancovas num flowers
summary(aov(lnumplants~Style*money*Front.Back+yard_area, data=flowernum))

ggplot(data=flowernum, aes(x=yard_area, y = lnumplants, color=money))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("Middle", "High"), limits=c("mid","high"))+
  ylab("Log(Num. Flowering Plants)")+
  xlab("Yard Area")


summary(aov(lnumflowers~Style*money*Front.Back+yard_area, data=flowernum))

ggplot(data=flowernum, aes(x=yard_area, y = lnumflowers, color=money))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("High", "Middle"))+
  ylab("Log(Num. Flowers)")+
  xlab("Yard Area")

ggplot(data=flowernum, aes(x=yard_area, y = lnumflowers, color=Style))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Life Stage", values=c("olivedrab", "navy"), labels=c("Middle Ground", "Senior Styles"))+
  ylab("Log(Num. Flowers)")+
  xlab("Yard Area")

summary(aov(larea~Style*money*Front.Back+yard_area, data=flowernum))

ggplot(data=flowernum, aes(x=yard_area, y = larea, color=money))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("High", "Middle"))+
  ylab("Log(Floral Area)")+
  xlab("Yard Area")

ggplot(data=flowernum, aes(x=yard_area, y = larea, color=Front.Back))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Location", values=c("firebrick", "goldenrod"), labels=c("Back", "Front"))+
  ylab("Log(Floral Area)")+
  xlab("Yard Area")


ggplot(data=flowernum, aes(x=yard_area, y = larea, color=Style))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Life Stage", values=c("olivedrab", "navy"), labels=c("Middle Ground", "Senior Styles"))+
  ylab("Log(Floral Area)")+
  xlab("Yard Area")

###floral richness
noflowers_rich<-floral%>%
  filter(Genus=="NoFlowers")%>%
  mutate(richness=0, Evar=NA)%>%
  select(House_ID,richness, Evar, Front.Back)%>%
  separate(House_ID, into = c("NB", "House"))

length(unique(floral2$Genus))#subtract 1 for unknown

floral3<-floral2%>%
  mutate(homeid=paste(House_ID, Front.Back, sep="_"))

flower_rich<-community_structure(floral3, abundance.var="nplants", replicate.var="homeid")%>%
 separate(homeid, into=c("NB", "House", "Front.Back"))%>%
  bind_rows(noflowers_rich)%>%
  select(-Evar)%>%
  mutate(Nb=as.integer(NB), House=as.integer(House))%>%
  left_join(yardarea)%>%
  left_join(NB)%>%
  mutate(lrich=log(richness+1))

hist(flower_rich$lrich)

#ancova
summary(aov(lrich~Style*money*Front.Back+yard_area, data=flower_rich))

ggplot(data=flower_rich, aes(x=yard_area, y = lrich, color=money))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("Middle", "High"), limits=c("mid", "high"))+
  ylab("Log(Floral Richness)")+
  xlab("Yard Area")


color<-floral%>%
  mutate(fl.color=paste(color1, color2, sep=""))%>%
  filter(fl.color!="NoFlowers")%>%
  group_by(House_ID, Front.Back, fl.color)%>%
  summarize(fl.colors=length(fl.color))%>%
  mutate(homeid=paste(House_ID, Front.Back, sep="_"))

nocolor<-floral%>%
  filter(Genus=="NoFlowers")%>%
  mutate(richness=0)%>%
  separate(House_ID, into = c("NB","House"))%>%
  select(NB, House, Front.Back, richness)

color_rich<-community_structure(color, abundance.var="fl.colors", replicate.var="homeid")%>%
  separate(homeid, into=c("NB", "House", "Front.Back"))%>%
  bind_rows(nocolor)%>%
  select(-Evar)%>%
  mutate(Nb=as.integer(NB), House=as.integer(House))%>%
  left_join(yardarea)%>%
  left_join(NB)%>%
  mutate(lrich=log(richness+1))

hist(color_rich$lrich)

#ancova
summary(aov(lrich~Style*money*Front.Back+yard_area, data=color_rich))

ggplot(data=color_rich, aes(x=yard_area, y = lrich, color=money))+
  geom_point(size=5)+
  geom_smooth(method = "lm", se=F, size = 2)+
  scale_color_manual(name="Location", values=c("green4", "deepskyblue"), labels=c("High", "Middle"))+
  ylab("Log(Num. Colors)")+
  xlab("Yard Area")



#survey
survey1<-survey%>%
  separate(House_ID, into=c("Nb","House"), sep="_")%>%
  mutate(Nb=as.integer(Nb))%>%
  left_join(NB)

satisfied<-survey1%>%
  filter(A1!="No answer")%>%
  mutate(A1=as.numeric(as.character((A1))))

summary(aov(A1~Style*money, data=satisfied))

sat_toplot<-satisfied%>%
  group_by(Style, money)%>%
  summarize(mean=mean(A1),
            sd=sd(A1),
            n=length(A1))%>%
  mutate(se=sd/sqrt(n))

ggplot(data=sat_toplot, aes(x = Style, y = mean, fill=money))+
  geom_bar(stat = "identity", position = position_dodge(0.9))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,  position=position_dodge(.9))+
  ylab("Satisfied with Yard")+
  xlab("Life Stage")+
  scale_x_discrete(labels=c("Middle Ground", "Senior Styles"))+
  scale_fill_brewer("Income", labels=c("High", "Middle"), palette="Dark2")

sat_time<-survey1%>%
  filter(A204!="No answer")%>%
  mutate(A204=as.numeric(as.character((A204))))
summary(aov(A204~Style*money, data=sat_time))

sat_time_toplot<-sat_time%>%
  group_by(Style, money)%>%
  summarize(mean=mean(A204),
            sd=sd(A204),
            n=length(A204))%>%
  mutate(se=sd/sqrt(n))

ggplot(data=sat_time_toplot, aes(x = Style, y = mean, fill=money))+
  geom_bar(stat = "identity", position = position_dodge(0.9))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,  position=position_dodge(.9))+
  ylab("Too Much Time")+
  xlab("Life Stage")+
  scale_x_discrete(labels=c("Middle Ground", "Senior Styles"))+
  scale_fill_brewer("Income", labels=c("High", "Middle"), palette="Dark2")

sat_money<-survey1%>%
  select(A205, Style, money)%>%
  filter(A205!="No answer")%>%
  mutate(A205=as.numeric(as.character((A205))))

summary(aov(A205~Style*money, data=sat_money))

sat_money_toplot<-sat_money%>%
  group_by(Style, money)%>%
  summarize(mean=mean(A205),
            sd=sd(A205),
            n=length(A205))%>%
  mutate(se=sd/sqrt(n))

ggplot(data=sat_money_toplot, aes(x = Style, y = mean, fill=money))+
  geom_bar(stat = "identity", position = position_dodge(0.9))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,  position=position_dodge(.9))+
  ylab("Too Much Money")+
  xlab("Life Stage")+
  scale_x_discrete(labels=c("Middle Ground", "Senior Styles"))+
  scale_fill_brewer("Income", labels=c("High", "Middle"), palette="Dark2")

####what are people actually doing in their yards?
timespent<-survey1%>%
  select(Style, money, C101.1)%>%
  filter(C101.1!="No answer")%>%
  mutate(C101.1=as.numeric(as.character(C101.1)))

summary(aov(C101.1~Style*money, data=timespent))

ts_toplot<-timespent%>%
  group_by(money, Style)%>%
  summarize(mean=mean(C101.1, na.rm=T),
            sd=sd(C101.1),
            n=length(C101.1))%>%
  mutate(se=sd/sqrt(n),
         group=factor(money, levels=c("mid","high")))

ggplot(data=ts_toplot, aes(x = Style, y = mean, fill=group))+
  geom_bar(stat = "identity", position = position_dodge(0.9))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,  position=position_dodge(.9))+
  ylab("Time Spent (hours/week)")+
  xlab("Life Stage")+
  scale_x_discrete(labels=c("Middle Ground", "Senior Styles"))+
  scale_fill_brewer("Income", labels=c("Middle", "High"), palette="Dark2")

paywork<-survey1%>%
  mutate(pay=ifelse(C104==1|C203==1|C5==1, 1, 0))

summary(aov(pay~Style*money, data=paywork))

pay_toplot<-paywork%>%
  group_by(money, Style)%>%
  summarize(sum=sum(pay, na.rm=T),
            n=length(pay))%>%
  mutate(prop=(sum/n)*100,
         group=factor(money, levels=c("mid","high")))

ggplot(data=pay_toplot, aes(x = Style, y = prop, fill=group))+
  geom_bar(stat = "identity", position = position_dodge(0.9))+
  ylab("% Pay for Yard Work")+
  xlab("Life Stage")+
  scale_x_discrete(labels=c("Middle Ground", "Senior Styles"))+
  scale_fill_brewer("Income", labels=c("Middle", "High"), palette="Dark2")



###color bar chart
library(reshape2)

#get desired data
pie.test.1<-floral%>% 
  filter(Flower.Width..cm.!="", Genus!="NoFlowers")%>%
  mutate(numflowers=X.F.stems*ave_flower_perstem,
         area=as.numeric(as.character(Flower.Width..cm.))*as.numeric(as.character(Flower.Length..cm.))*numflowers)%>%
  group_by(House_ID, Genus, color1, color2)%>%
  summarize(nplants=sum(X..F.plants),
            nflowers=sum(numflowers),
            areatot=sum(area))%>%
  mutate(Color_area=(ifelse(color2!="", 0.5*areatot,1*areatot)))%>%
  separate(House_ID, into=c("Nb", "House"))%>%
  mutate(Nb=as.integer(Nb), House=as.integer(House))%>%
  left_join(NB)%>%
  ungroup()%>%
  select(Nb, House, color1, color2,Color_area, money)

#summarize per color per income type
pie.across<-melt(pie.test.1, id=c("Nb","House","Color_area","money"))%>%
  filter(value!=""&value!="brown")%>%
  group_by(value, money)%>%
  summarize(Sum_colarea=sum(Color_area, na.rm=T))

#ordering variables so legend is in rainbow color order
pie.across$value2<-factor(pie.across$value, levels = c("blue","purple-blue", "purple","red-purple", "red", "red-orange","orange","yellow-orange","yellow", "green-yellow","green","white"))

#graph!
ggplot(pie.across, aes(x = money, y=Sum_colarea/10000, fill=value2))+
  geom_bar(color="black", width = 0.75,stat="identity")+
  scale_x_discrete(limits=c("mid","high"), labels=c("Middle","High"))+
  scale_fill_manual(values=c("blue", "darkviolet", "purple","violetred3", "red","orangered1","orange","goldenrod1","yellow","yellowgreen","green4", "white"))+
  ylab("Color Area (m2)")+
  xlab("Income")+
  theme(axis.ticks=element_blank(),
        legend.title=element_blank(),
        panel.grid=element_blank(), legend.position = "none")
