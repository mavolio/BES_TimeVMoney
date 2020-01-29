setwd('C:\\Users\\megha\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')
 setwd('C:\\Users\\mavolio2\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')

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
invasive<-read.csv("Invasives_Balt18_cleaned.csv")


###same thing but no F/B
##lawns
lawn2<-lawn%>%
  select(NB, House, Species.combined, F1, F2, B1, B2)%>%
  unique()%>%
  filter(Species.combined!="Dead grass"&Species.combined!="No Lawn")%>%
  mutate(Ave = (F1+ F2 + B1+B2)/4)%>%
  select(-F1, -F2, -B1, -B2)%>%
  mutate(homeid_loc=paste(NB, House, sep="_"))


lawn_rich<-community_structure(lawn2, abundance.var="Ave", replicate.var = "homeid_loc")%>%
  separate(homeid_loc, into=c("NB", "House"))%>%
  mutate(Nb=as.integer(NB),
         House=as.integer(House),
         l.leven=log(Evar))%>%
  select(-NB)

lawn_analysis<-lawn_rich%>%
  rename(l.rich=richness,
         leven=l.leven)


#looking into FB and number of trees
notrees_stem<-trees%>%
  filter(Tree.species=="no trees")%>%
  mutate(num.trees=0)%>%
  group_by(NB, House)%>%
  summarise(n=length(num.trees))%>%
  filter(n==2)%>%
  mutate(num.trees=0)%>%
  select(-n)

stems2<-trees%>%
  filter(Tree.species!="no trees")%>%
  group_by(NB, House, Tree.species)%>%
  summarize(present=length(DBH1))%>%
  mutate(homeid=paste(NB, House, sep="_"))%>%
  group_by(NB, House)%>%
  summarise(num.trees=sum(present))%>%
  bind_rows(notrees_stem)%>%
  rename(Nb=NB)%>%
  mutate(l.num.trees=log(num.trees+1))


##tree richness
notrees_rich<-trees%>%
  filter(Tree.species=="no trees")%>%
  mutate(richness=0)%>%
  select(NB, House, richness)%>%
  rename(Nb=NB)%>%
  group_by(Nb, House)%>%
  summarise(n=length(richness))%>%
  filter(n==2)%>%
  mutate(richness=0)%>%
  select(-n)

trees2<-trees%>%
  filter(Tree.species!="no trees")%>%
  mutate(homeid=paste(NB, House, sep="::"))%>%
  group_by(homeid, Tree.species)%>%
  summarize(abund=length(Tree.species))

tree_rich<-community_structure(trees2, abundance.var="abund", replicate.var="homeid")%>%
  separate(homeid, into=c("NB", "House"))%>%
  select(-Evar)%>%
  mutate(Nb=as.integer(NB), House=as.integer(House))%>%
  select(-NB)%>%
  bind_rows(notrees_rich)%>%
  mutate(t.lrich=log(richness+1))%>%
  rename(t.rich=richness)

###tree DBH
notrees_dhb<-trees%>%
  filter(Tree.species=="no trees")%>%
  mutate(DBH=0)%>%
  select(NB, House, DBH, Tree.species)%>%
  group_by(NB, House)%>%
  summarise(n=length(DBH))%>%
  filter(n==2)%>%
  mutate(DBH=0)%>%
  select(-n)%>%
  mutate(Tree.species="no trees")

dbh_onetrunk<-trees%>%
  filter(is.na(DBH2), !is.na(DBH1))%>%
  rename(DBH=DBH1)%>%
  select(NB, House, Tree.species, DBH)

dbh_multiple<-trees%>%
  filter(!is.na(DBH2))%>%
  group_by(NB, House, Tree.species)%>%
  select(-notes, -dbh.unit)%>%
  mutate(rep=rank(DBH1, ties.method = "random"))%>%
  group_by(NB, House, Tree.species, rep)%>%
  gather(trunk, measure, DBH1:DBH6)%>%
  na.omit()%>%
  mutate(square=measure*measure)%>%
  ungroup()%>%
  group_by(NB, House, Tree.species, rep)%>%
  summarize(DBHsum=sum(square))%>%
  mutate(DBH=sqrt(DBHsum))%>%
  select(-DBHsum, -rep)

DBHdata<-dbh_multiple%>%
  bind_rows(dbh_onetrunk)%>%
  bind_rows(notrees_dhb)%>%
  group_by(NB, House)%>%
  summarise(DBH=sum(DBH))%>%
  rename(Nb=NB)%>%
  mutate(ldbh=log(DBH+1))

##floral
floral2<-floral%>% 
  filter(Flower.Width..cm.!="", Genus!="NoFlowers")%>%
  mutate(numflowers=X.F.stems*ave_flower_perstem,
         area=as.numeric(as.character(Flower.Width..cm.))*as.numeric(as.character(Flower.Length..cm.))*numflowers)%>%
  group_by(House_ID, Genus)%>%
  summarize(nplants=sum(X..F.plants),
            nflowers=sum(numflowers),
            areatot=sum(area))

noflowers_num<-floral%>%
  filter(Genus=="NoFlowers")%>%
  mutate(nplants=0,
         nflowers=0,
         areatot=0)%>%
  select(House_ID, nplants, nflowers, areatot)%>%
  group_by(House_ID)%>%
  summarise(n=length(nplants))%>%
  filter(n==2)%>%
  mutate(nplants=0,
         nflowers=0,
         areatot=0)%>%
  select(-n)

flowernum<-floral2%>%
  group_by(House_ID)%>%
  summarise(nplants=sum(nplants),
            nflowers=sum(nflowers),
            areatot=sum(areatot))%>%
  bind_rows(noflowers_num)%>%
  separate(House_ID, into=c("Nb", "House"))%>%
  mutate(Nb=as.integer(Nb),
         House=as.integer(House))%>%
  mutate(lnumplants=log(nplants+1),
         lnumflowers=log(nflowers+1),
         larea=log(areatot+1))

###floral richness
noflowers_rich<-floral%>%
  filter(Genus=="NoFlowers")%>%
  mutate(richness=0, Evar=NA)%>%
  group_by(House_ID)%>%
  summarize(n=length(richness))%>%
  filter(n==2)%>%
  mutate(richness=0, Evar=NA)%>%
  select(House_ID,richness, Evar)%>%
  separate(House_ID, into = c("NB", "House"))

flower_rich<-community_structure(floral2, abundance.var="nplants", replicate.var="House_ID")%>%
  separate(House_ID, into=c("NB", "House"))%>%
  bind_rows(noflowers_rich)%>%
  select(-Evar)%>%
  mutate(Nb=as.integer(NB), House=as.integer(House))%>%
  mutate(f.lrich=log(richness+1))%>%
  rename(f.rich=richness)%>%
  select(-NB)

color<-floral%>%
  mutate(fl.color=paste(color1, color2, sep=""))%>%
  filter(fl.color!="NoFlowers")%>%
  group_by(House_ID, fl.color)%>%
  summarize(fl.colors=length(fl.color))

nocolor<-floral%>%
  filter(Genus=="NoFlowers")%>%
  mutate(richness=0)%>%
  separate(House_ID, into = c("NB","House"))%>%
  group_by(NB, House)%>%
  summarize(n=length(richness))%>%
  filter(n==2)%>%
  mutate(richness=0)%>%
  select(-n)

color_rich<-community_structure(color, abundance.var="fl.colors", replicate.var="House_ID")%>%
  separate(House_ID, into=c("NB", "House"))%>%
  bind_rows(nocolor)%>%
  select(-Evar)%>%
  mutate(Nb=as.integer(NB), House=as.integer(House))%>%
  mutate(c.lrich=log(richness+1))%>%
  rename(c.rich=richness)%>%
  select(-NB)


##invasives
yardarea<-read.csv("YardArea.csv")%>%
  rename(homeid=House_ID)%>%
  mutate(homeid=as.character(homeid))

noinv<-invasive%>%
  rename(Nb=NB)%>%
  right_join(yardarea)%>%
  filter(is.na(Species.Latin))%>%
  mutate(richness=0)%>%
  select(Nb, House, richness)

invasive2<-invasive%>%
  filter(Species.Latin!="none"&Notes!="not on our list")%>%
  mutate(homeid=paste(NB, House, sep="::"),
         present=1)

inv_rich<-community_structure(invasive2, abundance.var="present", replicate.var="homeid")%>%
  separate(homeid, into=c("Nb", "House"))%>%
  select(-Evar)%>%
  mutate(Nb=as.integer(Nb), House=as.integer(House))%>%
  bind_rows(noinv)%>%
  mutate(i.lrich=log(richness+1))%>%
  rename(i.rich=richness)


alldiv2<-lawn_analysis%>%
  full_join(tree_rich)%>%
  full_join(stems2)%>%
  full_join(DBHdata)%>%
  full_join(inv_rich)%>%
  full_join(flower_rich)%>%
  full_join(color_rich)%>%
  full_join(flowernum)

#survey
survey1<-survey%>%
  separate(House_ID, into=c("Nb","House"), sep="_")%>%
  mutate(Nb=as.integer(Nb), House=as.integer(as.character(House)))%>%
  left_join(NB)

timespent<-survey1%>%
  select(Nb, House, Style, money, C101.1)%>%
  filter(C101.1!="No answer")%>%
  mutate(C101.1=as.numeric(as.character(C101.1)))

summary(aov(C101.1~Style*money, data=timespent))

avetime<-timespent%>%
  group_by(Nb)%>%
  summarize(time=mean(C101.1))

resident<-survey1%>%
  select(Nb, Style, House, money, D2)%>%
  filter(D2!="No answer")%>%
  mutate(D2=as.numeric(as.character(D2)))

averes<-resident%>%
  group_by(Nb)%>%
  summarize(time=mean(D2))

paywork<-survey1%>%
  mutate(pay=ifelse(C104==1|C203==1|C5==1, 1, 0))%>%
  select(Nb, House, pay)

survey2<-timespent%>%
  left_join(resident)%>%
  left_join(paywork)


####1. Linking lawn care to lawn richness

lawnmanage<-lawn_rich%>%
  left_join(survey1)%>%
  select(Nb, House, Style, money, richness, C701, C702, C5, C601, C602, C801, C802)

herbpest<-lawnmanage%>%
  filter(C701!="No answer")%>%
  group_by(C701)%>%
  summarise(rich=mean(richness), 
            sd=sd(richness),
            n=length(richness))%>%
  mutate(se=sd/sqrt(n),
         manage="Herb-Pesticide")%>%
  rename(Q=C701)

fertilize<-lawnmanage%>%
  filter(C601!="No answer")%>%
  group_by(C601)%>%
  summarise(rich=mean(richness), 
            sd=sd(richness),
            n=length(richness))%>%
  mutate(se=sd/sqrt(n),
         manage="Fertilize")%>%
  rename(Q=C601)

rake<-lawnmanage%>%
  filter(C801!="No answer")%>%
  group_by(C801)%>%
  summarise(rich=mean(richness), 
            sd=sd(richness),
            n=length(richness))%>%
  mutate(se=sd/sqrt(n),
         manage="Rake")%>%
  rename(Q=C801)

lmanage<-herbpest%>%
  bind_rows(fertilize)%>%
  bind_rows(rake)

ggplot(data=lmanage, aes(x=Q, y = rich))+
  geom_bar(stat="identity", position=position_dodge(0.9), fill="darkslateblue")+
  geom_errorbar(aes(ymin=rich-se, ymax=rich+se), position = position_dodge(0.9), size=1, width = 0.5)+
  ylab("Lawn Richness")+
  xlab ("Do you?")+
  facet_wrap(~manage)


lawncover<-lawn%>%
  select(NB, House, Species.combined, F1, F2, B1, B2, N.fixer)%>%
  unique()%>%
  filter(Species.combined!="Dead grass"&Species.combined!="No Lawn")%>%
  mutate(cover = (F1+ F2 + B1 + B2)/4)%>%
  select(-F1, -F2, -B1, -B2)%>%
  na.omit()%>%
  group_by(NB, House, N.fixer)%>%
  summarize(cover=mean(cover, rm.na=T))%>%
  filter(N.fixer==1)%>%
  rename(Nb=NB)%>%
  left_join(survey1)


fertilize_nfix<-lawncover%>%
  filter(C601!="No answer")%>%
  group_by(C601)%>%
  summarise(mcov=mean(cover), 
            sd=sd(cover),
            n=length(cover))%>%
  mutate(se=sd/sqrt(n))

fertilize_nfixs<-lawncover%>%
  filter(C601!="No answer")

t.test(cover~C601, data=fertilize_nfixs)

ggplot(data=fertilize_nfix, aes(x=C601, y = mcov))+
  geom_bar(stat="identity", position=position_dodge(0.9), fill="darkslate gray")+
  geom_errorbar(aes(ymin=mcov-se, ymax=mcov+se), position = position_dodge(0.9), size=1, width=0.5)+
  ylab("Cover N-Fixers")+
  xlab ("Do you Fertilize?")

fertilize_nfix<-lawncover%>%
  filter(C602!="No answer")%>%
  group_by(C601)%>%
  summarise(mcov=mean(cover), 
            sd=sd(cover),
            n=length(cover))%>%
  mutate(se=sd/sqrt(n))


###linking time spent to floral diversity
floraldata<-color_rich%>%
  left_join(flowernum)%>%
  left_join(flower_rich)%>%
  left_join(survey1)%>%
  select(Nb, House, Style, money, lrich, lnumplants, lnumflowers, larea, lcrich, C101.1)%>%
  filter(C101.1!="No answer"&C101.1!=20)%>%
  mutate(C101.1=as.numeric(as.character(C101.1)))

tograph_cor<-floraldata%>%
  gather(diversity, value, lrich:lcrich)%>%
  mutate(div_group=factor(diversity, levels = c("lnumplants", "lnumflowers","larea","lcrich", "lrich")))

rvalues <- tograph_cor %>% 
  group_by(div_group) %>%
  summarize(r.value = round((cor.test(value, C101.1)$estimate), digits=3),
            p.value = (cor.test(value, C101.1)$p.value))

div<-c(
  lnumplants = "Log(Num. Flowering Plants)",
  lnumflowers = "Log(Num. Flowers)",
  larea = "Log(Floral Area)",
  lcrich = "Log(Num. Colors)",
  lrich = "Log(Floral Richness)"
)


ggplot(data=tograph_cor, aes(x = C101.1, y = value))+
  geom_point(size=1, position="jitter")+
  #scale_color_manual(name="", values=c("darkgray", 'blue'))+
  geom_smooth(data=subset(tograph_cor, div_group=="lnumplants"), method="lm", se=F, color = "red")+
  geom_smooth(data=subset(tograph_cor, div_group=="lrich"), method="lm", se=F, color = "red")+
  geom_smooth(data=subset(tograph_cor, div_group=="lcrich"), method="lm", se=F, color = "red")+
  facet_wrap(~div_group, scales="free", labeller=labeller(div_group = div), ncol=3)+
  xlab("Time Spent (hours/week)")+
  ylab("Value")


####linking desire to variety to variety
floraldata2<-color_rich%>%
  left_join(flowernum)%>%
  left_join(flower_rich)%>%
  left_join(survey1)%>%
  select(Nb, House, Style, money, lrich, lnumplants, lnumflowers, larea, lcrich, A5, A6, A702, A703, A708, A710)%>%
  filter(A5!="No answer"&A6!="No answer"&A702!="No answer"&A703!="No answer"&A708!="No answer"&A710!="No answer")%>%
  mutate(A5=as.numeric(as.character(A5)),
         A6=as.numeric(as.character(A6)),
         A702=as.numeric(as.character(A702)),
         A703=as.numeric(as.character(A703)),
         A708=as.numeric(as.character(A708)),
         A710=as.numeric(as.character(A710)))

#want biodiversity have more biodiversity
with(floraldata2, cor.test(lrich, A6))
#want biodiversity have more biodiversity
with(floraldata2, cor.test(lrich, A710))

p1<-
ggplot(data=floraldata2, aes(x = A6, y = lrich))+
  geom_point(position = position_jitter(0.1))+
  geom_smooth(method="lm", color="red", se=F)+
  ylab("Log(Floral Richness)")+
  xlab("Importance of Biodiversity")

#want color have more biodiversity
with(floraldata2, cor.test(lcrich, A702))
p2<-
ggplot(data=floraldata2, aes(x = A702, y = lcrich))+
  geom_point(position=position_jitter(0.1))+
  geom_smooth(method="lm", color="red", se=F)+
  ylab("Log(Num. Colors)")+
  xlab("Importance of Color Variety")

treerich2<-tree_rich%>%
  left_join(survey1)%>%
  select(Nb, House, Style, money, lrich, A6, A709)%>%
  filter(A6!="No answer"&A709!="No answer")%>%
  mutate(A6=as.numeric(as.character(A6)),
         A709=as.numeric(as.character(A709)))

#want biodiversity not in trees
with(treerich2, cor.test(lrich, A6))
#want tree div have more biodiversity
with(treerich2, cor.test(lrich, A709))

p3<-
  ggplot(data=treerich2, aes(x = A709, y = lrich))+
    geom_point(position=position_jitter(0.1))+
    geom_smooth(method="lm", color="red", se=F)+
    ylab("Log(Tree Sp. Richness)")+
    xlab("Importance of Tree Sp. Variety")
grid.arrange(p1, p2, p3, ncol=2)


###preference for tree attributes
DBHdata2<-DBHdata%>%
  left_join(survey1)%>%
  select(Nb, House, Style, money, ldbh, B101, B801)%>%
  filter(B101!="No answer"&B801!="No answer")%>%
  mutate(B101=as.numeric(as.character(B101)),
         B801=as.numeric(as.character(B801)))

with(DBHdata2, cor.test(ldbh, B101))
#want tree div have more biodiversity
with(DBHdata2, cor.test(ldbh, B801))

p5<-
  ggplot(data=DBHdata2, aes(x = B101, y = ldbh))+
  geom_point()+
  ylab("Log(Tree DBH)")+
  xlab("Importance Shade Provision")

p6<-
  ggplot(data=DBHdata2, aes(x = B801, y = ldbh))+
  geom_point()+
  ylab("Log(Tree DBH)")+
  xlab("Importance Planting Shade Trees")
grid.arrange(p5, p6, ncol=2)

alldiv3<-alldiv2%>%
  left_join(survey2)

with(alldiv3, cor.test(c.lrich, C101.1))#num colors
with(alldiv3, cor.test(f.lrich, C101.1))#num genera
with(alldiv3, cor.test(lnumplants, C101.1))
with(alldiv3, cor.test(lnumflowers, C101.1))
with(alldiv3, cor.test(larea, C101.1))
with(alldiv3, cor.test(l.rich, C101.1))#lawn rich
with(alldiv3, cor.test(i.lrich, C101.1))#inv sp
with(alldiv3, cor.test(t.lrich, C101.1))#tree rich
with(alldiv3, cor.test(l.num.trees, C101.1))#num trees

with(alldiv3, cor.test(c.lrich, D2))#num colors
with(alldiv3, cor.test(f.lrich, D2))#num genera
with(alldiv3, cor.test(lnumplants, D2))
with(alldiv3, cor.test(lnumflowers, D2))
with(alldiv3, cor.test(larea, D2))
with(alldiv3, cor.test(l.rich, D2))#lawn rich
with(alldiv3, cor.test(i.lrich, D2))#inv sp
with(alldiv3, cor.test(t.lrich, D2))#tree rich
with(alldiv3, cor.test(l.num.trees, D2))#num trees

with(alldiv3, cor.test(c.lrich, pay))#num colors
with(alldiv3, cor.test(f.lrich, pay))#num genera
with(alldiv3, cor.test(lnumplants, pay))
with(alldiv3, cor.test(lnumflowers, pay))
with(alldiv3, cor.test(larea, pay))
with(alldiv3, cor.test(l.rich, pay))#lawn rich
with(alldiv3, cor.test(i.lrich, pay))#inv sp
with(alldiv3, cor.test(t.lrich, pay))#tree rich
with(alldiv3, cor.test(l.num.trees, pay))#num trees
