setwd('C:\\Users\\megha\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')
setwd('C:\\Users\\mavolio2\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')

#WD for Allie
setwd("C:\\Users\\ablanch4\\Dropbox\\Time V Money\\Data\\Cleaned Data\\")# put path here

library(tidyverse)
library(codyn)

NB<-read.csv("NB_Codes.csv")%>%
  select(Nb, Style, med_age, nb_inc, med_inc,size )%>%
  rename(money=nb_inc)
lawn<-read.csv("Lawn Quadrats_Balt18_AB.csv")
trees<-read.csv("Trees_Balt18_FB_clean.csv")
floral<-read.csv("Floral_Data_Balt18_clean_AB.csv")
survey<-read.csv("2018 Homeowner_Survey Data_081219.csv")%>%
  filter(House_ID!="")
yardarea<-read.csv("YardArea.csv")%>%
  rename(homeid=House_ID)%>%
  mutate(homeid=as.character(homeid))
invasive<-read.csv("Invasives_Balt18_cleaned.csv")
treetraits<-read.csv("BAL_treeTraits_AB.csv")%>%
  select(-Alternative.names, -On.CalPoly, -Notes)



# div yard with front/back ------------------------------------------------


##lawns
lawn2<-lawn%>%
  select(NB, House, Species.combined, F1, F2, B1, B2)%>%
  unique()%>%
  filter(Species.combined!="Dead grass"&Species.combined!="No Lawn")%>%
  mutate(Front = (F1+ F2)/2, Back = (B1+B2)/2)%>%
  select(-F1, -F2, -B1, -B2)%>%
  gather(Front.Back, Cover, Front:Back)%>%
  mutate(homeid_loc=paste(NB, House, Front.Back, sep="_"))


lawn_rich<-community_structure(lawn2, abundance.var="Cover", replicate.var = "homeid_loc")%>%
  separate(homeid_loc, into=c("NB", "House", "Front.Back"))%>%
  mutate(Nb=as.integer(NB),
         House=as.integer(House),
         l.leven=log(Evar))%>%
  select(-NB)

lawn_analysis<-lawn_rich%>%
 rename(l.rich=richness,
         leven=l.leven)

#looking into FB and number of trees
notrees_stemFB<-trees%>%
  filter(Tree.species=="no trees")%>%
  mutate(num.trees=0)%>%
  rename(loc=Front.Back)%>%
  mutate(Front.Back=ifelse(loc=="f", "Front", "Back"))%>%
  select(NB, House, num.trees, Front.Back)

stems2<-trees%>%
  filter(Tree.species!="no trees")%>%
  rename(loc=Front.Back)%>%
  mutate(Front.Back=ifelse(loc=="f", "Front", "Back"))%>%
  group_by(NB, House, Tree.species, Front.Back)%>%
  summarize(present=length(DBH1))%>%
  mutate(homeid=paste(NB, House, sep="_"))%>%
  group_by(NB, House, Front.Back)%>%
  summarise(num.trees=sum(present))%>%
  bind_rows(notrees_stemFB)%>%
  rename(Nb=NB)%>%
  mutate(l.num.trees=log(num.trees+1))
  

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

tree_rich<-community_structure(trees2, abundance.var="abund", replicate.var="homeid")%>%
  separate(homeid, into=c("NB", "House", "Front.Back"))%>%
  select(-Evar)%>%
  mutate(Nb=as.integer(NB), House=as.integer(House))%>%
  select(-NB)%>%
  bind_rows(notrees_rich)%>%
  mutate(t.lrich=log(richness+1))%>%
  rename(t.rich=richness)%>%
  rename(loc=Front.Back)%>%
  mutate(Front.Back=ifelse(loc=="f", "Front", "Back"))%>%
  select(-loc)

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
  mutate(ldbh=log(DBH+1))%>%
  rename(loc=Front.Back)%>%
  mutate(Front.Back=ifelse(loc=="f", "Front", "Back"))%>%
  select(-loc)


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
  mutate(lnumplants=log(nplants+1),
         lnumflowers=log(nflowers+1),
         larea=log(areatot+1))

###floral richness
noflowers_rich<-floral%>%
  filter(Genus=="NoFlowers")%>%
  mutate(richness=0, Evar=NA)%>%
  select(House_ID,richness, Evar, Front.Back)%>%
  separate(House_ID, into = c("NB", "House"))

floral3<-floral2%>%
  mutate(homeid=paste(House_ID, Front.Back, sep="_"))

flower_rich<-community_structure(floral3, abundance.var="nplants", replicate.var="homeid")%>%
 separate(homeid, into=c("NB", "House", "Front.Back"))%>%
  bind_rows(noflowers_rich)%>%
  select(-Evar)%>%
  mutate(Nb=as.integer(NB), House=as.integer(House))%>%
  mutate(f.lrich=log(richness+1))%>%
  rename(f.rich=richness)%>%
  select(-NB)

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
  mutate(c.lrich=log(richness+1))%>%
  rename(c.rich=richness)%>%
  select(-NB)


alldiv<-color_rich%>%
  left_join(flowernum)%>%
  left_join(stems2)%>%
  left_join(DBHdata)%>%
  left_join(tree_rich)%>%
  left_join(flower_rich)%>%
  full_join(lawn_analysis)%>%
  select(-NB)%>%
  unique()

write.csv(alldiv, "All_Front_Back_Diversity.csv", row.names = F)


# No Front Back -----------------------------------------------------------


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

#tree traits

notrees_traits<-trees%>%
  filter(Tree.species=="no trees")%>%
  mutate(num.trees=0)%>%
  group_by(NB, House)%>%
  summarise(n=length(num.trees))%>%
  filter(n==2)%>%
  select(-n)%>%
  mutate(shade=0,
         flower=0,
         native=0,
         fruit=0,
         fallcolor=0,
         fruitcontrast=0,
         litter=0)%>%
  rename(Nb=NB)


ttraits<-trees%>%
  rename(genus.species=Tree.species, Nb=NB)%>%
  filter(genus.species!="no trees")%>%
  group_by(Nb, House, genus.species)%>%
  summarize(abund=length(genus.species))%>%
  left_join(treetraits)%>%
  mutate(shade=ifelse(shading_potential==3, 1, 0),
         flower=ifelse(flower_showy==3, 1, 0))%>%
  mutate(numshade=abund*shade,
         numflower=abund*flower,
         numnative=abund*native_MD,
         numfruit=abund*fruit_edible,
         numfall=abund*fall_color,
         numcontrast=abund*fruit_contrast,
         numlitter=abund*litter)%>%
  group_by(Nb, House)%>%
  summarize(shading=sum(numshade, na.rm = T),
            flowers=sum(numflower, na.rm = T),
            natives=sum(numnative, na.rm = T),
            fruits=sum(numfruit, na.rm = T),
            fallc=sum(numfall, na.rm = T),
            contrast=sum(numcontrast, na.rm = T),
            litters=sum(litter, na.rm = T))%>%
  left_join(stems2)%>%
  select(-l.num.trees)%>%
  mutate(shade=shading/num.trees,
         flower=flowers/num.trees,
         native=natives/num.trees,
         fruit=fruits/num.trees,
         fallcolor=fallc/num.trees,
         fruitcontrast=contrast/num.trees,
         litter=litters/num.trees)%>%
  select(Nb, House, shade, flower, native, fruit, fallcolor, fruitcontrast, litter)%>%
  bind_rows(notrees_traits)


alldiv2<-lawn_analysis%>%
  full_join(tree_rich)%>%
  full_join(stems2)%>%
  full_join(DBHdata)%>%
  full_join(inv_rich)%>%
  full_join(flower_rich)%>%
  full_join(color_rich)%>%
  full_join(flowernum)%>%
  full_join(ttraits)

#export invasive alone b/c only at house level
write.csv(alldiv2, "All_Diversity_House.csv", row.names=F)
