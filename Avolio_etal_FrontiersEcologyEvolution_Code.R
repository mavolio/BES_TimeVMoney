setwd('C:\\Users\\megha\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')
setwd('C:\\Users\\mavolio2\\Dropbox\\BES Research\\Time V Money\\Data\\Cleaned Data')

#WD for Allie
setwd("C:\\Users\\ablanch4\\Dropbox\\Time V Money\\Data\\Cleaned Data\\")# put path here

library(tidyverse)
library(codyn)
library(gridExtra)
library(vegan)
library(gtable)
library(grid)
theme_set(theme_bw(12))


# read in data ------------------------------------------------------------

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

yardstats<-yardarea%>%
  group_by(Nb)%>%
  summarize(area=mean(yard_area, na.rm = T),
            back=mean(back_yard_area, na.rm = T),
            front=mean(front_yard_area, na.rm = T))%>%
  mutate(diff=front-back)



# Yard Management ANOVAs in text and Table 1 ------------------------------
#survey
survey1<-survey%>%
  separate(House_ID, into=c("Nb","House"), sep="_")%>%
  mutate(Nb=as.integer(Nb))%>%
  left_join(NB)

####what are people actually doing in their yards?
timespent<-survey1%>%
  select(Nb, House, Style, money, C101.1)%>%
  filter(C101.1!="No answer")%>%
  mutate(C101.1=as.numeric(as.character(C101.1)))

summary(aov(C101.1~Style*money, data=timespent))

avetime<-timespent%>%
  group_by(Nb)%>%
  summarize(time=mean(C101.1))


avetime<-timespent%>%
  group_by(Style, money)%>%
  summarize(time=mean(C101.1))


resident<-survey1%>%
  select(Nb, Style, House, money, D2)%>%
  filter(D2!="No answer")%>%
  mutate(D2=as.numeric(as.character(D2)))

averes<-resident%>%
  group_by(Nb)%>%
  summarize(time=mean(D2))

paywork<-survey1%>%
  mutate(pay=ifelse(C104==1|C203==1|C5==1, 1, 0))


paywork2<-survey1%>%
  mutate(C104.1=ifelse(C104==1, 1, 0), 
         C203.1=ifelse(C203==1, 1, 0), 
         C5.1=ifelse(C5==1, 1, 0),
         drop=ifelse(C104=="No answer"&C203=="No answer"&C5=="No answer", 1, 0))%>%
  mutate(pay=C104.1+C203.1+C5.1)%>%
  filter(drop!=1)

summary(aov(pay~Style*money, data=paywork2))

pay_nb<-paywork%>%
  group_by(Nb)%>%
  summarize(sum=sum(pay, na.rm=T),
            n=length(pay))%>%
  mutate(prop=(sum/n)*100)
# Rank abundance curves Figure 2 ------------------------------------------

###rank abundnace curves
lawnrank<-lawn2%>%
  group_by(NB, House, Species.combined)%>%
  summarize(meancov=mean(Cover))%>%
  ungroup()%>%
  group_by(Species.combined)%>%
  summarize(abund=sum(meancov), freq=length(Species.combined))%>%
  mutate(Frank=rank(-freq), Arank=rank(-abund), type="lawn")%>%
  rename(species=Species.combined)

treerank<-trees%>%
  filter(Tree.species!="no trees")%>%
  group_by(NB, House, Tree.species)%>%
  summarize(abund=length(DBH1))%>%
  ungroup()%>%
  group_by(Tree.species)%>%
  summarize(abund=sum(abund), freq=length(Tree.species))%>%
  mutate(Frank=rank(-freq), Arank=rank(-abund), type="tree")%>%
  rename(species=Tree.species)

floralrank<-floral2%>%
  group_by(House_ID, Genus)%>%
  summarize(totplants=sum(nplants))%>%
  ungroup%>%
  group_by(Genus)%>%
  summarize(abund=sum(totplants), freq=length(Genus))%>%
  mutate(Frank=rank(-freq), Arank=rank(-abund), type="floral")%>%
  rename(species=Genus)

all<-rbind(floralrank, treerank, lawnrank)

#write.csv(all, "rankcurves_raw.csv", row.names = F)
rank<-read.csv("rankcurves_clean.csv")

Frank<-rank%>%
  select(type, species, freq, Frank, native)%>%
  filter(Frank<11)

Arank<-rank%>%
  select(type, species, abund, Arank, native)%>%
  filter(Arank<11)

freq_flower<-
  ggplot(data=subset(Frank, type=="Flowering Plants"), aes(x=Frank, y=freq, color=as.factor(native)))+
  geom_point(size=3)+
  scale_color_manual(name="Native Status", label=c("Not Native", "Native"), values=c("black", "darkgray"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Rank")+
  ylab("Number of Yards Present")+
  annotate("text", x =2.8, y = 37, label='Hydrangea', size=3)+
  annotate("text", x =3, y = 31, label='Rosa', size=3)+
  annotate("text", x =4.4, y = 26, label='Rudbeckia', size=3)+
  annotate("text", x =5, y = 24, label='Hosta', size=3)+
  annotate("text", x =6.8, y = 15, label='Echinacea', size=3)+
  annotate("text", x =4.2, y = 13, label='Hemerocallis', size=3)+
  annotate("text", x =8.3, y = 13.5, label='Tagetes', size=3)+
  annotate("text", x =6.5, y = 11.5, label='Impatiens', size=3)+
  annotate("text", x =10, y = 12, label='Salvia', size=3)+
  annotate("text", x =8.8, y = 10, label='Buddleja', size=3)+
  annotate("text", x =Inf, y = Inf, label='C', size=3, hjust=1.5, vjust=1.5)+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  ggtitle("Flowering Plants")+
  theme(plot.title = element_text(size=10), axis.title=element_text(size=10))

freq_lawn <- 
  ggplot(data=subset(Frank, type=="Lawn"), aes(x=Frank, y=freq, color=as.factor(native)))+
  geom_point(size=3)+
  scale_color_manual(name="Native Status", label=c("Not Native", "Native"), values=c("black", "darkgray"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Rank")+
  ylab("Number of Yards Present")+
  annotate("text", x =3, y = 79, label='Poa pratensis', size=3)+
  annotate("text", x =4.2, y = 72, label='Festuca arund.', size=3)+
  annotate("text", x =5.2, y = 66, label='Trifolium repens', size=3)+
  annotate("text", x =6.5, y = 60, label='Cynodon dactylon', size=3)+
  annotate("text", x =3, y = 55, label='Digitaria sang.', size=3)+
  annotate("text", x =8.2, y = 55, label='Taraxacum off.', size=3)+
  annotate("text", x =5.2, y = 53, label='Oxalis stricta', size=3)+
  annotate("text", x =9.5, y = 52, label='Viola pap.', size=3)+
  annotate("text", x =7, y = 47, label='Plantago major', size=3)+
  annotate("text", x =8.8, y = 42, label='F. rubra', size=3)+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  annotate("text", x =Inf, y = Inf, label='A', size=3, hjust=1.5, vjust=1.5)+
  ggtitle("Lawn Species")+
  theme(plot.title = element_text(size=10), axis.title=element_text(size=10))

freq_trees<-  
  ggplot(data=subset(Frank, type=="Trees"), aes(x=Frank, y=freq, color=as.factor(native)))+
  geom_point(size=3)+
  scale_color_manual(name="Native Status", label=c("Not Native", "Native"), values=c("black", "darkgray"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Rank")+
  ylab("Number of Yards Present")+
  annotate("text", x =3, y = 25, label='Cornus florida', size=3)+
  annotate("text", x =5, y = 24, label='Lagerstroemia indica', size=3)+
  annotate("text", x =5.2, y = 20, label='Acer palmatum', size=3)+
  annotate("text", x =2.5, y = 17, label='A. sacch.', size=3)+
  annotate("text", x =6.2, y = 16, label='Ilex opaca', size=3)+
  annotate("text", x =8.2, y = 17, label='Prunus cultivar', size=3)+
  annotate("text", x =8.5, y = 13, label='A. rubrum', size=3)+
  annotate("text", x =6.2, y = 10, label='Morus alba', size=3)+
  annotate("text", x =7, y = 8, label='Magnolia grand.', size=3)+
  annotate("text", x =10, y = 9, label='Thuja occ.', size=3)+
  annotate("text", x =Inf, y = Inf, label='E', size=3, hjust=1.5, vjust=1.5)+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  ggtitle("Tree Species")+
  theme(plot.title = element_text(size=10), axis.title=element_text(size=10))

abund_flower<-
  ggplot(data=subset(Arank, type=="Flowering Plants"), aes(x=Arank, y=abund, color=as.factor(native)))+
  geom_point(size=3)+
  scale_color_manual(name="Native Status", label=c("Not Native", "Native"), values=c("black", "darkgray"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Rank")+
  ylab("Abundance")+
  annotate("text", x =3, y = 223, label='Catharanthus', size=3)+
  annotate("text", x =3.8, y = 190, label='Hemerocallis', size=3)+
  annotate("text", x =4.5, y = 180, label='Rudbeckia', size=3)+
  annotate("text", x =5, y = 157, label='Hosta', size=3)+
  annotate("text", x =6.7, y = 120, label='Hydrangea', size=3)+
  annotate("text", x =4.5, y = 107, label='Impatiens', size=3)+
  annotate("text", x =5.5, y = 98, label='Echinacea', size=3)+
  annotate("text", x =9, y = 94, label='Rosa', size=3)+
  annotate("text", x =7, y = 80, label='Lysimachia', size=3)+
  annotate("text", x =9, y = 70, label='Begonia', size=3)+
  annotate("text", x =Inf, y = Inf, label='D', size=3, hjust=1.5, vjust=1.5)+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  ggtitle("Flowering Plants")+
  theme(plot.title = element_text(size=10), axis.title=element_text(size=10))

abund_lawn <-
  ggplot(data=subset(Arank, type=="Lawn"), aes(x=Arank, y=abund, color=as.factor(native)))+
  geom_point(size=3)+
  scale_color_manual(name="Native Status", label=c("Not Native", "Native"), values=c("black", "darkgray"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Rank")+
  ylab("Abundance")+
  annotate("text", x =3, y = 2090, label='Poa pratensis', size=3)+
  annotate("text", x =4.3, y = 1215, label='Festuca arund.', size=3)+
  annotate("text", x =5, y = 963, label='Zoysiagrass', size=3)+
  annotate("text", x =6.7, y = 806, label='Cynodon dactylon', size=3)+
  annotate("text", x =7.5, y = 703, label='Trifolium repens', size=3)+
  annotate("text", x =4.5, y = 620, label='F. rubra', size=3)+
  annotate("text", x =9.2, y = 597, label='Digitaria sang.', size=3)+
  annotate("text", x =5, y = 420, label='Glechoma hederacea', size=3)+
  annotate("text", x =7.6, y = 320, label='Viola pap.', size=3)+
  annotate("text", x =7.8, y = 200, label='Lolium perenne', size=3)+
  annotate("text", x =Inf, y = Inf, label='B', size=3, hjust=1.5, vjust=1.5)+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  ggtitle("Lawn Species")+
  theme(plot.title = element_text(size=10), axis.title=element_text(size=10))

abund_tree<-
  ggplot(data=subset(Arank, type=="Trees"), aes(x=Arank, y=abund, color=as.factor(native)))+
  geom_point(size=3)+
  scale_color_manual(name="Native Status", label=c("Not Native", "Native"), values=c("black", "darkgray"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Rank")+
  ylab("Abundance")+
  annotate("text", x =3, y = 34, label='Ilex opaca', size=3)+
  annotate("text", x =4, y = 33, label='Cornus florida', size=3)+
  annotate("text", x =5.5, y = 31, label='Lagerstroemia indica', size=3)+
  annotate("text", x =6.5, y = 32, label='Thuja occidentalis', size=3)+
  annotate("text", x =7, y = 29, label='Acer palmatum', size=3)+
  annotate("text", x =3.8, y = 25, label='A. saccharinum', size=3)+
  annotate("text", x =8.5, y = 24, label='Prunus cultivar', size=3)+
  annotate("text", x =10, y = 25, label='Ailanthus alt.', size=3)+
  annotate("text", x =7.5, y = 18, label='A. rubrum', size=3)+
  annotate("text", x =8.3, y = 16, label='A. negundo', size=3)+
  annotate("text", x =Inf, y = Inf, label='F', size=3, hjust=1.5, vjust=1.5)+
  scale_x_continuous(limits=c(1,11), breaks = c(1:10))+
  ggtitle("Tree Species")+
  theme(plot.title = element_text(size=10), axis.title=element_text(size=10))



legend=gtable_filter(ggplot_gtable(ggplot_build(freq_trees)), "guide-box") 
grid.draw(legend)


Fig2<-grid.arrange(arrangeGrob(freq_lawn+theme(legend.position="none"),
                               abund_lawn+theme(legend.position="none"),
                               freq_flower+theme(legend.position="none"),
                               abund_flower+theme(legend.position="none"),
                               freq_trees+theme(legend.position="none"),
                               abund_tree+theme(legend.position="none"),
                               ncol=2),legend, 
                   widths=unit.c(unit(1, "npc") - legend$width, legend$width),nrow=1)

ggsave("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\TimeMoney\\reviews\\Final to publish\\Proof\\Fig2.jpg", plot=Fig2, width=180, height=200, unit="mm", dpi=300 )


# Community composition analyses Table 2 and Figure 3 ------------------------------------------

###lawns
lawn3<-lawn2%>%
  filter(homeid_loc!="1003_21_Front"&homeid_loc!="1003_21_Back")%>%
  group_by(NB, House, Species.combined)%>%
  summarize(cover=mean(Cover))%>%
  filter(Species.combined!="Unk-Tree seedling"&Species.combined!="Unk-Poaceae"&Species.combined!="Unknown"&Species.combined!="Unknown Asteraceae")%>%
  spread(Species.combined, cover, fill=0)%>%
  rename(Nb=NB)%>%
  left_join(NB)

adonis(lawn3[3:100]~lawn3$money)#p<0.001, F = 5.808
adonis(lawn3[3:100]~lawn3$Style)

permutest(betadisper(vegdist(lawn3[3:100]), lawn3$money, type = "centroid"))
permutest(betadisper(vegdist(lawn3[3:100]), lawn3$Style, type = "centroid"))


#flowers
floral3<-floral2%>%
  filter(Genus!="Unknown.Asteraceae")%>%
  group_by(House_ID, Genus)%>%
  summarize(ntot=sum(nplants))%>%
  separate(House_ID, into=c("Nb", "House"), sep="_")%>%
  mutate(Nb=as.integer((as.character(Nb))))%>%
  left_join(NB)%>%
  spread(Genus, ntot, fill=0)

#doing this based on number of plants, but are the same result if use nflowers or area
adonis(floral3[8:101]~floral3$money)#p< 0.001; F= 2.638
adonis(floral3[8:101]~floral3$Style)# p = 0.321; F =1.104

permutest(betadisper(vegdist(floral3[8:101]), floral3$money, type = "centroid"))
permutest(betadisper(vegdist(floral3[8:101]), floral3$money, type = "centroid"))

#trees
trees3<-trees2%>%
  filter(Tree.species!="Unknown conifer"&Tree.species!="Unknown Shrub-like"&Tree.species!="Unknown sp."&Tree.species!="Unknown skinny"&Tree.species!="Unknown sharp")%>%
  separate(homeid, into=c("Nb", "House", "fb"), sep="::")%>%
  mutate(Nb=as.integer(as.character(Nb)))%>%
  group_by(Nb, House, Tree.species)%>%
  summarise(ntot=sum(abund))%>%
  left_join(NB)%>%
  spread(Tree.species, ntot, fill=0)

adonis(trees3[8:108]~trees3$money)#p = 0.295, F = 1.111
adonis(trees3[8:108]~trees3$Style)# p = 0.126, F = 1.309

permutest(betadisper(vegdist(trees3[8:108]), trees3$money, type = "centroid"))
permutest(betadisper(vegdist(trees3[8:108]), trees3$Style, type = "centroid"))

###NMDS figures

#doing NMDS on the NB level

#lawns
lawn4<-lawn2%>%
  filter(homeid_loc!="1003_21_Front"&homeid_loc!="1003_21_Back")%>%
  rename(Nb=NB)%>%
  left_join(NB)%>%
  group_by(Nb, House, money, Species.combined)%>%
  summarize(cover=mean(Cover))%>%
  filter(Species.combined!="Unk-Tree seedling"&Species.combined!="Unk-Poaceae"&Species.combined!="Unknown"&Species.combined!="Unknown Asteraceae")%>%
  group_by(Nb, money, Species.combined)%>%
  summarise(mcov=mean(cover))%>%
  spread(Species.combined, mcov, fill=0)


plotinfo=lawn4[c(1:2)]

lnmds<-metaMDS(lawn4[3:100], trymax=100)

scores<-data.frame(lnmds$points)%>%
  bind_cols(plotinfo)

lawn<-ggplot(data=scores, aes(x=MDS1, y=MDS2, color=money))+
  geom_point(size=3)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("Middle", "High"), limits=c("mid","high"))+
  ylab("NMDS1")+
  xlab("NMDS2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Lawn Communities")+
  annotate("text", x=-0.4, y = 0.4, label="A", size=5)

#flowers
floral4<-floral2%>%
  filter(Genus!="Unknown.Asteraceae")%>%
  group_by(House_ID, Genus)%>%
  summarize(ntot=sum(nplants))%>%
  separate(House_ID, into=c("Nb", "House"), sep="_")%>%
  mutate(Nb=as.integer((as.character(Nb))))%>%
  left_join(NB)%>%
  group_by(Nb, money, Genus)%>%
  summarise(mntot=mean(ntot))%>%
  spread(Genus, mntot, fill=0)

plotinfo=floral4[c(1:2)]

fnmds<-metaMDS(floral4[3:96], trymax=100)

fscores<-data.frame(fnmds$points)%>%
  bind_cols(plotinfo)

flower<-ggplot(data=fscores, aes(x=MDS1, y=MDS2, color=money))+
  geom_point(size=3)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("Middle", "High"), limits=c("mid","high"))+
  ylab("NMDS1")+
  xlab("NMDS2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Floral Communities")+
  annotate("text", x=-0.85, y = 0.4, label="B", size=5)


#trees
trees4<-trees2%>%
  filter(Tree.species!="Unknown conifer"&Tree.species!="Unknown Shrub-like"&Tree.species!="Unknown sp."&Tree.species!="Unknown skinny"&Tree.species!="Unknown sharp")%>%
  separate(homeid, into=c("Nb", "House", "fb"), sep="::")%>%
  mutate(Nb=as.integer(as.character(Nb)))%>%
  group_by(Nb, House, Tree.species)%>%
  summarise(ntot=sum(abund))%>%
  left_join(NB)%>%
  group_by(Nb, money, Tree.species)%>%
  summarise(mntot=sum(ntot))%>%
  spread(Tree.species, mntot, fill=0)

plotinfo=trees4[c(1:2)]

tnmds<-metaMDS(trees4[3:103], trymax=100)

tscores<-data.frame(tnmds$points)%>%
  bind_cols(plotinfo)

trees<-ggplot(data=tscores, aes(x=MDS1, y=MDS2, color=money))+
  geom_point(size=3)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("Middle", "High"), limits=c("mid","high"))+
  ylab("NMDS1")+
  xlab("NMDS2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Tree Communities")+
  annotate("text", x=-0.55, y = 0.65, label="C", size=5)


#figure 3
legend5=gtable_filter(ggplot_gtable(ggplot_build(trees)), "guide-box") 
grid.draw(legend5)


fig3<-grid.arrange(arrangeGrob(lawn+theme(legend.position="none"),
                               flower+theme(legend.position="none"),
                               trees+theme(legend.position="none"),
                               ncol=1),legend5, 
                   widths=unit.c(unit(1, "npc") - legend5$width, legend5$width),nrow=1)

ggsave("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\TimeMoney\\reviews\\Final to publish\\Proof\\Fig3.jpg", plot=fig3, width=85, height=150, unit="mm", dpi=300 )

# ANCOVA analyses Table 3 and Figure 4 ------------------------------------

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
  left_join(NB)%>%
  rename(lrich=richness,
         leven=Evar)

hist(lawn_rich$lric)
hist(log(lawn_rich$leven))

#how many species?
length(unique(lawn2$Species.combined))#subtract 10 for unknowns

lawntype<-lawn%>%
  select(Species.combined, Type)%>%
  unique()

lawninvasive<-lawn%>%
  select(Species.combined, Invasive)%>%
  unique()

weeds<-lawn2%>%
  filter(Cover!=0)%>%
  group_by(NB, House, Species.combined)%>%
  summarize(cov=mean(Cover))%>%
  group_by(Species.combined)%>%
  summarize(n=length(House))

#ANCOVA
summary(aov(lrich~Style*money*Location+yard_area, data=lawn_analysis))
#nothing with richness

summary(aov(leven~Style*money*Location+yard_area, data=lawn_analysis))


#trees

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

treesp<-trees2%>%
  group_by(Tree.species)%>%
  summarize(sum=sum(abund))

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


##floral
floral2<-floral%>% 
  filter(Flower.Width..cm.!="", Genus!="NoFlowers")%>%
  mutate(numflowers=X.F.stems*ave_flower_perstem,
         area=as.numeric(as.character(Flower.Width..cm.))*as.numeric(as.character(Flower.Length..cm.))*numflowers)%>%
  group_by(House_ID, Genus, Front.Back)%>%
  summarize(nplants=sum(X..F.plants),
            nflowers=sum(numflowers),
            areatot=sum(area))

commongener<-floral2%>%
  group_by(Genus)%>%
  summarize(num=sum(nplants),
            flower=sum(nflowers),
            area=sum(areatot))

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
  mutate(perflower=areatot/yard_area)%>%
  left_join(NB)%>%
   mutate(lnumplants=log(nplants+1),
         lnumflowers=log(nflowers+1),
         larea=log(areatot+1),
         lperfl=log(perflower+1))

hist(flowernum$lperfl)

#Ancovas num flowers
summary(aov(lnumplants~Style*money*Front.Back+yard_area, data=flowernum))

summary(aov(lnumflowers~Style*money*Front.Back+yard_area, data=flowernum))

summary(aov(larea~Style*money*Front.Back+yard_area, data=flowernum))

summary(aov(lperfl~Style*money*Front.Back, data=flowernum))

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


##invasives
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

commoninv<-invasive2%>%
  group_by(Species.Latin)%>%
  summarize(n=sum(present))

inv_rich<-community_structure(invasive2, abundance.var="present", replicate.var="homeid")%>%
  separate(homeid, into=c("NB", "House"))%>%
  select(-Evar)%>%
  mutate(Nb=as.integer(NB), House=as.integer(House))%>%
  bind_rows(noinv)%>%
  left_join(yardarea)%>%
  left_join(NB)%>%
  mutate(lrich=log(richness+1))

hist(inv_rich$richness)

summary(aov(richness~Style*money*yard_area, data=inv_rich))


###Figure 4
stemsfbl<-ggplot(data=stems2, aes(x=yard_area, y = lstems, color=Front.Back))+
  geom_point(size=3, alpha=0.5)+
  geom_smooth(method = "lm", se=T, size = 2)+
  scale_color_manual(name="Location", values=c("firebrick", "goldenrod"), labels=c("Back", "Front"))+
  ylab("Log(Num. Trees)")+
  xlab("Yard Area")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50, y = 4.5, label="F", size=4)

treerichl<-ggplot(data=tree_rich, aes(x=yard_area, y = lrich, color=money))+
  geom_point(size=3, alpha= 0.5)+
  geom_smooth(method = "lm", se=T, size = 2)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("Middle", "High"), limits=c("mid","high"))+
  ylab("Log(Tree Richness)")+
  xlab("Yard Area")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50, y = 2.9, label="B", size=4)

inv<-ggplot(data=inv_rich, aes(x=yard_area, y = richness, color=Style))+
  geom_point(size=3, alpha= 0.5)+
  geom_smooth(method = "lm", se=T, size = 2)+
  scale_color_manual(name="Lifestage", values=c("olivedrab", "navy"), labels=c("Middle\nGround", "Senior\nStyles"))+
  ylab("Num. Invasive Sp.")+
  xlab("Yard Area")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50, y = 5.5, label="D", size=4)

areafbl<-ggplot(data=flowernum, aes(x=yard_area, y = larea, color=Front.Back))+
  geom_point(size=3, alpha=0.5)+
  geom_smooth(method = "lm", se=T, size = 2)+
  scale_color_manual(name="Location", values=c("firebrick", "goldenrod"), labels=c("Back", "Front"))+
  ylab("Log(Floral Area)")+
  xlab("Yard Area")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50, y = 14, label="E", size=4)

areastylel<-ggplot(data=flowernum, aes(x=yard_area, y = larea, color=Style))+
  geom_point(size=3, alpha=0.5)+
  geom_smooth(method = "lm", se=T, size = 2)+
  scale_color_manual(name="Lifestage", values=c("olivedrab", "navy"), labels=c("Middle\nGround", "Senior\nStyles"))+
  ylab("Log(Floral Area)")+
  xlab("Yard Area")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50, y = 12, label="C", size=4)+
  scale_y_continuous(limits = c(0,12), breaks = c(0,2,4,6,8,10,12))

general<-ggplot(data=flower_rich, aes(x=yard_area, y = lrich, color=money))+
  geom_point(size=3, alpha=0.5)+
  geom_smooth(method = "lm", se=T, size = 2)+
  scale_color_manual(name="Income", values=c("green4", "deepskyblue"), labels=c("Middle", "High"), limits=c("mid", "high"))+
  ylab("Log(Num. Floral Genera)")+
  xlab("Yard Area")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50, y = 3.2, label="A", size=4)


###fig a - income
legend1=gtable_filter(ggplot_gtable(ggplot_build(treerichl)), "guide-box") 
grid.draw(legend1)


inc<-grid.arrange(arrangeGrob(treerichl+theme(legend.position="none"),
                         general+theme(legend.position="none"),
                          ncol=2),legend1, 
                        widths=unit.c(unit(1, "npc") - legend2$width, legend2$width),nrow=1)

###fig b - lifestage
legend2=gtable_filter(ggplot_gtable(ggplot_build(inv)), "guide-box") 
grid.draw(legend2)


life<-grid.arrange(arrangeGrob(areastylel+theme(legend.position="none"),
                         inv+theme(legend.position="none"),
                         ncol=2),legend2, 
             widths=unit.c(unit(1, "npc") - legend2$width, legend2$width),nrow=1)

###fig c - frontback
legend3=gtable_filter(ggplot_gtable(ggplot_build(stemsfbl)), "guide-box") 
grid.draw(legend3)


fb<-grid.arrange(arrangeGrob(stemsfbl+theme(legend.position="none"),
                         areafbl+theme(legend.position="none"),
                         ncol=2),legend3, 
             widths=unit.c(unit(1, "npc") - legend3$width, legend3$width),nrow=1)

Figure4<-grid.arrange(general, treerichl, areastylel, inv, areafbl, stemsfbl , ncol=2)

ggsave("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\TimeMoney\\reviews\\Final to publish\\Proof\\Fig4.jpg", plot=Figure3, width=180, height=200, unit="mm", dpi=300 )


# Floral Area Figure 5 ----------------------------------------------------
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

#fig5
fig5<-ggplot(pie.across, aes(x = money, y=Sum_colarea/10000, fill=value2))+
  geom_bar(color="black", width = 0.75,stat="identity")+
  scale_x_discrete(limits=c("mid","high"), labels=c("Middle","High"))+
  scale_fill_manual(values=c("blue", "darkviolet", "purple","violetred3", "red","orangered1","orange","goldenrod1","yellow","yellowgreen","green4", "white"))+
  ylab(expression (paste("Floral Area (",~m^2,")")))+
  xlab("Income")+
  theme(axis.ticks=element_blank(),
        legend.title=element_blank(),
        panel.grid=element_blank(), legend.position = "none")

ggsave("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\TimeMoney\\reviews\\Final to publish\\Proof\\Fig5.jpg", plot=fig5, width=85, height=100, unit="mm", dpi=300 )

# Measures of floral diversity Figure 6 -----------------------------------

###correlating flower stuff
flowersall<-color_rich%>%
  rename(crich=richness)%>%
  select(-lrich)%>%
  left_join(flower_rich)%>%
  left_join(flowernum)

cor(flowersall[,c(4, 18, 20:22)])

##Fig 6
panel.cor <- function(x, y, digits = 3, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  test <- cor.test(x,y) 
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 1),
                   symbols = c("*", " "))
  
  
  text(0.5, 0.5, txt, cex = 1.5)
  text(0.8, 0.5, Signif, cex=3, col="red")
}

colnames(codyndat_allmetrics)# double check columns are correct
#comparing absolute S and E changes

jpeg("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\TimeMoney\\reviews\\Final to publish\\Proof\\PairsPlot.jpeg", width=150, height=150, unit="mm", res=300)

pairs(flowersall[,c(4, 18, 20:22)],upper.panel = panel.cor, cex.axis = 2, labels=c("Num.\n Colors", "Num.\nGenera", "Num.\nPlants", "Num.\nFlowers", "Floral\nArea"))
par(xpd=T)

dev.off()