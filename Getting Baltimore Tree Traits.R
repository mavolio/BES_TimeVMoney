###code to get tree attributes merged from SLC and LA

library(tidyverse)


BAL<-read.csv("C:/Users/megha/Dropbox/BES Research/Time V Money/Data/Cleaned Data/balt_treesp.csv")%>%
  select(-X)%>%
  rename(genus.species=Tree.species)

SLC<-read.csv("C:/Users/megha/Dropbox/Pataki Lab/Summer2014/Data/Tree Analysis/SLC_Traits_PresentSpeciesOnly.csv")%>%
  rename(genus.species=Species, Family=Family.Name, flower=Flowers,growth_rt=Growth, fall_color=Fall.Color, fruit=Fruit)%>%
  select(-Biome, -Crown.Type, -leaf_shape, -Drainage.Level, -Salt.Tolerance, -Shade.Tolerance, -Alkalinity.Level, -Drainage.Level, -Native, -Drought.Level, -present, -PowerLines, -Bark, -Foliage, -flower_color)

LA<-read.csv("C:/Users/megha/Dropbox/Pataki Lab/LA COMPARE/LA_Trees_Categories_TOUSE.csv")%>%
  select(-Plot_ID, -STREET, -survey, -present, -county, -neighborhood, -Continent, -Category, -nursery, -palm, -wateruse, -native, -leaf_type, -Grow, -flower_color, -fruit_type)%>%
  unique()%>%
  rename(genus.species=SPECIES_NA, Continent=Continent2)

alltrees<-SLC%>%
  bind_rows(LA)

merge1<-BAL%>%
  left_join(SLC)%>%
  left_join(LA)

write.csv(merge1, "C:/Users/megha/Dropbox/BES Research/Time V Money/Data/Cleaned Data/BAL_treeTraits.csv", row.names=F)
