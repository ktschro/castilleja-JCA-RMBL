#Castilleja 2012 data from Honors Thesis project
#Look at changes in community structure after Castilleja removal

castilleja<-read.csv("Castilleja.csv")
head(castilleja)

#Convert to long format so cover for each species is in a single column and the species ID is in a another column
library(tidyverse)
library(magrittr)
castilleja<-transform(castilleja,Erigeron.Speciosus=as.numeric(Erigeron.Speciosus))
castilleja %<>% pivot_longer(cols = Castilleja.miniata:Stipa.richardsonii,
                            names_to = "species",
                            values_to = "cover")

#Graph changes in cover over time for each plot type
library(ggplot2)
ggplot(filter(castilleja,cover>=0.2),aes(x = Time, y = cover, color = species)) +
  geom_point() +
  geom_line(aes(group=interaction(species,Plot.1))) + 
  theme_bw() +
  facet_wrap(.~Treatment)

#Look at data for non-Erigeron sp becauses Erigeron has the highest cover in most plots
ggplot(filter(castilleja,species!="Erigeron.Speciosus"),aes(x = Time, y = cover, color = species)) +
  geom_point() +
  geom_line(aes(group=interaction(species,Plot.1))) + 
  theme_bw() +
  facet_wrap(.~Treatment)
#A mess. Need to figure out the functional group for each species and summarize.

#Look at just Castilleja
ggplot(filter(castilleja,species=="Castilleja.miniata"),aes(x = Time, y = cover, color = species)) +
  geom_point() +
  geom_line(aes(group=interaction(species,Plot.1))) + 
  theme_bw() +
  facet_wrap(.~Treatment)

#Look at percent change in each of the treatments 
#Summarize cover of each species by treatment group in each year
cover_summary <- castilleja %>% group_by(Treatment, Time, species) %>% summarize(mean_cover = mean(cover),
                                                                              se_cover = sqrt(var(cover,na.rm=T)/n()),
                                                                              number = length(cover), 
                                                                              ID = paste(Treatment,Time,species,sep="-"))
#Remove duplicated lines
cover_summary %<>% distinct(ID,.keep_all=TRUE)

#graph summary

#Look at functional groups in each plot type between the two years
#read in sheet with functional group information
castilleja_fx<-read.csv("castilleja_cover_fx_group.csv")

#join to cover data
castilleja_ed <- left_join(castilleja,castilleja_fx,by="species")

#summarize cover by functional group
cover_fx <- castilleja_ed %>% group_by(Treatment, Time, fx_group) %>% summarize(mean_cover = mean(cover,na.rm=T),
                                                                               se_cover = sqrt(var(cover,na.rm=T)/n()),
                                                                               ID = paste(Treatment,Time,fx_group,sep = "-"))
#remove all duplicated lines
cover_fx %<>% distinct(ID,.keep_all=TRUE)
ggplot(cover_fx,aes(x=Time,y=mean_cover,color=fx_group)) +
  geom_point() +
  geom_line(aes(group=fx_group)) +
  theme_bw() +
  facet_wrap(.~Treatment) +
  geom_pointrange(aes(ymin=mean_cover-se_cover,ymax=mean_cover+se_cover))


#look at functional groups
ggplot(cover_fx,aes(x=Time,y=mean_cover,color=Treatment)) +
  geom_point() +
  geom_line(aes(group=Treatment)) +
  theme_bw() +
  facet_wrap(.~fx_group,scales="free") +
  geom_pointrange(aes(ymin=mean_cover-se_cover,ymax=mean_cover+se_cover))

#Looking at total cover
total_cover <- castilleja_ed %>% group_by(Time,Treatment) %>% summarize(mean_total_cover = mean(Total.cover,na.rm=T),
                                                                        total_cover_se = sqrt(var(Total.cover,na.rm=T)/n()),
                                                                        ID = paste(Treatment,Time,sep="-")) %>%
  ggplot(aes(x=Time,y=mean_total_cover,color=Treatment)) +
  geom_point() +
  geom_pointrange(aes(ymin=mean_total_cover-total_cover_se,ymax=mean_total_cover+total_cover_se)) +
  theme_bw()

