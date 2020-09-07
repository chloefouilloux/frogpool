# FROGPOOL 2019 AND 2020 COMBINED DATA 
# F19/20 COMBINATION USED FOR MULTIYEAR AND MONTH POOL COMPARISON. 
#########################################################################################
#First, let's load some packages
library(ggplot2)
library(gridExtra)
library(cowplot)
library(dplyr)
library(tidyverse)
library(corrplot)
library(ggpubr)
library(Hmisc)
library(lubridate)

#Dealing with combined data sets
##############
#COMBINED----
############
pool_2019 <- read.csv("Pool_Data.csv", header = T, sep= ",") #CHLOE DATA
pool_2018<- read.csv("frogpool19_v2_reduced_2020-09-03.csv", 
                                      header = T, fileEncoding="UTF-8-BOM") #ANDRIUS

#Okay first thing is first we need to add a year to differentiate. 
pool_2019$Year<- 2019
pool_2018$Year<- 2018

#then we also need to match up pool types. 
pool_2019[pool_2019$Type %in% c("Waka_Pool", "Buttress", "Tree_Hole"), "pool_type"] <- "alive"
pool_2019[pool_2019$Type %in% c("Palm", "Fallen_Tree"), "pool_type"] <- "dead"
pool_2019[pool_2019$Type %in% c("Andrius"), "pool_type"] <- "arboreal"

#At this point, the only thing I feel confident "mixing" of the two data sets are pool heights
#and tictorius counts.
#manipulating fp_2018 data

#Also, during the FP_Team discussion in September 2020 we said that we only should have
#ONE observation of the 2019 data to compare to the 2018 data (so here we are taking
#the FIRST tadpole observation in 2019).

pools_chloe<-
  pool_2019 %>%
  unite("pool_id", PoolID: TreeID, sep = "") %>%
  dplyr::rename(height = Height, 
                tadpoles_tinc = Dt_Tadpole_Num) %>%
  replace_na(list(tadpoles_tinc = 0, tadpoles_other = 0))%>%
  mutate(Total_Tinc = tadpoles_tinc)%>%
  filter(Week == 1) %>%
  select( pool_id, height, pool_type,
          tadpoles_tinc, Year)

pool_andri<- 
  pool_2018 %>%
  replace_na(list(tadpoles_tinc = 0))%>%
  mutate(Total_Tinc = tadpoles_tinc) %>%
  select(pool_id, height, pool_type, Year, tadpoles_tinc,  
         Total_Tinc)

################################################
#What about pools sampled in 2018 and again 2019?
#DOUBLE SAMPLE POOLS----
###############################
#run code above first xxx
#So first I have to look at the pools that we have in common
#at least in my data sheet we were able to narrow down to at least the tree, 
#but sometimes not the exact pool. This is worth discussion later.

FP_pools<-
  pool_andri %>%
  filter(pool_id %in% c("frogpool_24.1",
                        "frogpool_37.1",
                        "frogpool_37.2",
                        "frogpool_20.1",
                        "frogpool_20.2",
                        "frogpool_43.1",
                        "frogpool_43.2",
                        "frogpool_39.1",
                        "frogpool_30.1",
                        "frogpool_38.1",
                        "frogpool_29.1",
                        "frogpool_29.2",
                        "frogpool_7.1",
                        "frogpool_13.1",
                        #arboreal
                        "frogpool_17.1",
                        "frogpool_15.1",
                        "frogpool_28.1",
                        "frogpool_34.1",
                        "frogpool_34.2",
                        "frogpool_32.1",
                        "frogpool_47.1"))%>%
  rename(FP_equivalent = pool_id)%>%
  select(FP_equivalent, pool_type, height,
         tadpoles_tinc, Year, Total_Tinc)

pools_chloe$FP_equivalent[ pools_chloe$pool_id == "1B" ] <- "frogpool_24.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "1E" ] <- "frogpool_37.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "2E" ] <- "frogpool_37.2"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "1J " ] <- "frogpool_20.1" #FP POOL UNCERTAINTY
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "1K" ] <- "frogpool_43.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "2K" ] <- "frogpool_43.2"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "1L" ] <- "frogpool_39.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "1M" ] <- "frogpool_20.2" #FP POOL UNCERTAINTY
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "1N" ] <- "frogpool_30.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "1P" ] <- "frogpool_38.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "1Q" ] <- "frogpool_29.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "2Q" ] <- "frogpool_29.2"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "1U" ] <- "frogpool_7.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "1V" ] <- "frogpool_13.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "17.1FP" ] <- "frogpool_17.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "15.1FP" ] <- "frogpool_15.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "28.1FP" ] <- "frogpool_28.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "34.1FP" ] <- "frogpool_34.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "34.2FP" ] <- "frogpool_34.2"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "32.1FP" ] <- "frogpool_32.1"
pools_chloe$FP_equivalent[ pools_chloe$pool_id == "47.1FP" ] <- "frogpool_47.1"

#Now filter all of the pools from Chloe's data set that only have an FP2018 equivalent.

pools_chloe_FP<- 
  pools_chloe %>% 
  select(- pool_id) %>%
  filter(FP_equivalent != "NA")

###############################
#FINAL COMBINED TWO YEAR DATA SHEET----
#These are pools samples both in 2018 and 2019
###############################
pools_18_19<-
  full_join(FP_pools, pools_chloe_FP) 


##################################
#### METEO 2020 ----##################
##################################
meteo<- read.csv("PARARE_Meteo_Janv-Juin2020.csv", 
                 header = T, fileEncoding="UTF-8-BOM") #ANDRIUS

meteo$timestamp<- dmy_hm(meteo$TIMESTAMP)
class(meteo$Rain_mm_Tot)
meteo$Rain_mm <- as.numeric(as.character(meteo$Rain_mm_Tot)) #workaround, cannot go as.numeric directly
#now let's subset for the month we were there
precip_Feb <- meteo %>%
  filter(timestamp >= as.Date('2020-01-31') & timestamp <= as.Date('2020-02-27')) %>%
  tidyr::drop_na()%>%
  group_by(week = week(timestamp))%>%
  summarise (week_ave_rain = mean(Rain_mm), 
             week_total_rain = sum(Rain_mm))

precip_Feb$Week <- c("1","2", "3", "4", "5")

#So now let's plot this together with tadpole numbers
#Now let's combine data_frames
chloe_rain <- pool_2019 %>%
  mutate(Week = as.character(Week))%>%
  select("Week", "pH", "Dt_Tadpole_Num", "pool_type")

chloe_rain_all<- dplyr::full_join(chloe_rain, precip_Feb,by = "Week")
chloe_rain_all<-chloe_rain_all %>%
              mutate(Week = as.numeric(Week))

###############################
#2 year graphing exploration----
###############################

table(pools_18_19$pool_type) #WARNING: quite different sample sizes. 

ggplot(pools_18_19, aes( x= Year, y = Total_Tinc, color = FP_equivalent))+
  geom_point()+
  scale_x_continuous(breaks = seq(2018, 2019, 1))+
  ylim(0,55)+
  ylab("Tinc Tad Count")+
  stat_summary( geom = "line")+ #mean se
  theme_bw()

##For pool types//species
pools_18_19$pool_type <- as.factor(pools_18_19$pool_type)
pools_18_19$pool_type<-relevel(pools_18_19$pool_type, "arboreal")

ggplot(pools_18_19, aes(x= Year, y = Total_Tinc, color = pool_type))+
  geom_point(size = 3, alpha = 0.8)+
  scale_x_continuous(breaks = seq(2018, 2019, 1))+
  ylim(0,50)+
  stat_summary(geom = "line", linetype = 2)+
  ylab("Tadpole count")+
  theme_bw()+
  scale_color_manual( values = c("darkgreen", "lightgreen", "#e6a81e" ))


###############################
#1 month graphing exploration----
###############################

ggplot(subset(pool_2019, pool_type %in% c("alive", "dead")), 
          aes( x = Week, y = Dt_Tadpole_Num, color = pool_type))+
  geom_point(size =3, alpha = 0.3)+
  stat_smooth( method = "loess", se = F)+
  theme_bw()+
  scale_color_manual( values = c("lightgreen", "#e6a81e" ))

ggplot(subset(pool_2019, pool_type %in% c("alive", "dead")), 
       aes( x = Week, y = pH, color = pool_type))+
  geom_point(size =3, alpha = 0.3)+
  stat_smooth( method = "loess", se = F)+
  theme_bw()+
  scale_color_manual( values = c("lightgreen", "#e6a81e" ))

ggplot(subset(pool_2019, pool_type %in% c("alive", "dead")), 
       aes( x = Week, y = Dt_Tadpole_Num, color = pH))+
  geom_point(size =3)+
  stat_smooth( method = "loess", se = F)+
  facet_wrap(~ pool_type)+
  theme_bw()

#######################
### RAIN PLOTTING---- #
#What about the rain?

ggplot(precip_Feb, aes(y = week_ave_rain, x = week))+
  geom_point()+
  geom_line()+
  theme_bw()

#So now let's plot this together with tadpole numbers
#Now let's combine data_frames

coeff<- 0.01
coeff1<-0.03

tads<-ggplot(subset(chloe_rain_all, pool_type %in% c("alive", "dead")), aes(x= Week)) +
  geom_point(aes(y = Dt_Tadpole_Num, color = pool_type), size =3, alpha = 0.3)+
  stat_smooth(aes(y = Dt_Tadpole_Num, color = pool_type), 
              method = "loess", formula = y ~ x, se = F)+
  geom_line(aes(y = week_ave_rain/coeff), color = "lightblue", 
            linetype = "dashed")+
  geom_point(aes(y = week_ave_rain/coeff), color = "blue")+
  scale_y_continuous(name = "Total Tadpoles",
                     sec.axis = sec_axis(~.*coeff, name = "Ave. rain (mm/week)"))+
  theme_bw()+
  scale_color_manual( values = c("lightgreen", "#e6a81e" ))


ph<-ggplot(subset(chloe_rain_all, pool_type %in% c("alive", "dead")), aes(x= Week)) +
  geom_point(aes(y = pH, color = pool_type), size =3, alpha = 0.3)+
  stat_smooth(aes(y = pH, color = pool_type), 
              method = "loess", formula = y ~ x, se = F)+
  geom_line(aes(y = week_ave_rain/coeff1), color = "lightblue", 
            linetype = "dashed")+
  geom_point(aes(y = week_ave_rain/coeff1), color = "blue")+
  scale_y_continuous(name = "pH",
                     sec.axis = sec_axis(~.*coeff1, name = "Ave. rain (mm/week)"))+
  theme_bw()+
  scale_color_manual( values = c("lightgreen", "#e6a81e" ))

grid.arrange(tads, ph)
