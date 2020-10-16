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
#For models
library(glmmTMB)
library(sjPlot)
library(DHARMa)
library(lubridate)
#survival plots
library(survival)
library(survminer)
library(coxme)


#Dealing with combined data sets
##############
#COMBINED----
############
pool_2020 <- read.csv("Pool_Data.csv", header = T, sep= ",") #CHLOE DATA
pool_2019<- read.csv("frogpool19_v2_reduced_2020-09-03.csv", 
                                      header = T, fileEncoding="UTF-8-BOM") #ANDRIUS

#Okay first thing is first we need to add a year to differentiate. 
pool_2020$Year<- 2020
pool_2019$Year<- 2019

#then we also need to match up pool types. 
pool_2020[pool_2020$Type %in% c("Waka_Pool", "Buttress", "Tree_Hole"), "pool_type"] <- "alive"
pool_2020[pool_2020$Type %in% c("Palm", "Fallen_Tree"), "pool_type"] <- "dead"
pool_2020[pool_2020$Type %in% c("Andrius"), "pool_type"] <- "arboreal"

#At this point, the only thing I feel confident "mixing" of the two data sets are pool heights
#and tictorius counts.
#manipulating fp_2018 data

#Also, during the FP_Team discussion in September 2020 we said that we only should have
#ONE observation of the 2019 data to compare to the 2018 data (so here we are taking
#the FIRST tadpole observation in 2019).

pools_chloe<-
  pool_2020 %>%
  unite("pool_id", PoolID: TreeID, sep = "") %>%
  dplyr::rename(height = Height, 
                Total_Tinc = Dt_Tadpole_Num) %>%
  filter(Week == 1) %>%
  dplyr::select( pool_id, height, pool_type,
          Total_Tinc, Year)

pool_andri<- 
  pool_2019 %>%
  replace_na(list(tadpoles_tinc = 0))%>%
  mutate(Total_Tinc = tadpoles_tinc) %>%
  dplyr::select(pool_id, height, pool_type, Year, tadpoles_tinc,  
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
                        "frogpool_46.1", #not sampled 2020
                        "frogpool_47.1"))%>%
  rename(FP_equivalent = pool_id)%>%
  dplyr::select(FP_equivalent, pool_type, height,
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
  dplyr::select(- pool_id) %>%
  filter(FP_equivalent != "NA")

###############################
#FINAL COMBINED TWO YEAR DATA SHEET----
#These are pools samples both in 2018 and 2019
###############################
pools_19_20<-
  full_join(FP_pools, pools_chloe_FP) 

pools_both <- pools_19_20 %>%
              group_by(pool_type, Year) %>%
              summarise(Tinc_Mean = mean (Total_Tinc)) %>%
              select(pool_type, Tinc_Mean, Year)


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
chloe_rain <- pool_2020 %>%
  mutate(Week = as.character(Week))%>%
  dplyr::select("Week", "pH", "Dt_Tadpole_Num", "pool_type")

chloe_rain_all<- dplyr::full_join(chloe_rain, precip_Feb,by = "Week")
chloe_rain_all<-chloe_rain_all %>%
              mutate(Week = as.numeric(Week))

###############################
#2 year graphing exploration----
###############################

table(pools_19_20$pool_type) #WARNING: quite different sample sizes. 

ggplot(pools_19_20, aes( x= Year, y = Total_Tinc, color = FP_equivalent))+
  geom_point()+
  scale_x_continuous(breaks = seq(2019, 2020, 1))+
  ylim(0,55)+
  ylab("Tinc Tad Count")+
  stat_summary( geom = "line")+ #mean se
  theme_bw()

##For pool types//species
pools_19_20$pool_type <- as.factor(pools_19_20$pool_type)
pools_19_20$pool_type<-relevel(pools_19_20$pool_type, "arboreal")

twoyear<- 
  ggplot(pools_19_20, aes(x= Year, y = Total_Tinc, color = pool_type))+
  geom_point(size = 3, alpha = 0.8)+
  geom_point(data = pools_both, aes(x= Year, y = Tinc_Mean, shape = pool_type), size = 4)+
  scale_x_continuous(breaks = seq(2019, 2020, 1))+
  ylim(0,50)+
  stat_summary(geom = "line", linetype = 2)+
  ylab("Tadpole count")+
  theme_bw()+
  theme(legend.position = "bottom", text = element_text(size=13))+
  scale_color_manual( values = c("#7fbeec", "#05e300", "#e68d00" ), 
                      name ="Pool type", labels = c("High climbing", 
                                                    "Low climbing", 
                                                    "Ground access"))+
  scale_shape_manual(values = c(6, 6, 6), 
                     name = "", 
                     breaks = c("dead"), labels = c("Pool mean"))+
  labs(tag = "A")

twoyear

legend_a<- get_legend(twoyear)


twoyear<- twoyear + theme(legend.position = "none")
twoyear

###############################
#1 month graphing exploration----
###############################

ggplot(subset(pool_2020, pool_type %in% c("alive", "dead")), 
          aes( x = Week, y = Dt_Tadpole_Num, color = pool_type))+
  geom_point(size =3, alpha = 0.3)+
  stat_smooth( method = "loess", se = F)+
  theme_bw()+
  scale_color_manual( values = c("lightgreen", "#e6a81e" ))

ggplot(subset(pool_2020, pool_type %in% c("alive", "dead")), 
       aes( x = Week, y = pH, color = pool_type))+
  geom_point(size =3, alpha = 0.3)+
  stat_smooth( method = "loess", se = F)+
  theme_bw()+
  scale_color_manual( values = c("lightgreen", "#e6a81e" ))

ggplot(subset(pool_2020, pool_type %in% c("alive", "dead")), 
       aes( x = Week, y = Dt_Tadpole_Num, color = pH))+
  geom_point(size =3)+
  stat_smooth( method = "loess", se = F)+
  facet_wrap(~ pool_type)+
  theme_bw()

#######################
### RAIN PLOTTING---- #
#What about the rain?

d<-ggplot(precip_Feb, aes(y = week_ave_rain, x = week))+
  geom_point(aes(shape = "Average rain"), size =3, color = "blue")+
  geom_line(color = "lightblue",
            linetype = "dashed")+
  theme_bw()+
  theme(legend.title = element_blank(), text = element_text(size=13))
d

legend<- get_legend(d)

full_legend<-grid.arrange(legend_a, legend, ncol = 2, widths = c(1, 0.3))

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
  scale_y_continuous(name = "Tadpole count",
                     sec.axis = sec_axis(~.*coeff, name = "Ave. rain (mm/week)"))+
  theme_bw()+
  theme(legend.position = "none", text = element_text(size=13))+
  scale_color_manual( values = c("#05e300", "#e68d00"))+
  labs(tag = "C")

tads

ph<-ggplot(subset(chloe_rain_all, pool_type %in% c("alive", "dead")), aes(x= Week)) +
  geom_point(aes(y = pH, color = pool_type), size =3, alpha = 0.3)+
  xlim (1,4)+
  stat_smooth(aes(y = pH, color = pool_type), 
              method = "loess", formula = y ~ x, se = F)+
  geom_line(aes(y = week_ave_rain/coeff1), color = "lightblue", 
            linetype = "dashed")+
  geom_point(aes(y = week_ave_rain/coeff1), color = "blue")+
  scale_y_continuous(name = "pH",
                     sec.axis = sec_axis(~.*coeff1, name = "Ave. rain (mm/week)"))+
  theme_bw()+
  theme(text = element_text(size=14), legend.position = "top")+
  scale_color_manual( values = c("#05e300", "#e68d00"), name = "Pool substrate",
                      labels = c("Live", "Dead"))
ph

####MEGA PLOT

a<-grid.arrange(ph, tads,  nrow = 2)

b<- cowplot::plot_grid(twoyear, a, ncol = 2)

grid.arrange(b, full_legend, nrow = 2, heights = c(1, 0.15))

#############################
#### FP20 TIMESCALES MODELS ---- ####
#################################
#First, let's load some packages

#let's subset out arboreal data as well.
#also, let's go ahead and center pH
pool_2020_terr<- pool_2020 %>%
  tidyr::unite("pool_id", PoolID: TreeID, sep = "") %>%
  mutate(pool_id = as.factor(pool_id),
         center_pH = pH - mean(pH),
         water_capacity = Depth * Length * Width, 
         tinc_y_n = ifelse(Dt_Tadpole_Num > 0, 0, 1)) %>% #watch out! 1 = death! so if tinc is more than 0, give 0!
  dplyr::filter(pool_type != "arboreal")

pool_2020_math<- pool_2020_terr %>%
  group_by(pool_type) %>%
  summarise(ave_ph = mean(pH),
            ave_tinc = mean (Dt_Tadpole_Num)) %>%
  select(ave_ph, ave_tinc, pool_type)


ggplot(pool_2020_terr, aes( y = Depth, x = Type))+
  geom_point()

ggplot(pool_2020_terr, aes( y = water_capacity, x = Type))+
  geom_point()


#First, let's look at the effect of pool type + time + (rain?) on pH

m1<- glmmTMB(pH ~ pool_type + Week + Dt_Tadpole_Num + water_capacity + (1| pool_id), 
             family = gaussian, data = pool_2020_terr)

summary(m1)

step(m1)
drop1(m1, test = "Chi") #drop Dt_Tadpole_Num and water capacity

m2<- glmmTMB(pH ~ pool_type + Week + (1| pool_id), family = gaussian, data = pool_2020_terr)
m3<- glmmTMB(pH ~ pool_type * Week + (1| pool_id), family = gaussian, data = pool_2020_terr) 
#pool type : week not significant
summary(m3)
#m2 interaction between pool type and week does not yield significant interaction. 
drop1(m3, test = "Chi")

res <- simulateResiduals(m2, plot = T) #looks good. 
testDispersion(m2)
summary(m2) 

tab_model(m2,
          show.r2 = F, 
          show.ngroups = F ,
          show.stat = "z",
          show.icc = F, 
          show.obs = F,
          string.ci = "CI",
          string.est = "Estimates",
          string.stat = "z",
          transform = NULL,
          pred.labels = c("(Intercept)", 
                          "Pool type [Dead]",
                          "Week"))


#COXME MODELS----
#TINC MODEL ----
#First, let's look at the effect of pool type + time + pH on tadpole counts!
#We only consider terrestrial pools here

#Okay, so after some thinking, I don't think using tinc numbers across time is a very
#good estimate for several reasons. For 1. I removed some tadpoles from pools (for exp.), so the 
#fluctuation between pools wasn't so accurate. 2. sometimes tail clipping would kill 
#tadpoles which would fuck with dynamics. 

#Models m4-m6c are using raw tincotirus numbers. But in reality, my work in the 2020
#field season was more about swabbing tadpoles and sampling water for chytrid.

#MAYBE A SURVIVAL CURVE! PROBABILITY ACROSS TIME!

m7<- coxme(Surv(Week, tinc_y_n) ~ pool_type + pH + water_capacity + (1| pool_id), data = pool_2020_terr)
m8<- coxme(Surv(Week, tinc_y_n) ~ pool_type + pH  + (1| pool_id), data = pool_2020_terr)
m9<- coxme(Surv(Week, tinc_y_n) ~  pH  + water_capacity+(1| pool_id), data = pool_2020_terr)

AIC(m7, m8, m9, m10)
anova(m8, m7)
anova(m9, m7)
fixed.effects(m7)
coxme::VarCorr(m7)
summary(m7)

#overall survival for a month
summary(survfit(Surv(Week, tinc_y_n) ~ 1, data = pool_2020_terr), times = 4) #20%
survdiff(Surv(Week, tinc_y_n) ~ pool_type, data = pool_2020_terr)

survplot<-ggsurvplot(survfit(Surv(Week, tinc_y_n) ~ pool_type, data = pool_2020_terr),
                     conf.int = T,
                     palette = c("#05e300", "#e68d00"), 
                     legend.labs =
                       c("Live", "Dead"),
                     xlab = "Time (weeks)",
                     xlim = c(0,4),
                     linetype = "F1", 
)

survplot

tab_model(m7, 
          show.est = T, 
          string.est = "coef (exp)", 
          show.se = T, 
          string.se = "SE",
          show.stat = T, 
          show.obs = F,
          string.stat = "z", 
          show.ci = F,
          digits = 5,
          
          dv.labels = "Presence across time (month)",
          pred.labels = c( 
                          "Pool type [Dead]",
                          "pH",
                          "Water capacity"))

#let's see
#for plotting

m7a<- survfit(Surv(Week, tinc_y_n) ~ pool_type + pH , data = pool_2020_terr)

pool_2020_terr$ph3<- cut(pool_2020_terr$pH, breaks = c(2.29, 3.84, 5.40, 6.98))

survplot<-ggsurvplot(survfit(Surv(Week, tinc_y_n) ~ ph3, data = pool_2020_terr),
           conf.int = T,
           palette = c("pink", "lightgreen", "lightblue"), 
           legend.labs =
             c("pH = 2.29-3.84", "pH = 3.84-5.4", "pH = 5.4-6.98"),
           xlab = "Time (weeks)",
           xlim = c(0,4),
           linetype = "F1", 
           )

survplot

##NO LONGER IN USE----
###for GLMMS
m4<- glmmTMB(Dt_Tadpole_Num ~ pool_type + Week + pH + (1| pool_id), family = poisson, data = pool_2020_terr)

#Water capacity in m4 causing model convergence problems. When fit with only water capacity
#we see that there are sig. quantile deviations. Because we showed that it wasn't a big
#deal above, let's just leave it. 

m5<- glmmTMB(Dt_Tadpole_Num ~ pool_type + Week + pH +
               (1| pool_id), family = nbinom1, data = pool_2020_terr)

m6<- glmmTMB(Dt_Tadpole_Num ~ pool_type + Week + pH+ 
               (1| pool_id), family = nbinom2, data = pool_2020_terr) #best.

m7<- glmmTMB(Dt_Tadpole_Num ~ pool_type + Week + pH+
               (1| pool_id), family = poisson, data = pool_2020_terr)


AIC(m5, m6, m7) #best model m6

drop1(m6, test = "Chi") #drop week 

m6a<- glmmTMB(Dt_Tadpole_Num ~ pool_type + pH+ (1| pool_id), family = nbinom2, data = pool_2020_terr)

drop1(m6a, test = "Chi") #Everything looks good. Let's check the interaction

m6b<- glmmTMB(Dt_Tadpole_Num ~ pool_type * pH+ (1| pool_id), family = nbinom2, data = pool_2020_terr)

drop1(m6b, test = "Chi") #hm. Almost. Let's look at both.

res <- simulateResiduals(m6a, plot = T)  # Quantile deviations detected!
res <- simulateResiduals(m6b, plot = T) 

#Model needs fixing
testOverdispersion(m6a) #yeah, not awesome

m6c<- glmmTMB(Dt_Tadpole_Num ~ pool_type * pH+ (1| pool_id), 
                family = nbinom2, ziformula = ~ pool_type, dispformula = ~ pH, data = pool_2020_terr)


res <- simulateResiduals(m6c, plot = T) 
testOverdispersion(m6c) #better
testZeroInflation(m6c)

summary(m6c)
