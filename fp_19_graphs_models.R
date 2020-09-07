
# FROGPOOL 2019 (ONLY) DATA USED FOR SPECIES COMPARISONS IN 2019. NO DATA HERE IS COMBINED.
# FP 2019 GRAPHING AND MODELS ONLY. 
#########################################################################################

#What is presence by size?
options("scipen"=6)

#First, let's load some packages
library(ggplot2)
library(gridExtra)
library(cowplot)
library(dplyr)
library(tidyverse)
library(corrplot)
library(ggpubr)
library(Hmisc)
###
#For raincloud plot
library(PupillometryR)
library(forcats)

#Now, let's load and tidy up the frogpool 2019 data.

frog<- read.csv("frogpool19.csv", header = T,fileEncoding="UTF-8-BOM") #fromg Andrius

#LET'S TRY THIS UPDATED ONE FROM SEPTEMBER
frog1<- read.csv("frogpool19_v2_reduced_2020-09-03.csv", 
                header = T,fileEncoding="UTF-8-BOM") #fromg Andrius

head(frog1)

frog1 <- frog1 %>%
         mutate(Total_Tads = tp_femo_hahneli + oophagus_tp + tadpoles_tinc)

###############################
#FINAL COMBINED DATA SHEET----
###############################
pools_all<-
  dplyr::full_join(pools_chloe, pools_andri) %>%
  group_by(pool_type)

###############
### SPECIES----
###########
##Now I have to think how to do this for 3 species.
tincpool<- frog1 %>%
          dplyr::select(-oophagus_tp, -tp_femo_hahneli)%>%
          mutate(Tadpoles = tadpoles_tinc,
                  Species = "Tinctorius")%>%
          dplyr::select(height, Tadpoles, Species,
                        pool_type, water_capacity, 
                        leaflit_vol_ml, KH, hardness, no3,
                        salinity, invert_diversity, predator_count,
                        predator_size_sum, amphib_diversity, Total_Tads)%>%
          filter( Tadpoles > 0)


oopool<- frog1 %>%
         dplyr::select(-tadpoles_tinc, -tp_femo_hahneli) %>%
  mutate(Tadpoles = oophagus_tp,
         Species = "Oophagus")%>%
  dplyr::select(height, Tadpoles, Species,
                pool_type, water_capacity, 
                leaflit_vol_ml, KH, hardness, no3,
                salinity, invert_diversity, predator_count,
                predator_size_sum, amphib_diversity, Total_Tads)%>%
  filter( Tadpoles > 0)

femopool<- frog1 %>%
  dplyr::select(-tadpoles_tinc, -oophagus_tp)%>%
  mutate(Tadpoles = tp_femo_hahneli,
         Species = "Femoralis")%>%
  dplyr::select(height, Tadpoles, Species,
                pool_type, water_capacity, 
                leaflit_vol_ml, KH, hardness, no3,
                salinity, invert_diversity, predator_count,
                predator_size_sum, amphib_diversity, Total_Tads)%>%
  filter( Tadpoles > 0)


pools_species<- full_join(tincpool, femopool, 
                          by = c("height", "Species", "Tadpoles", 
                                 "KH", "Total_Tads", "leaflit_vol_ml", 
                                 "salinity", "predator_size_sum", 
                                 "predator_count", "hardness", "pool_type"))

pools_species<- full_join(pools_species, oopool, 
                          by = c("height", "Species", "Tadpoles", 
                                 "KH", "Total_Tads", "leaflit_vol_ml",
                                 "salinity", "predator_size_sum", 
                                 "predator_count", "hardness", "pool_type")) 

pools_species$Species <- as.factor(pools_species$Species)

##Now what if we only consider pools that are terrestrial?
###TERRESTRIAL POOLS ----
######################################

pool_terr <- pools_species %>%
             filter( height < 200)


#First thing is first, let's see what variables are correlated.
#################
#CORRELATION----
###############
#Correlation must be numeric so let's just subset the numeric variables.

numgogo<- gogo %>%
          dplyr::select(- species, - pool_type, - date, - pool_id, - tree, -Leaf_YN) %>%
#There is one row in leaflitter that has an NA that I think is giving the correlation plot
#some trouble. So we are going to go ahead and drop that row. 
          tidyr::drop_na()

numfrog<- frog1 %>%
  dplyr::select(- pool_type, - date, - pool_id, - tree, 
                -pH_strip) %>%
  #There is one row in leaflitter that has an NA that I think is giving the correlation plot
  #some trouble. So we are going to go ahead and drop that row. 
  tidyr::drop_na()

corfrog <- cor(numfrog)
head(round(corfrog, 2))
corrplot(corfrog, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

#Okay, this plot is a little busy, so let's only show significant correlations
corfrog_sig<- rcorr(as.matrix(numfrog), type = "spearman")
M <- corfrog_sig$r
p_mat<-corfrog_sig$P

corrplot(corfrog, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, p.mat= p_mat, sig.level = 0.05, 
         insig = "blank")

?rcorr()
?corrplot ()

################
### HEIGHT ----
###############
#My goodness I love these raincloud plots so fucking much.
#This is with only FP2019 data. Maybe we should combine it with the 2020 data as well.

#Let's relevel to make the graph more intuitive

fct_relevel(pools_species$Species, "Tinctorius", "Oophagus", "Femoralis")

a<- ggplot(pools_species, 
  aes(y = height, x = Species))+
  geom_flat_violin(aes(fill = Species), 
                   position = position_nudge(x = .2, y = 0),
                   alpha = 0.4)+
  geom_point(aes(y = height, color = Species), 
             position = position_jitter(width = .15), 
             alpha = 0.6)+
  theme_bw()+
  expand_limits(x = 3.75) +
  coord_flip()+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff" ))+
  scale_fill_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))+
  theme(legend.position = "none")+
  ylab("Height (cm)")+
  labs(tag = "A")
a
#Just terrestrial pools
b<- ggplot(pools_species, 
       aes(y = height, x = Species))+
  geom_flat_violin(aes(fill = Species), 
                   position = position_nudge(x = .2, y = 0),
                   alpha = 0.4)+
  geom_boxplot(width = .1, aes(fill = Species), 
               outlier.shape = NA, alpha = 0.3) +
  geom_point(aes(y = height, color = Species), 
             position = position_jitter(width = .15), 
             alpha = 0.6)+
  theme_bw()+
  ylim(0, 220)+
  ylab("Terrestrial height (cm)")+
  expand_limits(x = 3.75) +
  coord_flip()+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff" ))+
  scale_fill_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))+
  theme(legend.position = "none")+
  labs(tag = "B")

b
grid.arrange(a, b)

################
### KH ----
###############

ggplot(pools_species, 
       aes(x= KH, y = Total_Tads, color = Species))+
  geom_point(alpha = 0.8, size = 3)+
  stat_smooth(method = "glm", formula = y~ log (x), 
              se = T, fill = "lightgrey", fullrange = F)+
  theme_bw()+ 
  ylim(0, 60)+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))

ggplot(pools_species, 
       aes(x= KH, y = height, color = Species))+
  geom_point(alpha = 0.8, size = 3)+
  stat_smooth(method = "glm", formula = y~ (x), 
              se = T, fill = "lightgrey", fullrange = F)+
  theme_bw()+ 
  ylim(0, 1700)+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))

#KH AND SALINITY----
ggplot(pools_species, 
       aes(x= KH, y = salinity, color = Species))+
  annotate("rect", xmin = 2, xmax = 7, ymin = 0, ymax = 50,
           alpha = .3, fill = "darkgrey", color= "darkgrey",
           linetype ="dashed")+
  annotate("rect", xmin = 2, xmax = 8, ymin = 100, ymax = 250,
           alpha = .3, fill = "#ffd16b", color= "#ffd16b", linetype =
             "dashed")+
  geom_jitter(size = 3, alpha = 0.4)+
  stat_smooth(method = "glm", formula = y~ log (x), 
              se = T, fill = "lightgrey", fullrange = F)+
  ylim(0, 1000)+
  ylab("Salinity (ppm)")+
  theme_bw()+  
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))

################
### LEAF LITTER ----
###############

ggplot(pools_species, 
       aes(x= leaflit_vol_ml, y = Total_Tads, color = Species))+
  annotate("rect", xmin = 0, xmax = 1300, ymin = -Inf, ymax = Inf,
           alpha = .3, fill = "#80e6ff")+
  geom_point(alpha = 0.8, size = 3)+
  stat_smooth(method = "glm", formula = y~ poly(x, 1), 
              se = F, fill = "lightgrey", fullrange = F, 
              size = 0.5)+
  ylim(0, 60)+
  ylab("Tadpole Count")+
  xlab("Leaf litter (vol/mL)")+
  theme_bw()+  
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))

ggplot(pools_species, 
       aes(x= leaflit_vol_ml, y = predator_size_sum, color = Species))+
  stat_smooth(formula = y~ (x), method = "glm",
              se = T, fill = "lightgrey", fullrange = F, 
              size = 0.8)+
  geom_point(alpha = 0.8, size = 3)+
  ylim(0, 150)+
  theme_bw()+  
  geom_vline( xintercept = 1200, linetype = "dashed",
              color = "#80e6ff")+
  geom_vline( xintercept = 50, linetype = "dashed",
              color = "#ffd16b")+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))

ggplot(pools_species, 
       aes(y = leaflit_vol_ml, x = Species))+
  geom_boxplot(width = .1, aes(fill = Species), 
               outlier.shape = NA, alpha = 0.3) +
  geom_flat_violin(aes(fill = Species), 
                   position = position_nudge(x = .2, y = 0),
                   alpha = 0.4) +
  geom_jitter(aes(y = leaflit_vol_ml, color = Species), 
             position = position_jitter(width = .15), 
             alpha = 0.6)+
  theme_bw()+
  coord_flip()+
  ylab(" Leaflitter (vol/mL)")+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff" ))+
  scale_fill_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))+
  theme(legend.position = "none")+
  labs(tag = "C")

################
### Salinity ----
###############

ggplot(pools_species, 
       aes(x= salinity, y = hardness, color = Species))+
  theme_bw()+  
  ylim(0, 25)+
  xlab("Salinity (ppm)")+
  ylab("Hardness")+
  geom_jitter(alpha = 0.8, size = 3, height = 2)+
  stat_smooth(method = "glm", formula = y ~ (x), fill = "lightgrey")+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))

ggplot(pools_species, 
       aes(y = salinity, x = Species))+
  geom_boxplot(width = .1, aes(fill = Species), 
               outlier.shape = NA, alpha = 0.3) +
  geom_flat_violin(aes(fill = Species), 
                   position = position_nudge(x = .2, y = 0),
                   alpha = 0.4) +
  geom_jitter(aes(y = salinity, color = Species), 
              position = position_jitter(width = .15), 
              alpha = 0.6)+
  theme_bw()+
  coord_flip()+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff" ))+
  scale_fill_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))+
  theme(legend.position = "none")+
  labs(tag = "C")

###################
### PREDICTORS GRAPH ALL----
#######################

kh_high<-
  ggplot(pools_species, 
       aes(x= KH, y = height, color = Species))+
  geom_jitter(alpha = 0.8, size = 3, height = 2)+
  stat_smooth(method = "glm", formula = y~ (x), 
              se = T, fill = "lightgrey", fullrange = F)+
  theme_bw()+ 
  theme(legend.position = "none")+
  ylim(0, 1700)+
  xlab("KH")+
  ylab("Height (cm)")+
  geom_vline( xintercept = 6.2, linetype = "dashed",
              color = "#ffd16b")+
  geom_vline( xintercept = 6.4, linetype = "dashed",
              color = "darkgrey")+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))+
  labs(tag = "A")

kh_high


leaf_pred<- 
  ggplot(pools_species, 
       aes(x= leaflit_vol_ml, y = predator_size_sum, color = Species))+
  stat_smooth(formula = y~ (x), method = "glm",
              se = T, fill = "lightgrey", fullrange = F, 
              size = 0.8)+
  geom_point(alpha = 0.8, size = 3)+
  ylim(0, 150)+
  theme_bw()+ 
  theme(legend.position = "none")+
  geom_vline( xintercept = 1200, linetype = "dashed",
              color = "#80e6ff")+
  geom_vline( xintercept = 50, linetype = "dashed",
              color = "#ffd16b")+
  xlab("Leaf litter (vol/mL)")+
  ylab("Cumulative predator size (sum)")+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))+
  labs(tag = "B")

sal_hard<-
  ggplot(pools_species, 
       aes(x= salinity, y = hardness, color = Species))+
  theme_bw()+  
  ylim(0, 25)+
  xlab("Salinity (ppm)")+
  ylab("Hardness")+
  geom_jitter(alpha = 0.8, size = 3, height = 2)+
  stat_smooth(method = "glm", formula = y ~ (x), fill = "lightgrey")+
  geom_vline( xintercept = 240, linetype = "dashed",
              color = "#ffd16b")+
  geom_vline( xintercept = 53, linetype = "dashed",
              color = "darkgrey")+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))+
  labs(tag = "C")

top<-plot_grid(kh_high, leaf_pred)
plot_grid(top, sal_hard, nrow = 2)

##############################
### PREDICTORS GRAPH TERRESTRIAL SUBSET----
#######################

kh_high_terr<-
  ggplot(pool_terr, 
         aes(x= KH, y = height, color = Species))+
  geom_jitter(alpha = 0.8, size = 3, height = 2)+
  stat_smooth(method = "glm", formula = y~ log(x), 
              se = T, fill = "lightgrey", fullrange = F)+
  theme_bw()+ 
  theme(legend.position = "none")+
  xlab("KH")+
  ylab("Height (cm)")+
  ylim(0, 200)+
  geom_vline( xintercept = 6.2, linetype = "dashed",
              color = "#ffd16b")+
  geom_vline( xintercept = 6.4, linetype = "dashed",
              color = "darkgrey")+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))+
  labs(tag = "A")

kh_high_terr

leaf_pred_terr<- 
  ggplot(pool_terr, 
         aes(x= leaflit_vol_ml, y = predator_size_sum, color = Species))+
  stat_smooth(formula = y~ (x), method = "glm",
              se = T, fill = "lightgrey", fullrange = F, 
              size = 0.8)+
  geom_point(alpha = 0.8, size = 3)+
  ylim(0, 150)+
  theme_bw()+ 
  theme(legend.position = "none")+
  geom_vline( xintercept = 1200, linetype = "dashed",
              color = "#80e6ff")+
  geom_vline( xintercept = 50, linetype = "dashed",
              color = "#ffd16b")+
  xlab("Leaf litter (vol/mL)")+
  ylab("Cumulative predator size (sum)")+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))+
  labs(tag = "B")

leaf_pred_terr

sal_hard_terr<-
  ggplot(pool_terr, 
         aes(x= salinity, y = hardness, color = Species))+
  theme_bw()+  
  ylim(0, 25)+
  xlab("Salinity (ppm)")+
  ylab("Hardness")+
  geom_jitter(alpha = 0.8, size = 3, height = 2)+
  stat_smooth(method = "glm", formula = y ~ (x), fill = "lightgrey")+
  geom_vline( xintercept = 240, linetype = "dashed",
              color = "#ffd16b")+
  geom_vline( xintercept = 53, linetype = "dashed",
              color = "darkgrey")+
  scale_color_manual( values = c("darkgrey", "#ffd16b", "#80e6ff"))+
  labs(tag = "C")

sal_hard_terr
