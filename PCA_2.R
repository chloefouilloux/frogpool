#First load some packages.
library(dplyr)
library(AICcmodavg)
library(sjPlot)
library(MASS)
library(ggplot2)
library(vegan)
library(ggfortify)
library(gridExtra)

# Load
library(FactoMineR)
library(factoextra)
library(ISLR)
#let's make some cute tables!
library(gt)

#### DATA----
#Let's load in that sweet, sweet 2019 data.
#LET'S TRY THIS UPDATED ONE FROM SEPTEMBER
frog1<- read.csv("frogpool19_v2_reduced_2020-09-03.csv", 
                 header = T,fileEncoding="UTF-8-BOM") #fromg Andrius

################
#Tidy Data----
###############
#Correlation must be numeric so let's just subset the numeric variables.

numfrog<- frog1 %>%
  mutate(total_other = oophagus_tp + tp_femo_hahneli,
         other_y_n = ifelse (oophagus_tp > 0, 1, 
                             ifelse(tp_femo_hahneli > 0, 1, 0)),
         tinc_Y_N = ifelse( tadpoles_tinc > 0, 1, 0),
         species = ifelse (tp_femo_hahneli> 0, "Femoralis", 
                           ifelse(oophagus_tp >0, "Oophagus", "Tinctorius")), 
         species = as.factor(species)) %>%
  #There is one row in leaflitter that has an NA that I think is giving the correlation plot
  #some trouble. So we are going to go ahead and drop that row. 
  tidyr::drop_na()

numfrog1<- numfrog %>%
  dplyr::select(- pool_type, - date, - pool_id, - tree, 
                -pH_strip, -tadpoles_other, -species, -other_y_n) #remove cat. factors


random_frog<- numfrog1 %>%
  dplyr::select(-tinc_Y_N, -tadpoles_tinc) #remove response variable here

##################
#### PCA ---- ####
################
#should all my variables be here? 
#slightly doctored.
pca<- prcomp(numfrog1[,-c(15, 18)], scale = T, center = T) #remove all tinctorius data
summary(pca)
#dissect the PCA for indexing
loadings<- pca$rotation #correlation or anticorrelation, columns are eigenvectors
loadings
loadings1<-loadings[, 1:3] #only take the loadings of the first three vectors
loadings1<-as_tibble(loadings1)

#get eigenvalues for the first 3 pca's
vals3 <- eigenvals(pca)[1:3]
vals3

######## ######## ######## ########
# Let's make a loadings table!----
######## ######## ######## ########
loadings2<- loadings1
loadings2$Variable<- c("height", "water_capacity", "leaflit", "KH",
                       "Hardness", "no3", "salinity", "invert_div",
                       "dens_invert_tot", "pred_count", "pred_size_sum",
                       "amphib_div", "femo", "oophagus", "presence_by_size", 
                       "total_other")

col_order <- c("Variable", "PC1", "PC2", "PC3")
loadings2 <- loadings2[, col_order]
loadings2<- as_tibble(loadings2)

loadings2 %>%
  gt()%>%
  cols_label(
    Variable= md("**Variable**"),
    PC1 = md("**PC1**"),
    PC2= md("**PC2**"),
    PC3= md("**PC3**")) %>%
  tab_style(
    style = list(
      cell_fill(color = "#DeF7E9"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(PC1), # not needed if coloring all columns
      rows = c(4, 5, 6, 7, 8, 10, 13)))%>%
  fmt_number(
    columns = vars(PC1, PC2, PC3),
    decimals = 3) %>%
  tab_style(
    style = list(
      cell_fill(color = "#DeF7E9"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(PC2), 
      rows = c(1, 2, 3, 11)))%>%
  tab_style(
    style = list(
      cell_fill(color = "#DeF7E9"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(PC3), 
      rows = c(9, 12, 14, 15, 16)))

                ######## ######## ######## ########
                          # PCA MODELS ----
                ######## ######## ######## ########
#first we have to get all this data together
#remember our observation based PCA obs_PCA
dataset_obj = random_frog
obs_PCA = prcomp(dataset_obj,scale=T, center = T)
summary(obs_PCA)
axes <- predict(obs_PCA, newdata = numfrog) #does this just extract pca results?
head(axes, 4)

dat <- cbind(numfrog, axes)
dat

########
#----modelling
#########
m1<-glm(tadpoles_tinc ~ PC1 + PC2 + PC3, data = dat, family = "poisson") #for tinc count
m2<-glm.nb(tadpoles_tinc ~ PC1 + PC2 + PC3, data = dat)

#trying interactions
m3<-glm(tadpoles_tinc ~ PC1 * PC3 + PC2, data = dat, family = "poisson") #for tinc count
m4<-glm(tadpoles_tinc ~ PC1 * PC2 + PC3, data = dat, family = "poisson") #for tinc count
m5<-glm(tadpoles_tinc ~ PC3 * PC2 + PC1, data = dat, family = "poisson") #for tinc count

m6<-glm.nb(tadpoles_tinc ~ PC1 * PC2 + PC3, data = dat)
m7<-glm.nb(tadpoles_tinc ~ PC1 * PC3 + PC2, data = dat)
m8<-glm.nb(tadpoles_tinc ~ PC2 * PC3 + PC1, data = dat)
m9<-glm.nb(tadpoles_tinc ~ PC2 * PC3 * PC1, data = dat)


#AIC MODEL SELECTION----
aic<-AIC(m1,
         m2, 
         m3, 
         m4, 
         m5, 
         m6, 
         m7, 
         m8
) 
aic[order(aic$AIC),] 
#AIC SUMMARY TABLE---- For negative binomials (need same model class)
aic.cand<- list( 
                m2, 
                m6, 
                m7, 
                m8,
                m9)
Cand.names <- c( "m2",
                 "m6", 
                 "m7", "m8", "m9")
aictab(aic.cand, Cand.names, sort = T)

summary(m2)

######################## ########### ###########
#### Index of Loadings (original data) ---- ####
################ ########### ########### ###########
#ILij = uij^2 * lambai^2
#where uij is jth **loading** of the ith **eigenvector**
#saved in "loadings" for first 3 components
### Function for permuting a data frame by swapping values in a column ###
data_permute = function(x){
  for (i in 1:ncol(x)){
    x[,i]=base::sample(x[,i],size=length(x[,i]),replace=F)}
  return(x)
}

### IL function for prcomp ###
# From Björklund (2019)
get_IL = function(x,num_PCs){
  eigs = ((x$sdev^2)^2)[1:num_PCs] 
  loads = ((data.frame(x$rotation))^2)[1:num_PCs]
  ILs = apply(loads,MARGIN = 1, FUN = function(x){x*eigs})
  return(ILs)
}

### Funtion list to array ###
list2ary = function(input.list){  #input a list of lists
  rows.cols <- dim(input.list[[1]])
  sheets <- length(input.list)
  output.ary <- array(unlist(input.list), dim = c(rows.cols, sheets))
  colnames(output.ary) <- colnames(input.list[[1]])
  row.names(output.ary) <- row.names(input.list[[1]])
  return(output.ary)    # output as a 3-D array
}
##############################################################################

### SET UP ###################################################################

#Alright here we're taking random_frog, because we've subsetted all of the tinc data out of it.
dataset_obj = random_frog
PCs_to_retain = 3
p_value_correction_method = "BH" 
#holm, bonferroni, hochberg, hommel, BY all attempted. 
number_randomizations = 1000

##############################################################################

### ANALYSIS #################################################################

# True IL

obs_PCA = prcomp(dataset_obj,scale=T, center = T)
summary(obs_PCA) #okay, identical to pca from original
IL_obs = get_IL(obs_PCA,PCs_to_retain) #this is the idex loading from our original pca (only first three components)

# Nulls ILs #Let's randomize this bitch!

nulls = list()
for (i in 1:number_randomizations){
  nulls[[i]] = prcomp(data_permute(dataset_obj),scale=T, center = T)
}


IL_nulls = lapply(nulls,get_IL,num_PCs=PCs_to_retain )
IL_nulls_array = list2ary(IL_nulls) # Save this to calculate p-values

pvalues_out = expand.grid(PC = rownames(IL_nulls_array[,,1]), trait = colnames(IL_nulls_array[,,1]))
pvals = c()

for (i in 1:nrow(pvalues_out)){
  trait = rownames(IL_nulls_array[,,1])==pvalues_out[i,1]
  PC = colnames(IL_nulls_array[,,1])==pvalues_out[i,2]
  null_vals = IL_nulls_array[trait,PC,]
  
  obs_val = IL_obs[trait,PC]
  
  pvals[[i]] = mean(null_vals >= obs_val)
}


pvalues_out$p_values = pvals
pvalues_out$p_values_adj = p.adjust(pvals,method=p_value_correction_method) 
# adjust p values this is with BH may be a little intense
pvalues_out$p_values_adj_1 = p.adjust(pvals,method=p_value_correction_method1) 


pvalues_out = pvalues_out[order(pvalues_out$PC),] ### THIS IS THE DATAFRAME WITH PVALUES
pvalues_sig <- pvalues_out %>%
              filter(p_values_adj <= 0.05)

#gotta deal with these pvalues of less than 0
pvalues_sig %>%
  gt() %>%
  cols_align(
    align = "left") %>%
  cols_label(
    PC= md("**PC**"),
    trait = md("**Trait**"),
    p_values= md("**PVal**"),
    p_values_adj= md("**P-adj (BH)**")) %>%
   #fmt_scientific(
    #columns = vars(p_values_adj),
    #decimals = 2) %>%
  fmt_number(
    columns = vars(p_values_adj),
    decimals = 3) %>%
   tab_style(
    style = list(
      cell_fill(color = "#DeF7E9")),
    locations = cells_body(
      columns = vars(PC), # not needed if coloring all columns
      rows = c(1:5))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#ffe099")),
    locations = cells_body(
      columns = vars(PC), # not needed if coloring all columns
      rows = c(6:10))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#d6fbff")),
    locations = cells_body(
      columns = vars(PC), # not needed if coloring all columns
      rows = c(11: 12)))


##############################################################################

### PLOTTING ################################################################

# custom quantiles function for making ggplot boxplot
f = function(x) {
  r = quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 0.95))
  names(r) = c("ymin", "lower", "middle", "upper", "ymax")
  r
  
}

# create data frame for ggplotting

#from randomized data
IL_nulls_plot = data.frame(do.call(rbind,IL_nulls))
IL_nulls_plot$trait = rownames(IL_nulls_plot)
IL_nulls_plot = tidyr::gather(IL_nulls_plot,"PC","IL_score",1:(ncol(IL_nulls_plot)-1)) # 1 to number of PCs (last column with trait names should be excluded)

IL_nulls_PC1 <- IL_nulls_plot[grep("PC1", IL_nulls_plot$trait), ]
IL_nulls_PC1 <- IL_nulls_PC1 %>%
                rename("PC1" = trait ,
                       "Trait" = PC) 

IL_nulls_PC2 <- IL_nulls_plot[grep("PC2", IL_nulls_plot$trait), ]
IL_nulls_PC2 <- IL_nulls_PC2 %>%
  rename("PC2" = trait ,
         "Trait" = PC) 

IL_nulls_PC3 <- IL_nulls_plot[grep("PC3", IL_nulls_plot$trait), ]
IL_nulls_PC3 <- IL_nulls_PC3 %>%
  rename("PC3" = trait ,
         "Trait" = PC) 


#real observed data
IL_obs = data.frame(PC = rownames(IL_obs),IL_obs)
IL_obs_plot = tidyr::gather(IL_obs,"trait","IL_score",2:ncol(IL_obs))
IL_obs_PC1<- IL_obs_plot %>%
         filter(PC == "PC1")

IL_obs_PC2<- IL_obs_plot %>%
  filter(PC == "PC2")

IL_obs_PC3<- IL_obs_plot %>%
  filter(PC == "PC3")

#FOR PC1
ggplot() +
  stat_summary(data= IL_nulls_PC1, fun.data = f, geom="boxplot",aes(y= IL_score,x= Trait))+ #random 95box
  geom_point(data=IL_obs_PC1, aes(x = trait, y= IL_score), color = "darkgreen", shape = 4, size = 3)+
  ylab("Index Loading for PC1")+
  theme_bw()

#FOR PC2
ggplot() +
  stat_summary(data= IL_nulls_PC2, fun.data = f, geom="boxplot",aes(y= IL_score,x= Trait))+ #random 95box
  geom_point(data=IL_obs_PC2, aes(x = trait, y= IL_score), color = "darkgreen", shape = 4, size = 3)+
  ylab("Index Loading for PC2")+
  theme_bw()

#FOR PC3
ggplot() +
  stat_summary(data= IL_nulls_PC3, fun.data = f, geom="boxplot",aes(y= IL_score,x= Trait))+ #random 95box
  geom_point(data=IL_obs_PC3, aes(x = trait, y= IL_score), color = "darkgreen", shape = 4, size = 3)+
  ylab("Index Loading for PC3")+
  theme_bw()
