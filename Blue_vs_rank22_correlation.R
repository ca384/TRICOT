# packages
packages_used <- c("workflowr","tidyverse", "ggplot2", "here","climatrends" ,"tidyverse", "PlackettLuce","gosset", "patchwork","qvcalc","ggparty","igraph", "ClimMobTools", "multcompView","ggplot2", "gtools","remotes","here", "ggpubr", "psych")

#install.packages(c("agricolae"))
ip <- installed.packages()
all_packages_installed <- TRUE
for (package in packages_used){
  if (!(package %in% ip[,"Package"])){
    print(paste("Please install package", package))
    all_packages_installed <- FALSE
  } else {
    library(package, character.only = T) # load required package
  } # end else statement
}#END packages_used
if (!all_packages_installed) stop("Need to install packages")



#using the 3 9MAP traits labels =  "Overall_impression",  "Canopy", "Branching")
trait_list5 = list(c("Best_overimpr9MAP","Worst_overimpr9MAP"),
                   c("Best_weed9MAP","Worst_weed9MAP"),
                   c("Best_branch9MAP","Worst_branch9MAP"))


tricot_rank_ouptput9 = list() #
for(trt in 1:length(trait_list5)){
  trt1 = trait_list5[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R_tri <- rank_tricot(data = Rank22a,
                       items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                       input = trt1)
  tricot_rank_ouptput9[[trt]] <- R_tri
}


tricot_rank_model9 = list()#
for(trt in 1:length(tricot_rank_ouptput9)){
  trt1 = tricot_rank_ouptput9[[trt]]

  mod_R_1p <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank_model9[[trt]] <- mod_R_1p
}

do.call(rbind,tricot_rank_model9)
tricot_rank_model9 = tricot_rank_model9
tricot_rank_model9[[trt]]$coefficients

tricot_rank_model9[[trt]]$coefficients
tricot_rank_model_sum9 = list()

for(i in 1:length(tricot_rank_model9)){
 trdat =  tricot_rank_model9[[i]]$coefficients
 tileng = which(names(trdat) %in% "tie2")

 if(length(tileng) == 0 ){
   trdat1 = trdat
 }else{
   trdat1 = trdat[-tileng]
 }
tricot_rank_model_sum9[[i]] <- trdat1
}

tricot_sumMean9 = do.call(cbind, tricot_rank_model_sum9)
dim(tricot_sumMean9)
tricot_sumMean9 <- as.data.frame(tricot_sumMean9) # tricot worth at 9MAP

tricot_sumMean9 <- tricot_sumMean9 %>%
 dplyr:: rename(Overall_impression = V1,   Canopy= V2, Branching= V3)

#### tricot worth at harvest
trait_list6 = list(c("Best_HvOvi12", "Worst_HvOvi12"),
                   c("Best_HvYield12", "Worst_HvYield12"))


tricot_rank_ouptput_H = list() #
for(trt in 1:length(trait_list6)){
  trt1 = trait_list6[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R_tri <- rank_tricot(data = Rank22a,
                       items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                       input = trt1)
  tricot_rank_ouptput_H[[trt]] <- R_tri
}


tricot_rank_model_H = list()#
for(trt in 1:length(tricot_rank_ouptput_H)){
  trt1 = tricot_rank_ouptput_H[[trt]]

  mod_R_1p <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank_model_H[[trt]] <- mod_R_1p
}

do.call(rbind,tricot_rank_model_H)
tricot_rank_model_H = tricot_rank_model_H
tricot_rank_model_H[[trt]]$coefficients

tricot_rank_model_sum_H = list()

for(i in 1:length(tricot_rank_model_H)){
  trdat =  tricot_rank_model_H[[i]]$coefficients
  tileng = which(names(trdat) %in% "tie2")

  if(length(tileng) == 0 ){
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng]
  }
  tricot_rank_model_sum_H[[i]] <- trdat1
}

tricot_sumMean_H = do.call(cbind, tricot_rank_model_sum_H)
dim(tricot_sumMean_H)
tricot_sumMean_H <- as.data.frame(tricot_sumMean_H) # tricot worth at harvest
tricot_sumMean_H <- tricot_sumMean_H %>%
 dplyr:: rename(Overall_impression_H = V1, Yield = V2)

tricot_sumMean_H
tricot_sumMean9
harvest_9map_rank<- round(cor(tricot_sumMean9, tricot_sumMean_H), 2)# correlating ranking at 9map and harvest

write.csv(harvest_9map_rank,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/pearson_correlation_harvest_9map_rank.csv" )

tricot_sumMean_H
tricot_sumMean9
harvest_9map_worth<-cbind(tricot_sumMean9,tricot_sumMean_H )

# Blupmean_9wide = Blupmean_9wide[order(Blupmean_9wide$Genotype),]
# data.frame(rownames(Blupmean_9wide),rownames(tricot_sumMean9))
# tricot_sumMean9 = tricot_sumMean9[order(rownames(tricot_sumMean9)),]
# tricot_blup_meancomb9 = data.frame(Blupmean_9wide,tricot_sumMean9)
# dim(tricot_blup_meancomb9)
# head(tricot_blup_meancomb9)
# cor(tricot_blup_meancomb9[,-1])

# library(agricolae)
# crr <- correlation(tricot_blup_meancomb[,-1])
# crr$correlation
# crr$pvalue

# using blups of numeric data from Modelling_GXE_Tricot.R
numeric_Blup<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/onfarm_BLUP_mean") # the BLUP means for onfarm numeric data
colnames(numeric_Blup)

num_Blup<-numeric_Blup %>%
  dplyr::select( "yield_per_plot", "shwt_plot" , "mean_brnch_ht" , "mean_ht",
                 "root_num_plot")

colnames(num_Blup)
num_Blup<-num_Blup %>%
  dplyr::rename( yield_per_N = yield_per_plot ,  root_num_N = root_num_plot, shwt_plot_N = shwt_plot,
                 mean_brnch_ht_N= mean_brnch_ht, mean_ht_N= mean_ht,)



harvest_9map_worth_sel<-harvest_9map_worth %>%
  dplyr::select("Canopy",
               "Branching" , "Overall_impression_H",
                "Yield")

harvest_9map_worth_sel<-harvest_9map_worth %>%
  dplyr::rename(Canopy_9MAP_R = Canopy, Branching_9MAP_R= Branching,
                Harvest_Ov_impr_R= Overall_impression_H,Yield_R= Yield)


worth_Blup<-round(cor(harvest_9map_worth_sel, num_Blup),2) # correlation of the worth of. the ranking at 9
                                    #MAP and harvest with the Blup from on farm numeric data

library(corrplot)
rank_blup_cor <- corTest(harvest_9map_worth_sel, num_Blup)

rank_blup_cor$r = round(rank_blup_cor$r, 2)
rank_blup_cor$r
rank_blup_cor$p




corrplot::corrplot(corr = rank_blup_cor$r ,tl.cex = 0.6,p.mat = rank_blup_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
#
write.csv(worth_Blup,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/correlation_9MAP_Harvest_worth_Harvest_Blup.csv")

#####################################

# correlation of processing BLUPs

Process_Blup <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/processing_BLUP_mean" ) # the BLUP means for onfarm numeric data

trait_list7 = list(c("Best_GapOvi12","Worst_GapOvi12"),
                   c("Best_HvWodyfil12" , "Worst_HvWodyfil12"),
                   c("Best_EaseP12" , "Worst_EaseP12"),
                   c("Best_GarEaseP12" , "Worst_GarEaseP12"),
                   c("Best_Garmash12" , "Worst_Garmash12"),
                   c("Best_Waterelz12","Worst_Waterelz12"),
                   c("Best_SwellMash12", "Worst_SwellMash12"),
                   c("Best_Gayield12" , "Worst_Gayield12")      )

tricot_rank_ouptput_PQ = list() #
for(trt in 1:length(trait_list7)){
  trt1 = trait_list7[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R_tri <- rank_tricot(data = Rank22a,
                       items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                       input = trt1)
  tricot_rank_ouptput_PQ[[trt]] <- R_tri
}

tricot_rank_model_PQ = list()#
for(trt in 1:length(tricot_rank_ouptput_PQ)){
  trt1 = tricot_rank_ouptput_PQ[[trt]]

  mod_R_1p <- PlackettLuce(trt1, npseudo = 0.5)
  tricot_rank_model_PQ[[trt]] <- mod_R_1p
}

do.call(rbind,tricot_rank_model_PQ)
tricot_rank_model_PQ = tricot_rank_model_PQ
tricot_rank_model_PQ[[5]]$coefficients

tricot_rank_model_sum_PQ = list()

for(i in 1:length(tricot_rank_model_PQ)){
  trdat_pq =  tricot_rank_model_PQ[[i]]$coefficients
  tileng = which(names(trdat_pq) %in% "tie2")

  if(length(tileng) == 0 ){
    trdat1 = trdat_pq
  }else{
    trdat1 = trdat_pq[-tileng]
  }
  tricot_rank_model_sum_PQ[[i]] <- trdat1
}

tricot_sumMean_PQ = do.call(cbind, tricot_rank_model_sum_PQ)
dim(tricot_sumMean_PQ)
tricot_sumMean_PQ <- as.data.frame(tricot_sumMean_PQ) # tricot worth at harvest

# name for each model
labels =  c("Overall_impres","Woodiness", "ease_peeling", "Ease_processing", "Mash_quality", "Water_content", "Mash_swell", "garri_yield")
tricot_sumMean_PQ <- tricot_sumMean_PQ %>%
  dplyr:: rename(Overall_impres_R = V1, Woodiness_R = V2,  ease_peeling_R = V3, Ease_proc_R = V4, Mash_quality_R = V5, Water_content_R = V6, Mash_swell_R= V7, garri_yield_R = V8)

worth_Process<-round(tricot_sumMean_PQ,2) # correlation of the worth of. the ranking at 9

write.csv(worth_Process,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Worth_processing.csv" )

Process_Blup <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/processing_BLUP_mean" ) # the BLUP means for onfarm numeric data
worth_process <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Worth_processing" ) # the BLUP means for onfarm numeric data

Process_Blup_sel<-Process_Blup %>%
  dplyr::select("chaff_wt_gm", "final_temp_F", "grating_time_min",
                "initial_temp_F", "mid_temp_F", "peeling_time_Min",
                "sieve_time_min", "toast_time_min", "wt_garri_kg")

cor(Process_Blup_sel[-c(1,2)], worth_process[-1] )

Proc_rank_blup_cor <- corTest(Process_Blup_sel[-c(1,2)], worth_process[-1])

Proc_rank_blup_cor$r = round(Proc_rank_blup_cor$r, 2)
Proc_rank_blup_cor$p
Proc_rank_blup_cor$p.adj
dim(Proc_rank_blup_cor$r)


corrplot::corrplot(corr = Proc_rank_blup_cor$r ,tl.cex = 0.6,p.mat = Proc_rank_blup_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

write.csv(worth_Blup,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/correlation_9MAP_Harvest_worth_Harvest_Blup.csv")



library(PlackettLuce)
library(gosset)
########################
#correlating JAR JAR BLUP and rankings
JAR_BLUP <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/JAR_BLUP_mean") # the BLUP means for onfarm numeric data


trait_list9 = list( c( "Best_EbaMould12" , "Worst_EbaMould12"),   c("Best_EbaSoft12" , "Worst_EbaSoft12"),
                   c("Best_EbaDraw12" , "Worst_EbaDraw12"),    c("Best_EbaSmooth12" ,"Worst_EbaSmooth12"),
                   c("Best_Ebacolour12" , "Worst_Ebacolour12"),  c("Best_EbaNstick12" , "Worst_EbaNstick12"),
                   c("Best_Ebaswell12" , "Worst_Ebaswell12"),  c("Best_EbaTaste12" , "Worst_EbaTaste12"),
                   c("Best_EbaOvi12", "Worst_EbaOvi12"))

tricot_rank_ouptput_EQ = list() #
for(trt in 1:length(trait_list9)){
  trt1 = trait_list9[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R_tri <- rank_tricot(data = Rank22a,
                       items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                       input = trt1)
  tricot_rank_ouptput_EQ[[trt]] <- R_tri
}

tricot_rank_model_EQ = list()#
for(trt in 1:length(tricot_rank_ouptput_EQ)){
  trt1 = tricot_rank_ouptput_EQ[[trt]]

  mod_R_1p <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank_model_EQ[[trt]] <- mod_R_1p
}


do.call(rbind,tricot_rank_model_EQ)
tricot_rank_model_EQ = tricot_rank_model_EQ
tricot_rank_model_EQ[[trt]]$coefficients

tricot_rank_model_sum_EQ1 = list()

for(i in 1:length(tricot_rank_model_EQ)){
  trdat_J =  tricot_rank_model_EQ[[i]]$coefficients
  tileng = which(names(trdat_J) %in% c("tie2",'tie3'))

  if(length(tileng) == 0 ){
    trdat1 = trdat_J
  }else{
    trdat1 = trdat_J[-tileng]
  }
  tricot_rank_model_sum_EQ1[[i]] <- trdat1
}

tricot_sumMean_EQ = do.call(cbind, tricot_rank_model_sum_EQ1)
dim(tricot_sumMean_EQ)
tricot_sumMean_EQ <- as.data.frame(tricot_sumMean_EQ) # tricot worth at harvest
tricot_sumMean_EQ <- tricot_sumMean_EQ %>%

  dplyr:: rename( "Mouldability_R"="V1" , "Softness_R"="V2", "Strechiness_R"="V3" , "Smoothness_R"="V4",
                 "color_R"= "V5", "Stickiness_R"= "V6", "Swelling_R" ="V7", "Taste_R"= "V8","Eba_Overall_impression_R"= "V9")

tricot_sumMean_EQ# eba quality rank


write.csv(tricot_sumMean_EQ,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/eba_quality_worth_estimate.csv" )

JAR_BLUP <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/JAR_BLUP_mean" ) # the BLUP means for onfarm numeric data
tricot_sumMean_EQ <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/eba_quality_worth_estimate.csv" )
tricot_sumMean_EQ_1 <- tricot_sumMean_EQ %>%
  dplyr::select("Mouldability_R","Smoothness_R","Stickiness_R" , "color_R",
                "Swelling_R", "Strechiness_R", "Eba_Overall_impression_R")
cor(JAR_BLUP[-c(1,2)], tricot_sumMean_EQ_1 )
JAR_BLUP1= JAR_BLUP%>%
  dplyr::select("mouldability","Smoothness","stickiness",
                 "color")%>%
  dplyr::rename("Mouldability_N"="mouldability", "Smoothness_N"="Smoothness" ,
                "Stickiness_N"= "stickiness" ,"color_N"= "color")


Eba_quality_rank_blup_cor <- corTest(JAR_BLUP1, tricot_sumMean_EQ_1)

Eba_quality_rank_blup_cor$r = round(Eba_quality_rank_blup_cor$r, 2)
Eba_quality_rank_blup_cor$p
Eba_quality_rank_blup_cor$p.adj
Eba_quality_rank_blup_cor$p


corrplot::corrplot(corr = Eba_quality_rank_blup_cor$r ,tl.cex = 0.6,p.mat = Eba_quality_rank_blup_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))





#worth_map(tricot_rank_model_EQ, labels,size=4) + theme(axis.text.x = element_text(angle = 90))

#Jar with tpa correlation
JAR_BLUP <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/JAR_BLUP_mean" ) # the BLUP means for onfarm numeric data

TPA_BLUP<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/TPA_mean.csv" ) # the BLUP means for onfarm numeric data
#cor(JAR_BLUP[-c(1,2)], TPA_BLUP[-c(1,2)] )
JAR_BLUP1= JAR_BLUP%>%
  dplyr::select("mouldability","Smoothness","stickiness")%>%
  dplyr::rename("Mouldability_J"="mouldability", "Smoothness_J"="Smoothness" ,
                "Stickiness_J"= "stickiness")


JAR_TPA_blup_cor <- corTest(JAR_BLUP1,TPA_BLUP[-c(1,2,4, 8)])

JAR_TPA_blup_cor$r = round(JAR_TPA_blup_cor$r, 2)
JAR_TPA_blup_cor$p
JAR_TPA_blup_cor$p.adj
JAR_TPA_blup_cor$p


corrplot::corrplot(corr = JAR_TPA_blup_cor$r ,tl.cex = 0.6,p.mat = JAR_TPA_blup_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))


#####TPA versus ranking
cor(TPA_BLUP[-c(1,2,4,8)], tricot_sumMean_EQ_1 )


TPA_rank_blup_cor <- corTest(TPA_BLUP[-c(1,2,4,8)], tricot_sumMean_EQ_1)

TPA_rank_blup_cor$r = round(TPA_rank_blup_cor$r, 2)
TPA_rank_blup_cor$p
TPA_rank_blup_cor$p.adj
TPA_rank_blup_cor$p


corrplot::corrplot(corr = TPA_rank_blup_cor$r ,tl.cex = 0.6,p.mat = TPA_rank_blup_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

# importing blues of the chemical and functional properties of the cassava roots
Bluemean_chem_wide = read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_chem_Blue.csv",row.names = 1) # rownames =1 removes the column that has row names as number

head(Bluemean_chem_wide)

cor(Bluemean_chem_wide[,-1])

trace(corrplot, edit=TRUE)
library(psych)
chem_Blue_TPA_blup_cor <- corTest(TPA_BLUP[-c(1,2,4,8)], Bluemean_chem_wide[,-1])

chem_Blue_TPA_blup_cor$r = round(chem_Blue_TPA_blup_cor$r, 2)
chem_Blue_TPA_blup_cor$p
chem_Blue_TPA_blup_cor$p.adj
chem_Blue_TPA_blup_cor$p


corrplot::corrplot(corr = chem_Blue_TPA_blup_cor$r ,tl.cex = 0.6,p.mat = chem_Blue_TPA_blup_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
#Redo this was chem blues with TPA blup

#chem BLUES and Rank
#cor(tricot_sumMean_EQ_1, Bluemean_chem_wide[,-1])


chem_Blue_rank_sensory_cor <- corTest(tricot_sumMean_EQ_1, Bluemean_chem_wide[,-1])

chem_Blue_rank_sensory_cor$r = round(chem_Blue_rank_sensory_cor$r, 2)
chem_Blue_rank_sensory_cor$p
chem_Blue_rank_sensory_cor$p.adj
chem_Blue_rank_sensory_cor$p


corrplot::corrplot(corr = chem_Blue_rank_sensory_cor$r ,tl.cex = 0.6,p.mat = chem_Blue_rank_sensory_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))


# correlation between chemical parameters

cor(Bluemean_chem_wide[,-1])

chem_cor <- corTest(Bluemean_chem_wide[,-1])

chem_cor$r = round(chem_cor$r, 2)
chem_cor$p
chem_cor$p.adj
chem_cor$p


#corrplot::corrplot(corr = chem_cor$r ,tl.cex = 0.6,p.mat = chem_cor$p, method = 'color',addCoef.col ='black',
                #   sig.level = c(0.001,0.01, 0.05),
                #  insig = "label_sig", pch.cex = 0.7,
                #  number.cex = 0.7,
                #  number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

pairs.panels(Bluemean_chem_wide[,-1],   # plot distributions and correlations for all the data
             gap = 0,
             pch = ".",
             cex = 1.5,
             lm = TRUE,
             ellipses=FALSE,stars = TRUE, method = "pearson")

##############################
#  overall quality  traits for eba fresh, overnight and gari with chemical and TPA
Rank22a =  Rank22a[-17,]



trait_list_ALL_Quality = list(c("Best_GaqOvi12" , "Worst_GaqOvi12"),
                      c("Best_EbaOvi12", "Worst_EbaOvi12"),
                      c("Best_EbaAFOvi12", "Worst_EbaAFOvi12"),
                      c("Best_replant12" , "Worst_replant12"))

tricot_rank_output_ALL_Quality = list() #
for(trt in 1:length(trait_list_ALL_Quality)){
  trt1 = trait_list_ALL_Quality[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R_ALL_Q <- rank_tricot(data = Rank22a,
                        items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                        input = trt1)
  tricot_rank_output_ALL_Quality[[trt]] <-  R_ALL_Q
}


tricot_rank_model_ALL_Quality = list()#
for(trt in 1:length(tricot_rank_output_ALL_Quality)){
  trt1 = tricot_rank_output_ALL_Quality[[trt]]
  mod_R_ALL_Q <- PlackettLuce(trt1, npseudo = 0.5)
  tricot_rank_model_ALL_Quality[[trt]] <- mod_R_ALL_Q
}

tricot_rank_model_ALL_Quality1 = list()
for(i in 1:length(tricot_rank_model_ALL_Quality)){
  trdat =  tricot_rank_model_ALL_Quality[[i]]$
  tileng = which(names(trdat) %in% "tie3")

  if(length(tileng) == 0 ){
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng]
  }
  tricot_rank_model_ALL_Quality1[[i]] <- trdat1
}


tricot_sumMean_Quality_All = do.call(cbind, tricot_rank_model_ALL_Quality1)
dim(tricot_sumMean_Quality_All)
tricot_sumMean_Quality_All<- as.data.frame(tricot_sumMean_Quality_All) # tricot worth at harvest

Traitnames <- c("Overall Garri Quality", "Overall Fresh Eba Quality", "Overall Overnight Eba quality","Replant")

colnames(tricot_sumMean_Quality_All) <- Traitnames

chem_blues_22<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_chem_Blue.csv" ) # the BLUE means for onfarm numeric data


#Tratnames_chem<-c("varieties",  "Amylose_ch" , "Amylose_garri", "Bulk_Density",
                   #  "Crude Fiber_ch" ,"Crude_Fiber_Garri" , "DMC", "SI", "SP","Starch_ch", "Starch_garri", "Sugar_garri", "WAC" )
chem_blues_22<-chem_blues_22[,-1]
#colnames(chem_blues_22) <- Traitnames_chem
head(chem_blues_22)
rownames(tricot_sumMean_Quality_All)[which(!rownames(tricot_sumMean_Quality_All) %in% chem_blues_22$varieties)]

idrw_All_q <- which(rownames(tricot_sumMean_Quality_All) %in% chem_blues_22$varieties) #looking for missing genotypes
tricot_sumMean_Quality_All_Sel <- tricot_sumMean_Quality_All[idrw_All_q,]
idrw_chem <- which(chem_blues_22$varieties %in% rownames(tricot_sumMean_Quality_All_Sel)) #looking for missing genotypes
length(idrw_chem)
chem_blues_22_sel <- chem_blues_22[idrw_chem,]
dim(chem_blues_22_sel)
dim(tricot_sumMean_Quality_All_Sel)

chem_blues_overall_quality_cor <- corTest(tricot_sumMean_Quality_All_Sel, chem_blues_22_sel[,-c(1)])

chem_blues_overall_quality_cor$r = round(chem_blues_overall_quality_cor$r, 2)
chem_blues_overall_quality_cor$r
chem_blues_overall_quality_cor$p

corrplot::corrplot(corr = chem_blues_overall_quality_cor$r ,tl.cex = 0.6,p.mat = chem_blues_overall_quality_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

trace(corrplot, edit = T)




##################################
#Quality Versus TPA
TPA_BLUES<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_Blue.csv" ) # the BLUE means for onfarm numeric data

TPA_BLUES<-TPA_BLUES[,-1]

rownames(tricot_sumMean_Quality_All)[which(!rownames(tricot_sumMean_Quality_All) %in% TPA_BLUES$genotype.name)]

idrw_All_TPA <- which(rownames(tricot_sumMean_Quality_All) %in% TPA_BLUES$genotype.name) #looking for missing genotypes
tricot_sumMean_Quality_All_Sel <- tricot_sumMean_Quality_All[idrw_All_TPA,]
idrw_TPA <- which(TPA_BLUES$genotype.name %in% rownames(tricot_sumMean_Quality_All_Sel)) #looking for missing genotypes
length(idrw_TPA)
TPA_BLUES_sel <- TPA_BLUES[idrw_TPA,]
dim(TPA_BLUES_sel)

TPA_BLUES1<-TPA_BLUES_sel[,-7]
TPA_blues_overall_quality_cor <- corTest(tricot_sumMean_Quality_All_Sel, TPA_BLUES1[,-1])

TPA_blues_overall_quality_cor$r = round(TPA_blues_overall_quality_cor$r, 2)
TPA_blues_overall_quality_cor$r
TPA_blues_overall_quality_cor$p

corrplot::corrplot(corr = TPA_blues_overall_quality_cor$r ,tl.cex = 0.6,p.mat = TPA_blues_overall_quality_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))





############################################
#Agronomy overall Versus worth Harvest on farm
trait_list_Agro = list(c("Best_Ovi1", "Worst_Ovi1"),
                            c("Best_Ovi3",  "Worst_Ovi3"),
                            c("Best_Ovi6", "Worst_Ovi6"),
                            c("Best_overimpr9MAP","Worst_overimpr9MAP"),
                            c("Best_HvOvi12", "Worst_HvOvi12"),
                            c("Best_replant12" , "Worst_replant12"))

tricot_rank_output_ALL_Agro = list() #
for(trt in 1:length(trait_list_Agro)){
  trt1 = trait_list_Agro[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R_ALL_Agro <- rank_tricot(data = Rank22a,
                         items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                         input = trt1)
  tricot_rank_output_ALL_Agro[[trt]] <-  R_ALL_Agro
}


tricot_rank_model_ALL_Agro = list()#
for(trt in 1:length(tricot_rank_output_ALL_Agro)){
  trt1 = tricot_rank_output_ALL_Agro[[trt]]

  mod_R_ALL_Agro <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank_model_ALL_Agro[[trt]] <- mod_R_ALL_Agro
}

tricot_rank_model_ALL_Agro[[trt]]$coefficients

tricot_rank_model_ALL_Agro1 = list()
for(i in 1:length(tricot_rank_model_ALL_Agro)){
  trdat =  tricot_rank_model_ALL_Agro[[i]]$coefficients
  tileng = which(names(trdat) %in% "tie2")

  if(length(tileng) == 0 ){
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng]
  }
  tricot_rank_model_ALL_Agro1[[i]] <- trdat1
}


tricot_sumMean_Agro_All = do.call(cbind, tricot_rank_model_ALL_Agro1)
dim(tricot_sumMean_Agro_All)
tricot_sumMean_Agro_All<- as.data.frame(tricot_sumMean_Agro_All) # tricot worth at harvest

Traitnames_Agro <- c("Overall Impression_1MAP", "Overall Impression_3MAP", "Overall Impression_6MAP","Overall Impression_9MAP", "Overall Impression_Harvest" ,"Replant")

colnames(tricot_sumMean_Agro_All) <- Traitnames_Agro

onfarm_BLUES <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/onfarm_BLUE_mean_after_prophav.csv" ) # the BLUe means for onfarm numeric data



rownames(tricot_sumMean_Agro_All)[which(!rownames(tricot_sumMean_Agro_All) %in% onfarm_BLUES$X)]

idrw_All_agro <- which(rownames(tricot_sumMean_Agro_All) %in% onfarm_BLUES$X) #looking for missing genotypes
tricot_sumMean_Agro_All_Sel <- tricot_sumMean_Agro_All[idrw_All_agro,]
idrw_onfarm <- which(onfarm_BLUES$X %in% rownames(tricot_sumMean_Agro_All_Sel)) #looking for missing genotypes
length(idrw_onfarm)
onfarm_BLUES_sel <- onfarm_BLUES[idrw_onfarm,]
dim(onfarm_BLUES_sel)


onfarm_BLUES_Sel1<- onfarm_BLUES_sel

onfarm_blues_overall_Agro_cor <- corTest(tricot_sumMean_Agro_All_Sel, onfarm_BLUES_Sel1[-1])

onfarm_blues_overall_Agro_cor$r = round(onfarm_blues_overall_Agro_cor$r, 2)
onfarm_blues_overall_Agro_cor$r
onfarm_blues_overall_Agro_cor$p

corrplot::corrplot(corr = onfarm_blues_overall_Agro_cor$r ,tl.cex = 0.6,p.mat = onfarm_blues_overall_Agro_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

################## onfarm harvest data versus ranking harvest data

trait_list_H = list(c("Best_HvOvi12", "Worst_HvOvi12"),
                   c("Best_HvYield12", "Worst_HvYield12"),
                   c("Best_HvRootsiz12" ,"Worst_HvRootsiz12"),
                   c("Best_HvRootshp12" , "Worst_HvRootshp12"),
                   c("Best_replant12", "Worst_replant12"))

tricot_rank_ouptput_H = list() #
for(trt in 1:length(trait_list_H)){
  trt1 = trait_list_H[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R_tri <- rank_tricot(data = Rank22a,
                       items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                       input = trt1)
  tricot_rank_ouptput_H[[trt]] <- R_tri
}


tricot_rank_model_ALL_harvest= list()#
for(trt in 1:length(tricot_rank_ouptput_H)){
  trt1 = tricot_rank_ouptput_H[[trt]]

  mod_R_H <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank_model_ALL_harvest[[trt]] <- mod_R_H
}
length(tricot_rank_model_ALL_harvest)


tricot_rank_model_ALL_harvest[[trt]]$coefficients

tricot_rank_model_ALL_harvest1 = list()
for(i in 1:length(tricot_rank_model_ALL_harvest)){
  trdat =  tricot_rank_model_ALL_harvest[[i]]$coefficients
  tileng = which(names(trdat) %in% "tie2")

  if(length(tileng) == 0 ){
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng]
  }
  tricot_rank_model_ALL_harvest1[[i]] <- trdat1
}


tricot_sumMean_harvest_All = do.call(cbind, tricot_rank_model_ALL_harvest1)
dim(tricot_sumMean_harvest_All)
tricot_sumMean_harvest_All<- as.data.frame(tricot_sumMean_harvest_All) # tricot worth at harvest

Traitnames_Harvest=  c("Overall_impression", "Yield_R", "Root_size_R", "Root_shape_R", "Replant_R")

colnames(tricot_sumMean_harvest_All) <- Traitnames_Harvest

onfarm_BLUES_Sel1 # bring in the blues

rownames(tricot_sumMean_harvest_All)[which(!rownames(tricot_sumMean_harvest_All) %in% onfarm_BLUES_Sel1$genotype.name)]

colnames(onfarm_BLUES_Sel1)

onfarm_BLUES_Sel2<- onfarm_BLUES_Sel1
 # dplyr::rename( "Yield_N"="yield_of" , "Shoot_weight_N" = "shoot_weight_of",
               #  "Number_of_roots_N"= "number_of_roots_of" ,"Plant_height_N"= "plant_height_of" ,
                # "First_branch_ht_N"= "First_branch_ht_of", "Rotten_roots_N"= "rotten_roots_of" )


idrw_All_rank <- which(rownames(tricot_sumMean_harvest_All) %in% onfarm_BLUES_Sel2$X) #looking for missing genotypes
tricot_sumMean_harvest_All_Sel <- tricot_sumMean_harvest_All[idrw_All_rank, ]
dim(tricot_sumMean_harvest_All_Sel)
idrw_harvest_numeric <- which(onfarm_BLUES_Sel2$X %in% rownames(tricot_sumMean_harvest_All_Sel)) #looking for missing genotypes

onfarm_BLUES_sel <- onfarm_BLUES_Sel2[idrw_harvest_numeric, ]
dim(onfarm_BLUES_sel)
all(rownames(tricot_sumMean_harvest_All_Sel) %in% onfarm_BLUES_sel$X)
# onfarm_BLUES_sel1<- onfarm_BLUES_sel%>%
#   dplyr::select("yield_per_plot","root_num_plot" , "shwt_plot","mean_brnch_ht", "mean_ht" )

tricot_sumMean_harvest_All_Sel1<- tricot_sumMean_harvest_All_Sel%>%
  dplyr::select("Yield_R", "Root_size_R", "Root_shape_R","Overall_impression", "Replant_R" )%>%
dplyr::rename("Overall_impression_harvest_R"= "Overall_impression")
tricot_sumMean_harvest_All_cor <- corTest(tricot_sumMean_harvest_All_Sel1, onfarm_BLUES_sel[-1])

tricot_sumMean_harvest_All_cor$r = round(tricot_sumMean_harvest_All_cor$r, 2)
tricot_sumMean_harvest_All_cor$r
tricot_sumMean_harvest_All_cor$p

corrplot::corrplot(corr = tricot_sumMean_harvest_All_cor$r ,tl.cex = 0.6,p.mat = tricot_sumMean_harvest_All_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

########Fresh eba quality with TPA and chemical

trait_list_All_eba_f = list(c("Best_EbaMould12", "Worst_EbaMould12"),
c("Best_EbaDraw12" , "Worst_EbaDraw12" ),
c("Best_EbaSmooth12" , "Worst_EbaSmooth12"),
c("Best_Ebacolour12"  , "Worst_Ebacolour12" ),
c("Best_EbaNstick12" , "Worst_EbaNstick12"),
c("Best_Ebaswell12"  , "Worst_Ebaswell12"),
c("Best_EbaTaste12" ,  "Worst_EbaTaste12"),
c("Best_EbaOvi12" , "Worst_EbaOvi12"),
c("Best_replant12" , "Worst_replant12"))

tricot_rank_output_eba_f = list() #
for(trt in 1:length(trait_list_All_eba_f)){
  trt1 = trait_list_All_eba_f[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R_ALL_eba_f <- rank_tricot(data = Rank22a,
                         items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                         input = trt1)
  tricot_rank_output_eba_f[[trt]] <-  R_ALL_eba_f
}


tricot_rank_model_eba_f = list()#
for(trt in 1:length(tricot_rank_output_eba_f)){
  trt1 = tricot_rank_output_eba_f[[trt]]

  mod_R_eba_f <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank_model_eba_f[[trt]] <- mod_R_eba_f
}

tricot_rank_model_eba_f[[trt]]$coefficients

tricot_rank_model_eba_f1 = list()
for(i in 1:length(tricot_rank_model_eba_f)){
  trdat =  tricot_rank_model_eba_f[[i]]$coefficients
  tileng = which(names(trdat) %in% c("tie2", "tie3"))

  if(length(tileng) == 0 ){
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng]
  }
  tricot_rank_model_eba_f1[[i]] <- trdat1
}
length( tricot_rank_model_eba_f1)

tricot_sumMean_eba_f = do.call(cbind, tricot_rank_model_eba_f1)

tricot_sumMean_eba_f<- as.data.frame(tricot_sumMean_eba_f) # tricot worth at harvest


Traitnames_Eba <- c("Mouldability_1_R", "Strechiness_1_R", "Smoothness_1_R", "Eba color_1_R",  "Stickiness_1_R", "Swelling_1_R", "Taste_1_R", "Overall Impression", "Replant" )


colnames(tricot_sumMean_eba_f) <- Traitnames_Eba

#chem_blues_22<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_chem_Blue.csv" ) # the BLUE means for onfarm numeric data


#Tratnames_chem<-c("varieties",  "Amylose_ch" , "Amylose_garri", "Bulk_Density",
                 # "Crude Fiber_ch" ,"Crude_Fiber_Garri" , "DMC", "SI", "SP","Starch_ch", "Starch_garri", "Sugar_garri", "WAC" )
#chem_blues_22<-chem_blues_22[,-1]
#colnames(chem_blues_22) <- Tratnames_chem
head(chem_blues_22)
rownames(tricot_sumMean_eba_f)[which(!rownames(tricot_sumMean_eba_f) %in% chem_blues_22$varieties)]

idrw_All_E <- which(rownames(tricot_sumMean_eba_f) %in% chem_blues_22$varieties) #looking for missing genotypes
tricot_sumMean_eba_f_Sel <- tricot_sumMean_eba_f[idrw_All_E,]
idrw_chem <- which(chem_blues_22$varieties %in% rownames(tricot_sumMean_eba_f_Sel)) #looking for missing genotypes
length(idrw_chem)
chem_blues_22_sel <- chem_blues_22[idrw_chem,]
dim(chem_blues_22_sel)
dim(tricot_sumMean_eba_f_Sel)

chem_blues_fresh_eba_cor <- corTest(tricot_sumMean_eba_f_Sel[,-c(4,7)], chem_blues_22_sel[,-c(1)])

chem_blues_fresh_eba_cor$r = round(chem_blues_fresh_eba_cor$r, 2)
chem_blues_fresh_eba_cor$r
chem_blues_fresh_eba_cor$p

corrplot::corrplot(corr = chem_blues_fresh_eba_cor$r ,tl.cex = 0.6,p.mat = chem_blues_fresh_eba_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

###################### sensory and TPA
tricot_sumMean_eba_f


idrw_eba <- which(rownames(tricot_sumMean_eba_f) %in% TPA_BLUES$genotype.name) #looking for missing genotypes
tricot_sumMean_eba_f_Sel <- tricot_sumMean_eba_f[idrw_eba,]
idrw_TPA <- which(TPA_BLUES$genotype.name %in% rownames(tricot_sumMean_eba_f_Sel)) #looking for missing genotypes
length(idrw_TPA)
TPA_BLUES_sel <- TPA_BLUES[idrw_TPA,]
dim(TPA_BLUES_sel)

TPA_BLUES1<-TPA_BLUES_sel[-c(7,3)]
TPA_blues_overall_eba_cor <- corTest(TPA_BLUES1[,-1], tricot_sumMean_eba_f_Sel[,-c(4,7)])

TPA_blues_overall_eba_cor$r = round(TPA_blues_overall_eba_cor$r, 2)
TPA_blues_overall_eba_cor$r
TPA_blues_overall_eba_cor$p

corrplot::corrplot(corr = TPA_blues_overall_eba_cor$r ,tl.cex = 0.6,p.mat = TPA_blues_overall_eba_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))


#### garri quality with chemical

trait_list_garri_Quality = list(c("Best_GaqDry12","Worst_GaqDry12"),
                                c("Best_GaColour12", "Worst_GaqColour12"),
                              c("Best_GaqHeavy12" , "Worst_GaqHeavy12"),
                              c( "Best_Gaqsmooth12", "Worth_Gaqsmooth12"),
                              c("Best_GaqTaste12" , "Worst_GaqTaste12"),
                              c("Best_Gaqwoodyfil12","Worst_Gaqwoodyfil12"),
                              c("Best_GaqOvi12","Worst_GaqOvi12"),
                              c("Best_replant12" , "Worst_replant12"))

tricot_rank_output_garri_Quality = list() #
for(trt in 1:length(trait_list_garri_Quality)){
  trt1 = trait_list_garri_Quality[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R_garri_Q <- rank_tricot(data = Rank22a,
                         items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                         input = trt1)
  tricot_rank_output_garri_Quality[[trt]] <-  R_garri_Q
}


tricot_rank_model_garri_Quality = list()#
for(trt in 1:length(tricot_rank_output_garri_Quality)){
  trt1 = tricot_rank_output_garri_Quality[[trt]]

  mod_R_garri_Q <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank_model_garri_Quality[[trt]] <- mod_R_garri_Q
}

tricot_rank_model_garri_Quality[[trt]]$coefficients

tricot_rank_model_garri_Quality1 = list()
for(i in 1:length(tricot_rank_model_garri_Quality)){
  trdat =  tricot_rank_model_garri_Quality[[i]]$coefficients
  tileng = which(names(trdat) %in% "tie3")

  if(length(tileng) == 0 ){
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng]
  }
  tricot_rank_model_garri_Quality1[[i]] <- trdat1
}


tricot_sumMean_Quality_garri = do.call(cbind, tricot_rank_model_garri_Quality1)
dim(tricot_sumMean_Quality_garri)
tricot_sumMean_Quality_garri<- as.data.frame(tricot_sumMean_Quality_garri) # tricot worth at harvest


Traitnames_garri <- c("Dryness_R", "Garri color_R","Heaviness_R" ,"Smoothness_R","Taste_R", "Woodiness_R" ,"Overall Garri Quality_R","Replant_R")

colnames(tricot_sumMean_Quality_garri) <- Traitnames_garri

#chem_blues_22<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_chem_Blue.csv" ) # the BLUE means for onfarm numeric data


#Tratnames_chem<-c("varieties",  "Amylose_ch" , "Amylose_garri", "Bulk_Density",
#  "Crude Fiber_ch" ,"Crude_Fiber_Garri" , "DMC", "SI", "SP","Starch_ch", "Starch_garri", "Sugar_garri", "WAC" )
#chem_blues_22<-chem_blues_22[,-1]
#colnames(chem_blues_22) <- Traitnames_chem
head(chem_blues_22)
rownames(tricot_sumMean_Quality_garri)[which(!rownames(tricot_sumMean_Quality_garri) %in% chem_blues_22$varieties)]

idrw_garri_q <- which(rownames(tricot_sumMean_Quality_garri) %in% chem_blues_22$varieties) #looking for missing genotypes
tricot_sumMean_garri_Sel <- tricot_sumMean_Quality_garri[idrw_garri_q,]
idrw_chem <- which(chem_blues_22$varieties %in% rownames(tricot_sumMean_garri_Sel)) #looking for missing genotypes
length(idrw_chem)
chem_blues_22_sel <- chem_blues_22[idrw_chem,]
dim(chem_blues_22_sel)
dim(tricot_sumMean_garri_Sel)

chem_blues_garri_cor <- corTest(tricot_sumMean_garri_Sel, chem_blues_22_sel[,-c(1)])

chem_blues_garri_cor$r = round(chem_blues_garri_cor$r, 2)
chem_blues_garri_cor$r
chem_blues_garri_cor$p

corrplot::corrplot(corr = chem_blues_garri_cor$r ,tl.cex = 0.6,p.mat = chem_blues_garri_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

trace(corrplot, edit = T)
#line 443
########### Over night Eba versus chemical

trait_list_ebaN = list(c("Best_EbaAFMould12","Worst_EbaAFMould12" ),
                                c("Best_EbaAFDraw12","Worst_EbaAFDraw12"),
                                c("Best_EbaAFSmooth12", "Worst_EbaAFSmooth12"),
                                c("Best_EbaAFOvi12" ,  "Worst_EbaAFOvi12"),
                                c("Best_replant12" , "Worst_replant12"))

tricot_rank_output_ebaN_Quality = list() #
for(trt in 1:length(trait_list_ebaN)){
  trt1 = trait_list_ebaN[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R_ebaN_Q <- rank_tricot(data = Rank22a,
                           items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                           input = trt1)
  tricot_rank_output_ebaN_Quality[[trt]] <-  R_ebaN_Q
}


tricot_rank_model_ebaN_Quality = list()#
for(trt in 1:length(tricot_rank_output_ebaN_Quality)){
  trt1 = tricot_rank_output_ebaN_Quality[[trt]]

  mod_R_ebaN_Q <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank_model_ebaN_Quality[[trt]] <- mod_R_ebaN_Q
}

tricot_rank_model_ebaN_Quality[[trt]]$coefficients


tricot_rank_model_ebaN_Quality1 = list()
for(i in 1:length(tricot_rank_model_ebaN_Quality)){
  trdat =  tricot_rank_model_ebaN_Quality[[i]]$coefficients
  tileng = which(names(trdat) %in% c("tie1","tie2","tie3"))

  if(length(tileng) == 0 ){
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng]
  }
  tricot_rank_model_ebaN_Quality1[[i]] <- trdat1
}


tricot_sumMean_ebaN = do.call(cbind, tricot_rank_model_ebaN_Quality1)
dim(tricot_sumMean_ebaN)
for(i in 1:5){
  len = length(tricot_rank_model_ebaN_Quality1[[i]])
  print(c(i, len))
}
tricot_sumMean_ebaN<- as.data.frame(tricot_sumMean_ebaN) # tricot worth at harvest


Traitnames_ebaN <- c("Mouldable_2_R", "Strechiness_2_R", "Smoothness_2_R" ,"Overall Eba_ov Quality","Replant")

colnames(tricot_sumMean_ebaN) <- Traitnames_ebaN

#chem_blues_22<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_chem_Blue.csv" ) # the BLUE means for onfarm numeric data


#Tratnames_chem<-c("varieties",  "Amylose_ch" , "Amylose_garri", "Bulk_Density",
#  "Crude Fiber_ch" ,"Crude_Fiber_Garri" , "DMC", "SI", "SP","Starch_ch", "Starch_garri", "Sugar_garri", "WAC" )
#chem_blues_22<-chem_blues_22[,-1]
#colnames(chem_blues_22) <- Traitnames_chem
head(chem_blues_22)
rownames(tricot_sumMean_ebaN)[which(!rownames(tricot_sumMean_ebaN) %in% chem_blues_22$varieties)]

idrw_ebaN_q <- which(rownames(tricot_sumMean_ebaN) %in% chem_blues_22$varieties) #looking for missing genotypes
tricot_sumMean_ebaN_Sel <- tricot_sumMean_ebaN[idrw_ebaN_q,]
idrw_chem <- which(chem_blues_22$varieties %in% rownames(tricot_sumMean_ebaN_Sel)) #looking for missing genotypes
length(idrw_chem)
chem_blues_22_sel <- chem_blues_22[idrw_chem,]
dim(chem_blues_22_sel)
dim(tricot_sumMean_ebaN_Sel)

chem_blues_ebaN_cor <- corTest(tricot_sumMean_ebaN_Sel, chem_blues_22_sel[,-c(1)])

chem_blues_ebaN_cor$r = round(chem_blues_ebaN_cor$r, 2)
chem_blues_ebaN_cor$r
chem_blues_ebaN_cor$p

corrplot::corrplot(corr = chem_blues_ebaN_cor$r ,tl.cex = 0.6,p.mat = chem_blues_ebaN_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

data.frame(rownames(tricot_sumMean_eba_f), rownames(tricot_sumMean_ebaN)) #side by side view of the genotype name
Eba_fresh_ov<- cbind(tricot_sumMean_eba_f, tricot_sumMean_ebaN)

write.csv(x=Eba_fresh_ov, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/fres_overnight_eba_worth_estimate.csv" )
##### Over night Eba versus TPA
TPA_BLUES_ov <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_ov_Blue.csv") # the BLUE means for TPA overnight


idrw_Quality_ebaN<- which(rownames(tricot_sumMean_ebaN) %in% TPA_BLUES_ov$genotype.name) #looking for missing genotypes
tricot_sumMean_Quality_ebaN_Sel <- tricot_sumMean_ebaN[idrw_Quality_ebaN,]
idrw_TPA_ov <- which(TPA_BLUES_ov$genotype.name %in% rownames(tricot_sumMean_Quality_ebaN_Sel)) #looking for missing genotypes
length(idrw_TPA_ov)
TPA_BLUES_ov_sel <- TPA_BLUES_ov[idrw_TPA_ov,]
dim(TPA_BLUES_ov_sel)

TPA_BLUES_ov1 <- TPA_BLUES_ov_sel[-c(1,2,4,8)]
TPA_blues_Quality_ebaN_cor <- corTest(tricot_sumMean_Quality_ebaN_Sel, TPA_BLUES_ov1)

TPA_blues_Quality_ebaN_cor$r = round(TPA_blues_Quality_ebaN_cor$r, 2)
TPA_blues_Quality_ebaN_cor$r
TPA_blues_Quality_ebaN_cor$p

corrplot::corrplot(corr = TPA_blues_Quality_ebaN_cor$r ,tl.cex = 0.6,p.mat = TPA_blues_Quality_ebaN_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

################# overnight TPA with chemical
dim(chem_blues_22)
dim(TPA_BLUES_ov)
TPA_BLUES_ov1 <- TPA_BLUES_ov[-1]

idrw_ov_eba <- which(TPA_BLUES_ov1$genotype.name %in% chem_blues_22$varieties) #looking for missing genotypes
ov_eba_Sel <- TPA_BLUES_ov1[idrw_ov_eba,]
idrw_chem_2 <- which(chem_blues_22$varieties %in% TPA_BLUES_ov1$genotype.name) #looking for missing genotypes
length(idrw_chem_2)
chem_blues_22_sel2 <- chem_blues_22[idrw_chem_2,]
dim(chem_blues_22_sel2)
dim(ov_eba_Sel)



chem_tpa_ebaN_cor <- corTest( ov_eba_Sel[-c(1, 3, 7)], chem_blues_22_sel2[-1])

chem_tpa_ebaN_cor$r = round(chem_tpa_ebaN_cor$r, 2)
chem_tpa_ebaN_cor$r
chem_tpa_ebaN_cor$p

corrplot::corrplot(corr = chem_tpa_ebaN_cor$r ,tl.cex = 0.6,p.mat = chem_tpa_ebaN_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

