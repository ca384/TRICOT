#harvest_blues<- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/tricot21_numeric_Blue.csv") # the BLUE means for on farm numeric data

harvest_blues<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/tricot21_numeric_Blue_missing_rename.csv" ) # the BLUE means for on farm numeric data

chem_blues<- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/tricot21_garri_chem_Blue.csv" ) # the BLUE means for on farm numeric data

################# worth of harvest

Trait_list21_HV= list(c("Root_yield_Best","Root_yield_Worst"),
                      c( "Root_size_Best","Root_size_Worst"),
                      #c( "Root_shape_Best","Root_shape_Worst"),
                      c("Root_rot_Best","Root_rot_Worst"),
                      c("Harvest_Ovi_Best" , "Harvest_Ovi_Worst"))

tricot_rank21_ouptput_HV = list() #
for(trt in 1:length(Trait_list21_HV)){
  trt1 = Trait_list21_HV[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_HV <- rank_tricot(data = rank21a,
                        items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                        input = trt1)
  tricot_rank21_ouptput_HV[[trt]] <- R21_HV
}


tricot_rank21_model_HV = list()#
for(trt in 1:length(tricot_rank21_ouptput_HV)){
  trt1 = tricot_rank21_ouptput_HV[[trt]]

  mod_R21_HV <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_HV[[trt]] <- mod_R21_HV
}

# do.call(rbind,tricot_rank21_model_HV)

tricot_rank21_model_HV[[trt]]$coefficients

tricot_rank21_model_HV1 = list()
for(i in 1:length(tricot_rank21_model_HV)){
  trdat =  tricot_rank21_model_HV[[i]]$coefficients
  tileng = which(names(trdat) %in% "tie3")

  if(length(tileng) == 0 ){
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng]
  }
  tricot_rank21_model_HV1[[i]] <- trdat1
}

tricot_sumMean_H = do.call(cbind, tricot_rank21_model_HV1)
dim(tricot_sumMean_H)
tricot_sumMean_H <- as.data.frame(tricot_sumMean_H) # tricot worth at harvest



Traitnames <- c("Root_yield_R","Root_size_R", "Root_shape_R",
"Root_rot_R","Harvest_impression_R")
colnames(tricot_sumMean_H) <- Traitnames
harvest_blues <- harvest_blues[,-1]


Tratnames_numeric<-c("Genotype" , "yield_of", "shoot_weight_of" , "number_roots_of",
                     "plant_height_of","first_branch_ht_of","rotten_roots_of" )


colnames(harvest_blues) <- Tratnames_numeric
library(psych)
library(corrplot)
rank_BLUE_cor <- corTest(tricot_sumMean_H, harvest_blues[,-c(1)])

rank_BLUE_cor$r = round(rank_BLUE_cor$r, 2)
rank_BLUE_cor$r
rank_BLUE_cor$p



# harvest_BLUE_rank<- round(cor(tricot_sumMean_H, harvest_blues[,-c(1,2)]), 2)# correlating ranking at 9map and harvest
# harvest_BLUE_rank$

corrplot::corrplot(corr = rank_BLUE_cor$r ,tl.cex = 0.6,p.mat = rank_BLUE_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
# to change the position of the asterics, trace(corrplot, edit=TRUE) then go to lin 443 and add +025 and save

########################### correlate garri quality worth map with the chemical BLUES
chem_blues<- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/tricot21_garri_chem_Blue.csv" ) # the BLUE means for on farm numeric data

head(chem_blues)


Trait_list21_Gaq= list(c("Garri_Dryness_Best","Garri_Dryness_Worst"),
                       c( "Garri_color_Best","Garri_color_Worst"),
                       c( "Garri_Heaviness_Best","Garri_Heaviness_Worst"),
                       c( "Garri_smoothness_Best","Garri_smoothness_Worst" ),
                       c(  "Garri_taste_Best","Garri_taste_Worst" ),
                       c("Garri_woodiness_Best" , "Garri_woodiness_Worst"),
                       c("Garri_Ovi_Best", "Garri_Ovi_Worst" ))


tricot_rank21_ouptput_Gaq = list() #
for(trt in 1:length(Trait_list21_Gaq)){
  trt1 = Trait_list21_Gaq[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_Gaq <- rank_tricot(data = rank21a,
                         items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                         input = trt1)
  tricot_rank21_ouptput_Gaq[[trt]] <- R21_Gaq
}


tricot_rank21_model_Gaq = list()#
for(trt in 1:length(tricot_rank21_ouptput_Gaq)){
  trt1 = tricot_rank21_ouptput_Gaq[[trt]]

  mod_R21_Gaq <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_Gaq[[trt]] <- mod_R21_Gaq
}


tricot_rank21_model_Gaq[[trt]]$coefficients

tricot_rank21_model_Gaq1 = list()
for(i in 1:length(tricot_rank21_model_Gaq)){
  trdat =  tricot_rank21_model_Gaq[[i]]$coefficients
  tileng = which(names(trdat) %in% "tie3")

  if(length(tileng) == 0 ){
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng]
  }
  tricot_rank21_model_Gaq1[[i]] <- trdat1
}


# name for each model
#labels = c("Dryness", "Garri color", "Heaviness","Smoothness", "Taste" , "Woodiness", "Overall quality")

tricot_sumMean_Gaq = do.call(cbind, tricot_rank21_model_Gaq1)
dim(tricot_sumMean_Gaq)
tricot_sumMean_Gaq <- as.data.frame(tricot_sumMean_Gaq) # tricot worth at harvest

Traitnames <- c("Dryness_R", "Garri color_R", "Heaviness_R","Smoothness_R", "Taste_R" , "Woodiness_R", "Overall quality_R")

colnames(tricot_sumMean_Gaq) <- Traitnames



Tratnames_numeric<-c("VARIETIES",  "AMYLOPECTIN_N" , "AMYLOSE_N", "DM_garri_N",
                     "FREE.SUGAR_N" ,"pH.VALUE_N" , "DMC_N", "SP_N", "STARCH_N","WAC_N" )
chem_blues1<-chem_blues[,-1]
colnames(chem_blues1) <- Tratnames_numeric

rownames(tricot_sumMean_Gaq)[which(!rownames(tricot_sumMean_Gaq) %in% chem_blues1$VARIETIES)]

idrw_gaq <- which(rownames(tricot_sumMean_Gaq) %in% chem_blues1$VARIETIES)
tricot_sumMean_GaqSel <- tricot_sumMean_Gaq[idrw_gaq,]
chem_BLUE_sensory_rank_cor <- corTest(tricot_sumMean_GaqSel[,-c(1)], chem_blues1[,-c(1,2,4,6)])

chem_BLUE_sensory_rank_cor$r = round(chem_BLUE_sensory_rank_cor$r, 2)
chem_BLUE_sensory_rank_cor$r
chem_BLUE_sensory_rank_cor$p



# harvest_BLUE_rank<- round(cor(tricot_sumMean_H, harvest_blues[,-c(1,2)]), 2)# correlating ranking at 9map and harvest
# harvest_BLUE_rank$

corrplot::corrplot(corr = chem_BLUE_sensory_rank_cor$r ,tl.cex = 0.6,p.mat = chem_BLUE_sensory_rank_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
# to change the position of the asterics, trace(corrplot, edit=TRUE) then go to lin 443 and add +025 and save

trace(corrplot, edit=TRUE)
################################# Eba quality

Trait_list21_ebq= list(c("Eba_Mouldability_Best" ,"Eba_Mouldability_Worst" ),
                       c( "Eba_Softness_Best" ,"Eba_Softness_Worst"),
                       c(  "Eba_Strechyness_Best","Eba_Strechyness_Worst"),
                       c( "Eba__Smoothness_Best","Eba__Smoothness_Worst" ),
                       c(  "Eba_Colour_Best","Eba_Colour_Worst" ),
                       c("Eba_stickiness_Best" ,  "Eba_stickiness_Worst"),
                       c("Eba_Swelling_Best", "Eba_Swelling_Worst"),
                       c("Eba_Taste_Best", "Eba_Taste_Worst"),
                       c("Eba_ovi_Best", "Eba_ovi_Worst" ))


tricot_rank21_ouptput_ebq = list() #
for(trt in 1:length(Trait_list21_ebq)){
  trt1 = Trait_list21_ebq[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_ebq <- rank_tricot(data = rank21a,
                         items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                         input = trt1)
  tricot_rank21_ouptput_ebq[[trt]] <- R21_ebq
}


tricot_rank21_model_ebq = list()#
for(trt in 1:length(tricot_rank21_ouptput_ebq)){
  trt1 = tricot_rank21_ouptput_ebq[[trt]]

  mod_R21_ebq <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_ebq[[trt]] <- mod_R21_ebq
}
length( tricot_rank21_model_ebq)



# name for each model
#labels = c("Mouldability", "Softness", "Stretchiness","Smoothness", "Eba_color", "Stickiness" , "Swelling", "Eba_Taste", "Overall Eba quality")

tricot_rank21_model_ebq[[trt]]$coefficients

tricot_rank21_model_ebq1 = list()
for(i in 1:length(tricot_rank21_model_ebq)){
  trdat =  tricot_rank21_model_ebq[[i]]$coefficients
  tileng = which(names(trdat) %in% "tie3")

  if(length(tileng) == 0 ){
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng]
  }
  tricot_rank21_model_ebq1[[i]] <- trdat1
}



tricot_sumMean_ebq = do.call(cbind, tricot_rank21_model_ebq1)
dim(tricot_sumMean_ebq)
tricot_sumMean_ebq <- as.data.frame(tricot_sumMean_ebq) # tricot worth at harvest

Traitnames <- c("Mouldability_R", "Softness_R", "Stretchiness_R","Smoothness_R", "Eba_color_R", "Stickiness_R" , "Swelling_R", "Eba_Taste_R", "Overall Eba quality_R")

colnames(tricot_sumMean_ebq) <- Traitnames
Tratnames_numeric<-c("VARIETIES",  "AMYLOPECTIN_N" , "AMYLOSE_N", "DM_garri_N",
                     "SUGAR_N" ,"pH.VALUE_N" , "DMC_N", "SP_N", "STARCH_N","WAC_N" )
#chem_blues1<-chem_blues[,-1]
colnames(chem_blues1) <- Tratnames_numeric

rownames(tricot_sumMean_ebq)[which(!rownames(tricot_sumMean_ebq) %in% chem_blues1$VARIETIES)]

idrw_ebq <- which(rownames(tricot_sumMean_ebq) %in% chem_blues1$VARIETIES)
tricot_sumMean_ebqSel <- tricot_sumMean_ebq[idrw_ebq,]
chem_BLUE_sensoryebq_rank_cor <- corTest(tricot_sumMean_ebqSel[,-c(1)], chem_blues1[,-c(1,2,4,6)])

chem_BLUE_sensoryebq_rank_cor$r = round(chem_BLUE_sensoryebq_rank_cor$r, 2)
chem_BLUE_sensoryebq_rank_cor$r
chem_BLUE_sensoryebq_rank_cor$p



# harvest_BLUE_rank<- round(cor(tricot_sumMean_H, harvest_blues[,-c(1,2)]), 2)# correlating ranking at 9map and harvest
# harvest_BLUE_rank$

corrplot::corrplot(corr = chem_BLUE_sensoryebq_rank_cor$r ,tl.cex = 0.6,p.mat = chem_BLUE_sensoryebq_rank_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
# to change the position of the asterics, trace(corrplot, edit=TRUE) then go to lin 443 and add +025 and save




#Tratnames_numeric<-c("VARIETIES",  "AMYLOPECTIN_N" , "AMYLOSE_N", "DM_garri_N",
                    # "FREE.SUGAR_N" ,"pH.VALUE_N" , "ROOT_DMC_N", "SP_N", "STARCH_N","WAC_N" )

#colnames(chem_blues) <- Tratnames_numeric

############################# overnight eba quality


Trait_list21_OvE= list(c("OvNight_mouldability_Best" ,"OvNight_mouldability_Worst"),
                       c( "OvNight_softness_Best" ,"OvNight_softness_Worst"),
                       c(  "OvNight_strechyness_Best","OvNight_strechyness__Worst"),
                       c( "OvNight_smoothness_Best","OvNight_smoothness_Worst"),
                       c(  "OvNight_stickiness_Best","OvNight_stickiness_Worst" ),
                       c("OvNight_taste_Best" ,  "OvNight_taste_Worst"),
                       c("OvNight_color_Best" , "OvNight_color_Worst"),
                       c("OvNight_aroma_Best", "OvNight_aroma_Worst"),
                       c("OvI_OvNight_Best", "OvI_OvNight_Worst"))


tricot_rank21_ouptput_OvE= list() #
for(trt in 1:length(Trait_list21_OvE)){
  trt1 = Trait_list21_OvE[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_OvE <- rank_tricot(data = rank21a,
                         items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                         input = trt1)
  tricot_rank21_ouptput_OvE[[trt]] <- R21_OvE
}


tricot_rank21_model_OvE = list()#
for(trt in 1:length(tricot_rank21_ouptput_OvE)){
  trt1 = tricot_rank21_ouptput_OvE[[trt]]

  mod_R21_OvE <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_OvE[[trt]] <- mod_R21_OvE
}
length( tricot_rank21_model_OvE)

labels = c("Mouldability", "Softness", "Stretchiness","Smoothness",  "Stickiness", "Eba_Taste", "Eba_color" , "Smell" , "Overall Overnight Eba Quality")

worth_map(tricot_rank21_model_OvE, labels,size=4) + theme(axis.text.x = element_text(angle = 90))



tricot_rank21_model_OvE[[trt]]$coefficients

tricot_rank21_model_OvE1 = list()
for(i in 1:length(tricot_rank21_model_OvE)){
  trdat =  tricot_rank21_model_OvE[[i]]$coefficients
  tileng = which(names(trdat) %in% c("tie2",'tie3'))

  if(length(tileng) == 0 ){
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng]
  }
  tricot_rank21_model_OvE1[[i]] <- trdat1
}

#rm(tricot_rank21_model_OvE1)
tricot_sumMean_OvE = do.call(cbind, tricot_rank21_model_OvE1)
dim(tricot_sumMean_OvE)
tricot_sumMean_OvE <- as.data.frame(tricot_sumMean_OvE) # tricot worth at harvest

Traitnames <- c("Mouldability_R", "Softness_R", "Stretchiness_R","Smoothness_R",  "Stickiness_R",
                "Eba_Taste_R", "Eba_color_R", "Aroma_R", "Overall Eba quality_R")


colnames(tricot_sumMean_OvE) <- Traitnames
Tratnames_numeric<-c("VARIETIES",  "AMYLOPECTIN_N" , "AMYLOSE_N", "DM_garri_N",
                     "FREE.SUGAR_N" ,"pH.VALUE_N" , "DMC_N", "SP_N", "STARCH_N","WAC_N" )
#chem_blues1<-chem_blues[,-1]
colnames(chem_blues1) <- Tratnames_numeric

rownames(tricot_sumMean_OvE)[which(!rownames(tricot_sumMean_OvE) %in% chem_blues1$VARIETIES)]

idrw_OvE <- which(rownames(tricot_sumMean_OvE) %in% chem_blues1$VARIETIES)
tricot_sumMean_OvESel <- tricot_sumMean_OvE[idrw_OvE,]
chem_BLUE_sensoryOvE_rank_cor <- corTest(tricot_sumMean_OvESel[,-c(1)], chem_blues1[,-c(1,2,4, 6)])

chem_BLUE_sensoryOvE_rank_cor$r = round(chem_BLUE_sensoryOvE_rank_cor$r, 2)
chem_BLUE_sensoryOvE_rank_cor$r
chem_BLUE_sensoryOvE_rank_cor$p





# harvest_BLUE_rank<- round(cor(tricot_sumMean_H, harvest_blues[,-c(1,2)]), 2)# correlating ranking at 9map and harvest
# harvest_BLUE_rank$

corrplot::corrplot(corr = chem_BLUE_sensoryOvE_rank_cor$r ,tl.cex = 0.6,p.mat = chem_BLUE_sensoryOvE_rank_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
# to change the position of the asterics, trace(corrplot, edit=TRUE) then go to lin 443 and add +025 and save

