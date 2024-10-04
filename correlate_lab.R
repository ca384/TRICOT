packages_used <- c("workflowr","dplyr", "tidyverse", "ggplot2", "here",
                   "stringr", "FactoMineR","corrplot","psych",
                    "reshape2", "agricolae", "gosset",
                    "PlackettLuce", "readr")

#install.packages(c("agricolae"))
ip <- installed.packages()
all_packages_installed <- TRUE
for (package in packages_used){
  if (!(package %in% ip[,"Package"])){
    print(paste("Please install package", package))
    install.packages(package)
    all_packages_installed <- FALSE
  } else {
    library(package, character.only = T) # load required package
  } # end else statement
}#END packages_used
if (!all_packages_installed) {
  stop("Some packages were installed.")
}


chem22 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_chem_Blue.csv" ) # the BLUE means for onfarm numeric data
head(chem22)
dim(chem22)
TPA_fresh <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_Blue.csv" ) # the BLUE means for onfarm numeric data
dim (TPA_fresh)
TPA_fresh2<-TPA_fresh%>%
dplyr:: rename("Adhesiveness_1"="Adhesiveness","Chewiness_1" = "Chewiness" , "Cohesiveness_1" = "Cohesiveness",
               "Gumminess_1"= "Gumminess" , "Resilience_1" = "Resilience", "Springiness_1" = "Springiness")%>%
  dplyr::select("Adhesiveness_1", "Cohesiveness_1",  "Gumminess_1", "Resilience_1" )
TPA_ov <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_ov_Blue.csv" ) # the BLUE means for onfarm numeric data
dim(TPA_ov)

TPA_ov2= TPA_ov%>%
 dplyr:: rename("Adhesiveness_2"="Adhesiveness","Chewiness_2" = "Chewiness" , "Cohesiveness_2" = "Cohesiveness",
                "Gumminess_2"= "Gumminess" , "Resilience_2" = "Resilience", "Springiness_2" = "Springiness")%>%
  dplyr::select("Adhesiveness_2", "Cohesiveness_2",  "Gumminess_2", "Resilience_2" )

full_chem_blue<-cbind(chem22, TPA_fresh2, TPA_ov2 )
dim(full_chem_blue)
full_chem_blue=full_chem_blue%>%
  dplyr::rename("AMY_garri"="amy_garri","CF_garri" = "crude_fiber_garri",
                 "BD_garri" = "Bulk_Density", "STC_ch" ="Starch_ch",
                "STC_garri" = "starch_garri", "SC_garri"= "sugar_garri",
                "SI_garri"="SI" ,  "SP_garri"="SP", "WAC_garri"="WAC",
                "Adhes_1"= "Adhesiveness_1" , "Cohes_1"="Cohesiveness_1",
                "Gummi_1"="Gumminess_1" , "Res_1"=  "Resilience_1" ,
                "Adhes_2"= "Adhesiveness_2", "Cohes_2"="Cohesiveness_2",
                "Gummi_2"="Gumminess_2" ,   "Resil_2"= "Resilience_2" )

full_chem_blue=full_chem_blue%>%
  dplyr::rename ("Amy_ch"= "AMY_ch", "Amy_garri" = "AMY_garri" )
#######
cor_chem_Blue <- corTest(full_chem_blue[-c(1,2)])

cor_chem_Blue$r = round(cor_chem_Blue$r, 1)
cor_chem_Blue$p
cor_chem_Blue$p.adj
cor_chem_Blue$p

trace(corrplot, edit = T)
#line 443
corrplot::corrplot(corr = cor_chem_Blue$r,
                   tl.cex = 0.5,p.mat = cor_chem_Blue$p,type = "lower",
                   method = 'color',addCoef.col ='black',diag = FALSE,
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.5,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
#Redo this was chem blues with TPA blup

write.csv(x=full_chem_blue, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_Chem_BLUE.csv")

# All_Chem<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_Chem_BLUE.csv")
# dim(All_Chem)
   All_eba_worth<- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/fres_overnight_eba_worth_estimate.csv")
dim(All_eba_worth)
All_eba_worth1<-All_eba_worth%>%
  dplyr::rename("Overall_Eba_2"="Overall.Eba_ov.Quality","Overall_Eba_1"= "Overall.Impression", "Stretchiness_1_R" = "Strechiness_1_R","Stretchiness_2_R"= "Strechiness_2_R",)%>%
  dplyr::select( "Mouldability_1_R", "Stretchiness_1_R", "Smoothness_1_R","Eba.color_1_R",
                 "Stickiness_1_R", "Swelling_1_R", "Taste_1_R", "Overall_Eba_1",
                "Mouldable_2_R", "Stretchiness_2_R", "Smoothness_2_R", "Overall_Eba_2", "Replant")

# cor_rank_eba <- corTest(All_eba_worth1, full_chem_blue[-c(1,2)])
#
# cor_rank_eba$r = round(cor_rank_eba$r, 2)
# cor_rank_eba$p
# cor_rank_eba$p.adj
# cor_rank_eba$p
#
#
# corrplot::corrplot(corr = cor_rank_eba$r,
#                    tl.cex = 0.5,p.mat = cor_rank_eba$p,type = "full",
#                    method = 'color',addCoef.col ='black',diag = TRUE,
#                    sig.level = c(0.001,0.01, 0.05),
#                    insig = "label_sig", pch.cex = 0.7,
#                    number.cex = 0.5,
#                    number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

#####################
# #Eba tricot rank with chemical
#
# cor_ranking_chem_eba <- corTest(All_eba_worth1, All_Chem[-c(1,2,3)])
#
# cor_ranking_chem_eba$r = round(cor_ranking_chem_eba$r, 2)
# cor_ranking_chem_eba$p
# cor_ranking_chem_eba$p.adj
# cor_ranking_chem_eba$p
#
#
# corrplot::corrplot(corr =cor_ranking_chem_eba$r,
#                    tl.cex = 0.5,p.mat = cor_ranking_chem_eba$p,type = "full",
#                    method = 'color',addCoef.col ='black',
#                    sig.level = c(0.001,0.01, 0.05),
#                    insig = "label_sig", pch.cex = 0.7,
#                    number.cex = 0.5,
#                    number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
# #R
#
#
# cor_ranking_chem_eba2 <- corTest(All_eba_worth1, full_chem_blue[-c(1,2)])
#
# cor_ranking_chem_eba2$r = round(cor_ranking_chem_eba2$r, 2)
# cor_ranking_chem_eba2$p
# cor_ranking_chem_eba2$p.adj
# cor_ranking_chem_eba2$p
#
#
# corrplot::corrplot(corr =cor_ranking_chem_eba2$r,
#                    tl.cex = 0.5,p.mat = cor_ranking_chem_eba2$p,type = "full",
#                    method = 'color',addCoef.col ='black',
#                    sig.level = c(0.001,0.01, 0.05),
#                    insig = "label_sig", pch.cex = 0.7,
#                    number.cex = 0.5,
#                    number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

######garri
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
tricot_sumMean_Quality_garri= as.data.frame(tricot_sumMean_Quality_garri)
tricot_sumMean_Quality_garri1 = tricot_sumMean_Quality_garri %>%
  dplyr::arrange((rownames(tricot_sumMean_Quality_garri)))


tricot_sumMean_Quality_garri1= tricot_sumMean_Quality_garri1%>%
  dplyr::rename("Fragments_vascular_tissues_R"="Woodiness_R")


#chem_blues_22<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_chem_Blue.csv" ) # the BLUE means for onfarm numeric data


#Tratnames_chem<-c("varieties",  "Amylose_ch" , "Amylose_garri", "Bulk_Density",
#  "Crude Fiber_ch" ,"Crude_Fiber_Garri" , "DMC", "SI", "SP","Starch_ch", "Starch_garri", "Sugar_garri", "WAC" )
#chem_blues_22<-chem_blues_22[,-1]
#colnames(chem_blues_22) <- Traitnames_chem

#rownames(tricot_sumMean_Quality_garri)[which(!rownames(tricot_sumMean_Quality_garri) %in% full_chem_blue$varieties)]

# idrw_garri_q <- which(rownames(tricot_sumMean_Quality_garri) %in% full_chem_blue$varieties) #looking for missing genotypes
# tricot_sumMean_garri_Sel <- tricot_sumMean_Quality_garri[idrw_garri_q,]
# idrw_chem <- which(full_chem_blue$varieties %in% rownames(tricot_sumMean_garri_Sel)) #looking for missing genotypes
# length(idrw_chem)
# chem_blues_22_sel <- full_chem_blue[idrw_chem,]
# dim(chem_blues_22_sel)
# dim(tricot_sumMean_garri_Sel)

chem_blues_garri_cor <- corTest(tricot_sumMean_Quality_garri1, full_chem_blue[-c(1,2,15:22)])

chem_blues_garri_cor$r = round(chem_blues_garri_cor$r, 2)
chem_blues_garri_cor$r
chem_blues_garri_cor$p

corrplot::corrplot(corr = chem_blues_garri_cor$r ,tl.cex = 0.6,p.mat = chem_blues_garri_cor$p,
                   method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))


summary_chem=summary(full_chem_blue)
write.csv(x = summary_chem,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/summary_stat_chem.csv" )
#####

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


Traitnames_Eba <- c("Mouldability_1_R", "Strechiness_1_R", "Smoothness_1_R", "Eba color_1_R",  "Stickiness_1_R", "Swelling_1_R", "Taste_1_R", "Overall_Impression_1", "Replant1" )


colnames(tricot_sumMean_eba_f) <- Traitnames_Eba
tricot_sumMean_eba_f1 = tricot_sumMean_eba_f %>%
  dplyr::arrange(rownames(tricot_sumMean_eba_f))



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


Traitnames_ebaN <- c("Mouldable_2_R", "Strechiness_2_R", "Smoothness_2_R" ,"Overall_impression_2","Replant2")

colnames(tricot_sumMean_ebaN) <- Traitnames_ebaN

tricot_sumMean_ebaN= as.data.frame(tricot_sumMean_ebaN)

tricot_sumMean_ebaN1 = tricot_sumMean_ebaN %>%
  dplyr::arrange(rownames(tricot_sumMean_ebaN))

#join fresh and overnight eba
All_eba_farmers<-cbind(tricot_sumMean_eba_f1,tricot_sumMean_ebaN)

All_eba_farmers= All_eba_farmers%>%
  dplyr::rename("Replant_R"="Replant1")

#chem_blues_22<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_chem_Blue.csv" ) # the BLUE means for onfarm numeric data


#Tratnames_chem<-c("varieties",  "Amylose_ch" , "Amylose_garri", "Bulk_Density",
# "Crude Fiber_ch" ,"Crude_Fiber_Garri" , "DMC", "SI", "SP","Starch_ch", "Starch_garri", "Sugar_garri", "WAC" )
#chem_blues_22<-chem_blues_22[,-1]
#colnames(chem_blues_22) <- Tratnames_chem
# head(chem_blues_22)
# rownames(tricot_sumMean_eba_f)[which(!rownames(tricot_sumMean_eba_f) %in% chem_blues_22$varieties)]
#
# idrw_All_E <- which(rownames(tricot_sumMean_eba_f) %in% chem_blues_22$varieties) #looking for missing genotypes
# tricot_sumMean_eba_f_Sel <- tricot_sumMean_eba_f[idrw_All_E,]
# idrw_chem <- which(chem_blues_22$varieties %in% rownames(tricot_sumMean_eba_f_Sel)) #looking for missing genotypes
# length(idrw_chem)
# chem_blues_22_sel <- chem_blues_22[idrw_chem,]
# dim(chem_blues_22_sel)
# dim(tricot_sumMean_eba_f_Sel)

chem_blues_fresh_eba_cor <- corTest(All_eba_farmers[-14], full_chem_blue[-c(1,2)])

chem_blues_fresh_eba_cor$r = round(chem_blues_fresh_eba_cor$r, 2)
chem_blues_fresh_eba_cor$r
chem_blues_fresh_eba_cor$p

corrplot::corrplot(corr = chem_blues_fresh_eba_cor$r ,tl.cex = 0.6,p.mat = chem_blues_fresh_eba_cor$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

###################### sensory and TPA




#### New yield correlation with the model that has missing plot as covariate
onfarm22<- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/2022_blues_model_missing_data.csv")

