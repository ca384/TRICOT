#using the five 9MAP traits labels =  "Overall_impression", "Pgwt", "Canopy", "Branching", "Suitability")
trait_list5 = list(c("Best_overimpr9MAP","Worst_overimpr9MAP"),
                   c("Best_pltgrwt9MAP","Worst_pltgrwt9MAP"),   
                   c("Best_weed9MAP","Worst_weed9MAP"),      
                   c("Best_branch9MAP","Worst_branch9MAP"),     
                   c("Best_suitability9MAP","Worst_suitability9MAP"))

tricot_rank_ouptput9 = list() #
for(trt in 1:length(trait_list5)){
  trt9 = trait_list[[trt]] # subsetting of the trait list
  
  # use rank tricot to rank the variets (base on the genotype names)
  R_tr9 <- rank_tricot(data = Rank22a,
                       items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                       input = trt9)
  tricot_rank_ouptput9[[trt]] <- R_tri
}


tricot_rank_model9 = list()# 
for(trt in 1:length(tricot_rank_ouptput9)){
  trt9 = tricot_rank_ouptput9[[trt]]
  
  mod_R_1p <- PlackettLuce(trt9, npseudo = 0.5) 
  
  tricot_rank_model9[[trt]] <- mod_R_1p
}


tricot_rank_model9
do.call(rbind,tricot_rank_model9 )
tricot_rank_model9 = tricot_rank_model9
tricot_rank_model9[[5]]$coefficients

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
Blupmean_9wide = Blupmean_9wide[order(Blupmean_9wide$Genotype),]
data.frame(rownames(Blupmean_9wide),rownames(tricot_sumMean9))
tricot_sumMean9 = tricot_sumMean9[order(rownames(tricot_sumMean9)),]
tricot_blup_meancomb9 = data.frame(Blupmean_9wide,tricot_sumMean9)
dim(tricot_blup_meancomb9)
head(tricot_blup_meancomb9)
cor(tricot_blup_meancomb9[,-1])

library(agricolae)
crr <- correlation(tricot_blup_meancomb[,-1])
crr$correlation
crr$pvalue

# using harvest ranking
 # for the yield data= #Overall_impression", "Yield", "Density", "Root_color", "Root_size", "Root_shape"
tricot_rank_model_H
do.call(rbind,tricot_rank_model_H ) # for the rbind
tricot_rank_model2 = tricot_rank_model_H
tricot_rank_model2[[5]]$coefficients

tricot_rank_mode2_sum = list()

for(i in 1:length(tricot_rank_model_H)){
  trdat =  tricot_rank_model_H[[i]]$coefficients # select the coefficients
  tileng = which(names(trdat) %in% "tie2") # select the ties
  
  if(length(tileng) == 0 ){ #if the is tiles then make it 0
    trdat1 = trdat
  }else{
    trdat1 = trdat[-tileng] # remove the zeros (that is the ties)
  }
  tricot_rank_model_sum[[i]] <- trdat1 # does not contain ties
}

tricot_sumMean = do.call(cbind, tricot_rank_model_sum)
dim(tricot_sumMean)
Blupmean_1wide = Blupmean_1wide[order(Blupmean_1wide$Genotype),]
data.frame(rownames(Blupmean_1wide),rownames(tricot_sumMean))
tricot_sumMean = tricot_sumMean[order(rownames(tricot_sumMean)),]
tricot_blup_meancomb = data.frame(Blupmean_1wide,tricot_sumMean)
dim(tricot_blup_meancomb)
head(tricot_blup_meancomb)
cor(tricot_blup_meancomb[,-1])
crr <- correlation(tricot_blup_meancomb[,-1])
crr$correlation

