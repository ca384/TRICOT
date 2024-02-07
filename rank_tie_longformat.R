head(Rank22)
colnames(Rank22)
trtex <- list(c("Best_Ovi1" , "Worst_Ovi1"),           
c("Best_Germ1","Worst_Germ1"),        
c("Best_Pgwt1","Worst_Pgwt1"),
c("Best_SuitSE1","Worst_SuitSE1"),       
c("Best_Ovi3","Worst_Ovi3"),           
c("Best_Sprout3", "Worst_Sprout3"),       
c("Best_Pgwt3","Worst_Pgwt3"),          
c("Best_Canopy3","Worst_Canopy3"),        
c("Best_Branch3" ,"Worst_Branch3"),        
c("Best_Disease3" ,"Worst_Disease3"),       
c("Best_Earlybulk3", "Worst_Earlybulk3"),     
c("Best_Ovi6" ,"Worst_Ovi6"),           
c("Best_Sprout6","Worst_Sprout6"),       
c("Best_Pgwt6" , "Worst_Pgwt6"),        
c("Best_Canopy6", "Worst_Canopy6"),        
c("Best_Branch6" , "Worst_Branch6"),        
c("Best_Disease6" ,  "Worst_Disease6"),       
c("Best_Earlybulk6"  ,  "Worst_Earlybulk6"),     
c("Best_overimpr9MAP" , "Worst_overimpr9MAP"),   
c("Best_pltgrwt9MAP" , "Worst_pltgrwt9MAP"),    
c("Best_weed9MAP"  , "Worst_weed9MAP" ),      
c("Best_branch9MAP" , "Worst_branch9MAP"),     
c("Best_suitability9MAP" , "Worst_suitability9MAP"),
c("Best_HvYield12" , "Worst_HvYield12"),      
c("Best_HvDensity12" , "Worst_HvDensity12"),    
c("Best_HvRootcol12" , "Worst_HvRootcol12" ),   
c("Best_HvRootsiz12" , "Worst_HvRootsiz12"),    
c("Best_HvRootshp12" , "Worst_HvRootshp12" ),   
c("Best_HvRootrot12" , "Worst_HvRootrot12"),    
c("Best_HvWodyfil12" , "Worst_HvWodyfil12"),    
c("Best_EaseP12"  , "Worst_EaseP12"),        
c("Best_HvOvi12"  , "Worst_HvOvi12"),        
c("Best_GarEaseP12" , "Worst_GarEaseP12"),     
c("Best_Garmash12" , "Worst_Garmash12"),      
c("Best_Waterelz12" , "Worst_Waterelz12"),     
c("Best_SwellMash12" , "Worst_SwellMash12"),    
c("Best_Gayield12" ,"Worst_Gayield12"),      
c("Best_GapOvi12" , "Worst_GapOvi12"),       
c("Best_GaqDry12" , "Worst_GaqDry12"),       
c("Best_GaColour12" , "Worst_GaqColour12"),    
c("Best_GaqHeavy12" , "Worst_GaqHeavy12"),    
c("Best_Gaqsmooth12" , "Worth_Gaqsmooth12"),    
c("Best_GaqTaste12" , "Worst_GaqTaste12"),    
c("Best_Gaqwoodyfil12" , "Worst_Gaqwoodyfil12"),  
c("Best_GaqOvi12" , "Worst_GaqOvi12"),       
c("Best_EbaMould12" , "Worst_EbaMould12"),     
c("Best_EbaSoft12" , "Worst_EbaSoft12"),      
c("Best_EbaDraw12" , "Worst_EbaDraw12"),      
c("Best_EbaSmooth12" , "Worst_EbaSmooth12"),    
c("Best_Ebacolour12" , "Worst_Ebacolour12"),    
c("Best_EbaNstick12", "Worst_EbaNstick12"),    
c("Best_Ebaswell12" , "Worst_Ebaswell12"),     
c("Best_EbaTaste12" , "Worst_EbaTaste12"),     
c("Best_EbaOvi12" , "Worst_EbaOvi12"),       
c("Best_EbaAFMould12" , "Worst_EbaAFMould12"),   
c("Best_EbaAFSoftness12" , "Worst_EbaAFSoftness12"),
c("Best_EbaAFDraw12" , "Worst_EbaAFDraw12"),    
c("Best_EbaAFSmooth12" , "Worst_EbaAFSmooth12"),  
c("Best_EbaAFNodour12" , "Worst_EbaAFNodour12"),  
c("Best_EbaAFTaste12" , "Worst_EbaAFTaste12"),   
c("Best_EbaAFcolour12" ,"Worst_EbaAFcolour12"),  
c("Best_EbaAFOvi12" ,"Worst_EbaAFOvi12"),    
c("Best_FEOvi12" ,"Worst_FEOvi12"),       
c("Best_replant12" , "Worst_replant12")) 
rowtie = tibble()
for(i in trtex){
  for(j in rownames(Rank22)){
    dt = Rank22[j,c("VARIETY.A", "VARIETY.B", "VARIETY.C", i)]
    dt1 = dt[,i]
    
    if(dt1[[1]] == dt1[[2]]){
      xx <-cbind(trait = t(i), rownb = j)
      head(xx)
      colnames(xx)[1:2] <- c("Trait1","Trait2")
      rowtie = rbind(rowtie,xx)
    }
   #  mid = c("A","B","C")[which(!c("A","B","C") %in% c(dt[,i][[1]],dt[,i][[2]]))]
   # dt$middle <- mid
  }
 
}

