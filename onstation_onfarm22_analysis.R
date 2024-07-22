library(dplyr)
on_station<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/on_station.csv")
colnames(on_station)
count_table_df <- table(on_station$studyName, on_station$studyDesign )
count_df <- table(on_station$studyDesign )
count2_df <- table(on_station$studyYear, on_station$locationName)

write.csv(count_table_df,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/onstation_experiment_design.csv")
write.csv(count2_df, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/onstation_count.csv")
Traits<- c(1, 2, 6, 8 , 17, 19, 24, 25,55, 57, 58,59, 79, 81, 82, 83,
            84, 86, 89,  99, 100, 101)
New_on_station<- on_station[,Traits]
trials<- unique(New_on_station$studyName)
trials_area <-c(33,33,33,33,33,33,33,33,33,33,33,
                5, 16, 8, 33, 33, 33.5,
                16,16,16,8,8, 16,16, 22.4, 22.4,
              33, 33, 33, 33, 33, 33, 33.6, 33.6, 33.6, 33.6)
correc_fact<- 10000/trials_area # convert to hectare
on_sta_trait<- c("fresh.shoot.weight.measurement.in.kg.per.plot.CO_334.0000016",
                 "fresh.storage.root.weight.per.plot.CO_334.0000012" ,
                 "root.number.per.plot.counting.CO_334.0000011",
                 "rotted.storage.root.counting.CO_334.0000084")
#Loop
area_ha<-tibble()
for(trial in trials){
  dt <-  on_station[on_station$studyName==trial,]
  for(trait in on_sta_trait){
    for(cf in correc_fact){ # cf is the plant pop per plot area( it varies from trials)


  tonne <-  dt[,trait]*cf/1000
  dt[,paste(c(trait, "tonne_ha"),collapse = "_")] <- tonne

    }

  }
  area_ha <- rbind(area_ha,dt)
}
  onstat_tonne_ha<-area_ha
  #onstat_tonne_ha$prophav <- onst
  onstat_tonne_ha[, c()]

  summary(onstat_tonne_ha[,on_sta_trait])

 #####
  area_ha_prop_hav<-tibble()
  for(trial in trials){
    dt <-  onstat_tonne_ha[onstat_tonne_ha$studyName==trial,]
    for(trait in "plant.stands.harvested.counting.CO_334.0000010"){
      for(cf in trials_area){ #plant density


        Proportion_harvested<-  dt[,trait]/cf  # no of plant available
        dt[, "Prop_hav"] <- Proportion_harvested

      }

    }
    area_ha_prop_hav <- rbind(area_ha_prop_hav,dt)
  }

  New_on_station1<-area_ha_prop_hav
unique(New_on_station1$studyName)
#convert the0 in plant height to Na
ib = which(New_on_station1$plant.height.measurement.in.cm.CO_334.0000018==0)
New_on_station1[ib,]
New_on_station1[ib,"plant.height.measurement.in.cm.CO_334.0000018"]
New_on_station1[ib,"plant.height.measurement.in.cm.CO_334.0000018"]=NA #
New_on_station1[ib,"plant.height.measurement.in.cm.CO_334.0000018"]




Trait_names<- colnames(New_on_station)[-c(1:8)]
library(dplyr)
New_on_station3 = New_on_station1%>%
dplyr::rename("First_branch_ht_os"= "first.apical.branch.height.measurement.in.cm.CO_334.0000106",
              "yield_os" = "fresh.storage.root.weight.per.plot.CO_334.0000012_tonne_ha",
              "shoot_weight_os"= "fresh.shoot.weight.measurement.in.kg.per.plot.CO_334.0000016_tonne_ha",
              "plant_height_os" = "plant.height.measurement.in.cm.CO_334.0000018","number_harvested_os"= "plant.stands.harvested.counting.CO_334.0000010" ,
              "number_of_roots"="root.number.per.plot.counting.CO_334.0000011_tonne_ha",
              "rotten_roots_os" = "rotted.storage.root.counting.CO_334.0000084_tonne_ha"
              )%>%
  dplyr::select("programDbId" , "studyName" ,
              "studyDesign","locationName" , "germplasmName" ,
              "replicate","blockNumber","yield_os", "shoot_weight_os" , "number_of_roots","plant_height_os",
              "First_branch_ht_os","rotten_roots_os","Prop_hav")

unique(New_on_station3$germplasmName)
colnames(New_on_station3)
New_on_station3$locationName<- as.factor(New_on_station3$locationName)
#New_on_station1$studyYear <- as.factor(New_on_station1$studyYear)
New_on_station3$germplasmName <- as.factor(New_on_station3$germplasmName)
New_on_station3$replicate <- as.factor(New_on_station3$replicate)
New_on_station3$blockNumber<- as.factor(New_on_station3$blockNumber)
New_on_station3$studyName <- as.factor(New_on_station3$studyName)
on_station_summary=summary(New_on_station3)
str(New_on_station3)
write.csv(on_station_summary, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/on_station_summary.csv" )
sd(New_on_station2$yield_os, na.rm=T)
sd(New_on_station2$shoot_weight_os, na.rm=T)
sd(New_on_station2$number_of_roots, na.rm=T)
sd(New_on_station2$plant_height_os, na.rm=T)
sd(New_on_station2$First_branch_ht_os, na.rm=T)
sd(New_on_station2$rotten_roots_os, na.rm=T)

Trait_names<- c("yield_os", "shoot_weight_os" , "number_of_roots",
                "rotten_roots_os")


str(New_on_station3)
Bluemean_station = tibble()
for(Trait in Trait_names){
  eval(parse(text = paste(
    "outmod_N <- lmer(",Trait," ~ (1|germplasmName) + (1|studyName) + (1|replicate:studyName),data = New_on_station3)" # better model
  )))
  idout <- which(abs(stats::rstudent(outmod_N)) >3) # remove outliers
  if(length(idout) == 0){
    Numx <- New_on_station3}else{
      Numx <- New_on_station3[-idout,]
    }
library(sommer)
  eval(parse(text = paste(
    "model1_N <- mmer(fixed = ",Trait," ~ germplasmName-1 + Prop_hav,
     random = ~ studyName + replicate:studyName , data = Numx )")))
  prd = model1_N$Beta
  colnames(prd)[2:3] <- c("Genotype","BLUEMean")

  Bluemean_station = rbind(Bluemean_station, prd)
}

#which(abs(stats::rstudent(model1)) >3)
library(reshape2)
idm2 <- which(model1$Beta$Effect %in% "Prop_hav")

BLUEmean_station_wide = dcast(data = Bluemean_station, formula = Genotype ~ Trait,
                        value.var = "BLUEMean", na.rm = T)
BLUEmean_station_wide$Genotype <- gsub(pattern = "germplasmName", replacement = "", BLUEmean_station_wide$Genotype)

rownames(BLUEmean_station_wide) = BLUEmean_station_wide$Genotype

idm2 <- which(BLUEmean_station_wide$Genotype %in% "Prop_hav")

BLUEmean_station_wide[-idm2,]






############## On_farm numeric data
onfarm22<- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/2022_blues_model_missing_data.csv")


colnames(onfarm22)

BLUEmean_station_wide$Genotype %in% onfarm22$Genotype

prent_geno_farm <-which( onfarm22$Genotype %in% BLUEmean_station_wide$Genotype)
BLUEmean_farm_sel =  onfarm22[prent_geno_farm, ] # genotypes present both on farm and on station
dim(BLUEmean_farm_sel)

prent_geno_sta <-which(BLUEmean_station_wide$Genotype  %in% BLUEmean_farm_sel$Genotype )
BLUEmean_station_wide_sel = BLUEmean_station_wide[prent_geno_sta,]
dim(BLUEmean_station_wide_sel)
# Blupmean_station_wide_sel=Blupmean_station_wide_sel%>%
#   dplyr::rename(Rot_os = rotted.storage.root_os, root_number_plot_os= root.number.per.plot_os  )
all(BLUEmean_station_wide_sel$Genotype %in% BLUEmean_farm_sel$Genotype )
BLUEmean_station_wide_sel=BLUEmean_station_wide_sel[order(BLUEmean_station_wide_sel$Genotype),]
BLUEmean_farm_sel = BLUEmean_farm_sel[order(BLUEmean_farm_sel$Genotype),]
#data.frame(Blupmean_1wide_sel$Genotype,Blupmean_station_wide_sel$Genotype)
#Blupsmean_comb_Farm_sta = cbind(Blupmean_1wide_sel,Blupmean_station_wide_sel)
#colnames(Blupsmean_comb_Farm_sta)[c(1,20)]
library(agricolae)
# corFamSta = correlation(Blupsmean_comb_Farm_sta[,-c(1,20)])
# corFamSta$correlation
# corFamSta$pvalue
# corF <- cor(Blupsmean_comb_Farm_sta[,-c(1,20)], use = "p")
#
library(psych)
#tst <- round(cor(Blupmean_1wide_sel[,-1],Blupmean_station_wide_sel[,-1], use = "p"),2)# p=pairwise comparison
library(agricolae)

#tst1 <- correlation(Blupmean_1wide_sel[,-1],Blupmean_station_wide_sel[,-1])
#tst1$correlation
#tst1$pvalue
BLUEmean_farm_sel2= BLUEmean_farm_sel%>%
  dplyr:: rename("yield_of"="total.rt.wt", "shoot_weight_of"= "sht.wt",
                 "First_branch_ht_of"="mean_brnch_ht","plant_height_of"="mean_ht", "number_of_roots_of" = "total.rt.no",
                 "rotten_roots_of" = "rot" )%>%
  dplyr::select("yield_of", "shoot_weight_of", "number_of_roots_of","plant_height_of", "First_branch_ht_of","rotten_roots_of")

colnames(BLUEmean_farm_sel2)
colnames(BLUEmean_station_wide_sel)

# BLUEmean_2farm2_sel = BLUEmean_farm_sel%>%
#   dplyr:: rename("yield_of" ="yield_per_plot_of",  "First_branch_ht_of"= "height_branch_of",
#                  "number_roots_of" =  "root_number_of")%>%
#   dplyr::select("yield_of","number_roots_of", "shoot_weight_of","plant_height_of",
#                 "1st_branch_height_of", "rotten_roots_of" )

 BLUEmean_station_wide_sel2= BLUEmean_station_wide_sel%>%
  dplyr:: rename("number_of_roots_os" ="number_of_roots")

  rrs <- corTest(BLUEmean_farm_sel2,BLUEmean_station_wide_sel2[-1], adjust = "bonferroni")
rrs$r = round(rrs$r, 2)
rrs$r
rrs$p
rrs$p.adj
dim(rrs$r)
library(corrplot)


write.csv(x=BLUEmean_2wide_sel,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/onfarm_BLUE_mean.csv" ) # the BLUe means for onfarm numeric data
write.csv(x=BLUEmean_station_wide_sel2,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/onstation_BLUE_mean.csv" ) # the BLUP means for onfarm numeric data


corrplot::corrplot(corr = rrs$r,tl.cex = 0.6,p.mat = rrs$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
# to change the position of the significant asterics go use trace(corrplot, edit = TRUE), go to line 443 and place_points = function(sig.locs, point) {
# text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25,
#      labels = point, col = pch.col, cex = pch.cex,
#      lwd = 2)
# the significant level position changes by adding +0.25
trace(corrplot, edit=TRUE)
corrplot::corrplot(corr = rrs$r,tl.cex = 0.6,p.mat = rrs$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

# pairs.panels(Blupmean_1wide_sel[,-c(1,2, 3,4,13, 14,15,18, 19 )],Blupmean_station_wide_sel[,-c(1, 4,7)],   # plot distributions and correlations for all the data
#              gap = 0,
#              pch = ".",
#              cex = 1.5,
#              lm = TRUE,
#              ellipses=FALSE,stars = TRUE, method = "pearson")


psych::cor.plot(r = tst1$correlation)
BLUEmean_1wide_sel$initial.plant.vigor
# corfst <- corr.test(x = Blupsmean_comb_Farm_sta[,-c(1,20)], use = "p")
# corFarm_Station_numeric<-round(corfst$r,3)
# corPval_Farm_Station_numeric<-round(corfst$p,3)
#write.csv(x= tst, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_corr_OnstationNum_blup_vs_onfarmNumeric_blup.csv")
#write.csv(x= corPval_Farm_Station_numeric, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/all_Pval_OnstationNum_blup_vs_onfarmNumeric_blup.csv")
write.csv(x= tst, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/New_corr_OnstationNum_blup_vs_onfarmNumeric_blup.csv")

