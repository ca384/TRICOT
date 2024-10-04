library(dplyr)
library(gosset)
library(PlackettLuce)
library(corrplot)
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
  #onstat_tonne_ha[, c()]


  summary(onstat_tonne_ha[,on_sta_trait])
dim(onstat_tonne_ha)
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




Trait_names<- colnames(New_on_station1)[-c(1:8)]
library(dplyr)
New_on_station3 = New_on_station1%>%
dplyr::rename("First_branch_ht_os"= "first.apical.branch.height.measurement.in.cm.CO_334.0000106",
              "Yield_os" = "fresh.storage.root.weight.per.plot.CO_334.0000012_tonne_ha",
              "Shoot_weight_os"= "fresh.shoot.weight.measurement.in.kg.per.plot.CO_334.0000016_tonne_ha",
              "Plant_height_os" = "plant.height.measurement.in.cm.CO_334.0000018","Number_harvested_os"= "plant.stands.harvested.counting.CO_334.0000010" ,
              "Number_of_roots"="root.number.per.plot.counting.CO_334.0000011_tonne_ha",
              "Rotten_roots_os" = "rotted.storage.root.counting.CO_334.0000084_tonne_ha"
              )%>%
  dplyr::select("programDbId" , "studyName" ,
              "studyDesign","locationName" , "germplasmName" ,
              "replicate","blockNumber","Yield_os", "Shoot_weight_os" , "Number_of_roots", "Plant_height_os",
              "First_branch_ht_os","Rotten_roots_os","Prop_hav")

New_on_station3$Rotten_roots_os= New_on_station3$Rotten_roots_os*1000
New_on_station3$Number_of_roots= New_on_station3$Number_of_roots*1000
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
sd(New_on_station3$Yield_os, na.rm=T)
sd(New_on_station3$Shoot_weight_os, na.rm=T)
sd(New_on_station3$Number_of_roots, na.rm=T)
sd(New_on_station3$Plant_height_os, na.rm=T)
sd(New_on_station3$First_branch_ht_os, na.rm=T)
sd(New_on_station3$Rotten_roots_os, na.rm=T)

Trait_names<- c("Yield_os", "Shoot_weight_os" , "Number_of_roots",
                "Rotten_roots_os")


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

BLUEmean_station_wide= BLUEmean_station_wide[-idm2,]

################### blue for plant height and height at first branching
Trait_names_h<- c("Plant_height_os",  "First_branch_ht_os")

Bluemean_station_h = tibble()
for(Trait in Trait_names_h){
  eval(parse(text = paste(
    "outmod_h <- lmer(",Trait," ~ (1|germplasmName) + (1|studyName) + (1|replicate:studyName),data = New_on_station3)" # better model
  )))
  idout <- which(abs(stats::rstudent(outmod_N)) >3) # remove outliers
  if(length(idout) == 0){
    Numx <- New_on_station3}else{
      Numx <- New_on_station3[-idout,]
    }
  library(sommer)
  eval(parse(text = paste(
    "model1_h <- mmer(fixed = ",Trait," ~ germplasmName-1,
     random = ~ studyName + replicate:studyName , data = Numx )")))
  prd = model1_h$Beta
  colnames(prd)[2:3] <- c("Genotype","BLUEMean")

  Bluemean_station_h = rbind(Bluemean_station_h, prd)
}

#which(abs(stats::rstudent(model1)) >3)
library(reshape2)
#idmh <- which(model1_h$Beta$Effect %in% "Prop_hav")

BLUEmean_station_wide_h = dcast(data = Bluemean_station_h, formula = Genotype ~ Trait,
                              value.var = "BLUEMean", na.rm = T)
BLUEmean_station_wide_h$Genotype <- gsub(pattern = "germplasmName", replacement = "", BLUEmean_station_wide_h$Genotype)

rownames(BLUEmean_station_wide_h) = BLUEmean_station_wide_h$Genotype

#idm2 <- which(BLUEmean_station_wide_h$Genotype %in% "Prop_hav")


BLUEmean_station_wide1<- cbind(BLUEmean_station_wide,BLUEmean_station_wide_h)

BLUEmean_station_wide1[-6]
BLUEmean_station_wide1= BLUEmean_station_wide1%>%
  dplyr:: select("Genotype" , "Yield_os", "Shoot_weight_os" , "Number_of_roots",
                 "Plant_height_os", "First_branch_ht_os", "Rotten_roots_os")





############## On_farm numeric data
Num_22 <-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Tricot_2022_harvest_numeric.csv")

Num_22 = as.data.frame(Num_22)
head(Num_22)
colnames(Num_22)
Num22 = Num_22[,!colnames(Num_22) %in% c("comment")] # removed coment
colnames(Num22)
#Num22$missing_plot<- 30 - Num22$noh
#Num22$missing_plot<-NULL
Num22$location <- as.factor(Num22$location)
Num22$farmers_number <- as.factor(Num22$farmers_number)
Num22$farmers.name<- as.factor(Num22$farmers.name)
Num22$variety_code <- as.factor(Num22$variety_code)
Num22$genotype.name <- as.factor(Num22$genotype.name)
Num22$genotype <- as.factor(Num22$genotype)
Num22$Package_Number <- as.factor(Num22$Package_Number)
#Num22$package.number <- as.factor(Num22$package.number)
Num22$doh <- as.factor(Num22$doh)
Num22$plt.ht1 <-as.numeric(Num22$plt.ht1)
Num22$plt.ht2 <-as.numeric(Num22$plt.ht2)
Num22$plt.ht3 <-as.numeric(Num22$plt.ht3)

Num22$brnch.ht1 <-as.numeric(Num22$brnch.ht1)
Num22$brnch.ht2 <-as.numeric(Num22$brnch.ht2)
Num22$brnch.ht3 <-as.numeric(Num22$brnch.ht3)
Num22$rt.shape <- as.numeric(Num22$rt.shape)
Num22$rt.colour <- as.numeric(Num22$rt.colour)
Num22$unsell.rt.no <- as.numeric(Num22$unsell.rt.no)
Num22$sell.rt.no <- as.numeric(Num22$sell.rt.no)
Num22$total.rt.no <- as.numeric(Num22$total.rt.no)
Num22$sellable.rt.wt <- as.numeric(Num22$sellable.rt.wt)
Num22$total.rt.wt <- as.numeric(Num22$total.rt.wt)
Num22$sht.wt<- as.numeric(Num22$sht.wt)
Num22$wa <- as.numeric(Num22$wa)
Num22$ww <- as.numeric(Num22$ww)
Num22$cbbi <- as.numeric(Num22$cbbi)
Num22$cbbs<- as.numeric(Num22$cbbs)
Num22$cadi <- as.numeric(Num22$cadi)
Num22$cads <- as.numeric(Num22$cads)
Num22$initial.plant.vigor<- as.numeric(Num22$initial.plant.vigor)

Num22$mean_ht= (Num22$plt.ht1 + Num22$plt.ht2 + Num22$plt.ht3)/3
Num22$mean_brnch_ht= (Num22$brnch.ht1 + Num22$brnch.ht2 +Num22$brnch.ht3)/3



Traits1<- c(1, 3, 4, 7, 17, 20, 23, 26, 27, 35,36)
New_onfarm<- Num22[,Traits1]
New_onfarm$rot=as.numeric(New_onfarm$rot)
trials_area1 <-c(30)
correc_fact1<- 10000/trials_area1 # convert to hectare
on_farm_trait<- c( "rot", "total.rt.no", "total.rt.wt" , "sht.wt", "mean_ht" , "mean_brnch_ht")

#Loop
area_ha1<-tibble()
for(trial in unique(New_onfarm$location)){
  dt= New_onfarm[New_onfarm$location==trial,]
  for(trait in on_farm_trait){
    for(cf in correc_fact1){ # cf is the plant pop per plot area( it varies from trials)

tonne <- dt[,trait]*cf/1000
dt[,paste(c(trait, "tonne_ha"),collapse = "_")] <- tonne

    }
  }
  area_ha1 <- rbind(area_ha1,dt)
  }

onfarm_tonne_ha<-area_ha1

sd(area_ha1$mean_ht, na.rm=T)
sd(area_ha1$mean_brnch_ht, na.rm=T)
sd(area_ha1$total.rt.no_tonne_ha, na.rm=T)
sd(area_ha1$total.rt.wt_tonne_ha, na.rm=T)
sd(area_ha1$sht.wt_tonne_ha, na.rm=T)
#onstat_tonne_ha$prophav <- onst
idl= which(onfarm_tonne_ha$genotype=="local")
onfarm2_tonne_ha= droplevels(onfarm_tonne_ha[-idl,])# remove the local


#####
area_ha_prop_hav1<-tibble()
for(trial in unique(onfarm2_tonne_ha$location)){
  dt <- onfarm2_tonne_ha[onfarm2_tonne_ha$location==trial,]#subsetting location as trial
  for(trait in "noh"){
    for(cf in trials_area1){ #plant density


      Proportion_harvested1<-  dt[,trait]/cf  # no of plant available divide by 30 9plant density
      dt[, "Prop_hav"] <- Proportion_harvested1

    }

  }
  area_ha_prop_hav1 <- rbind(area_ha_prop_hav1,dt)
}

area_ha_prop_hav1$mean_ht_tonne_ha= NULL
area_ha_prop_hav1$mean_brnch_ht_tonne_ha= NULL

New_farm<-area_ha_prop_hav1


#convert the0 in plant height to Na
#ib = which(New_on_station2$mean_ht==0)
#New_on_station1[ib,]
# New_on_station1[ib,"plant.height.measurement.in.cm.CO_334.0000018"]
# New_on_station1[ib,"plant.height.measurement.in.cm.CO_334.0000018"]=NA #
# New_on_station1[ib,"plant.height.measurement.in.cm.CO_334.0000018"]




Trait_names<- colnames(New_farm)[-c(1:4,5:9,16)]
library(dplyr)
New_farm2 = New_farm%>%
  dplyr::rename("First_branch_ht_of"= "mean_brnch_ht",
                "Yield_of" = "total.rt.wt_tonne_ha",
                "Shoot_weight_of"= "sht.wt_tonne_ha" ,
                "Plant_height_of" = "mean_ht" ,
                "Number_of_roots_of"= "total.rt.no_tonne_ha",
                "Rotten_roots_of" =  "rot"
  )%>%
  dplyr::select("genotype" , "Package_Number" ,"farmers_number", "location",
                "Yield_of", "Shoot_weight_of" , "Number_of_roots_of","Plant_height_of",
                "First_branch_ht_of","Rotten_roots_of","Prop_hav")

str(New_farm2)
New_farm2$Number_of_roots_of= New_farm2$Number_of_roots_of*1000
New_farm2$Rotten_roots_of= New_farm2$Rotten_roots_of*1000
sd(New_farm2$Rotten_roots_of, na.rm=T)
sd(New_farm2$Number_of_roots_of, na.rm=T)
summary(New_farm2)
on_station_summary_of=summary(New_farm2)
# unique(New_farm2$genotype)
# write.csv(on_station_summary, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/on_station_summary.csv" )
# sd(New_on_station2$yield_os, na.rm=T)
# sd(New_on_station2$shoot_weight_os, na.rm=T)
# sd(New_on_station2$number_of_roots, na.rm=T)
# sd(New_on_station2$plant_height_os, na.rm=T)
# sd(New_on_station2$First_branch_ht_os, na.rm=T)
# sd(New_on_station2$rotten_roots_os, na.rm=T)

Trait_names<- c("Yield_of", "Shoot_weight_of" , "Number_of_roots_of","Plant_height_of", "First_branch_ht_of",
                "Rotten_roots_of")


str(New_farm2)
Bluemean_farm = tibble()
for(Trait in Trait_names){
  eval(parse(text = paste(
    "outmod_f <- lmer(",Trait," ~ (1|genotype) + (1|location) + (1|farmers_number:location),data = New_farm2 )" # better model
  )))
  idout <- which(abs(stats::rstudent(outmod_f)) >3) # remove outliers
  if(length(idout) == 0){
    Numx <- New_farm2}else{
      Numx <- New_farm2[-idout,]
    }
  library(sommer)
  eval(parse(text = paste(
    "model1_f <- mmer(fixed = ",Trait," ~ genotype-1 + Prop_hav,
     random = ~ farmers_number:location+location, data = Numx)")))
  prd = model1_f$Beta
  colnames(prd)[2:3] <- c("Genotype","BLUEMean")

  Bluemean_farm = rbind(Bluemean_farm, prd)
}

#which(abs(stats::rstudent(model1)) >3)
library(reshape2)
idm3 <- which(model1_f$Beta$Effect %in% "Prop_hav")

BLUEmean_farm_wide = dcast(data =  Bluemean_farm, formula = Genotype ~ Trait,
                              value.var = "BLUEMean", na.rm = T)
BLUEmean_farm_wide$Genotype <- gsub(pattern = "genotype", replacement = "",BLUEmean_farm_wide$Genotype)

rownames(BLUEmean_farm_wide) =BLUEmean_farm_wide$Genotype

idm3 <- which(BLUEmean_farm_wide$Genotype %in% "Prop_hav")

BLUEmean_farm_wide=BLUEmean_farm_wide[-idm3,]


############################
BLUEmean_station_wide1$Genotype %in% BLUEmean_farm_wide$Genotype
dim(BLUEmean_station_wide1)
prent_geno_farm <-which( BLUEmean_farm_wide$Genotype %in% BLUEmean_station_wide1$Genotype)
BLUEmean_farm_sel =  BLUEmean_farm_wide[prent_geno_farm, ] # genotypes present both on farm and on station
dim(BLUEmean_farm_wide)

prent_geno_sta <-which(BLUEmean_station_wide1$Genotype  %in% BLUEmean_farm_sel$Genotype )
BLUEmean_station_wide_sel = BLUEmean_station_wide1[prent_geno_sta,]
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
  dplyr::select("Yield_of", "Shoot_weight_of", "Number_of_roots_of","Plant_height_of", "First_branch_ht_of","Rotten_roots_of")

colnames(BLUEmean_farm_sel2)
colnames(BLUEmean_station_wide_sel)

# BLUEmean_2farm2_sel = BLUEmean_farm_sel%>%
#   dplyr:: rename("Yield_of" ="yield_per_plot_of",  "First_branch_ht_of"= "height_branch_of",
#                  "Number_roots_of" =  "root_number_of")%>%
#   dplyr::select("Yield_of","number_roots_of", "Shoot_weight_of","Plant_height_of",
#                 "First_branch_ht_of"", "Rotten_roots_of" )

 BLUEmean_station_wide_sel2= BLUEmean_station_wide_sel%>%
  dplyr:: rename("Number_of_roots_os" ="Number_of_roots")
  rrs <- corTest(BLUEmean_farm_sel2,BLUEmean_station_wide_sel2[-1], adjust = "bonferroni")
rrs$r = round(rrs$r, 2)
rrs$r
rrs$p
rrs$p.adj
dim(rrs$r)
library(corrplot)

write.csv(x=BLUEmean_farm_wide,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/complete_onfarm_BLUE_mean_after_prophav.csv" ) # the BLUe means for onfarm numeric data

write.csv(x=BLUEmean_farm_sel2,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/onfarm_BLUE_mean_after_prophav.csv" ) # the BLUe means for onfarm numeric data
write.csv(x=BLUEmean_station_wide_sel2,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/onstation_BLUE_mean_after_prophav.csv" ) # the BLUP means for onfarm numeric data


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

