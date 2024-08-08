packages_used <- c("workflowr","dplyr", "tidyverse", "ggplot2", "here",
                   "stringr","lme4","metan", "FactoMineR","corrplot",
                   "ASRgenomics", "reshape2", "Hmisc", "agricolae",
                   "sommer", "PlackettLuce", "readr", "pheatmap", "Matrix")

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

Num21 <-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/2020_2021_tricot/2020_2021tricot numeric data.csv")
rank21 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/2020_2021_tricot/2020_2021_tricot_rank.csv")

# Get Blups of  the numeric data

Num21 = as.data.frame(Num21)

#Num21 = Num_21[,!colnames(Num_21) %in% c("comment")] # removed coment



Num21$COMMUNITY <- as.factor(Num21$COMMUNITY)
Num21$Farmer_number <- as.factor(Num21$Farmer_number)
Num21$VARIETIES <- as.factor(Num21$VARIETIES)

Num21$No.Std.Hav <- as.numeric(Num21$No.Std.Hav)
Num21$Pt.Hht1 <-as.numeric(Num21$Pt.Hht1)
Num21$Pt.Ht.2 <-as.numeric(Num21$Pt.Ht.2)
Num21$Pt.Ht.3 <-as.numeric(Num21$Pt.Ht.3)

Num21$Bch.1 <-as.numeric(Num21$Bch.1)
Num21$Bch.2 <-as.numeric(Num21$Bch.2)
Num21$Bch.3 <-as.numeric(Num21$Bch.3)
Num21$Shape <- as.numeric(Num21$Shape)
Num21$colour <- as.numeric(Num21$colour)

# Num21$root.number.per.plot<- as.numeric(Num21$root.number.per.plot)
#
# Num21$root.yield.per.plot <- as.numeric(Num21$root.yield.per.plot)
# Num21$shoot.weight.kg<- as.numeric(Num21$shoot.weight.kg)

Num21$Arch<- as.numeric(Num21$Arch)

Num21$mean_ht= (Num21$Pt.Hht1 + Num21$Pt.Ht.2 + Num21$Pt.Ht.3)/3
Num21$mean_brnch_ht= (Num21$Bch.1 + Num21$Bch.2 + Num21$Bch.3)/3


colnames(Num21)

Num21$Rot = as.numeric(Num21$Rot)
Num21$missing= 30-Num21$No.Std.Hav
onstat_blue= read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/onstation_BLUE_mean_after_prophav.csv")

##################################################################################
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
                "Yield_os" = "fresh.storage.root.weight.per.plot.CO_334.0000012_tonne_ha",
                "Shoot_weight_os"= "fresh.shoot.weight.measurement.in.kg.per.plot.CO_334.0000016_tonne_ha",
                "Plant_height_os" = "plant.height.measurement.in.cm.CO_334.0000018","Number_harvested_os"= "plant.stands.harvested.counting.CO_334.0000010" ,
                "Number_of_roots"="root.number.per.plot.counting.CO_334.0000011_tonne_ha",
                "Rotten_roots_os" = "rotted.storage.root.counting.CO_334.0000084_tonne_ha"
  )%>%
  dplyr::select("programDbId" , "studyName" ,
                "studyDesign","locationName" , "germplasmName" ,
                "replicate","blockNumber","Yield_os", "Shoot_weight_os" , "Number_of_roots","Plant_height_os",
                "First_branch_ht_os","Rotten_roots_os","Prop_hav")

unique(New_on_station3$germplasmName)
colnames(New_on_station3)
New_on_station3$locationName<- as.factor(New_on_station3$locationName)
#New_on_station1$studyYear <- as.factor(New_on_station1$studyYear)
New_on_station3$germplasmName <- as.factor(New_on_station3$germplasmName)
New_on_station3$replicate <- as.factor(New_on_station3$replicate)
New_on_station3$blockNumber<- as.factor(New_on_station3$blockNumber)
New_on_station3$studyName <- as.factor(New_on_station3$studyName)
#on_station_summary=summary(New_on_station3)
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

BLUEmean_station_wide=BLUEmean_station_wide[-idm2,]


#################################################################################

Traits2 <- colnames(Num21)[c(1, 4, 5,7, 10, 11, 14, 16, 36, 37)] # remove these columns
library(tibble)

Num21$Total.Root.Num = as.numeric(Num21$Total.Root.Num)
Num21$rep= as.factor(Num21$rep)
Num21$Rot=as.numeric(Num21$Rot)
New_onfarm21<- Num21[,Traits2]
trials_area21 <-c(30)
correc_fact21<- 10000/trials_area21 # convert to hectare
on_farm_trait21<- c( "Rot", "Total.Root.Num", "Total.Rt.Wt" , "shoot.weight.kg" , "mean_ht" , "mean_brnch_ht")

#Loop
area_ha21<-tibble()
for(trial in unique(New_onfarm21$COMMUNITY)){
  dt21= New_onfarm21[New_onfarm21$COMMUNITY==trial,]
  for(trait in on_farm_trait21){
    for(cf in correc_fact21){ # cf is the plant pop per plot area( it varies from trials)

      tonne <- dt21[,trait]*cf/1000
      dt21[,paste(c(trait, "tonne_ha"),collapse = "_")] <- tonne

    }
  }
  area_ha21 <- rbind(area_ha21,dt21)
}

onfarm21_tonne_ha<-area_ha21
#onstat_tonne_ha$prophav <- onst
#idl= which(onfarm21_tonne_ha$VARIETIES=="local")
#onfarm2_tonne_ha= droplevels(onfarm_tonne_ha[-idl,])# remove the local


#####
area_ha_prop_hav21<-tibble()
for(trial in unique(onfarm21_tonne_ha$COMMUNITY)){
  dt21a <- onfarm21_tonne_ha[onfarm21_tonne_ha$COMMUNITY==trial,]#subsetting location as trial
  for(trait in "No.Std.Hav"){
    for(cf in trials_area21){ #plant density


      Proportion_harvested1a<-  dt21a[,trait]/cf  # no of plant available divide by 30 9plant density
      dt21a[, "Prop_hav"] <- Proportion_harvested1a

    }

  }
  area_ha_prop_hav21 <- rbind(area_ha_prop_hav21,dt21a)
}

area_ha_prop_hav21$mean_ht_tonne_ha= NULL
area_ha_prop_hav21$mean_brnch_ht_tonne_ha= NULL

New_farm21<-area_ha_prop_hav21

New_farm21$Rot_ha= New_farm21$Rot_tonne_ha*1000
New_farm21$Total.Root.Num_tonne_ha= New_farm21$Total.Root.Num_tonne_ha*1000
New_farm21$Total.Root.Num_ha=NULL
New_farm21<- New_farm21%>%
 dplyr:: rename("Total.Root.Num_ha"="Total.Root.Num_tonne_ha")

sd(New_farm21$mean_ht, na.rm=T)
sd(New_farm21$Rot_ha, na.rm=T)
sd(New_farm21$Total.Root.Num_ha, na.rm=T)
sd(New_farm21$Total.Rt.Wt_tonne_ha, na.rm=T)
sd(New_farm21$shoot.weight.kg_tonne_ha, na.rm=T)
#convert the0 in plant height to Na
#ib = which(New_on_station2$mean_ht==0)
#New_on_station1[ib,]
# New_on_station1[ib,"plant.height.measurement.in.cm.CO_334.0000018"]
# New_on_station1[ib,"plant.height.measurement.in.cm.CO_334.0000018"]=NA #
# New_on_station1[ib,"plant.height.measurement.in.cm.CO_334.0000018"]




Trait_names21<- colnames(New_farm21)[-c(1:8,15)]
library(dplyr)
New_farm21a = New_farm21%>%
  dplyr::rename("First_branch_ht_of"= "mean_brnch_ht",
                "Yield_of" = "Total.Rt.Wt_tonne_ha",
                "Shoot_weight_of"= "shoot.weight.kg_tonne_ha" ,
                "Plant_height_of" = "mean_ht" ,
                "Number_of_roots_of"= "Total.Root.Num_ha" ,
                "Rotten_roots_of" =  "Rot_ha"
  )%>%
  dplyr::select("VARIETIES"  , "Farmer_number", "COMMUNITY",
                "Yield_of", "Shoot_weight_of" , "Number_of_roots_of","Plant_height_of",
                "First_branch_ht_of","Rotten_roots_of","Prop_hav")

str(New_farm21a )
on_station_summary_of=summary(New_farm21a)
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


str(New_farm21a)
Bluemean_farm21 = tibble()
for(Trait in Trait_names){
  eval(parse(text = paste(
    "outmod_f21 <- lmer(",Trait," ~ (1|VARIETIES) + (1|COMMUNITY) + (1|Farmer_number:COMMUNITY),data = New_farm21a )" # better model
  )))
  idout <- which(abs(stats::rstudent(outmod_f21)) >3) # remove outliers
  if(length(idout) == 0){
    Numx <- New_farm21a}else{
      Numx <- New_farm21a[-idout,]
    }
  library(sommer)
  eval(parse(text = paste(
    "model1_f21 <- mmer(fixed = ",Trait," ~ VARIETIES-1 + Prop_hav,
     random = ~ Farmer_number:COMMUNITY+COMMUNITY, data = Numx)")))
  prd = model1_f21$Beta
  colnames(prd)[2:3] <- c("Genotype","BLUEMean")

  Bluemean_farm21 = rbind(Bluemean_farm21, prd)
}

#which(abs(stats::rstudent(model1)) >3)
library(reshape2)
idmf <- which(model1_f21$Beta$Effect %in% "Prop_hav")

BLUEmean_farm21_wide = dcast(data =  Bluemean_farm21, formula = Genotype ~ Trait,
                           value.var = "BLUEMean", na.rm = T)
BLUEmean_farm21_wide$Genotype <- gsub(pattern = "VARIETIES", replacement = "",BLUEmean_farm21_wide$Genotype)

rownames(BLUEmean_farm21_wide) =BLUEmean_farm21_wide$Genotype

idmf <- which(BLUEmean_farm21_wide$Genotype %in% "Prop_hav")

BLUEmean_farm21_wide=BLUEmean_farm21_wide[-idmf,]

dim(BLUEmean_farm21_wide)


write.csv(x=BLUEmean_farm21_wide,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/tricot21_numeric_Blue_Prophav.csv" ) # the BLUE means for on farm numeric data



######


library(reshape2)




onstation <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/onstation_BLUE_mean_after_prophav.csv" ) # the BLUP means for onfarm numeric data


onfarm_21 <-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/tricot21_numeric_Blue_Prophav.csv" ) # the BLUE means for on farm numeric data

#onstationA<- onstation%>%
#dplyr:: rename(  "rotten_roots_os" = "rot", "number_of_roots"= "total.rt.no" , "yield_os"= "total.rt.wt" ,
           "shoot_weight_os" ="sht.wt" ,   "plant_height_os" = "mean_ht" , "first_branch_ht_os"= "mean_brnch_ht")%>%
#dplyr::select("Genotype","yield_os", "shoot_weight_os", "number_of_roots", "plant_height_os", "first_branch_ht_os","rotten_roots_os" )


#onfarm_21A<-onfarm_21%>%
  dplyr::select("Genotype" ,"yield_of" , "shoot_weight_of",
                "number_of_roots_of" , "plant_height_of",
                "First_branch_ht_of" ,"rotten_roots_of")

onfarm_21A<-onfarm_21
onstationA<- onstation
write.csv(x=onfarm_21A,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/tricot21_numeric_Blue_Prophav_rename.csv" ) # the BLUE means for on farm numeric data

present_onfarm<- which(onfarm_21A$Genotype %in% onstationA$Genotype)
onfarm_sel= onfarm_21A[present_onfarm, ]
dim(onfarm_sel)
present_onstation<- which(onstationA$Genotype %in% onfarm_sel$Genotype)
onstation_sel<- onstationA[present_onstation, ]
dim(onstation_sel)

all( onfarm_sel$Genotype %in% onstation_sel$Genotype)
order_onstation_sel <- onstation_sel[order(onstation_sel$Genotype), ]
order_onfarm_sel <- onfarm_sel[order(onfarm_sel$Genotype), ]

cor_onfarm21_onstation <- corTest(order_onfarm_sel[-c(1,2)],order_onstation_sel[-c(1,2)], adjust = "bonferroni")
cor_onfarm21_onstation$r = round(cor_onfarm21_onstation$r, 1)
cor_onfarm21_onstation$r
cor_onfarm21_onstation$p
cor_onfarm21_onstation$p.adj
dim(cor_onfarm21_onstation$r)
library(corrplot)


corrplot::corrplot(corr = cor_onfarm21_onstation$r,tl.cex = 0.6,p.mat = cor_onfarm21_onstation$p, method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.7,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
# to change the position of the significant asterics go use trace(corrplot, edit = TRUE), go to line 443 and place_points = function(sig.locs, point) {
# text(


Des_stat_2021=summary(Num2)

write.csv(x=Des_stat_2021,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/Descriptive_stat_2021.csv" ) # the BLUE means for on farm numeric data

stdevyield=sd(Num2$root.yield.per.plot, na.rm = TRUE)
stdevroot_num=sd(Num2$root.number.per.plot, na.rm = TRUE)
stdevshoot=sd(Num2$shoot.weight.kg, na.rm = TRUE)
stdevarch=sd(Num2$Arch , na.rm = TRUE)
stdevheight=sd(Num2$mean_ht , na.rm = TRUE)
stdevbranch=sd(Num2$mean_brnch_ht , na.rm = TRUE)
stdevrot=sd(Num2$Rot,  na.rm = TRUE)



# blue of the chemical data

chem_gari_21<- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/2020_2021_tricot/tricot_2021_chem_dmc.csv")

str(chem_gari_21)
chem_gari_21$WAC<- as.numeric(chem_gari_21$WAC)
chem_gari_21$Farmer_number<- as.factor(chem_gari_21$Farmer_number)
chem_gari_21$COMMUNITY<- as.factor(chem_gari_21$COMMUNITY)
chem_gari_21$VARIETIES <- as.factor(chem_gari_21$VARIETIES )
summary(chem_gari_21)


Traits <- colnames(chem_gari_21)[-c(1:5)] # remove these cells
library(tibble)


Bluemean_chem21 = tibble()
for(Trait in Traits){
  eval(parse(text = paste(
    "outmod_chem <- lmer(",Trait," ~ (1|VARIETIES) +
(1|Farmer_number:COMMUNITY) + (1|COMMUNITY), data = chem_gari_21)" # better model
  )))
  idout_chem21 <- which(abs(stats::rstudent(outmod_chem)) >3) # remove outliers
  if(length(idout_chem21) == 0){
  garri_chem21 <- chem_gari_21}else{
      garri_chem21 <- chem_gari_21[-idout_chem21,]
    }

  eval(parse(text = paste(
    "model_blue_chem21 <- lmer(",Trait," ~ (VARIETIES) +
(1|Farmer_number:COMMUNITY) + (1|COMMUNITY), data = garri_chem21 )")))
  summary(model_blue_chem21)
  em_chem21 <- as.data.frame(emmeans::emmeans(object = model_blue_chem21, specs = "VARIETIES"))
  prd_chem21= em_chem21[,1:2] # get the coefficient of the model
  # prd_chem = prd_chem$varieties
  # colnames(prd_chem)[1] = "BLUPsMean_chem"
  # prd_chem$varieties = rownames(prd_chem)
  # prd_chem = prd_chem[,c(2,1)]
  # rownames(prd_chem) = NULL
  prdTrait_chem <- cbind(Trait = Trait, prd_chem21)
  Bluemean_chem21= rbind(Bluemean_chem21, prdTrait_chem)
}

Bluemean_chem21_wide <- dcast(data = Bluemean_chem21,
                             formula = VARIETIES ~ Trait,
                             fun.aggregate = mean, value.var = "emmean")


write.csv(x=Bluemean_chem21_wide,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/tricot21_garri_chem_Blue.csv" ) # the BLUE means for on farm numeric data





# Blupmean_21 = tibble()
# for(Trait in Traits){
#   eval(parse(text = paste(
#     "outmod_21 <- lmer(",Trait," ~ (1|VARIETIES) +
# (1|Farmer_number:COMMUNITY)+ (1|COMMUNITY), data = Num21 )" # better model
#   )))
#   idout_21 <- which(abs(stats::rstudent(outmod_21)) >3) # remove outliers
#   if(length(idout_21) == 0){
#     Num_21a <- Num21}else{
#       Num_21a <- Num21[-idout_21,]
#     }
#
#   eval(parse(text = paste(
#     "model21 <- lmer(",Trait," ~ (1|VARIETIES) + (1|Farmer_number:COMMUNITY) + (1|COMMUNITY), data = Num_21a )")))
#   summary(model21)
#   prd = coef(model21) # get the coefficient of the model
#   prd1 = prd$VARIETIES
#   colnames(prd1)[1] = "BLUPsMean"
#   prd1$Genotype = rownames(prd1)
#   prd1 = prd1[,c(2,1)]
#   rownames(prd1) = NULL
#   prdT <- cbind(Trait = Trait, prd1)
#   Blupmean_21 = rbind(Blupmean_21, prdT)
# }
#
# #which(abs(stats::rstudent(model1)) >3)
# library(reshape2)
# Blupmean_21wide = dcast(data = Blupmean_21, formula = Genotype ~ Trait,
#                        fun.aggregate = mean, value.var = "BLUPsMean", na.rm = T)
#
#
# Blupmean_21wide = Blupmean_21wide %>%
#   dplyr::select("mean_brnch_ht", "mean_ht", "mkt_root_number_plot", "mkt_yield_plot", "root_num_plot","rot","rt.colour", "rt.shape","shwt_plot", "yield_per_plot" )
# rownames(Blupmean_21wide) = Blupmean_21wide$Genotype
# meanheat = Blupmean_21wide[,-1]
# str(meanheat)
# head(meanheat)
# meanheat = as.matrix(meanheat)
# meanheatsc = scale(meanheat)
#
#
# pheatmap(meanheatsc, display_numbers = T, fontsize = 7) # A heatmap that displays values in the cell
# #heatmap(x = meanheatsc, cex.axis =10)
# str(Blupmean_1wide)
#
# str(Blupmean_1wide)








