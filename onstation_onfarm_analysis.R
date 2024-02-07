library(dplyr)
on_station<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/on_station.csv")
colnames(on_station)

Traits<- c(1, 2, 6, 8 , 17, 19, 24, 25, 57, 58, 79, 81, 82,
            84, 86, 89,  99, 100, 101)
New_on_station<- on_station[,Traits]
head(New_on_station)

Trait_names<- c("fresh.root.yield_os", "fresh.shoot.weight_os","initial.vigor_os",
                "plant.architecture_os","plant.height_os","plant.stands_os",
                "root.number.per.plot_os", "rotted.storage.root_os","root.color_os",
                "root.shape_os", "root.size_os")
colnames(New_on_station)[-c(1:8)] <- Trait_names
unique(New_on_station$germplasmName) # 26 genotypes
summary(New_on_station)
New_on_station$locationName<- as.factor(New_on_station$locationName)
New_on_station$studyYear <- as.factor(New_on_station$studyYear)
New_on_station$germplasmName <- as.factor(New_on_station$germplasmName)
New_on_station$replicate <- as.factor(New_on_station$replicate)
New_on_station$blockNumber<- as.factor(New_on_station$blockNumber)
New_on_station$studyName <- as.factor(New_on_station$studyName)
Blupmean_station = tibble()
for(Trait in Trait_names){
  eval(parse(text = paste(
    "outmod <- lmer(",Trait," ~ (1|germplasmName) + (1|studyName) + (1|replicate:studyName),data = New_on_station)" # better model
  )))
  idout <- which(abs(stats::rstudent(outmod)) >3) # remove outliers
  if(length(idout) == 0){
    Numx <- New_on_station}else{
      Numx <- New_on_station[-idout,]
    }
  
  eval(parse(text = paste(
    "model1 <- lmer(",Trait," ~ (1|germplasmName) + (1|studyName) + (1|replicate:studyName) , data = Numx )")))
  summary(model1)
  prd = coef(model1)$germplasmName # get the coefficient of the model
  # prd1 = prd$
  colnames(prd)[1] = "BLUPsMean"
  prd$Genotype = rownames(prd)
  prd = prd[,c(2,1)]
  rownames(prd) = NULL
  prdT <- cbind(Trait = Trait, prd)
  Blupmean_station = rbind(Blupmean_station, prdT)
}

which(abs(stats::rstudent(model1)) >3)
library(reshape2)
Blupmean_station_wide = dcast(data = Blupmean_station, formula = Genotype ~ Trait,
                       fun.aggregate = mean, value.var = "BLUPsMean", na.rm = T)
rownames(Blupmean_station_wide) = Blupmean_station_wide$Genotype



############## On_farm numeric data
Num_22 <-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Tricot_2022_harvest_numeric.csv")

Num_22 = as.data.frame(Num_22)
head(Num_22)
colnames(Num_22)
Num22 = Num_22[,!colnames(Num_22) %in% c("comment" , "initial plant vigor")]
colnames(Num22)
summary(Num22)

str(Num22)

Num22$location <- as.factor(Num22$location)
Num22$farmers_number <- as.factor(Num22$farmers_number)
Num22$farmers.name<- as.factor(Num22$farmers.name)
Num22$variety_code <- as.factor(Num22$variety_code)
Num22$genotype.name <- as.factor(Num22$genotype.name)
Num22$genotype <- as.factor(Num22$genotype)
Num22$package.number <- as.factor(Num22$package.number)
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


Num22$mean_ht= apply(Num22[,11:13],1,mean)
Num22$mean_brnch_ht= apply(Num22[,9:11],1,mean)
Num22$total_yield_plot = (Num22$total.rt.wt*30)/Num22$noh #  30 is total number of stands in a 5*6 plot, noh is number of stands harvested
Num22$mkt_yield_plot = (Num22$sellable.rt.wt *30)/Num22$noh
Num22$efficient_yield = (Num22$mkt_yield_plot/Num22$total_yield_plot)* 100
Num22$total_root_num_plot = (Num22$total.rt.no *30)/Num22$noh
Num22$mkt_root_number_plot = (Num22$sell.rt.no*30)/Num22$noh
Num22$efficient_rootNum = (Num22$mkt_root_number_plot/Num22$total_root_num_plot)*100
Num22$shwt_plot = (Num22$sht.wt *30)/Num22$noh


colnames(Num22)
idL= which(Num22$genotype=="local") # identify th local varieties
Num22a = Num22[-idL,] #remove the local varieties
dim(Num22a)
colnames(Num22a)

Num22a$rot = as.numeric(Num22a$rot)
Traits <- colnames(Num22a)[-c(1:14, 16, 17, 21, 24, 27:28, 29:32)] # remove these cells
library(tibble)
id_o <- which(Num22a$efficient_yield == 0) # find values less than 0

Num22b = Num22a
Num22b[id_o, "efficient_yield"] = NA # change the 0s to NA
#id_wt <- which(Num22a$unsellable.rt.wt == 0)
#Num22b[id_wt, "unsellable.rt.wt"] = NA
Blupmean_1 = tibble()
for(Trait in Traits){
  eval(parse(text = paste(
    "outmod <- lmer(",Trait," ~ (1|genotype) +
(1|farmers_number:location)+ (1|location), data = Num22b )" # better model
  )))
  idout <- which(abs(stats::rstudent(outmod)) >3) # remove outliers
  if(length(idout) == 0){
    Numx <- Num22b}else{
      Numx <- Num22b[-idout,]
    }
  
  eval(parse(text = paste(
    "model1 <- lmer(",Trait," ~ (1|genotype) + (1|farmers_number:location) + (1|location), data = Numx )")))
  summary(model1)
  prd = coef(model1) # get the coefficient of the model
  prd1 = prd$genotype
  colnames(prd1)[1] = "BLUPsMean"
  prd1$Genotype = rownames(prd1)
  prd1 = prd1[,c(2,1)]
  rownames(prd1) = NULL
  prdT <- cbind(Trait = Trait, prd1)
  Blupmean_1 = rbind(Blupmean_1, prdT)
}

which(abs(stats::rstudent(model1)) >3)
library(reshape2)
Blupmean_1wide = dcast(data = Blupmean_1, formula = Genotype ~ Trait,
                       fun.aggregate = mean, value.var = "BLUPsMean", na.rm = T)
rownames(Blupmean_1wide) = Blupmean_1wide$Genotype


# correlation
Blupmean_1wide$Genotype
Blupmean_station_wide$Genotype

prent_geno_farm <-which(Blupmean_1wide$Genotype %in% Blupmean_station_wide$Genotype)
Blupmean_1wide_sel = Blupmean_1wide[prent_geno_farm, ]
dim(Blupmean_1wide_sel)
prent_geno_sta <-which(Blupmean_station_wide$Genotype  %in% Blupmean_1wide_sel$Genotype )
Blupmean_station_wide_sel = Blupmean_station_wide[prent_geno_sta,]
dim(Blupmean_station_wide_sel)
all(Blupmean_station_wide_sel$Genotype %in% Blupmean_1wide_sel$Genotype )
Blupmean_station_wide_sel=Blupmean_station_wide_sel[order(Blupmean_station_wide_sel$Genotype),]
Blupmean_1wide_sel = Blupmean_1wide_sel[order(Blupmean_1wide_sel$Genotype),]
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
tst <- round(cor(Blupmean_1wide_sel[,-1],Blupmean_station_wide_sel[,-1], use = "p"),2)# p=pairwise comparison
# corfst <- corr.test(x = Blupsmean_comb_Farm_sta[,-c(1,20)], use = "p")
# corFarm_Station_numeric<-round(corfst$r,3)
# corPval_Farm_Station_numeric<-round(corfst$p,3)
#write.csv(x= tst, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_corr_OnstationNum_blup_vs_onfarmNumeric_blup.csv")
write.csv(x= corPval_Farm_Station_numeric, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/all_Pval_OnstationNum_blup_vs_onfarmNumeric_blup.csv")
write.csv(x= tst, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/New_corr_OnstationNum_blup_vs_onfarmNumeric_blup.csv")
