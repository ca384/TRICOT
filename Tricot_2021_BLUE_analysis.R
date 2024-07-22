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

Traits <- colnames(Num21)[-c(1:9, 12:13, 15, 17:35,38)] # remove these cells
library(tibble)


Bluemean_21 = tibble()
for(Trait in Traits[c(1,2,3,4)]){
  eval(parse(text = paste(
    "outmod_21 <- lmer(formula=",Trait," ~ (1|VARIETIES) +
(1|Farmer_number:COMMUNITY) + (1|COMMUNITY) + missing, data = Num21 )" # better model
  )))
  idout <- which(abs(stats::rstudent(outmod_21)) >3) # remove outliers
  if(length(idout) == 0){
    Numx_a <- Num21}else{
      Numx_a <- Num21[-idout,]
    }

  eval(parse(text = paste(
    "model21 <-mmer(fixed=",Trait," ~ VARIETIES-1 +  missing,
random= ~  Farmer_number:COMMUNITY + COMMUNITY,
rcov = ~ units, data = Numx_a )" # better model
  )))

  idm <- which(model21$Beta$Effect %in% "missing")
  Blue21 <- model21$Beta[-idm,]
  colnames(Blue21)[3] = "BLUEMean"
  colnames(Blue21)[2] = "Genotype"
  Blue21$Genotype <- gsub(pattern = "VARIETIES", replacement = "",
                         x = Blue21$Genotype )
  Bluemean_21 = rbind(Bluemean_21, Blue21)
}
# (10000 * xkg)/30
Bluemean_21$tonnes_ha <- (Bluemean_21$BLUEMean * 333.3)/1000
#which(abs(stats::rstudent(model1)) >3)
library(reshape2)
Bluemean_21wide = dcast(data = Bluemean_21, formula = Genotype ~ Trait,
                       fun.aggregate = mean, value.var = "tonnes_ha", na.rm = T)

# for height
Bluemean_21b = tibble()
for(Trait in Traits[c(5,6)]){
  eval(parse(text = paste(
    "outmod_21a <- lmer(formula=",Trait," ~ (1|VARIETIES) +
(1|Farmer_number:COMMUNITY) + (1|COMMUNITY) , data = Num21 )" # better model
  )))
  idout <- which(abs(stats::rstudent(outmod_21a)) >3) # remove outliers
  if(length(idout) == 0){
    Numx_b <- Num21}else{
      Numx_b <- Num21[-idout,]
    }

  eval(parse(text = paste(
    "model2_b <-mmer(fixed=",Trait," ~ VARIETIES-1,
random= ~  Farmer_number:COMMUNITY + COMMUNITY ,
rcov = ~ units, data = Numx_b )" # better model
  )))


  Blue21_b <- model2_b$Beta
  colnames(Blue21_b)[3] = "BLUEMean"
  colnames(Blue21_b)[2] = "Genotype"
  Blue21_b$Genotype <- gsub(pattern = "VARIETIES", replacement = "",
                         x = Blue21_b$Genotype )
  Bluemean_21b = rbind(Bluemean_21b, Blue21_b)
}

Bluemean_21b$cm <- (Bluemean_21b$BLUEMean)

library(reshape2)
Bluemean_21bwide = dcast(data = Bluemean_21b, formula = Genotype ~ Trait,
                       fun.aggregate = mean, value.var = "cm", na.rm = T)

Blue2021<- cbind(Bluemean_21wide,Bluemean_21bwide)

yield_21_blue<- Blue2021[-6]



write.csv(x=yield_21_blue,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/tricot21_numeric_Blue_missing.csv" ) # the BLUE means for on farm numeric data

onstation <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/2022_blues_model_missing_data2.csv")


onfarm_21 <-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/tricot21_numeric_Blue_missing.csv" ) # the BLUE means for on farm numeric data

onstationA<- onstation%>%
dplyr:: rename(  "rotten_roots_os" = "rot", "number_of_roots"= "total.rt.no" , "yield_os"= "total.rt.wt" ,
           "shoot_weight_os" ="sht.wt" ,   "plant_height_os" = "mean_ht" , "first_branch_ht_os"= "mean_brnch_ht")%>%
dplyr::select("yield_os", "shoot_weight_os", "number_of_roots", "plant_height_os", "first_branch_ht_os","rotten_roots_os" )





onfarm_21A<-onfarm_21%>%
  dplyr::select("VARIETIES","root.yield.per.plot", "root.number.per.plot",
                "shoot.weight.kg","mean_ht","mean_brnch_ht","Rot")%>%
  dplyr::rename("yield_of"="root.yield.per.plot", "number_roots_of" = "root.number.per.plot",
                "shoot_weight_of" ="shoot.weight.kg","plant_height_of" ="mean_ht","first_branch_ht_of"= "mean_brnch_ht",
                "rotten_roots_of" = "Rot")



present_onfarm<- which(onfarm_21A$VARIETIES %in% onstationA$VARIETIES)
onfarm_sel= onfarm_21A[present_onfarm, ]
dim(onfarm_sel)
present_onstation<- which(onstationA$VARIETIES %in% onfarm_21A$VARIETIES)
onstation_sel<- onstationA[present_onstation, ]
dim(onstation_sel)
all( onfarm_sel %in% onstation_sel)
order_onstation_sel <- onstation_sel[order(onstation_sel$VARIETIES), ]
order_onfarm_sel <- onfarm_sel[order(onfarm_sel$VARIETIES), ]

cor_onfarm21_onstation <- corTest(order_onfarm_sel[-1],order_onstation_sel[-1], adjust = "bonferroni")
cor_onfarm21_onstation$r = round(rs$r, 1)
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








