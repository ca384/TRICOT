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




# import numeric data
Num_22 <-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Tricot_2022_harvest_numeric.csv")

Num_22 = as.data.frame(Num_22)
head(Num_22)
colnames(Num_22)
Num22 = Num_22[,!colnames(Num_22) %in% c("comment")] # removed coment
colnames(Num22)


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

  # Calculate weight per plant
  Num22$weight_per_plant <- Num22$total.rt.wt / Num22$noh

  # Calculate yield per plot
  Num22$yield_per_plot <-  Num22$weight_per_plant * 30
#Num22$total_yield_plot = (Num22$total.rt.wt*30)/Num22$noh #  30 is total number of stands in a 5*6 plot, noh is number of stands harvested
Num22$mkt_yield_plot = (Num22$sellable.rt.wt /Num22$noh)*30
Num22$efficient_yield = (Num22$mkt_yield_plot/Num22$yield_per_plot)* 100
Num22$root_num_plot = (Num22$total.rt.no /Num22$noh)*30
Num22$mkt_root_number_plot = (Num22$sell.rt.no/Num22$noh)*30
Num22$efficient_rootNum = (Num22$mkt_root_number_plot/Num22$root_num_plot)*100
Num22$shwt_plot = (Num22$sht.wt/Num22$noh)*30

### snps data
# tricot_snp= read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/29_tricot_snps.tsv", sep = "\t")
# dim(tricot_snp)
# colnames(tricot_snp)
# Num22$genotype

# data transformation
#Num22$log_total_yield <- log(Num22$total_yield_plot)
#Num22$log_mkt_wt <-log(Num22$mkt_yield_plot)
#Num22$log_total_root_num <- log(Num22$total_root_num_plot)
#Num22$log_mkt_num <- log(Num22$mkt_root_number_plot)
#Num22$log_shwt <- log(Num22$shwt_plot)
#Num22$eff_yield <- log(Num22$efficiet_yield)
#Num22$log_eff_rootNum <- log(Num22$efficiet_rootNum)
# for yield per plot

colnames(Num22)
idL= which(Num22$genotype=="local") # identify th local varieties
Num22a = Num22[-idL,] #remove the local varieties
dim(Num22a)
colnames(Num22a)

Num22a$rot = as.numeric(Num22a$rot)
Traits <- colnames(Num22a)[-c(1:14, 16, 17, 21, 24, 27:28)] # remove these cells
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

#which(abs(stats::rstudent(model1)) >3)
library(reshape2)
Blupmean_1wide = dcast(data = Blupmean_1, formula = Genotype ~ Trait,
      fun.aggregate = mean, value.var = "BLUPsMean", na.rm = T)

Blupmean_1wide = Blupmean_1wide %>%
  dplyr::select("mean_brnch_ht", "mean_ht", "mkt_root_number_plot", "mkt_yield_plot", "root_num_plot","rot","rt.colour", "rt.shape","shwt_plot", "yield_per_plot" )
rownames(Blupmean_1wide) = Blupmean_1wide$Genotype
meanheat = Blupmean_1wide[,-1]
str(meanheat)
head(meanheat)
meanheat = as.matrix(meanheat)
meanheatsc = scale(meanheat)


pheatmap(meanheatsc, display_numbers = T, fontsize = 7) # A heatmap that displays values in the cell
#heatmap(x = meanheatsc, cex.axis =10)
str(Blupmean_1wide)

str(Blupmean_1wide)
install.packages("readr") # to read in the .tsv data
# Load the installed Package
library(readr)


# model1a <- lmer(formula = total_yield_plot ~ (1|genotype) + (1|location), data = Num22a ) # better model
# summary(model1a)
# anova(model1a)
#
# model1b <- lmer(formula = total_yield_plot~ (1|genotype) + (1|location) + (1|farmers_number), data = Num22a ) # better model
# summary(model1b)
# anova(model1b)
#
# model1c <- lmer(formula = total_yield_plot~ (1|genotype) + (1|location) + (1|farmers_number:location), data = Num22a ) # better model
# summary(model1c)
# anova(model1c)
#
# model1a <- lmer(formula = total_yield_plot ~ (1|genotype) + (1|location) +(1|farmers_number)+ (1|genotype:location), data = Num22a ) # better model
# summary(model1a)
# anova(model1a)
# model1a <- lmer(formula = total_yield_plot ~ (1|genotype) + (1|farmers_number), data = Num22a ) # better model
# summary(model1a)
# anova(model1a)
#
# lmerTest::ranova(model1b)
# summary(Num22)
# round(abs(stats::rstudent(model = model1)),3)
# model1$residuals
#
#
# summary.aov(model1)
# fff = fitted(model1)
# fff
# model1$coefficients
# pp = predict(object = model1)
# pp
# emmeans::emmeans(object = model1, specs = "genotype")
# off<- c(171, 197,261) # subset the outliers
#
# Num22[off,c("total_yield_plot","genotype.name","location", "farmers.name")] # select the. outliers from the data frame
# Num22[off,"total_yield_plot"]=NA # make the selected outliers NA
#
# #outlier_total_yield <-which(Num22$total_yield_plot %in% boxplot.stats(Num22$total_yield_plot)$out) #Removing outliers using boxplot stat
# #Num22$total_yield_plot[outlier_total_yield] = NA
#
#
# model1 <- lmer(formula = total_yield_plot~ (1|genotype) + location +   (1|genotype:location), data = Num22 ) # better model
# summary(model1)
#
# ff = coef(model1)
# predict(model1)
# plot(model1)
# ff$genotype
# data.frame(predict(model1))
# view(Num22)
# unique(Num22$genotype)
# library(sommer)
# model2 <- mmer(fixed = total_yield_plot~ location,
#                random = ~ genotype + genotype:location,
#                data = Num22 ) # better model
# model2$U$genotype$total_yield_plot + model2$
# summary(model2)

ggplot(data = Num22,aes(x= yield_per_plot, y=location,fill=genotype)) +
  geom_boxplot() # cluster on location
ggplot(data =  Numx,aes(x= yield_per_plot, y=genotype,fill=genotype)) +
  geom_boxplot()

des_stat=summary( Numx)
write.csv(x=des_stat, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/Descrptive_stat_2022.csv")
std_yield= sd(Numx$yield_per_plot, na.rm=TRUE)
std_yshwt= sd(Numx$shwt_plot, na.rm=TRUE)
std_number= sd(Numx$root_num_plot, na.rm=TRUE)
std_height= sd(Numx$mean_ht, na.rm=TRUE)
std_branch= sd(Numx$mean_brnch_ht, na.rm=TRUE)
std_rot= sd(Numx$rot, na.rm=TRUE)
# for total root number per plot

#Num22[Not,c("total_yield_plot","genotype.name","location", "farmers.name")]


off2 <- c(228,176,112)
Num22[off2,c("root_num_plot","genotype.name","location", "farmers.name")] # select the. outliers from the data frame
Num22[off2,"root_num_plot"]=NA
outlier_root_num_plot <-which(Num22$root_num_plot %in% boxplot.stats(Num22$root_num_plot)$out) #Removing outliers using boxplot stat
Num22$root_num_plot[outlier_root_num_plot] = NA

ggplot(data = Num22,aes(x= root_num_plot, y=location,fill=genotype)) +
  geom_boxplot()
ggplot(data = Num22,aes(x= root_num_plot, y=genotype,fill=genotype)) +
  geom_boxplot()

# model2 <- lm(formula = root_num_plot~ genotype + location + data = Num22 ) # better model
# plot(model2)
# summary.aov(model2)

# effecient yield
# model3 <- lm(formula = efficient_yield~ genotype + location +  farmers_number + location:farmers_number+ genotype:location,data = Num22 ) # better model
# plot(model3)
# summary.aov(model3)
# off3 <- c(209,173,190)
# Num22[off3,c("efficient_yield","genotype.name","location", "farmers.name")] # select the. outliers from the data frame
# Num22[off3,"efficient_yield"]=NA
# outlier_eff_yield <-which(Num22$efficient_yield %in% boxplot.stats(Num22$efficient_yield)$out) #Removing outliers using boxplot stat
# Num22$efficient_yield[outlier_eff_yield] = NA
#
# x11
# ggplot(data = Num22,aes(x= efficient_yield, y=location,fill=genotype)) +
#   geom_boxplot()
# ggplot(data = Num22,aes(x= efficient_yield, y=genotype,fill=genotype)) +
#   geom_boxplot()


# model2 <- lm(formula =log_total_yield~ genotype.name + location + farmers.name:location +
#                genotype.name:location,data = Num22 )
# plot(model2)
# summary.aov(model2)
# predict(model2)
# ggplot(data = Num22,aes(x= log_total_yield, y=genotype.name,fill=genotype.name)) +
#   geom_boxplot()

library(dplyr)
library(daewr)
# tab1 <- xtabs(~ genotype + farmers_number+location, data = Num22)
# fit.yield <- aov(total_yield_plot ~ genotype + farmers_number+location, data = Num22)
# drop1(fit.yield, test = "F")


library(multcomp)

# contr_yield <- glht(fit.yield, linfct = mcp(genotype  = "Tukey"))
# summary(contr_yield, test = adjusted("none"))
# contr_yield$vcov

library(stats)
# traitnames<- (c("Num22$mean_brnch_ht","mean_ht", "total_yield_plot", "mkt_yield_plot", "total_root_num_plot", "Cmkt_root_number_plot", "efficiet_yield", "efficiet_rootNum"))
# filtered_value <- tibble()
# for(traits in traitnames){
#   trs = paste0(traits) # the loop will be able to select  and analyse each trait
#   out_ind <- which(Num22[,paste(traits)] %in% boxplot.stats(Num22[,paste(traits)])$out) # outliers
#   if(length(out_ind) == 0){
#     New_Data = Num22
#   }else{
#     New_Data = Num22[-out_ind,] # detect outliers
#   }
#
#   Bx_1 <- New_Data[,traits]
#   Bx_2 <- cbind(farmers_number=as.character(New_Data$farmers_number), genotype.name=as.character(New_Data$genotype.name), location= as.character(New_Data$location), trait=paste0(traits), value=Bx_1)
#   filtered_value= rbind(filtered_value, Bx_2)
# }
#
# # long format
#
#
# filtered_value$value=as.numeric(filtered_value$value)
# global_size=8
# ggplot(filtered_value, aes(x = Location, y=value, fill=Location))+
#   geom_boxplot(outlier.shape = NA) +
#   facet_grid(trait~Year,  scales = "free") +
#   theme_classic(base_size = global_size)
#
# filtered_value$value = as.numeric(filtered_value$value)
# ddd_new = dcast(data = filtered_value, formula = Year + Location + Geno ~ trait, fun.aggregate = mean, value.var = "value", na.rm= T) # mean of the genotypes across location and years
# indv_cor <- round(cor(ddd_new[, -c(1,2,3)],use = "pairwise.complete.obs"),3)
# corrplot(indv_cor, type = "upper",method = "number", number.digits = 3, is.corr = T)
#
# H=(34.68/27)/((230.73 /79) + (161.57/6) + (51.00/(27*6)))
#


install.packages("PerformanceAnalytics")


 # library(PerformanceAnalytics)
#
# chart.Correlation(data, histogram = TRUE, method = "pearson")
#
#
# chart.Correlation(data, histogram = TRUE, method = "pearson")

# import rank data
Rank_22 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/ALL_TRICOT_Anchoring.csv")

#remove colunms with remarks from the Rank data frame
library(stringr)
idc = which(str_detect(colnames(Rank_22), "REMARK")) # identify colnames containing remarks
Rank22 = Rank_22[,colnames(Rank_22)[-idc]] # idc contains the colnames, subtract idc
colnames(Rank22)
Rank22 = as.data.frame(Rank22)
summary(Rank22)
#
colnames(Rank22)[12:14]

# convert the code names to genotype names so i can count the number of times a genotype was best and worst
rowconv = data.frame() # converted row
for(j in 1:nrow(Rank22)){
  rw = Rank22[j,]

  #removing Spaces in the variety code, and changing codes written in lowercase to uppercase
  vrt = rw[,12:14] # has the genotype names as Variety.A, B and C
  idA = which(as.character(rw[,15:length(colnames(rw))]) == "A")
  idA2 = which(as.character(rw[,15:length(colnames(rw))]) == "A ")
  idB = which(as.character(rw[,15:length(colnames(rw))]) == "B")
  idC = which(as.character(rw[,15:length(colnames(rw))]) == "C")
  idC1 = which(as.character(rw[,15:length(colnames(rw))]) == "C ")
  idA1 = which(as.character(rw[,15:length(colnames(rw))]) == "a")
  idB1 = which(as.character(rw[,15:length(colnames(rw))]) == "b")
  idC2 = which(as.character(rw[,15:length(colnames(rw))]) == "c")
  idS = which(as.character(rw[,15:length(colnames(rw))]) == "S")
  #idNR = which(as.character(rw[,15:length(colnames(rw))]) == "NR")
  idx = which(as.character(rw[,15:length(colnames(rw))]) == "")
  idz = which(as.character(rw[,15:length(colnames(rw))]) == "NA")
  rw[,colnames(rw)[15:length(colnames(rw))][idA]] = as.character(vrt[1]) #
  rw[,colnames(rw)[15:length(colnames(rw))][idA2]] = as.character(vrt[1]) #
  rw[,colnames(rw)[15:length(colnames(rw))][idB]] = as.character(vrt[2])
  rw[,colnames(rw)[15:length(colnames(rw))][idC]] = as.character(vrt[3])
  rw[,colnames(rw)[15:length(colnames(rw))][idC1]] = as.character(vrt[3])
  rw[,colnames(rw)[15:length(colnames(rw))][idA1]] = as.character(vrt[1])
  rw[,colnames(rw)[15:length(colnames(rw))][idB1]] = as.character(vrt[2])
  rw[,colnames(rw)[15:length(colnames(rw))][idC2]] = as.character(vrt[3])
  rw[,colnames(rw)[15:length(colnames(rw))][idS]] = as.character(vrt[1])
  #rw[,colnames(rw)[15:length(colnames(rw))][idNR]] = NA
  rw[,colnames(rw)[15:length(colnames(rw))][idx]] = NA
  rw[,colnames(rw)[15:length(colnames(rw))][idz]] = NA


  # as.character(vrt)
  # rw[,16:length(colnames(rw[,16:length(colnames(rw))]))] = gsub(pattern = "A",
  #                                         replacement = vrt[1],
  #                                   x = rw[,16:length(colnames(Rank22))],
  #                                   fixed = T, ignore.case = F, )
  # rw[,16:length(colnames(rw))] = gsub(pattern = "B",
  #                                         replacement = vrt[2],
  #                                         x = rw[,16:length(colnames(Rank22))],ignore.case = F,fixed =  T)
  # rw[,16:length(colnames(rw))] = gsub(pattern = "C",
  #                                         replacement = vrt[3],
  #                                         x = rw[,16:length(colnames(Rank22))], fixed = T,ignore.case = F)
  rowconv = rbind(rowconv, rw) # converted the ABC ranks to genotype names

}
#rowconv has the variety names instead the ranking alphabets

#checking to see If the code names were modified correctly
rowconv$Best_Ovi3
which(as.character(rowconv[30,]) %in%"C ")
which(rowconv == "A ")
which(rowconv == "S")
which(rowconv == "a")
which(rowconv == "b")
which(rowconv == "c")
which(rowconv== "NR")

unique(rowconv$VARIETY.A) # to see all the varieties in colunm VARIETY.A
unique(rowconv$VARIETY.B)
unique(rowconv$VARIETY.C)
colnames(rowconv)

#Want to transform the dataframe to have
rank22_chnged = rowconv[,-c(12:14)] # removed columns containing  variety.ABC because the genotype are already represented in the trait evaluated
head(rank22_chnged)

colnames(rank22_chnged)
# introduce a new colunm called the geno_Trait and convert the rank22_chnged into a long format
rank_22_new = tibble()
for(i in 12:length(colnames(rank22_chnged))){
  bb = rank22_chnged[,c(1:11,i)]
  cl1 = cbind(bb,Trait = colnames(rank22_chnged)[i] )
  colnames(cl1)[12] = "geno_Trait"
  rank_22_new = rbind(rank_22_new,cl1)
}
tail(rank_22_new)

unique(rank_22_new$geno_Trait)
head(rank_22_new)

which(is.na(rank_22_new$geno_Trait))# identify traits with no information
which(rank_22_new$Trait == "<NA>") # identify NAs
head(rank_22_new)
rank_22_new_ord = rank_22_new[order(rank_22_new$geno_Trait),]# order the df base on the geno_Trait
tail(rank_22_new_ord)
head(rank_22_new_ord)

write.csv(x = rank_22_new, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/rank222_forchecking.csv", row.names = F)
new_rank22= read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/rank222_forchecking.csv")
head(new_rank22)

#rew trait caled Trait that has the best and worst as 1 and 0
idW = which(str_detect(new_rank22$Trait, "Worst")) # identify worst
new_rank22$Trait[idW] = 0

idBst = which(str_detect(new_rank22$Trait, "Best")) # identify worst
new_rank22$Trait[idBst] = 1
# which(new_rank22 == "Best_Pgwt3")
summary(new_rank22)
unique(new_rank22$Trait)
new_rank22$Trait= as.numeric(new_rank22$Trait)
count_trait= table(new_rank22$geno_Trait, new_rank22$Trait)
gen1 = unique(new_rank22$geno_Trait)[-1]
gen_count = tibble()
for (i in gen1) {
  gl = new_rank22[new_rank22$geno_Trait %in%i ,]# counting genotypes and the number of times they were ranked best and worst
  unique(gl$PACKAGE)
  lgth = length(gl$Trait)
  lg2 = cbind(geno = i, observation = lgth)
  gen_count <- rbind(gen_count, lg2)# make a dataframe of the count and the genotypes
}

# counting the number of times a genotype was present in the trial
Package_count = tibble()
for(i in gen1) {
id1 = length(which(Rank22$VARIETY.A == i)) # counting the number of times a particular genotype was variety A, B and C
id2 = length(which(Rank22$VARIETY.B == i))
id3 = length(which(Rank22$VARIETY.C == i))
packn = id1 + id2 +id3 # adding the number of times a particular genotype was variety A, B and C
pk = cbind(variety = i, pack_count =  packn)
Package_count = rbind(Package_count,pk ) # make a table of genotypes and their count
}
write.csv(x= Package_count, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/number_times_geno_appeared_package.csv")

# converting the count of best(1) and worst (0) to the long format so it can be plotted
count_long = tibble()
for(i in colnames(count_trait)){
  cnt = count_trait[,i]
  cnt1 = cbind(geno = rownames(count_trait), Trait = i, cnt)
  count_long = rbind(count_long, cnt1)
}

countwide <- dcast(data = count_long, formula = Genotypes ~ Rank, value.var = "count")
countwide <- countwide[-1,]
colnames(countwide)[2:3] <- c("worst", "best")
countwide$DifB_W <- countwide$best-countwide$worst
write.csv(x=countwide, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/Difference_Best_worst.csv")

library(ggplot2)
count_long$cnt = as.numeric(count_long$cnt)
count_long$count = as.numeric(count_long$cnt)
colnames(count_long)[2] = "Rank" # rename the column
colnames(count_long)[1] = "Genotypes" #rename the column
dim(count_long)
# count_long[-1,]
is.data.frame(count_long) # convert to data frame
rownames(count_long) <- NULL
# count_long = count_long[!count_long$Genotypes),] # remove NAs

# make a barplot
ggplot(count_long[-c(1,29),], mapping = aes(x = Genotypes, y = count, fill = Rank)) +
  geom_bar(stat = "identity") + facet_grid(Rank~.) +
  theme(axis.text.x = element_text(angle = 90)) + #change the position of the x-axis label
  geom_text(aes(label= count, vjust = -0.3), size = 2) # edit the text
unique(rank_22_new$Trait)
unique(count_long$Genotypes)

# x11()
# par(mfrow=c(2,1))
# barplot(count_trait[,1], las=2)# worst genotype
# barplot(count_trait[,2], las=2) # best genotype
#############################################################
#import JAR data

Jar_22 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/JAR_TRICOT.csv")

colnames(Jar_22)

##converting the JAR data into the long format
idc = which(!colnames(Jar_22) %in% colnames(Jar_22)[1:11]) # colnames(Jar_22)[1:11] contains the meta data and farmers info, the others contain the trait info
colnew = colnames(Jar_22)[idc] #
variety = c("A", "B", "C","D")
grpA = c("VARIETY.A","color_A", "smoothness_A" ,"strechyness_A" , "stickiness_A" ,
         "odour_A" , "mouldability_A") # grouping all traits with A

grpB = c("VARIETY.B" ,"color_B" ,"smoothness_B"  ,
         "strechiness_B","stickiness_B", "odour_B"  ,
         "mouldability_B") # grouping all traits with B

grpC = c("VARIETY.C" , "color_C"  ,"smoothness_C" ,
         "strechiness_C", "stickiness_C" ,"odour_C" ,
         "mouldability_C")
grpD = c("VARIETY.D"   ,"color_D" ,"smoothness_D"  ,
         "strechiness_D","stickiness_D" , "odour_D",
         "mouldability_D")
# remove(grpA, grpB, grpC, grpD)
groups = list(grpA = grpA, grpB = grpB,
              grpC = grpC, grpD = grpD)
grps_name = c("grpA","grpB", "grpC", "grpD")

dat_arrange = tibble() # will contain the final output
for(i in 1:nrow(Jar_22)){ # look through the whole data frame
  for(gr in grps_name){ # for every grps_name

  clname = unlist(groups[[gr]])
  rdat = Jar_22[i,]
  com_colname = rdat[,1:11]
  rinf = rdat[,clname]
  colnames(rinf) = c("Variety", "color", "Smoothness",
                    "strechiness", "stickiness",
                    "odour", "mouldability")
  dat_A = cbind(com_colname,variety_code = paste(gr), rinf)
  dat_arrange = rbind(dat_arrange,dat_A)

}
}

dat_arrange$variety_code = gsub(pattern = "grp", replacement = "", x = dat_arrange$variety_code) # remove grp from variety code
Jar22 <- dat_arrange

Jar22$PACKAGE <- as.factor(Jar22$PACKAGE)
Jar22$COMMUNITY.NAME  <- as.factor(Jar22$COMMUNITY.NAME)
Jar22$Variety <- as.factor(Jar22$Variety)
Jar22$Smoothness <-as.numeric(Jar22$Smoothness)
Jar22$strechiness <-as.numeric(Jar22$strechiness)
Jar22$stickiness <-as.numeric(Jar22$stickiness)
Jar22$odour <-as.numeric(Jar22$odour)
Jar22$color <-as.numeric(Jar22$color)
Jar22$mouldability <-as.numeric(Jar22$mouldability)

ggplot(data =Jar22,aes(x= Smoothness, y=COMMUNITY.NAME,fill=Variety)) +
  geom_boxplot()


library(dplyr)
## Garri_color numeric

  #  my_sum_md <- Jar22 %>%
  #    group_by(Variety) %>%
  #    summarise(
  #      n=n(),
  #      mean_md=mean(mouldability),
  #      sd_md=sd(mouldability)
  #    ) %>%
  #    mutate( se_md=sd_md/sqrt(n))
  # p6 = ggplot(my_sum_md) +
  #    geom_bar( aes(x=Variety, y=mean_md), stat="identity", fill="blue", alpha=0.5) +
  #    geom_errorbar( aes(x=Variety, ymin=mean_md-se_md, ymax=mean_md+se_md), width=0.1,
  #                   colour="black", alpha=0.9, size=0.2) +
  #    labs(y= "mean mouldability scores", x = "Varieties") +
  #   theme(axis.text.x = element_text(angle = 90, size = 5, face = "bold"), axis.title.y = element_text(face = "bold", size = 5))

install.packages("gridExtra")
library(gridExtra)
library(ggpubr)

# make a matrix of all the plots

ggarrange(p1 + rremove("x.text"), p2 + rremove("x.text"), p3 + rremove("x.text"), p4 , p5, p6 ,
          labels = c("A", "B", "C", "D", "E", "F"), ncol = 3, nrow = 3,heights = c(20,20, 20, 20, 20, 20)
         )

grid.arrange(p1,p2,p3)
geno_count = tibble()
for (i in unique(Jar22$Variety)) {
  ln = length(which(Jar22$Variety %in% i))
  nb = cbind(geno = i, num_time = ln)
  geno_count = rbind(geno_count, nb)
}

Jarav22 = Jar22 %>% group_by(Variety) %>%
  summarise_at(.vars = c("color", "Smoothness", "strechiness", "stickiness", "odour" ,"mouldability"),
               .funs = c(mean, sd))

Jarav22 = as.data.frame(Jarav22)
Jarav22[, 8:13] = Jarav22[,8:13]/sqrt(as.numeric(geno_count$num_time))
Jarav22 = Jarav22[order(Jarav22$Variety), ]
geno_count = geno_count[order(geno_count$geno),]
head(Jarav22)

which(Jar22$Variety %in% "IITA-TMS-IBA000070")
jar22_long = tibble()
for(Trait in c("color_fn1" ,"Smoothness_fn1" ,"strechiness_fn1", "stickiness_fn1" ,"odour_fn1" ,"mouldability_fn1" )){
  dt = droplevels(Jarav22[,c("Variety",Trait)])
  dt1 = cbind(Trait = Trait, dt)
  colnames(dt1)[3] = "Score"
  jar22_long =rbind(jar22_long, dt1)
}
head(Jarav22)
jar22_SE_long = tibble()
for(Trait in c("color_fn2", "Smoothness_fn2" ,"strechiness_fn2",
               "stickiness_fn2", "odour_fn2", "mouldability_fn2" )){
  dt = droplevels(Jarav22[,c("Variety",Trait)])
  dt1 = cbind(Trait = Trait, dt)
  colnames(dt1)[3] = "Score"
  jar22_SE_long =rbind(jar22_SE_long, dt1)
}
str(jar22_long)
jar22_long$Trait = as.factor(jar22_long$Trait)
jar22_long$Trait = gsub(pattern = "_fn1", replacement = "", x = jar22_long$Trait)
ggplot(jar22_long ) +
  geom_bar(mapping =  aes(x=Variety, y=Score),
           stat="identity", fill= "skyblue") + theme_bw() +
  theme(axis.text.x  = element_text(angle = 90, face = "bold", size = 6)) +
  geom_errorbar( aes(x=Variety, ymin=Score-jar22_SE_long$Score, ymax=Score+jar22_SE_long$Score), width=0.1,
                 colour="black", alpha=0.9, size=0.2) + facet_wrap(Trait ~.)
 #facet_null()
 ccx = jar22_long[jar22_long$Variety == "Local",]
sum(ccx$Score)

  which(jar22_long$Score > 10)
summary(jar22_long)

Jar22
ggplot(Jar22, aes(x = Variety, y=stickiness))+
  geom_boxplot(outlier.shape = NA)+
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=rel(3.5)))

###############################

colnames(Jar22)
idL= which(Jar22$Variety=="Local") # identify th local varieties
Jar22 = Jar22[-idL,] #remove the local varieties
dim(Jar22)
colnames(Jar22)
Traits<-c("color","Smoothness","strechiness","stickiness","odour","mouldability")
Blupmean_J = tibble()
for(Trait in Traits){
  eval(parse(text = paste(
    "outmod_J <- lmer(",Trait," ~ (1|Variety) +
(1|PACKAGE:COMMUNITY.NAME) + (1|COMMUNITY.NAME), data = Jar22 )" # better model
  )))
  idout_J <- which(abs(stats::rstudent(outmod_J)) >3) # remove outliers
  if(length(idout_J) == 0){
    Numj <- Jar22}else{
      Numj <- Jar22[-idout_J,]
    }

  eval(parse(text = paste(
    "model_j <- lmer(",Trait," ~ (1|Variety) + (1|PACKAGE:COMMUNITY.NAME) + (1|COMMUNITY.NAME), data =Numj )")))
  summary(model_j)
  prdj = coef(model_j) # get the coefficient of the model
  prd1 = prdj$Variety
  colnames(prd1)[1] = "BLUPsMean"
  prd1$Variety = rownames(prd1)
  prd1 = prd1[,c(2,1)]
  rownames(prd1) = NULL
  prdT <- cbind(Trait = Trait, prd1)
  Blupmean_J = rbind(Blupmean_J, prdT)
}

#which(abs(stats::rstudent(model1)) >3)
library(reshape2)
Blupmean_jwide = dcast(data = Blupmean_J, formula = Variety ~ Trait,
                       fun.aggregate = mean, value.var = "BLUPsMean", na.rm = T)
rownames(Blupmean_jwide) = Blupmean_jwide$Variety
meanheat = Blupmean_jwide[,-1]
str(meanheat)
head(meanheat)
meanheat = as.matrix(meanheat)
meanheatsc = scale(meanheat)
library("pheatmap")
pheatmap(meanheatsc, display_numbers = T, fontsize = 7) # A heatmap that displays values in the cell
#heatmap(x = meanheatsc, cex.axis =10)
str(Blupmean_jwide)

write.csv(x=Blupmean_jwide,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/JAR_BLUP_mean.csv" ) # the BLUP means for onfarm numeric data

########
#PCA Jar22
library(ggplot2)

# Read the sensory analysis data

# Separate features (attributes)
PCA_Jar <- Jar22[, -c(1:13)]  # Assuming the first column contains sample IDs and is not a feature

# Standardize the features
PCA_Jar_scaled <- scale(PCA_Jar)

# Perform PCA
pca_Jar_result <- prcomp(PCA_Jar_scaled, scale = TRUE)

# Get the scores (principal component scores)
Jar_pca_scores <- pca_Jar_result$x
Jar_pca_scores<-as.data.frame(Jar_pca_scores)

 k <- 3  # Number of clusters
 kmeans_result <- kmeans(Jar_pca_scores, centers = k)

# Get cluster assignments
#clusters <- kmeans_result$cluster
traits = c("color", "Smoothness", "strechiness", "stickiness", "odour" , "mouldability")
# Visualize the results with color based on clusters
#scores_df <- data.frame(Jar_pca_scores, traits = as.factor(traits))
ggplot(Jar_pca_scores, aes(x = PC1, y = PC2, color =  kmeans_result$cluster)) +
  geom_point() +
  labs(title = "PCA with trait Coloring", x = "Principal Component 1", y = "Principal Component 2")   # Customize cluster colors as needed


group_by(unique.id) %>%
  summarise_all(.funs = mean)

Jar22_df <-Jar22[, -c(1:12)]
Jar22_df<-Jar22_df %>%
  group_by(Variety) %>%
  summarize_all(.funs = mean )
Jar22_df <- as.data.frame(Jar22_df)
head(Jar22_df)
rownames(Jar22_df) <- Jar22_df$Variety
Mean_Jar22 = Jar22_df[,-1]

# str(Mean_Jar22)
# head(Mean_Jar22)
# pc = PCA(Mean_Jar22)

Mean_Jar22_sc = scale(Mean_Jar22)
pc_jar = PCA(Mean_Jar22_sc)
ds_jar = dist(Mean_Jar22_sc)

dsj = as.matrix(ds_jar)
#ds2 = ds1[rowSums(is.na(ds1)) == 0, colSums(is.na(ds1)) == 0, drop = F]
hj = hclust(as.dist(dsj))

trj = cutree(hj, k =4)
length(tr1)
trj = data.frame(trj)
pcind_jar = as.data.frame(pc_jar$ind$coord)
idj = which(rownames(pc_jar$ind$coord) %in% rownames(trj))

dim(pcind_jar)
pcind_jar = pcind_jar[idj,]
pcind_jar$cl = trj[,1]
pcind_jar$cl = as.factor(pcind_jar$cl)
ggplot(data = pcind_jar, aes(x = Dim.1, Dim.2)) +
  geom_point(colour = pcind_jar$cl)  + theme_bw()
 trj$trj = as.factor(trj$trj)

 library(factoextra)
fviz_pca_biplot(pc_jar, geom.ind = c("text"),labelsize= 2, arrowsize = 0.5, arrowlength=1,
                 col.var = "red", col.ind = trj[,1],)+
  theme(text = element_text(size = 9))
heat_j = pheatmap(dsj, show_rownames = T, show_colnames = F)
heat_j$tree_row$labels

# kmean clustering
#  ?kmeans
# id =  rownames(Mean_GC1) %in% rownames(tr1)
# Mean_GCK = Mean_GC1[id,]
#  kk = kmeans(Mean_GCK, centers = 4)
#  kk$cluster # cluster number
#
############################################
process_22 <-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Procssing_22_Tricot.csv")
str(process_22)
idL= which(process_22$Variety=="local") # identify th local varieties
process_22 = process_22[-idL,] #remove the local varieties
#idL2= which(process_22$variety_code=="")
#process_22 = process_22[-idL2,]
process_22$Variety
colnames(process_22)
process_22a <- process_22
library(stringr)
process_22a$Variety <- str_replace_all(process_22a$Variety, "TMSIBA", "IITA-TMS-IBA")
process_22a$Variety <- str_replace_all(process_22a$Variety, "TMEB2 ", "TMEB2")

?str_replace_all

process_22a=process_22a%>%
  dplyr::rename(peeling_time_Min = peeling.time.Min., wt_peeled_root_Kg = weight.of.peeled.root.Kg.,
                washing_time_Min = washing.time.Min., grating_time_min = grating.time.min.,
                dewatered_mash_kg = wiegth.of.dewatered.mash.Kg., sieve_time_min = sieving.time.min.,
                chaff_wt_gm = weight.of.chaff..grams., initial_temp_F = initial.temp.pan.F.,
                mid_temp_F = mid.temp.pan.F., final_temp_F = final.temp.pan.F.,
                toast_time_min = toasting.time..min., wt_garri_kg = weight.of.garri..KG.)
Traits<-c("peeling_time_Min" , "wt_peeled_root_Kg" , "washing_time_Min" , "grating_time_min",
          "dewatered_mash_kg" , "sieve_time_min" , "chaff_wt_gm" , "initial_temp_F",
          "mid_temp_F" , "final_temp_F" , "toast_time_min" , "wt_garri_kg" )

# process_22 does not have genotype name so we want to collect genotype name from Num22
#ID_present <-Num22b[Num22b$Package_Number %in% process_22$Package_Number,]
process_22a$Variety= as.factor(process_22a$Variety)
process_22a$Package_Number= as.factor(process_22a$Package_Number)

#Num22b$Package_Number %in% process_22$Package_Number
# id <- which(unique(Num22b$farmers.name) %in% unique(process_22$Farmers.Name))
# var1 = unique(Num22b$farmers.name)[id]
# unique(Num22b$farmers.name)[!unique(Num22b$farmers.name) %in% var1]

#data.frame(unique(Num22b$farmers.name))
#data.frame(unique(process_22$Farmers.Name))
# ID_present2 <- ID_present[,1:7] # contains genoname from Num22  present in process_22
# ID_present2 = ID_present2[order(ID_present2$farmers.name),]
# process_22 = process_22[order(process_22$Farmers.Name), ]
# data.frame(ID_present2$farmers.name,  process_22$Farmers.Name )
# Num22b$farmers.name
# intfarmNum22 <- droplevels(unique(Num22b$farmers.name)[ which(unique(Num22b$farmers.name) %in%
#                     unique(process_22$Farmers.Name)) ])
# intfarmprocess <- unique(process_22$Farmers.Name)[ which(unique(process_22$Farmers.Name) %in%
#                                                      intfarmNum22) ]
#
# unique(Num22b$farmers.name)[!unique(Num22b$farmers.name) %in% intfarmNum22]
# unique(process_22$Package_Number)[!unique(process_22$Package_Number) %in% intfarmprocess]

# ID_present[ID_present$Farmers.Name == "",]
# process_22$Farmers.Name[282]
# process_22[282,"Farmers.Name"] = "Obasi Ethelbert"
# process_22[283,"Farmers.Name"] = "Obasi Ethelbert"
# unique(process_22$Farmers.Name)
# str(process_22)
# process_22[process_22$Farmers.Name %in% "",]
# is.data.frame(process_22)
# summary(process_22)
# unique(process_22$Farmers.Name)

Blupmean_P = tibble()
for(Trait in Traits){
  eval(parse(text = paste(
    "outmod_P <- lmer(",Trait," ~ (1|Variety) +
(1|Package_Number:Community_name) + (1|Community_name), data = process_22a )" # better model
  )))
  idout_P <- which(abs(stats::rstudent(outmod_P)) >3) # remove outliers
  if(length(idout_P) == 0){
    NumP <- process_22a}else{
      NumP <- process_22a[-idout_P,]
    }

  eval(parse(text = paste(
    "model_P <- lmer(",Trait," ~ (1|Variety) + (1|Package_Number:Community_name) + (1|Community_name), data = NumP)")))
  summary(model_P)
  prdP = coef(model_P) # get the coefficient of the model
  prdP = prdP$Variety
  colnames(prdP)[1] = "BLUPsMean"
  prdP$Variety = rownames(prdP)
  prdP = prdP[,c(2,1)]
  rownames(prdP) = NULL
  prdT <- cbind(Trait = Trait, prdP)
  Blupmean_P = rbind(Blupmean_P, prdT)
}

#which(abs(stats::rstudent(model1)) >3)
library(reshape2)
Blupmean_Pwide = dcast(data = Blupmean_P, formula = Variety ~ Trait,
                       fun.aggregate = mean, value.var = "BLUPsMean", na.rm = T)
rownames(Blupmean_Pwide) = Blupmean_Pwide$Variety
meanheat = Blupmean_Pwide[,-1]
str(meanheat)
head(meanheat)
meanheat = as.matrix(meanheat)
meanheatsc = scale(meanheat)
library("pheatmap")
pheatmap(meanheatsc, display_numbers = T, fontsize = 7) # A heatmap that displays values in the cell
#heatmap(x = meanheatsc, cex.axis =10)
str(Blupmean_Pwide)

write.csv(x=Blupmean_Pwide,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/processing_BLUP_mean" ) # the BLUP means for onfarm numeric data



##inport texture data (TPA)
texture_fresh <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/All_fresh_texture.csv")
head(texture_fresh)
texture_fresh1 <- as.data.frame(texture_fresh)
texture_fresh2 <-texture_fresh[-c(1,2)]

#texture_fresh_A <- texture_fresh[order(texture_fresh$package.name, decreasing = FALSE), ] # arrange data frame in alphabetic order

library(dplyr)
# sum up all the tech reps
texture_fresh_aggreg <-texture_fresh2 %>%
  group_by(package.name,variety_code,genotype.name, Community) %>%
  summarise_all (.funs = mean)

texture_fresh_aggreg = as.data.frame(texture_fresh_aggreg)
head(texture_fresh_aggreg)

texture_fresh1$package.name = as.factor(texture_fresh1$package.name)
texture_fresh1$variety_code = as.factor(texture_fresh1$variety_code)
texture_fresh1$genotype.name = as.factor(texture_fresh1$genotype.name)
texture_fresh1$Community = as.factor(texture_fresh1$Community)
unique(texture_fresh1$genotype.name)



idL_T= which(texture_fresh1$variety_code=="D") # identify and remove local varieties
texture_fresh1 = texture_fresh1[-idL_T,]

colnames(texture_fresh1)
Traits_T<-c("Adhesiveness","Springiness","Cohesiveness", "Gumminess", "Chewiness","Resilience")
Blupmean_T = tibble()
for(Trait in Traits_T){
  eval(parse(text = paste(
    "outmod_T <- lmer(",Trait," ~ (1|genotype.name) +
(1|package.name:Community) + (1|Community), data = texture_fresh1 )" # better model
  )))
  idout_T <- which(abs(stats::rstudent(outmod_T)) >3) # remove outliers
  if(length(idout_T) == 0){
    NumT <- texture_fresh2}else{
      NumT <- texture_fresh2[-idout_T,]
    }

  eval(parse(text = paste(
    "model_T <- lmer(",Trait," ~ (1|genotype.name) +
(1|package.name:Community) + (1|Community), data =texture_fresh1 )")))
  summary(model_T)
  prdT = coef(model_T) # get the coefficient of the model
  prdT = prdT$genotype.name
  colnames(prdT)[1] = "BLUPsMean_T"
  prdT$genotype.name = rownames(prdT)
  prdT = prdT[,c(2,1)]
  rownames(prdT) = NULL
  prdTrait <- cbind(Trait = Trait, prdT)
  Blupmean_T = rbind(Blupmean_T, prdTrait)
}

#which(abs(stats::rstudent(model1)) >3)
library(reshape2)
Blupmean_Twide = dcast(data = Blupmean_T, formula = genotype.name ~ Trait,
                       fun.aggregate = mean, value.var = "BLUPsMean_T", na.rm = T)
rownames(Blupmean_Twide) = Blupmean_Twide$genotype.name
meanheat_T = Blupmean_Twide[,-1]
str(meanheat)
head(meanheat)
meanheat_T = as.matrix(meanheat_T)
meanheatsc_T = scale(meanheat_T)
library("pheatmap")
pheatmap(meanheatsc_T, display_numbers = T, fontsize = 7) # A heatmap that displays values in the cell
#heatmap(x = meanheatsc, cex.axis =10)
str(Blupmean_Twide)

write.csv(x=Blupmean_Twide,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/TPA_mean.csv" ) # the BLUP means for onfarm numeric data







#collect package name from the process22a
# process_22b <- process_22a
# process_22a_ordered =process_22b[order(process_22b$Farmers.Name),]
# dd <-data.frame(process_22a_ordered$Farmers.Name,texture_fresh_aggreg$farmers_name)
#
# prs_order = tibble()
# for(i in unique(process_22a_ordered$Farmers.Name)){
#   for(j in unique(texture_fresh_aggreg$farmers_name)){
#     if( i == j){
#       prs <- cbind(process_22a_ordered[process_22a_ordered$Farmers.Name == i, c(i,"Package_Number")])
#       prs_order = rbind(prs_order, prs)
#     }
#   }
# }
# #unlist(process_22a_ordered$Farmers.Name[1],f = " ")
# sp_proccFA <- c()
# for(i in 1:length(process_22a_ordered$Farmers.Name)){
# sp <- strsplit(x = process_22a_ordered$Farmers.Name[i], split = " ")[[1]][1]
# sp_proccFA = c(sp_proccFA,sp)
# }
#
#
# #split the farmers name of the texture data
#
# #unlist(process_22a_ordered$Farmers.Name[1],f = " ")
# sp_textFA <- c()
# for(i in 1:length(texture_fresh_aggreg$farmers_name)){
#   sp_T <- strsplit(x = texture_fresh_aggreg$farmers_name[i], split = " ")[[1]][1]
#   sp_textFA = c(sp_textFA,sp_T )
# }
#
# Pro_txt_FN <- as.data.frame(unique(sp_textFA),unique(sp_proccFA))
# unique(texture_fresh_aggreg$farmers_name)
#
#
#  xx <- gsub(pattern = "UGBOAKU", replacement = "Ugboaku Okorocha" ,x = texture_fresh_aggreg$farmers_name )
#
write.csv(x= texture_fresh_aggreg, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/texture_fresh_aggreg.csv")
####
#Linear model of the TPA
library(ggplot2)

ggplot(texture_fresh_aggreg, aes(x = genotype.name, y=Hardness))+
  geom_boxplot(outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=rel(3.5)))


ggplot(texture_fresh_aggreg, aes(x = genotype.name, y=Adhesiveness))+
  geom_boxplot(outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=rel(3.5)))

ggplot(texture_fresh_aggreg, aes(x = genotype.name, y=Springiness))+
  geom_boxplot(outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=rel(3.5)))

ggplot(texture_fresh_aggreg, aes(x = genotype.name, y=Gumminess))+
  geom_boxplot(outlier.shape = NA)+
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=rel(3.5)))

ggplot(texture_fresh_aggreg, aes(x = genotype.name, y=Chewiness))+
  geom_boxplot(outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=rel(3.5)))

summary(texture_fresh_aggreg)


JAR_BLUP<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/JAR_BLUP_mean.csv" ) # the BLUP means for onfarm numeric data
ggplot(JAR_BLUP, aes(x = Variety, y=stickiness ))+
  geom_boxplot(outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=rel(3.5)))

####################################################################
# overnight texture Analysis






# str_detect(string = as.character(Num22$farmers.name),
#            pattern = "Azuama")
# as.character(Num22$farmers.name) %in% texture_fresh_aggreg$farmers_name
#
# for(i in unique(as.character(Num22$farmers.name)))
# idF = which(str_detect(string = texture_fresh_aggreg$farmers_name,
#            pattern = "Anyanwu Rose"))
# ccc = texture_fresh_aggreg$farmers_name
# ccc[idF] = "Azuama Joy"
# }
#data.frame(unique(Num22$farmers.name),unique(texture_fresh_aggreg$farmers_name))
#plot(texture_fresh$Adhesiveness)
#plot(texture_overnight$Adhesiveness)


#######################################
chips22= read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Tricot_Chem_Func/Tricot_22_chem_amy_starch_cf.csv")
garri_func22 = read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Tricot_Chem_Func/Tricot_22_functional.csv")
garri_chem22 = read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Tricot_Chem_Func/Tricor_22_garri_chem.csv")
head(chips22)
head(garri_func22)
head(garri_chem22)




library(dplyr)
Data_joint1 <- tibble()
for(i in colnames(chips22)[-c(1:3)]){
  dd <- chips22[,i]
  dd1 <- cbind(chips22[,1:3],Trait = i, value = dd)
  Data_joint1 <- rbind(Data_joint1, dd1)
}

Data_joint2 <- tibble()
for(i in colnames(garri_func22)[-c(1:3)]){
  dd <- garri_func22[,i]
  dd1 <- cbind(garri_func22[,1:3],Trait = i, value = dd)
  Data_joint2 <- rbind(Data_joint2, dd1)
}

Data_joint3 <- tibble()
for(i in colnames(garri_chem22)[-c(1:3)]){
  dd <- garri_chem22[,i]
  dd1 <- cbind(garri_chem22[,1:3],Trait = i, value = dd)
  Data_joint3 <- rbind(Data_joint3, dd1)
}


DataJoincomb <- rbind(Data_joint1, Data_joint2, Data_joint3)

library(reshape2)

DataJoincomb$value <- as.numeric(DataJoincomb$value)

Data_JoinCombwide <- dcast(data = DataJoincomb, formula = package_number + location+ varieties ~ Trait, fun.aggregate = mean,
      value.var = "value", na.rm = T)
summary(Data_JoinCombwide)
head(Data_JoinCombwide)
write.csv(x = Data_JoinCombwide, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Tricot_Chem_Func/All_ticot_chem22.csv")
All_chem22=Data_JoinCombwide
dim(All_chem22)

All_chem22$package_number= as.factor(All_chem22$package_number)
All_chem22$location = as.factor(All_chem22$location)
All_chem22$varieties = as.factor(All_chem22$varieties)
All_chem22$DMC_1= as.numeric(All_chem22$DMC_1)
All_chem22$DMC_2 = as.numeric(All_chem22$DMC_2)


idL_All_chem22= which(All_chem22$varieties=="local") # identify and remove local varieties
All_chem22_new = All_chem22[-idL_All_chem22,]

All_chem22_new$DMC= rowMeans(All_chem22_new[,c("DMC_1", "DMC_2")])

colnames(All_chem22_new)
head(All_chem22_new)

Tricot_chem22 <-All_chem22_new  %>%
  dplyr::select( "package_number","location", "varieties", "Amy_chips", "amy_garri", "Bulk_Density", "CF_chips.", "crude_fiber_garri", "SI", "SP", "Starch_chips", "starch_garri","sugar_garri", "WAC","DMC" )

# convert each of the samples to dry weight basis

Tricot_chem22$Starch_ch = (Tricot_chem22$Starch_chips*(Tricot_chem22$DMC/100))
Tricot_chem22$AMY_ch = (Tricot_chem22$Amy_chips*(Tricot_chem22$DMC/100))
Tricot_chem22$CF_ch = (Tricot_chem22$CF_chips.*(Tricot_chem22$DMC/100))

Tricot_chem22<- Tricot_chem22 %>%
 dplyr::select("package_number", "location","varieties","SI","SP","WAC", "amy_garri",
"Bulk_Density" , "crude_fiber_garri", "starch_garri", "sugar_garri" ,
 "DMC", "Starch_ch" ,"AMY_ch" ,"CF_ch" )

#chips_all2<- chips_all %>%
 # dplyr::rename("STC_ch" = "STC_Dm","AMY_ch" ="AMY_Dm", "CF_ch" = "CF_Dm", "SC_ch"= "SC_Dm")


Traits_chem<-c("SI","SP","WAC", "amy_garri",
               "Bulk_Density" , "crude_fiber_garri", "starch_garri", "sugar_garri" ,
               "DMC", "Starch_ch" ,"AMY_ch" ,"CF_ch")
Blupmean_chem = tibble()
for(Trait in Traits_chem){
  eval(parse(text = paste(
    "outmod_chem <- lmer(",Trait," ~ (1|varieties) +
(1|package_number:location) + (1|location), data = Tricot_chem22)" # better model
  )))
  idout_chem <- which(abs(stats::rstudent(outmod_chem)) >3) # remove outliers
  if(length(idout_chem) == 0){
    Num_chem <- Tricot_chem22}else{
      Num_chem <- Tricot_chem22[-idout_chem,]
    }

  eval(parse(text = paste(
    "model_chem <- lmer(",Trait," ~ (1|varieties) +
(1|package_number:location) + (1|location), data = Num_chem )")))
  summary(model_chem)
  prd_chem = coef(model_chem) # get the coefficient of the model
  prd_chem = prd_chem$varieties
  colnames(prd_chem)[1] = "BLUPsMean_chem"
  prd_chem$varieties = rownames(prd_chem)
  prd_chem = prd_chem[,c(2,1)]
  rownames(prd_chem) = NULL
  prdTrait_chem <- cbind(Trait = Trait, prd_chem)
  Blupmean_chem= rbind(Blupmean_chem, prdTrait_chem)
}

#which(abs(stats::rstudent(model1)) >3)
library(reshape2)
Blupmean_chem_wide = dcast(data = Blupmean_chem, formula = varieties ~ Trait,
                       fun.aggregate = mean, value.var = "BLUPsMean_chem", na.rm = T)
rownames(Blupmean_chem_wide) = Blupmean_chem_wide$varieties
meanheat_chem = Blupmean_chem_wide[,-1]
str(meanheat_chem)
head(meanheat_chem)
meanheat_chem = as.matrix(meanheat_chem)
meanheatsc_chem = scale(meanheat_chem)
library("pheatmap")
pheatmap(meanheatsc_chem, display_numbers = T, fontsize = 7) # A heatmap that displays values in the cell
#heatmap(x = meanheatsc, cex.axis =10)
str(Blupmean_chem_wide)

write.csv(x=Blupmean_chem_wide,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_chem_BLUPs.csv" ) # the BLUP means for onfarm numeric data







###############making it BLUE
# for chem

Bluemean_chem = tibble()
for(Trait in Traits_chem){
  eval(parse(text = paste(
    "outmod_chem <- lmer(",Trait," ~ (1|varieties) +
(1|package_number:location) + (1|location), data = Tricot_chem22)" # better model
  )))
  idout_chem <- which(abs(stats::rstudent(outmod_chem)) >3) # remove outliers
  if(length(idout_chem) == 0){
    Num_chem <- Tricot_chem22}else{
      Num_chem <- Tricot_chem22[-idout_chem,]
    }

  eval(parse(text = paste(
    "model_blue <- lmer(",Trait," ~ (varieties) +
(1|package_number:location) + (1|location), data = Num_chem )")))
  summary(model_blue)
  em <- as.data.frame(emmeans::emmeans(object = model_blue, specs = "varieties"))
  prd_chem = em[,1:2] # get the coefficient of the model
  # prd_chem = prd_chem$varieties
  # colnames(prd_chem)[1] = "BLUPsMean_chem"
  # prd_chem$varieties = rownames(prd_chem)
  # prd_chem = prd_chem[,c(2,1)]
  # rownames(prd_chem) = NULL
  prdTrait_chem <- cbind(Trait = Trait, prd_chem)
  Bluemean_chem= rbind(Bluemean_chem, prdTrait_chem)
}

Bluemean_chem_wide <- dcast(data = Bluemean_chem,
                            formula = varieties ~ Trait,
                            fun.aggregate = mean, value.var = "emmean")


write.csv(x=Bluemean_chem_wide,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_chem_Blue.csv" ) # the BLUE means for onfarm numeric data

############# for TPA fresh
colnames(texture_fresh1)
texture_fresh1<- texture_fresh1[,-1]
Traits_T<-c("Adhesiveness","Springiness","Cohesiveness", "Gumminess", "Chewiness","Resilience")

Bluemean_TPA = tibble()
for(Trait in Traits_T){
  eval(parse(text = paste(
    "outmod_TPA <- lmer(",Trait," ~ (1|genotype.name) +
(1|package.name:Community) + (1|Community), data = texture_fresh1)" # better model
  )))
  idout_TPA <- which(abs(stats::rstudent(outmod_TPA)) >3) # remove outliers
  if(length(idout_TPA) == 0){
    TPA_DATA <- texture_fresh1}else{
      TPA_DATA<- texture_fresh1[-idout_TPA,]
    }

  eval(parse(text = paste(
    "model_blue_TPA <- lmer(",Trait," ~ (genotype.name) +
(1|package.name:Community) + (1|Community), data = TPA_DATA)")))
  summary(model_blue_TPA)
  em <- as.data.frame(emmeans::emmeans(object = model_blue_TPA, specs = "genotype.name"))
  prd_TPA = em[,1:2] # get the coefficient of the model
  # prd_chem = prd_chem$varieties
  # colnames(prd_chem)[1] = "BLUPsMean_chem"
  # prd_chem$varieties = rownames(prd_chem)
  # prd_chem = prd_chem[,c(2,1)]
  # rownames(prd_chem) = NULL
  prdTrait_TPA <- cbind(Trait = Trait, prd_TPA)
  Bluemean_TPA= rbind(Bluemean_TPA, prdTrait_TPA)
}
library(reshape2)
Bluemean_TPA_wide <- dcast(data = Bluemean_TPA,
                            formula = genotype.name ~ Trait,
                            fun.aggregate = mean, value.var = "emmean")


write.csv(x=Bluemean_TPA_wide,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_Blue.csv" ) # the BLUE means for onfarm numeric data

############################TPA overnight
texture_overnight <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/All_over_night_texture.csv")
texture_overnight <- texture_overnight[(-1)]
texture_overnight_A <- texture_overnight[order(texture_overnight$package.name, decreasing = FALSE), ]

texture_overnight_aggreg <-texture_overnight %>% group_by(package.name  ,genotype.name,location ) %>%
  summarise_at(.vars = c("Force.1", "Area.FT.1.2", "Time.diff..1.2", "Area.FT.1.3",
                         "Area.FT.2.3", "Area.FT.4.6", "Time.diff..4.5", "Hardness",
                         "Adhesiveness","Springiness",  "Cohesiveness", "Gumminess",
                         "Chewiness", "Resilience"), .funs = mean, na.rm = T
  )
texture_overnight_aggreg = as.data.frame(texture_overnight_aggreg)
head(texture_overnight_aggreg)

No_local= which(texture_overnight_aggreg$genotype.name=="local") # identify and remove local varieties
TPA_overnight_aggreg = texture_overnight_aggreg[-No_local,]


write.csv(x= TPA_overnight_aggreg, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/TPA_aggreg_overnight.csv")

#Re-import the data

New_text_ovnght = read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/TPA_aggreg_overnight.csv")

colnames(New_text_ovnght)
New_text_ovnght$genotype.name = as.factor(New_text_ovnght$genotype.name)
New_text_ovnght$package.name = as.factor(New_text_ovnght$package.name)
New_text_ovnght$location = as.factor(New_text_ovnght$location)

# remove local





str(New_text_ovnght)

colnames(New_text_ovnght)
texture_OV<- New_text_ovnght
Traits_TF<-c("Adhesiveness","Springiness","Cohesiveness", "Gumminess", "Chewiness","Resilience")

Bluemean_TPA_ov = tibble()
for(Trait in Traits_TF){
  eval(parse(text = paste(
    "outmod_TPA_ov <- lmer(",Trait," ~ (1|genotype.name) +
(1|package.name:location) + (1|location), data = texture_OV)" # better model
  )))
  idout_TPA_ov <- which(abs(stats::rstudent(outmod_TPA_ov)) >3) # remove outliers
  if(length(idout_TPA_ov) == 0){
    TPA_ov_DATA <- texture_OV}else{
      TPA_ov_DATA <- texture_OV[-idout_TPA_ov,]
    }

  eval(parse(text = paste(
    "model_blue_TPA_ov <- lmer(",Trait," ~ (genotype.name) +
(1|package.name:location) + (1|location), data = TPA_ov_DATA)")))
  summary(model_blue_TPA_ov)
  em_ov <- as.data.frame(emmeans::emmeans(object = model_blue_TPA_ov, specs = "genotype.name"))
  prd_TPA_ov = em_ov[,1:2] # get the coefficient of the model
  # prd_chem = prd_chem$varieties
  # colnames(prd_chem)[1] = "BLUPsMean_chem"
  # prd_chem$varieties = rownames(prd_chem)
  # prd_chem = prd_chem[,c(2,1)]
  # rownames(prd_chem) = NULL
  prdTrait_TPA_ov <- cbind(Trait = Trait, prd_TPA_ov)
  Bluemean_TPA_ov= rbind(Bluemean_TPA_ov, prdTrait_TPA_ov)
}
library(reshape2)
Bluemean_TPA_ov_wide <- dcast(data = Bluemean_TPA_ov,
                           formula = genotype.name ~ Trait,
                           fun.aggregate = mean, value.var = "emmean")


write.csv(x=Bluemean_TPA_ov_wide,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_ov_Blue.csv" ) # the BLUE means for onfarm numeric data
###




#################### Blue on_farm Numeric
colnames(Num22b)
Num22b_sel<- Num22b %>%
  dplyr::select("package_number","location" , "genotype.name", "rt.shape", "mean_ht", "mean_brnch_ht","yield_per_plot", "mkt_yield_plot", "root_num_plot", "mkt_root_number_plot","shwt_plot" )
Traits_onfarm<- c("rt.shape", "mean_ht", "mean_brnch_ht","yield_per_plot", "mkt_yield_plot", "root_num_plot", "mkt_root_number_plot","shwt_plot")
Bluemean_onfarm = tibble()
for(Trait in Traits_onfarm){
  eval(parse(text = paste(
    "outmod_onfarm <- lmer(",Trait," ~ (1|genotype.name) +
(1|package_number:location) + (1|location), data = Num22b_sel)" # better model
  )))
  idout_onfarm <- which(abs(stats::rstudent(outmod_onfarm)) >3) # remove outliers
  if(length(idout_onfarm) == 0){
    onfarm_DATA <- Num22b_sel}else{
      onfarm_DATA<- Num22b_sel[-idout_onfarm,]
    }

  eval(parse(text = paste(
    "model_blue_onfarm <- lmer(",Trait," ~ (genotype.name) +
(1|package_number:location) + (1|location), data = onfarm_DATA)")))
  summary(model_blue_onfarm)
  em <- as.data.frame(emmeans::emmeans(object = model_blue_onfarm, specs = "genotype.name"))
  prd_onfarm = em[,1:2] # get the coefficient of the model
  # prd_chem = prd_chem$varieties
  # colnames(prd_chem)[1] = "BLUPsMean_chem"
  # prd_chem$varieties = rownames(prd_chem)
  # prd_chem = prd_chem[,c(2,1)]
  # rownames(prd_chem) = NULL
  prdTrait_onfarm <- cbind(Trait = Trait, prd_onfarm)
  Bluemean_onfarm= rbind(Bluemean_onfarm, prdTrait_onfarm)
}
library(reshape2)
Bluemean_onfarm_wide <- dcast(data = Bluemean_onfarm,
                           formula = genotype.name ~ Trait,
                           fun.aggregate = mean, value.var = "emmean")


write.csv(x=Bluemean_onfarm_wide,"/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_onfarm_Blue.csv" ) # the BLUE means for onfarm numeric data



# plot(model4)
# off4 <- c(631,214,482)
# Jar22[off4,c("Smoothness","Variety","COMMUNITY.NAME", "PACKAGE")] # select the. outliers from the data frame
# Jar22[off4,"Smoothness"]=NA
# outlier_Smoothness <-which(Jar22$Smoothness %in% boxplot.stats(Jar22$Smoothness)$out) #Removing outliers using boxplot stat
# Jar22$Smoothness[outlier_Smoothness] = NA

# ggplot(data =Jar22,aes(x= Smoothness, y=COMMUNITY.NAME,fill=Variety)) +
#   geom_boxplot()
# ggplot(data = Jar22,aes(x= Smoothness, y=Variety,fill=Variety)) +
#   geom_boxplot()

# insert processing data



# linear model and repeatability
# model1 <- lmer(Num22$total_yield_plot~Farmer, data= Num22)
# Num22[Num22$farmers.name=="Chinazom Oguguo",c("total_yield_plot","genotype.name","location", "farmers.name")]

