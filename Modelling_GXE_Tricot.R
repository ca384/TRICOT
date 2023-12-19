packages_used <- c("workflowr","dplyr", "tidyverse", "ggplot2", "here", "stringr","lme4","metan", "FactoMineR","corrplot", "ASRgenomics", "reshape2", "Hmisc", "agricolae", "sommer", "PlackettLuce")

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
Num22 = Num_22[,!colnames(Num_22) %in% c("comment" , "initial plant vigor")]
colnames(Num22)
head(Num22)

str(Num22)
colnames(Num22)
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
Num22$unsell.rt.no <- as.numeric(Num22$sell.rt.no)
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

### snps data
tricot_snp= read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/29_tricot_snps.tsv", sep = "\t")
dim(tricot_snp)
colnames(tricot_snp)
Num22$genotype

# data transformation
#Num22$log_total_yield <- log(Num22$total_yield_plot)
#Num22$log_mkt_wt <-log(Num22$mkt_yield_plot)
#Num22$log_total_root_num <- log(Num22$total_root_num_plot)
#Num22$log_mkt_num <- log(Num22$mkt_root_number_plot)
#Num22$log_shwt <- log(Num22$shwt_plot)
#Num22$eff_yield <- log(Num22$efficiet_yield)
#Num22$log_eff_rootNum <- log(Num22$efficiet_rootNum)
# for yield per plot
model1 <- lm(formula = total_yield_plot~ genotype + farmers_number, data = Num22 ) # better model
summary.aov(model1)
anova(model1)

model1a <- lm(formula = total_yield_plot~ genotype + farmers_number + farmers_number:location + location +  genotype:location,data = Num22 ) # better model
summary.aov(model1a)

model1b <- lmer(formula = total_yield_plot~ genotype +
                  (1|farmers_number:location) + location +
                  genotype:location,data = Num22 ) # better model
anova(model1b)
summary(model1b)
library(lmerTest)

lmerTest::ranova(model1b)
summary(Num22)
round(abs(stats::rstudent(model = model1)),3)
model1$residuals


summary.aov(model1)
fff = fitted(model1)
fff
model1$coefficients
pp = predict(object = model1)
pp
emmeans::emmeans(object = model1, specs = "genotype")
off<- c(171, 197,261) # subset the outliers

Num22[off,c("total_yield_plot","genotype.name","location", "farmers.name")] # select the. outliers from the data frame
Num22[off,"total_yield_plot"]=NA # make the selected outliers NA

#outlier_total_yield <-which(Num22$total_yield_plot %in% boxplot.stats(Num22$total_yield_plot)$out) #Removing outliers using boxplot stat
#Num22$total_yield_plot[outlier_total_yield] = NA


model1 <- lmer(formula = total_yield_plot~ (1|genotype) + location +   (1|genotype:location), data = Num22 ) # better model
summary(model1)

ff = coef(model1)
predict(model1)
plot(model1)
ff$genotype
data.frame(predict(model1))
view(Num22)
unique(Num22$genotype)
library(sommer)
model2 <- mmer(fixed = total_yield_plot~ location,
               random = ~ genotype + genotype:location,
               data = Num22 ) # better model
model2$U$genotype$total_yield_plot + model2$
summary(model2)

x11
ggplot(data = Num22,aes(x= total_yield_plot, y=location,fill=genotype)) +
  geom_boxplot() # cluster on location
ggplot(data = Num22,aes(x= total_yield_plot, y=genotype,fill=genotype)) +
  geom_boxplot()



# for total root number per plot

#Num22[Not,c("total_yield_plot","genotype.name","location", "farmers.name")]


off2 <- c(228,176,112)
Num22[off2,c("total_root_num_plot","genotype.name","location", "farmers.name")] # select the. outliers from the data frame
Num22[off2,"total_root_num_plot"]=NA
outlier_total_rootNum <-which(Num22$total_root_num_plot %in% boxplot.stats(Num22$total_root_num_plot)$out) #Removing outliers using boxplot stat
Num22$total_root_num_plot[outlier_total_rootNum] = NA

x11
ggplot(data = Num22,aes(x= total_root_num_plot, y=location,fill=genotype)) +
  geom_boxplot()
ggplot(data = Num22,aes(x= total_root_num_plot, y=genotype,fill=genotype)) +
  geom_boxplot()

model2 <- lm(formula = total_root_num_plot~ genotype + location +  genotype:location,data = Num22 ) # better model
plot(model2)
summary.aov(model2)

# effecient yield
model3 <- lm(formula = efficient_yield~ genotype + location +  farmers_number + location:farmers_number+ genotype:location,data = Num22 ) # better model
plot(model3)
summary.aov(model3)
off3 <- c(209,173,190)
Num22[off3,c("efficient_yield","genotype.name","location", "farmers.name")] # select the. outliers from the data frame
Num22[off3,"efficient_yield"]=NA
outlier_eff_yield <-which(Num22$efficient_yield %in% boxplot.stats(Num22$efficient_yield)$out) #Removing outliers using boxplot stat
Num22$efficient_yield[outlier_eff_yield] = NA

x11
ggplot(data = Num22,aes(x= efficient_yield, y=location,fill=genotype)) +
  geom_boxplot()
ggplot(data = Num22,aes(x= efficient_yield, y=genotype,fill=genotype)) +
  geom_boxplot()



summary(Num22)

x11()
par(mfrow=c(2,5))
hist(Num22$mean_brnch_ht)
hist(Num22$mean_ht)
hist(Num22$total_yield_plot)
hist(Num22$mkt_yield_plot)
hist(Num22$total_root_num_plot)
hist(Num22$mkt_root_number_plot)
hist((Num22$efficiet_yield))
hist(Num22$efficiet_rootNum)
hist(log(Num22$shwt_plot))


x11()
par(mfrow=c(2,5))
hist(Num22$mean_brnch_ht)
hist(Num22$mean_ht)
hist(Num22$log_total_yield)
hist(Num22$log_mkt_wt)
hist(Num22$log_total_root_num)
hist(Num22$log_mkt_num)
hist(Num22$log_shwt)
hist(Num22$eff_yield)
hist(Num22$log_eff_rootNum)

library(gosset)
model2 <- lm(formula =log_total_yield~ genotype.name + location + farmers.name:location +
               genotype.name:location,data = Num22 )
plot(model2)
summary.aov(model2)
predict(model2)
ggplot(data = Num22,aes(x= log_total_yield, y=genotype.name,fill=genotype.name)) +
  geom_boxplot()



library(dplyr)
library(daewr)
tab1 <- xtabs(~ genotype + farmers_number+location, data = Num22)
fit.yield <- aov(total_yield_plot ~ genotype + farmers_number+location, data = Num22)
drop1(fit.yield, test = "F")


library(multcomp)

contr_yield <- glht(fit.yield, linfct = mcp(genotype  = "Tukey"))
summary(contr_yield, test = adjusted("none"))
contr_yield$vcov






library(stats)
traitnames<- (c("Num22$mean_brnch_ht","mean_ht", "total_yield_plot", "mkt_yield_plot", "total_root_num_plot", "Cmkt_root_number_plot", "efficiet_yield", "efficiet_rootNum"))
filtered_value <- tibble()
for(traits in traitnames){
  trs = paste0(traits) # the loop will be able to select  and analyse each trait
  out_ind <- which(Num22[,paste(traits)] %in% boxplot.stats(Num22[,paste(traits)])$out) # outliers
  if(length(out_ind) == 0){
    New_Data = Num22
  }else{
    New_Data = Num22[-out_ind,] # detect outliers
  }

  Bx_1 <- New_Data[,traits]
  Bx_2 <- cbind(farmers_number=as.character(New_Data$farmers_number), genotype.name=as.character(New_Data$genotype.name), location= as.character(New_Data$location), trait=paste0(traits), value=Bx_1)
  filtered_value= rbind(filtered_value, Bx_2)
}

# long format


filtered_value$value=as.numeric(filtered_value$value)
global_size=8
ggplot(filtered_value, aes(x = Location, y=value, fill=Location))+
  geom_boxplot(outlier.shape = NA) +
  facet_grid(trait~Year,  scales = "free") +
  theme_classic(base_size = global_size)

filtered_value$value = as.numeric(filtered_value$value)
ddd_new = dcast(data = filtered_value, formula = Year + Location + Geno ~ trait, fun.aggregate = mean, value.var = "value", na.rm= T) # mean of the genotypes across location and years
indv_cor <- round(cor(ddd_new[, -c(1,2,3)],use = "pairwise.complete.obs"),3)
corrplot(indv_cor, type = "upper",method = "number", number.digits = 3, is.corr = T)

H=(34.68/27)/((230.73 /79) + (161.57/6) + (51.00/(27*6)))







install.packages("PerformanceAnalytics")


  library(PerformanceAnalytics)

chart.Correlation(data, histogram = TRUE, method = "pearson")


chart.Correlation(data, histogram = TRUE, method = "pearson")

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
  rowconv = rbind(rowconv, rw)

}
#rowconv has the variety names insted the ranking alphabets

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
rank22_chnged = rowconv[,-c(12:14)] # variety.ABC because the genotype are already represented in the trait evaluated
view(rank22_chnged)

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
view(rank_22_new)
which(is.na(rank_22_new$geno_Trait))
which(rank_22_new$Trait == "<NA>")
view(rank_22_new)
rank_22_new_ord = rank_22_new[order(rank_22_new$geno_Trait),]
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
view(new_rank22)
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
View(dat_arrange)
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
ggpubr::ggarrange()
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
                 colour="black", alpha=0.9, size=0.2) + facet_wrap(Trait ~.)jar22_SE_long
 facet_null()
 ccx = jar22_long[jar22_long$Variety == "Local",]
sum(ccx$Score)


  which(jar22_long$Score > 10)
summary(jar22_long)
view(jar22_long)
summary(Jar22)

##linear model
summary(Jar22)
model_sm <- lm(formula = Smoothness~ Variety + COMMUNITY.NAME + Variety:COMMUNITY.NAME , data = Jar22 ) # better model
summary.aov(model_sm)

model_color <- lm(formula = color ~ Variety + COMMUNITY.NAME +  Variety:COMMUNITY.NAME, data = Jar22 ) # better model
summary.aov(model_color)

model_st <- lm(formula = stickiness ~ Variety + COMMUNITY.NAME +  Variety:COMMUNITY.NAME, data = Jar22 ) # better model
summary.aov(model_st)

model_strech <- lm(formula = strechiness ~ Variety + COMMUNITY.NAME +  Variety:COMMUNITY.NAME, data = Jar22 ) # better model
summary.aov(model_strech)

model_sm <- lm(formula = Smoothness ~ Variety + COMMUNITY.NAME +  Variety:COMMUNITY.NAME, data = Jar22 ) # better model
summary.aov(model_sm)

model_od <- lm(formula = odour ~ Variety + COMMUNITY.NAME +  Variety:COMMUNITY.NAME, data = Jar22 ) # better model
summary.aov(model_od)

model_md <- lm(formula = mouldability ~ Variety + COMMUNITY.NAME +  Variety:COMMUNITY.NAME, data = Jar22 ) # better model
summary.aov(model_md)









##inport texture data (TPA)
texture_fresh <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/All_fresh_texture.csv")
texture_fresh <-texture_fresh[-c(1:2)]
texture_fresh_A <- texture_fresh[order(texture_fresh$farmers_name, decreasing = FALSE), ] # arrange data frame in alphabetic order
view(texture_fresh_A)
# sum up all the tech reps
texture_fresh_aggreg <-texture_fresh %>% group_by(farmers_name,variety_code) %>%
  summarise_at(.vars = c("Force.1", "Area.FT.1.2", "Time.diff..1.2", "Area.FT.1.3",
                         "Area.FT.2.3", "Area.FT.4.6", "Time.diff..4.5", "Hardness",
                         "Adhesiveness","Springiness",  "Cohesiveness", "Gumminess",
                         "Chewiness", "Resilience"), .funs = mean, na.rm = T
  )
texture_fresh_aggreg = as.data.frame(texture_fresh_aggreg)
head(texture_fresh_aggreg)
write.csv(x= texture_fresh_aggreg, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/texture_fresh_aggreg.csv")

texture_overnight <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/All_over_night_texture.csv")
texture_overnight <- texture_overnight[-c(1:2)]
texture_overnight_A <- texture_overnight[order(texture_overnight$farmers_name, decreasing = FALSE), ]

texture_overnight_aggreg <-texture_overnight %>% group_by(farmers_name,variety_code) %>%
  summarise_at(.vars = c("Force.1", "Area.FT.1.2", "Time.diff..1.2", "Area.FT.1.3",
                         "Area.FT.2.3", "Area.FT.4.6", "Time.diff..4.5", "Hardness",
                         "Adhesiveness","Springiness",  "Cohesiveness", "Gumminess",
                         "Chewiness", "Resilience"), .funs = mean, na.rm = T
  )
texture_overnight_aggreg = as.data.frame(texture_overnight_aggreg)
head(texture_overnight_aggreg)

write.csv(x= texture_overnight_aggreg, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/texture_overnight_aggreg.csv")

#Re-import the data
New_text_fresh = read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/texture_fresh_aggreg_Am.csv")

New_text_ovnght = read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/texture_overnight_aggreg_Am.csv")

str(New_text_ovnght)

## Convert some columns in New_text_fresh to factor
names <- c(1:5)
New_text_fresh[,names] <- lapply(New_text_fresh[,names] , factor)
str(New_text_fresh)

## Convert some columns in
names_col <- c(1:5)
New_text_ovnght[,names_col] <- lapply(New_text_ovnght[,names_col] , factor)
str(New_text_ovnght)



library(dplyr)


library()
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
data.frame(unique(Num22$farmers.name),unique(texture_fresh_aggreg$farmers_name))
plot(texture_fresh$Adhesiveness)
plot(texture_overnight$Adhesiveness)




library(PlackettLuce)
## correlation with JAR22 and
Rank22
Rank22$VARIETY.A
R_1map <- rank_tricot(data = Rank22,
                      items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                      input = c("Best_Ovi1", "Worst_Ovi1"))
dim(R_1map)
mod_R_1p <- PlackettLuce(R_1map, npseudo = 0.5)
#summary(Mod_1MAP_qv)
#datplt = as.data.frame(summary(mod_R_1p$rankings))
Mod_1MAP_qv <- qvcalc(mod_R_1p )
Mod_1MAP_qv$qvframe <- Mod_1MAP_qv$qvframe[order(Mod_1MAP_qv$qvframe$estimate),]
#Mod_1MAP_qv$qvframe <- Mod_1MAP_qv$qvframe
#plot(x=Mod_1MAP_qv$qvframe)
plot(Mod_1MAP_qv, las = 2, ylab = "Worth estimates", cex.axis=0.5, main = NULL)
#axis(
Jar22

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
processing_22 <-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Procssing_22_Tricot.csv")
summary(processing_22)




# linear model and repeatability
model1 <- lmer(Num22$total_yield_plot~Farmer, data= Num22)
Num22[Num22$farmers.name=="Chinazom Oguguo",c("total_yield_plot","genotype.name","location", "farmers.name")]
