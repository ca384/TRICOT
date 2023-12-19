---
  title: "Tricot for 2022 data"
output: html_document
date: "2023-02-23"
---


# packages
packages_used <- c("workflowr","tidyverse", "ggplot2", "here","climatrends" ,"tidyverse", "PlackettLuce","gosset", "patchwork","qvcalc","ggparty","igraph", "ClimMobTools", "multcompView","ggplot2", "gtools","remotes","here", "ggpubr")

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


# inport 2022 farmers ranking and removing white space on the genotype names}
#Rank_22 <- read.csv(here::here("data","All_2021_2022Tricot _ranking_data.csv"), head=T)
Rank_22 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/ALL_TRICOT_Anchoring.csv")

#remove colunms with remarks from the Rank data frame
library(stringr)
idc = which(str_detect(colnames(Rank_22), "REMARK")) # identify colnames containing remarks
Rank22 = Rank_22[,colnames(Rank_22)[-idc]] # idc contains the colnames, subtract idc
colnames(Rank22)
Rank22 = as.data.frame(Rank22)

library(gosset)
library(PlackettLuce)


Rank22$VARIETY.A = gsub(pattern = " ", replacement = "", x = Rank_22$VARIETY.A) # remove spaces from genotype names
Rank22$VARIETY.B = gsub(pattern = " ", replacement = "", x = Rank_22$VARIETY.B) #
Rank22$VARIETY.C = gsub(pattern = " ", replacement = "", x = Rank_22$VARIETY.C) #

unique(Rank22$VARIETY.A)

# for overall for 1MAP}
Trait_1MAP= c("Best_Ovi1","Worst_Ovi1","Best_Germ1","Worst_Germ1","Best_Pgwt1","Worst_Pgwt1","Best_SuitSE1", "Worst_SuitSE1")

for(i in colnames(Rank22)){
  Rank22[,i] = as.factor(Rank22[,i])
}
summary(Rank22)
Rank22 == "C " = "C"
Rank22a = Rank22
Rank22a == "C "
Rank22a[Rank22a == "C "] = "C"
Rank22a[Rank22a == "A "] = "A"
Rank22a[Rank22a == "B "] = "B"
Rank22a[Rank22a == "S"] = "A"
Rank22a[Rank22a == "a"] = "A"
Rank22a[Rank22a == "b"] = "B"
Rank22a[Rank22a == "c"] = "C"
Rank22a[Rank22a == ""] = ""
Rank22a[Rank22a == " C"] = ""
summary(Rank22a)
Rank22a = droplevels(Rank22a)

idc = which(str_detect(colnames(Rank22), "Ovi")) # identify colnames containing remarks
colnames(Rank22)[idc]

trait_list = list(c("Best_Ovi1",  "Worst_Ovi1"),
                  c("Best_Ovi3",  "Worst_Ovi3") ,
                  c("Best_Ovi6", "Worst_Ovi6") , c("Best_overimpr9MAP","Worst_overimpr9MAP"),
                  c("Best_HvOvi12", "Worst_HvOvi12") ,
                  c("Best_GapOvi12", "Worst_GapOvi12") ,
                  c("Best_GaqOvi12" ,   "Worst_GaqOvi12") ,
                  c("Best_EbaOvi12" ,   "Worst_EbaOvi12"),
                  c("Best_EbaAFOvi12",  "Worst_EbaAFOvi12"),
                  c("Best_FEOvi12" ,    "Worst_FEOvi12"))


Rank22a =  Rank22a[-17,] # all colmns of the row are empty
tricot_rank_ouptput = list()
for(trt in 1:length(trait_list)){
  trt1 = trait_list[[trt]]

  R_1map <- rank_tricot(data = Rank22a,
                        items = c("VARIETY.A" ,"VARIETY.B",  "VARIETY.C"),
                        input = trt1)
  tricot_rank_ouptput[[trt]] <- R_1map
}


tricot_rank_model = list()
for(trt in 1:length(tricot_rank_ouptput)){
  trt1 = tricot_rank_ouptput[[trt]]

  mod_R_1p <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank_model[[trt]] <- mod_R_1p
}



# name for each model
labels = c("1MAP", "3MAP", "6MAP", "9MAP", "Harvest", "Processing","Garri_quality","Fresh_eba_quality","Eba_quality", "Replant")

worth_map(tricot_rank_model, labels,size=4) + theme(axis.text.x = element_text(angle = 90))

plots = list()
for(i in length(tricot_rank_model)){

  p_i = worth_bar(tricot_rank_model[[i]])
  plots[[i]] = p
}

p1 = worth_bar(tricot_rank_model[[1]]) +  theme(axis.text.y = element_text(size = 6))
p2 = worth_bar(tricot_rank_model[[2]]) +  theme(axis.text.y = element_text(size = 6))
p3 = worth_bar(tricot_rank_model[[3]]) +  theme(axis.text.y = element_text(size = 6))
p4 = worth_bar(tricot_rank_model[[4]]) +  theme(axis.text.y = element_text(size = 6))
p5 = worth_bar(tricot_rank_model[[5]]) +  theme(axis.text.y = element_text(size = 6))
p6 = worth_bar(tricot_rank_model[[6]]) +  theme(axis.text.y = element_text(size = 6))
p7 = worth_bar(tricot_rank_model[[7]]) +  theme(axis.text.y = element_text(size = 6))
p8 = worth_bar(tricot_rank_model[[8]]) +  theme(axis.text.y = element_text(size = 6))
p9 = worth_bar(tricot_rank_model[[9]]) +  theme(axis.text.y = element_text(size = 6))
p10 = worth_bar(tricot_rank_model[[10]]) +  theme(axis.text.y = element_text(size = 6))


library(ggpubr)
ggarrange(p1 ,p2,p3,p4,p5,p6, font.label = list(size = 10,face = "plain"), labels = c("A","B","C","D","E","F") ,ncol = 3, nrow = 2)

ggarrange(p7,p8,p9,p10, font.label = list(size = 10,face = "plain"), labels = c("A","B","C","D") ,ncol = 2, nrow = 2)

# converting numeric data to rank data
Num22 # the numeric data
colnames(Num22)
idL= which(Num22$genotype=="local") # identify th local varieties
Num22a = Num22[-idL,] #remove the local varieties
dim(Num22a)
colnames(Num22a)

common_cols = c("package.number","farmers_number", "location", "farmers.name", "genotype.name","genotype", "variety_code")
num_traits <- c("plt.arch","noh", "rt.shape", "rt.colour", "rot","sell.rt.no","unsell.rt.no",   "total.rt.no", "sellable.rt.wt", "unsellable.rt.wt","total.rt.wt","sht.wt","wa","ww",  "initial.plant.vigor" , "mean_ht","mean_brnch_ht","total_yield_plot",
                "mkt_yield_plot","efficient_yield","total_root_num_plot", "mkt_root_number_plot", "efficient_rootNum","shwt_plot")
num_trt_diseas = c("cbbi", "cbbs", "cadi", "cads")

Num22_sel <- Num22a
summary(Num22_sel)
summary(Num22a)
farm_num = unique(Num22_sel$farmers_number) # dentify the farmers number
data.frame(Num22_sel$farmers_number,Num22_sel$farmers.name ) #convert to dataframe
Rank_Num = tibble() #
for(i in farm_num){
  for(j in num_traits){
    dt = Num22_sel[Num22_sel$farmers_number == i,]
    dt1 = dt[,j]
    rnk = round(rank(desc(dt1)),0)

    dt3 = cbind(dt[,common_cols], Trait = j, dt1,rnk)
    Rank_Num = rbind(Rank_Num,dt3)
  }
}


# disease
Rank_disea = tibble()
for(i in farm_num){
  for(j in num_trt_diseas){
    dt = Num22_sel[Num22_sel$farmers_number == i,]
    dt1 = dt[,j]
    rnk = round(rank(dt1),0)

    dt3 = cbind(dt[,common_cols], Trait = j, dt1,rnk)
    Rank_disea = rbind(Rank_disea,dt3)
  }
}


All_Num_rank <- rbind(Rank_Num,Rank_disea)
tail(All_Num_rank)
All_Num_rank$dt1 = as.numeric(All_Num_rank$dt1)
All_Num_rank_wo_NA = All_Num_rank[which(!is.na(All_Num_rank$dt1)),]
summary(All_Num_rank_wo_NA$dt1)
view(All_Num_rank_wo_NA)
