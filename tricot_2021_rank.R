packages_used <- c("workflowr","dplyr", "tidyverse", "ggplot2", "here",
                   "stringr","lme4", "FactoMineR","corrplot",
                    "reshape2", "Hmisc", "agricolae", "gosset",
                   "PlackettLuce", "readr", "pheatmap", "Matrix")

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

rank21 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/2020_2021_tricot/2020_2021_tricot_rank.csv")

library(stringr)
idc = which(str_detect(colnames(rank21), "Remark")) # identify colnames containing remarks
rank21 = rank21[,colnames(rank21)[-idc]] # idc contains the colnames, subtract idc
colnames(rank21)
rank21 = as.data.frame(rank21)

library(gosset)
library(PlackettLuce)


rank21$Variety.A = gsub(pattern = " ", replacement = "", x = rank21$Variety.A) # remove spaces from genotype names
rank21$Variety.B = gsub(pattern = " ", replacement = "", x = rank21$Variety.B) #
rank21$Variety.C = gsub(pattern = " ", replacement = "", x = rank21$Variety.C) #




### fixing the codes

for(i in colnames(rank21)){
  rank21[,i] = as.factor(rank21[,i])
}
summary(rank21)
#Rank22 == "C "
rank21a = rank21
#Rank22a == "C "
rank21a[rank21a == "C "] = "C"
rank21a[rank21a == "A "] = "A"
rank21a[rank21a == "B "] = "B"
rank21a[rank21a == "S"] = "A"
rank21a[rank21a == "a"] = "A"
rank21a[rank21a == "b"] = "B"
rank21a[rank21a == "c"] = "C"
rank21a[rank21a == ""] = ""
rank21a[rank21a == " C"] = ""
rank21a = droplevels(rank21a)

#idc = which(str_detect(colnames(rank21), "Ovi")) # identify colnames containing remarks

#colnames(rank21)[idc]
# trait combination to give 10 total traits used in. the worth bar


# all overall impressions
#rank21a =  rank21a[-17,] # all colmns of the row are empty

trait_list21_ALL = list(c("X1MAP_ovI_Best" , "X1MAP_ovI_Worst" ),
                      c("X3MAP_ovI_Best",  "X3MAP_ovI_Worst"),
                      c("X6MAP_ovI_Best" , "X6MAP_ovI_Worst"),
                      c("X9MAP_ovI_Best","X9MAP_ovI_Worst"),
                      c("Harvest_Ovi_Best", "Harvest_Ovi_Worst" ),
                      c("ovi_process_Best" ,"ovi_process_Worst"),
                      c("Garri_Ovi_Best" , "Garri_Ovi_Best"),
                      c("Eba_ovi_Best", "Eba_ovi_Worst"),
                      c("OvI_OvNight_Best" , "OvI_OvNight_Worst"))


tricot_rank21_ouptput_ALL = list() #
for(trt in 1:length(trait_list21_ALL)){
  trt1 = trait_list21_ALL[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_ALL <- rank_tricot(data = rank21a,
                        items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                        input = trt1)
  tricot_rank21_ouptput_ALL[[trt]] <- R21_ALL
}


tricot_rank21_model_ALL = list()#
for(trt in 1:length(tricot_rank21_ouptput_ALL)){
  trt1 = tricot_rank21_ouptput_ALL[[trt]]

  mod_R21_ALL <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_ALL[[trt]] <- mod_R21_ALL
}

# name for each model
labels = c("1MAP", "3MAP", "6MAP", "9MAP", "Harvest", "Processing","Garri_quality","Fresh_eba_quality","Ov_Eba_quality")

#worthmap overall impression
worth_map(tricot_rank21_model_ALL, labels,size=4) + theme(axis.text.x = element_text(angle = 90))

#########IMAP worth map

Trait_list21_1map= list(c("X1MAP_ovI_Best" , "X1MAP_ovI_Worst"),
c("X1MAP_Gem_Best","X1MAP_Gem_Worst"),
c("X1MAP_PLT_GRW_Best", "X1MAP_PLT_GRW_Worst"),
c("X1MAP_Canopy_Best","X1MAP_Canopy_Worst"),
c("X1MAP_suit_Best" , "X1MAP_suit_Worst"))

tricot_rank21_ouptput_1map = list() #
for(trt in 1:length(Trait_list21_1map)){
  trt1 = Trait_list21_1map[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_1map <- rank_tricot(data = rank21a,
                          items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                          input = trt1)
  tricot_rank21_ouptput_1map[[trt]] <- R21_1map
}


tricot_rank21_model_1map = list()#
for(trt in 1:length(tricot_rank21_ouptput_1map)){
  trt1 = tricot_rank21_ouptput_1map[[trt]]

  mod_R21_1map <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_1map[[trt]] <- mod_R21_1map
}
length( tricot_rank21_model_1map)

# name for each model
labels = c("Overall", "Germination","Plant growth rate", "Canopy","SuitSE")

worth_map(tricot_rank21_model_1map, labels,size=4) + theme(axis.text.x = element_text(angle = 90))



############ 3MAP
Trait_list21_3map= list(c("X3MAP_ovI_Best" ,  "X3MAP_ovI_Worst"),
                        c( "X3MAP_Gem_Best", "X3MAP_Gem_Worst"),
                        c("X3MAP_PLT_GRW_Best" , "X3MAP_PLT_GRW_Worst"),
                        c("X3MAP_Canopy_Best","X3MAP_Canopy_Worst"),
                        c("X3MAP_arch_Best"  , "X3MAP_arch_Worst"),
                        c("X3MAP_disease_Best","X3MAP_disease_Worst"))

tricot_rank21_ouptput_3map = list() #
for(trt in 1:length(Trait_list21_3map)){
  trt1 = Trait_list21_3map[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_3map <- rank_tricot(data = rank21a,
                          items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                          input = trt1)
  tricot_rank21_ouptput_3map[[trt]] <- R21_3map
}


tricot_rank21_model_3map = list()#
for(trt in 1:length(tricot_rank21_ouptput_3map)){
  trt1 = tricot_rank21_ouptput_3map[[trt]]

  mod_R21_3map <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_3map[[trt]] <- mod_R21_3map
}
length( tricot_rank21_model_3map)

# name for each model
labels = c("Overall", "Germination","Plant growth rate", "Canopy","Architecture", "Disease")

worth_map(tricot_rank21_model_3map, labels,size=4) + theme(axis.text.x = element_text(angle = 90))
########################### 6MAP


Trait_list21_6map= list(c("X6MAP_ovI_Best" ,   "X6MAP_ovI_Worst"),
                        c( "X6MAP_Gem_Best", "X6MAP_Gem_Worst" ),
                        c( "X6MAP_PLT_GRW_Best" , "X6MAP_PLT_GRW_Worst"),
                        c( "X6MAP_Canopy_Best","X6MAP_Canopy_Worst"),
                        c("X6MAP_arch_Best"  , "X6MAP_arch_Worst" ),
                        c("X6MAP_disease_Best" ,"X6MAP_disease_Worst"))

tricot_rank21_ouptput_6map = list() #
for(trt in 1:length(Trait_list21_6map)){
  trt1 = Trait_list21_6map[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_6map <- rank_tricot(data = rank21a,
                          items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                          input = trt1)
  tricot_rank21_ouptput_6map[[trt]] <- R21_6map
}


tricot_rank21_model_6map = list()#
for(trt in 1:length(tricot_rank21_ouptput_6map)){
  trt1 = tricot_rank21_ouptput_6map[[trt]]

  mod_R21_6map <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_6map[[trt]] <- mod_R21_6map
}
length( tricot_rank21_model_6map)

# name for each model
labels = c("Overall", "Germination","Plant growth rate", "Canopy","Architecture", "Disease")

worth_map(tricot_rank21_model_6map, labels,size=4) + theme(axis.text.x = element_text(angle = 90))
#################################9map

Trait_list21_9map= list(c("X9MAP_ovI_Best","X9MAP_ovI_Worst"),
                        c( "X9MAP_Gem_Best","X9MAP_Gem_Worst"),
                        c(  "X9MAP_canopy_Best","X9MAP_canopy_Worst"),
                        c( "X9MAP_arch_Best","X9MAP_arch_Worst"),
                        c("X9MAP_suit_Best","X9MAP_suit_Worst"))

tricot_rank21_ouptput_9map = list() #
for(trt in 1:length(Trait_list21_9map)){
  trt1 = Trait_list21_9map[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_9map <- rank_tricot(data = rank21a,
                          items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                          input = trt1)
  tricot_rank21_ouptput_9map[[trt]] <- R21_9map
}


tricot_rank21_model_9map = list()#
for(trt in 1:length(tricot_rank21_ouptput_9map)){
  trt1 = tricot_rank21_ouptput_9map[[trt]]

  mod_R21_9map <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_9map[[trt]] <- mod_R21_9map
}
length( tricot_rank21_model_9map)

# name for each model
labels = c("Overall", "Germination", "Canopy","Architecture", "Suitability")

worth_map(tricot_rank21_model_9map, labels,size=4) + theme(axis.text.x = element_text(angle = 90))
########################################### Harvest

Trait_list21_HV= list(c("Root_yield_Best","Root_yield_Worst"),
                        c( "density_Best","density_Worst"),
                        c(  "Root_colour_Best","Root_colour_Worst"),
                        c( "Root_size_Best","Root_size_Worst"),
                        c("Woody_filament_Best" , "Woody_filament_Worst"),
                        c("Harvest_Ovi_Best" , "Harvest_Ovi_Worst"))

tricot_rank21_ouptput_HV = list() #
for(trt in 1:length(Trait_list21_HV)){
  trt1 = Trait_list21_HV[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_HV <- rank_tricot(data = rank21a,
                          items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                          input = trt1)
  tricot_rank21_ouptput_HV[[trt]] <- R21_HV
}


tricot_rank21_model_HV = list()#
for(trt in 1:length(tricot_rank21_ouptput_HV)){
  trt1 = tricot_rank21_ouptput_HV[[trt]]

  mod_R21_HV <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_HV[[trt]] <- mod_R21_HV
}
length( tricot_rank21_model_HV)

# name for each model
labels = c("Root yield", "Density", "Root color","Root size"  ,"woodiness", "Overall impression")

worth_map(tricot_rank21_model_HV, labels,size=4) + theme(axis.text.x = element_text(angle = 90))

######################################## processing

Trait_list21_pr= list(c("ease_of_peeling_Best","ease_of_peeling_Worst"),
                      c( "Mash_color_Best","Mash_color_Worst"),
                      c(  "water_Best","water_Worst"),
                      c( "Mash_swelling_Best","Mash_swelling_Worst" ),
                      c( "Gari_yield_Best","Gari.yield_Worst"),
                      c("ovi_process_Best" , "ovi_process_Worst"))


tricot_rank21_ouptput_pr = list() #
for(trt in 1:length(Trait_list21_pr)){
  trt1 = Trait_list21_pr[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_pr <- rank_tricot(data = rank21a,
                        items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                        input = trt1)
  tricot_rank21_ouptput_pr[[trt]] <- R21_pr
}


tricot_rank21_model_pr = list()#
for(trt in 1:length(tricot_rank21_ouptput_pr)){
  trt1 = tricot_rank21_ouptput_pr[[trt]]

  mod_R21_pr <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_pr[[trt]] <- mod_R21_pr
}
length( tricot_rank21_model_pr)



# name for each model
labels = c("Ease of peeling", "Mash color", "Water content","Mash swelling", "Garri yield" , "Overall processing")

worth_map(tricot_rank21_model_pr, labels,size=4) + theme(axis.text.x = element_text(angle = 90))
######################################### Garri quality

Trait_list21_Gaq= list(c("Garri_Dryness_Best","Garri_Dryness_Worst"),
                      c( "Garri_color_Best","Garri_color_Worst"),
                      c( "Garri_Heaviness_Best","Garri_Heaviness_Worst"),
                      c( "Garri_smoothness_Best","Garri_smoothness_Worst" ),
                      c(  "Garri_taste_Best","Garri_taste_Worst" ),
                      c("Garri_woodiness_Best" , "Garri_woodiness_Worst"),
                      c("Garri_Ovi_Best", "Garri_Ovi_Worst" ))


tricot_rank21_ouptput_Gaq = list() #
for(trt in 1:length(Trait_list21_Gaq)){
  trt1 = Trait_list21_Gaq[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_Gaq <- rank_tricot(data = rank21a,
                        items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                        input = trt1)
  tricot_rank21_ouptput_Gaq[[trt]] <- R21_Gaq
}


tricot_rank21_model_Gaq = list()#
for(trt in 1:length(tricot_rank21_ouptput_Gaq)){
  trt1 = tricot_rank21_ouptput_Gaq[[trt]]

  mod_R21_Gaq <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_Gaq[[trt]] <- mod_R21_Gaq
}
length( tricot_rank21_model_Gaq)



# name for each model
labels = c("Dryness", "Garri color", "Heaviness","Smoothness", "Taste" , "Woodiness", "Overall quality")

worth_map(tricot_rank21_model_Gaq, labels,size=4) + theme(axis.text.x = element_text(angle = 90))
################################# Eba quality

Trait_list21_ebq= list(c("Eba_Mouldability_Best" ,"Eba_Mouldability_Worst" ),
                       c( "Eba_Softness_Best" ,"Eba_Softness_Worst"),
                       c(  "Eba_Strechyness_Best","Eba_Strechyness_Worst"),
                       c( "Eba__Smoothness_Best","Eba__Smoothness_Worst" ),
                       c(  "Eba_Colour_Best","Eba_Colour_Worst" ),
                       c("Eba_stickiness_Best" ,  "Eba_stickiness_Worst"),
                       c("Eba_Swelling_Best", "Eba_Swelling_Worst"),
                      c("Eba_Taste_Best", "Eba_Taste_Worst"),
                      c("Eba_ovi_Best", "Eba_ovi_Worst" ))


tricot_rank21_ouptput_ebq = list() #
for(trt in 1:length(Trait_list21_ebq)){
  trt1 = Trait_list21_ebq[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_ebq <- rank_tricot(data = rank21a,
                         items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                         input = trt1)
  tricot_rank21_ouptput_ebq[[trt]] <- R21_ebq
}


tricot_rank21_model_ebq = list()#
for(trt in 1:length(tricot_rank21_ouptput_ebq)){
  trt1 = tricot_rank21_ouptput_ebq[[trt]]

  mod_R21_ebq <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_ebq[[trt]] <- mod_R21_ebq
}
length( tricot_rank21_model_ebq)



# name for each model
labels = c("Mouldability", "Softness", "Stretchability","Smoothness", "Eba_color", "Stickiness" , "Swelling", "Eba_Taste", "Overall Eba quality")

worth_map(tricot_rank21_model_ebq, labels,size=4) + theme(axis.text.x = element_text(angle = 90))
############################# overnight eba


Trait_list21_OvE= list(c("OvNight_mouldability_Best" ,"OvNight_mouldability_Worst"),
                       c( "OvNight_softness_Best" ,"OvNight_softness_Worst"),
                       c(  "OvNight_strechyness_Best","OvNight_strechyness__Worst"),
                       c( "OvNight_smoothness_Best","OvNight_smoothness_Worst"),
                       c(  "OvNight_stickiness_Best","OvNight_taste_Best" ),
                       c("OvNight_taste_Best" ,  "OvNight_taste_Worst"),
                       c("OvNight_color_Best" , "OvNight_color_Worst"),
                       c("OvNight_aroma_Best", "OvNight_aroma_Worst"),
                       c("OvI_OvNight_Best", "OvI_OvNight_Worst"))


tricot_rank21_ouptput_OvE= list() #
for(trt in 1:length(Trait_list21_OvE)){
  trt1 = Trait_list21_OvE[[trt]] # subsetting of the trait list

  # use rank tricot to rank the variets (base on the genotype names)
  R21_OvE <- rank_tricot(data = rank21a,
                         items = c("Variety.A" ,"Variety.B",  "Variety.C"),
                         input = trt1)
  tricot_rank21_ouptput_OvE[[trt]] <- R21_OvE
}


tricot_rank21_model_OvE = list()#
for(trt in 1:length(tricot_rank21_ouptput_OvE)){
  trt1 = tricot_rank21_ouptput_OvE[[trt]]

  mod_R21_OvE <- PlackettLuce(trt1, npseudo = 0.5)

  tricot_rank21_model_OvE[[trt]] <- mod_R21_OvE
}
length( tricot_rank21_model_OvE)



# name for each model
labels = c("Mouldability", "Softness", "Stretchability","Smoothness",  "Stickiness", "Eba_Taste", "Eba_color" , "Aroma" , "Overall Overnight Eba Quality")

worth_map(tricot_rank21_model_OvE, labels,size=4) + theme(axis.text.x = element_text(angle = 90))

#################counting genotypes in the 2021 tricot
# convert the code names to genotype names so i can count the number of times a genotype was best and worst


rank21a[rank21a == ""] <- NA


# convert the code names to genotype names so i can count the number of times a genotype was best and worst
rowconv21 = data.frame() # converted row
for(j in 1:nrow(rank21a)){
  rw = rank21a[j,]
  colnames(rw)
  #removing Spaces in the variety code, and changing codes written in lowercase to uppercase
  vrt = rw[,11:13] # has the genotype names as Variety.A, B and C
  idA = which(rw[,14:length(colnames(rw))] == "A")
  idB = which(rw[,14:length(colnames(rw))] == "B")
  idC = which(rw[,14:length(colnames(rw))] == "C")
  # replacement
  rw[,colnames(rw)[14:length(colnames(rw))][idA]] = vrt[1] #
  rw[,colnames(rw)[14:length(colnames(rw))][idB]] = vrt[2] #
  rw[,colnames(rw)[14:length(colnames(rw))][idC]] = vrt[3] #



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
  rowconv21 = rbind(rowconv21, rw) # converted the ABC ranks to genotype names

}


head(rowconv21)
colnames(rowconv21)
#Want to transform the dataframe to have
rank21_chnged = rowconv21[,-c(11:13)] # removed columns containing  variety.ABC because the genotype are already represented in the trait evaluated
head(rank21_chnged)


colnames(rank21_chnged)
# introduce a new colunm called the geno_Trait and convert the rank22_chnged into a long format
rank_21_new = tibble()
for(i in 11:length(colnames(rank21_chnged))){ #from row 11 where the ranking starts
  bb21 = rank21_chnged[,c(1:10,i)] # 1: 10 contains the farmers information
  cl21 = cbind(bb21,Trait = colnames(rank21_chnged)[i] )
  colnames(cl21)[11] = "geno_Trait"
  rank_21_new = rbind(rank_21_new,cl21)
}
tail(rank_21_new)

unique(rank_21_new$geno_Trait)
head(rank_21_new)

which(is.na(rank_21_new$geno_Trait))# identify traits with no information

head(rank_21_new)
rank_21_new_ord = rank_21_new[order(rank_21_new$geno_Trait),]# order the df base on the geno_Trait
tail(rank_21_new_ord)
head(rank_21_new_ord)
unique(rank_21_new_ord$geno_Trait)
idx <- which(is.na(rank_21_new_ord$geno_Trait))
rank_21_new_ord = rank_21_new_ord[-idx,]
tail(rank_21_new_ord)
write.csv(x = rank_21_new_ord, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/rank21_for_counting.csv", row.names = F)
new_rank21= read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/rank21_for_counting.csv")
tail(new_rank21)


idW21 = which(str_detect(new_rank21$Trait, "Worst")) # identify worst
new_rank21$Trait[idW21] = 0

idBst21 = which(str_detect(new_rank21$Trait, "Best")) # identify worst
new_rank21$Trait[idBst21] = 1
head(new_rank21)
# which(new_rank22 == "Best_Pgwt3")
summary(new_rank21)
unique(new_rank21$Trait)
new_rank21$Trait= as.numeric(new_rank21$Trait)

count_trait= table(new_rank21$geno_Trait, new_rank21$Trait)
count_trait = as.data.frame(count_trait)
colnames(count_trait) <- c("Variety","Eval", "count")
str(count_trait)
library(ggplot2)

ggplot(data = count_trait, mapping = aes(x = Variety, y = count, fill = Eval)) +
  geom_bar(stat = "identity", ) + facet_grid(Eval~.) +
  theme(axis.text.x  = element_text(angle = 90)) +
  geom_text(aes(label = count, y = count+3), size = 3)

count_trait_wide <- dcast(data = count_trait, formula = Variety ~ Eval, value.var = "count")
colnames(count_trait_wide)[2:3] <- c("Worst", "Best")
count_trait_wide$Best_Worst <- count_trait_wide$Best - count_trait_wide$Worst
write.csv(x= count_trait_wide, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/rank21_best_worst counting.csv")

gen21 = unique(new_rank21$geno_Trait)
gen_count21 = tibble()
for (i in gen21) {
  gl_21 = new_rank21[new_rank21$geno_Trait %in%i ,]# counting genotypes and the number of times they were ranked best and worst

  lgth = length(gl_21$Trait)
  lg2_21= cbind(geno = i, observation = lgth)
  gen_count21 <- rbind(gen_count21, lg2_21)# make a dataframe of the count and the genotypes
}


gen21 <- as.character(unique(c(rank21a$Variety.A,rank21a$Variety.B,rank21a$Variety.C)))
# counting the number of times a genotype was present in the trial
Package_count = tibble()
for(i in gen21) {
  id1 = length(which(rank21a$Variety.A == i)) # counting the number of times a particular genotype was variety A, B and C
  id2 = length(which(rank21a$Variety.B == i))
  id3 = length(which(rank21a$Variety.C == i))
  packn = id1 + id2 +id3 # adding the number of times a particular genotype was variety A, B and C
  pk = cbind(variety = i, pack_count =  packn)
  Package_count = rbind(Package_count,pk ) # make a table of genotypes and their count
}
write.csv(x= Package_count, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/number_times_geno_appeared_package2021.csv")

# converting the count of best(1) and worst (0) to the long format so it can be plotted


count_long21 = tibble()
for(i in colnames(count_trait)){
  cnt = count_trait[,i]
  cnt1 = cbind(geno = rownames(count_trait), Trait = i, count = cnt)
  count_long21 = rbind(count_long21, cnt1)
}

countwide <- dcast(data = count_long21, formula = geno ~ Trait, value.var = "count")
#countwide <- countwide[-1,]
colnames(countwide)[2:3] <- c("worst", "best")
countwide$DifB_W <- countwide$best-countwide$worst
write.csv(x=countwide, file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/Difference_Best_worst.csv")


