Txt_fresh1 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 1.xls")
Txt_fresh1 = as.data.frame(Txt_fresh1)
Txt_fresh1_A = Txt_fresh1[-c(1:3),]
head(Txt_fresh1_A)
library(stringi)
idc1 = which(str_detect(Txt_fresh1_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(Txt_fresh1_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
Txt_fresh1_A = Txt_fresh1_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(Txt_fresh1_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh1_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh1_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh1_A = Txt_fresh1_A[-idcomb2,]
head(Txt_fresh1_A)
view(Txt_fresh1_A)
Txt_fresh1_A = Txt_fresh1_A[,!colnames(Txt_fresh1_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(Txt_fresh1_A)

splt_let = strsplit(x = Txt_fresh1_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
Txt_fresh1_A$variety_code = x
#extracting farmer name from test_id

Txt_fresh1_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
  "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
  "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  Txt_fresh1_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh1_A$`Test ID`)
}
colnames(Txt_fresh1_A)[1] = "farmers_name"
Txt_fresh1_A = Txt_fresh1_A[,-2]
head(Txt_fresh1_A)


write.csv(x =Txt_fresh1_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh1.csv" )

# file 2
Txt_fresh2 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 2.xls")

Txt_fresh2 = as.data.frame(Txt_fresh2)
Txt_fresh2_A = Txt_fresh2[-c(1:3),]
head(Txt_fresh2_A)

idc1 = which(str_detect(Txt_fresh2_A[,1], "Start"))
idc2 = which(str_detect(Txt_fresh2_A[,1], "End"))
idcomb = c(idc1,idc2)
Txt_fresh2_A = Txt_fresh2_A[-idcomb,]

idc3 = which(str_detect(Txt_fresh2_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh2_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh2_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh2_A = Txt_fresh2_A[-idcomb2,]

Txt_fresh2_A = Txt_fresh2_A[,!colnames(Txt_fresh2_A) %in% c("...3","Fracturability")]
head(Txt_fresh2_A)

splt_let = strsplit(x = Txt_fresh2_A$Batch, split = "-")
x = c()
for(i in 1:length(splt_let)){
lt = splt_let[[i]][2]
x = c(x,lt)
}
Txt_fresh2_A$variety_code = x
head(Txt_fresh2_A)

Txt_fresh2_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh2_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh2_A$`Test ID`)
}
colnames(Txt_fresh2_A)[1] = "farmers_name"
Txt_fresh2_A = Txt_fresh2_A[,-2]

write.csv(x =Txt_fresh2_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh2.csv" )


Txt_fresh3 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 3.xls")
Txt_fresh3 = as.data.frame(Txt_fresh3)
Txt_fresh3_A = Txt_fresh3[-c(1:3),] # remove first 3 column from the data frame

idc1 = which(str_detect(Txt_fresh3_A[,1], "Start"))
idc2 = which(str_detect(Txt_fresh3_A[,1], "End"))
idcomb1 = c(idc1,idc2)
Txt_fresh3_A = Txt_fresh3_A[-idcomb1,]

idc3 = which(str_detect(Txt_fresh3_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh3_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh3_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh3_A = Txt_fresh3_A[-idcomb2,]

Txt_fresh3_A = Txt_fresh3_A[,!colnames(Txt_fresh3_A) %in% c("...3","Fracturability")]
head(Txt_fresh3_A)


splt_let = strsplit(x = Txt_fresh3_A$Batch, split = "-")
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2]
  x = c(x,lt)
}
Txt_fresh3_A$variety_code = x
head(Txt_fresh3_A)


Txt_fresh3_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh3_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh3_A$`Test ID`)# replacing the pattern with nothing
}
colnames(Txt_fresh3_A)[1] = "farmers_name" # name the new column name with for the new pattern
Txt_fresh3_A = Txt_fresh3_A[,-2]
dim(Txt_fresh3_A)
write.csv(x =Txt_fresh3_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh3.csv" )


# file 4
Txt_fresh4 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 4.xls")
Txt_fresh4 <- as.data.frame(Txt_fresh4)
Txt_fresh4_A <- Txt_fresh4[-c(1:3),]
idc1= which(str_detect(Txt_fresh4_A[,1], "Start"))
idc2= which(str_detect(Txt_fresh4_A[,1], "End"))
idcomb1=c(idc1, idc2)
Txt_fresh4_A=Txt_fresh4_A[-idcomb1,]

idc3 = which(str_detect(Txt_fresh4_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh4_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh4_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh4_A= Txt_fresh4_A[-idcomb2,]

Txt_fresh4_A = Txt_fresh4_A[,!colnames(Txt_fresh4_A) %in% c("...3", "Fracturability")] # remove unnecessary column

splt_let = strsplit(x = Txt_fresh4_A$Batch, split = "-") # include a column called variety_code by splitting the colunm, "BATCH"
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2]
  x = c(x,lt)
}
Txt_fresh4_A$variety_code = x
head(Txt_fresh4_A)


Txt_fresh4_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh4_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh4_A$`Test ID`)# replacing the pattern with nothing
}
colnames(Txt_fresh4_A)[1] = "farmers_name" # name the new column name with for the new pattern
Txt_fresh4_A = Txt_fresh4_A[,-2] # remove column 2 (BATCH)

head(Txt_fresh4_A)
write.csv(x =Txt_fresh4_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh4.csv" )

# file 5
Txt_fresh5 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 5.xls")
Txt_fresh5 <- as.data.frame(Txt_fresh5)
Txt_fresh5_A <- Txt_fresh5[-c(1:3),]
idc1= which(str_detect(Txt_fresh5_A[,1], "Start"))
idc2= which(str_detect(Txt_fresh5_A[,1], "End"))
idcomb1=c(idc1, idc2)
Txt_fresh5_A=Txt_fresh5_A[-idcomb1,]

idc3 = which(str_detect(Txt_fresh5_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh5_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh5_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh5_A= Txt_fresh5_A[-idcomb2,]

Txt_fresh5_A = Txt_fresh5_A[,!colnames(Txt_fresh5_A) %in% c("...3", "Fracturability")] # remove unnecessary column

splt_let = strsplit(x = Txt_fresh5_A$Batch, split = "-") # include a column called variety_code by splitting the colunm, "BATCH"
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2]
  x = c(x,lt)
}
Txt_fresh5_A$variety_code = x
head(Txt_fresh5_A)


Txt_fresh5_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh5_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh5_A$`Test ID`)# replacing the pattern with nothing
}
colnames(Txt_fresh5_A)[1] = "farmers_name" # name the new column name with for the new pattern
Txt_fresh5_A = Txt_fresh5_A[,-2] # remove column 2 (BATCH)

write.csv(x=Txt_fresh5_A, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh5.csv")

# file 6
Txt_fresh6 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 6.xls")
Txt_fresh6 <- as.data.frame(Txt_fresh6)
Txt_fresh6_A <- Txt_fresh6[-c(1:3),]
idc1= which(str_detect(Txt_fresh6_A[,1], "Start"))
idc2= which(str_detect(Txt_fresh6_A[,1], "End"))
idcomb1=c(idc1, idc2)
Txt_fresh6_A=Txt_fresh6_A[-idcomb1,]

idc3 = which(str_detect(Txt_fresh6_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh6_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh6_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh6_A= Txt_fresh6_A[-idcomb2,]

Txt_fresh6_A = Txt_fresh6_A[,!colnames(Txt_fresh6_A) %in% c("...3", "Fracturability")] # remove unnecessary column

splt_let = strsplit(x = Txt_fresh6_A$Batch, split = "-") # include a column called variety_code by splitting the colunm, "BATCH"
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2]
  x = c(x,lt)
}
Txt_fresh6_A$variety_code = x
head(Txt_fresh6_A)


Txt_fresh6_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh6_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh6_A$`Test ID`)# replacing the pattern with nothing
}
colnames(Txt_fresh6_A)[1] = "farmers_name" # name the new column name with for the new pattern
Txt_fresh6_A = Txt_fresh6_A[,-2] # remove column 2 (BATCH)


write.csv(x=Txt_fresh6_A, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh6.csv")


# file 7
Txt_fresh7 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 7.xls")
Txt_fresh7 <- as.data.frame(Txt_fresh7)
Txt_fresh7_A <- Txt_fresh7[-c(1:3),]
idc1= which(str_detect(Txt_fresh7_A[,1], "Start"))
idc2= which(str_detect(Txt_fresh7_A[,1], "End"))
idcomb1=c(idc1, idc2)
Txt_fresh7_A=Txt_fresh7_A[-idcomb1,]

idc3 = which(str_detect(Txt_fresh7_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh7_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh7_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh7_A= Txt_fresh7_A[-idcomb2,]

Txt_fresh7_A = Txt_fresh7_A[,!colnames(Txt_fresh7_A) %in% c("...3", "Fracturability")] # remove unnecessary column

splt_let = strsplit(x = Txt_fresh7_A$Batch, split = "-") # include a column called variety_code by splitting the colunm, "BATCH"
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2]
  x = c(x,lt)
}
Txt_fresh7_A$variety_code = x
head(Txt_fresh7_A)


Txt_fresh7_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh7_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh7_A$`Test ID`)# replacing the pattern with nothing
}
colnames(Txt_fresh7_A)[1] = "farmers_name" # name the new column name with for the new pattern
Txt_fresh7_A = Txt_fresh7_A[,-2] # remove column 2 (BATCH)


write.csv(x=Txt_fresh7_A, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh7.csv")


# file 8
Txt_fresh8 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 8.xls")
Txt_fresh8 <- as.data.frame(Txt_fresh8)
Txt_fresh8_A <- Txt_fresh8[-c(1:3),]
idc1= which(str_detect(Txt_fresh8_A[,1], "Start"))
idc2= which(str_detect(Txt_fresh8_A[,1], "End"))
idcomb1=c(idc1, idc2)
Txt_fresh8_A=Txt_fresh8_A[-idcomb1,]

idc3 = which(str_detect(Txt_fresh8_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh8_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh8_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh8_A= Txt_fresh8_A[-idcomb2,]

Txt_fresh8_A = Txt_fresh8_A[,!colnames(Txt_fresh8_A) %in% c("...3", "Fracturability")] # remove unnecessary column

splt_let = strsplit(x = Txt_fresh8_A$Batch, split = "-") # include a column called variety_code by splitting the colunm, "BATCH"
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2]
  x = c(x,lt)
}
Txt_fresh8_A$variety_code = x
head(Txt_fresh8_A)


Txt_fresh8_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh8_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh8_A$`Test ID`)# replacing the pattern with nothing
}
colnames(Txt_fresh8_A)[1] = "farmers_name" # name the new column name with for the new pattern
Txt_fresh8_A = Txt_fresh8_A[,-2] # remove column 2 (BATCH)


write.csv(x=Txt_fresh8_A, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh8.csv")


# file 9
Txt_fresh9 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 9.xls")
Txt_fresh9 <- as.data.frame(Txt_fresh9)
Txt_fresh9_A <- Txt_fresh9[-c(1:3),]
idc1= which(str_detect(Txt_fresh9_A[,1], "Start"))
idc2= which(str_detect(Txt_fresh9_A[,1], "End"))
idcomb1=c(idc1, idc2)
Txt_fresh9_A=Txt_fresh9_A[-idcomb1,]

idc3 = which(str_detect(Txt_fresh9_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh9_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh9_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh9_A= Txt_fresh9_A[-idcomb2,]

Txt_fresh9_A = Txt_fresh9_A[,!colnames(Txt_fresh9_A) %in% c("...3", "Fracturability")] # remove unnecessary column

splt_let = strsplit(x = Txt_fresh9_A$Batch, split = "-") # include a column called variety_code by splitting the colunm, "BATCH"
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2]
  x = c(x,lt)
}
Txt_fresh9_A$variety_code = x
head(Txt_fresh9_A)


Txt_fresh9_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh9_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh9_A$`Test ID`)# replacing the pattern with nothing
}
colnames(Txt_fresh9_A)[1] = "farmers_name" # name the new column name with for the new pattern
Txt_fresh9_A = Txt_fresh9_A[,-2] # remove column 2 (BATCH)


write.csv(x=Txt_fresh9_A, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh9.csv")


# file 10
Txt_fresh10 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 10.xls")
Txt_fresh10 <- as.data.frame(Txt_fresh10)
Txt_fresh10_A <- Txt_fresh10[-c(1:3),]
idc1= which(str_detect(Txt_fresh10_A[,1], "Start"))
idc2= which(str_detect(Txt_fresh10_A[,1], "End"))
idcomb1=c(idc1, idc2)
Txt_fresh10_A=Txt_fresh10_A[-idcomb1,]

idc3 = which(str_detect(Txt_fresh10_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh10_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh10_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh10_A= Txt_fresh10_A[-idcomb2,]

Txt_fresh10_A = Txt_fresh10_A[,!colnames(Txt_fresh10_A) %in% c("...3", "Fracturability")] # remove unnecessary column

splt_let = strsplit(x = Txt_fresh10_A$Batch, split = "-") # include a column called variety_code by splitting the colunm, "BATCH"
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2]
  x = c(x,lt)
}
Txt_fresh10_A$variety_code = x
head(Txt_fresh10_A)


Txt_fresh10_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh10_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh10_A$`Test ID`)# replacing the pattern with nothing
}
colnames(Txt_fresh10_A)[1] = "farmers_name" # name the new column name with for the new pattern
Txt_fresh10_A = Txt_fresh10_A[,-2] # remove column 2 (BATCH)


write.csv(x=Txt_fresh10_A, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh10.csv")


# file 11
Txt_fresh11 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 11.xls")
Txt_fresh11 <- as.data.frame(Txt_fresh11)
Txt_fresh11_A <- Txt_fresh11[-c(1:3),]
idc1= which(str_detect(Txt_fresh11_A[,1], "Start"))
idc2= which(str_detect(Txt_fresh11_A[,1], "End"))
idcomb1=c(idc1, idc2)
Txt_fresh11_A=Txt_fresh11_A[-idcomb1,]

idc3 = which(str_detect(Txt_fresh11_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh11_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh11_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh11_A= Txt_fresh11_A[-idcomb2,]

Txt_fresh11_A = Txt_fresh11_A[,!colnames(Txt_fresh11_A) %in% c("...3", "Fracturability")] # remove unnecessary column

splt_let = strsplit(x = Txt_fresh11_A$Batch, split = "-") # include a column called variety_code by splitting the colunm, "BATCH"
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2]
  x = c(x,lt)
}
Txt_fresh11_A$variety_code = x
head(Txt_fresh11_A)


Txt_fresh11_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh11_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh11_A$`Test ID`)# replacing the pattern with nothing
}
colnames(Txt_fresh11_A)[1] = "farmers_name" # name the new column name with for the new pattern
Txt_fresh11_A = Txt_fresh11_A[,-2] # remove column 2 (BATCH)


write.csv(x=Txt_fresh11_A, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh11.csv")


# file 12
Txt_fresh12 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 12.xls")
Txt_fresh12 <- as.data.frame(Txt_fresh12)
Txt_fresh12_A <- Txt_fresh12[-c(1:3),]
idc1= which(str_detect(Txt_fresh12_A[,1], "Start"))
idc2= which(str_detect(Txt_fresh12_A[,1], "End"))
idcomb1=c(idc1, idc2)
Txt_fresh12_A=Txt_fresh12_A[-idcomb1,]

idc3 = which(str_detect(Txt_fresh12_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh12_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh12_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh12_A= Txt_fresh12_A[-idcomb2,]

Txt_fresh12_A = Txt_fresh12_A[,!colnames(Txt_fresh12_A) %in% c("...3", "Fracturability")] # remove unnecessary column

splt_let = strsplit(x = Txt_fresh12_A$Batch, split = "-") # include a column called variety_code by splitting the colunm, "BATCH"
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2]
  x = c(x,lt)
}
Txt_fresh12_A$variety_code = x
head(Txt_fresh12_A)


Txt_fresh12_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh12_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh12_A$`Test ID`)# replacing the pattern with nothing
}
colnames(Txt_fresh12_A)[1] = "farmers_name" # name the new column name with for the new pattern
Txt_fresh12_A = Txt_fresh12_A[,-2] # remove column 2 (BATCH)


write.csv(x=Txt_fresh12_A, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh12.csv")


# file 13
Txt_fresh13 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 FRESH 13.xls")
Txt_fresh13 <- as.data.frame(Txt_fresh13)
Txt_fresh13_A <- Txt_fresh13[-c(1:3),]
idc1= which(str_detect(Txt_fresh13_A[,1], "Start"))
idc2= which(str_detect(Txt_fresh13_A[,1], "End"))
idcomb1=c(idc1, idc2)
Txt_fresh13_A=Txt_fresh13_A[-idcomb1,]

idc3 = which(str_detect(Txt_fresh13_A[,1], "Average"))
idc4 = which(str_detect(Txt_fresh13_A[,1], "S.D."))
idc5 = which(str_detect(Txt_fresh13_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
Txt_fresh13_A= Txt_fresh13_A[-idcomb2,]

Txt_fresh13_A = Txt_fresh13_A[,!colnames(Txt_fresh13_A) %in% c("...3", "Fracturability")] # remove unnecessary column

splt_let = strsplit(x = Txt_fresh13_A$Batch, split = "-") # include a column called variety_code by splitting the colunm, "BATCH"
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2]
  x = c(x,lt)
}
Txt_fresh13_A$variety_code = x
head(Txt_fresh13_A)


Txt_fresh13_A$`Test ID`
pttrn = c("FRESH -A-1", "FRESH -A-2", "FRESH -A-3",
          "FRESH -B-1", "FRESH -B-2", "FRESH -B-3", "FRESH -C-1",
          "FRESH -C-2", "FRESH -C-3", "FRESH -D-1", "FRESH -D-2", "FRESH -D-3")
#bb = Txt_fresh2_A$`Test ID`
for( i in pttrn){
  Txt_fresh13_A$`Test ID` = gsub(pattern = i, replacement = "", x = Txt_fresh13_A$`Test ID`)# replacing the pattern with nothing
}
colnames(Txt_fresh13_A)[1] = "farmers_name" # name the new column name with for the new pattern
Txt_fresh13_A = Txt_fresh13_A[,-2] # remove column 2 (BATCH)


write.csv(x=Txt_fresh13_A, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh13.csv")



Txt_fresh1 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh1.csv")
Txt_fresh2 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh2.csv")
Txt_fresh3 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh3.csv")
Txt_fresh4 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh4.csv")
Txt_fresh5 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh5.csv")
Txt_fresh6 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh6.csv")
Txt_fresh7 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh7.csv")
Txt_fresh8 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh8.csv")
Txt_fresh9 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh9.csv")
Txt_fresh10 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh10.csv")
Txt_fresh11 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh11.csv")
Txt_fresh12 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh12.csv")
Txt_fresh13 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/Txt_fresh13.csv")

fresh_txt = rbind(Txt_fresh1,Txt_fresh2, Txt_fresh3, Txt_fresh4,Txt_fresh5, Txt_fresh6,
                  Txt_fresh7, Txt_fresh8, Txt_fresh9, Txt_fresh10, Txt_fresh11, Txt_fresh12, Txt_fresh13)

write.csv(x=fresh_txt, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/ fresh_texture/All_fresh_texture.csv")


########################################################################################
#overnight file 1
ov_nght1 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 1.xls")
ov_nght1 = as.data.frame(ov_nght1)
ov_nght1_A = ov_nght1[-c(1:3),]
head(ov_nght1_A)
library(stringi)
idc1 = which(str_detect(ov_nght1_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght1_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght1_A = ov_nght1_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght1_A[,1], "Average"))
idc4 = which(str_detect(ov_nght1_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght1_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght1_A = ov_nght1_A[-idcomb2,]
head(ov_nght1_A)
view(ov_nght1_A)
ov_nght1_A= ov_nght1_A[,!colnames(ov_nght1_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght1_A)

splt_let = strsplit(x = ov_nght1_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght1_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght1_A)
ov_nght1_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght1_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght1_A$`Test ID`)
}
colnames(ov_nght1_A)[1] = "farmers_name"
ov_nght1_A= ov_nght1_A[,-2]
head(ov_nght1_A)


write.csv(x =ov_nght1_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght1_A.csv" )

## file 2

ov_nght2 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 2.xls")
ov_nght2 = as.data.frame(ov_nght2)
ov_nght2_A = ov_nght2[-c(1:3),]
head(ov_nght2_A)
library(stringi)
idc1 = which(str_detect(ov_nght2_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght2_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght2_A = ov_nght2_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght2_A[,1], "Average"))
idc4 = which(str_detect(ov_nght2_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght2_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght2_A = ov_nght2_A[-idcomb2,]

ov_nght2_A= ov_nght2_A[,!colnames(ov_nght2_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght2_A)

splt_let = strsplit(x = ov_nght2_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght2_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght2_A)
ov_nght2_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght2_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght2_A$`Test ID`)
}
colnames(ov_nght2_A)[1] = "farmers_name"
ov_nght2_A= ov_nght2_A[,-2]
head(ov_nght2_A)


write.csv(x =ov_nght2_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght2_A.csv" )

### file 3

ov_nght3 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 3.xls")
ov_nght3 = as.data.frame(ov_nght3)
ov_nght3_A = ov_nght3[-c(1:3),]
head(ov_nght3_A)
library(stringi)
idc1 = which(str_detect(ov_nght3_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght3_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght3_A = ov_nght3_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght3_A[,1], "Average"))
idc4 = which(str_detect(ov_nght3_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght3_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght3_A = ov_nght3_A[-idcomb2,]

ov_nght3_A= ov_nght3_A[,!colnames(ov_nght3_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght3_A)

splt_let = strsplit(x = ov_nght3_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght3_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght3_A)
ov_nght3_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght3_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght3_A$`Test ID`)
}
colnames(ov_nght3_A)[1] = "farmers_name"
ov_nght3_A= ov_nght3_A[,-2]
head(ov_nght3_A)


write.csv(x =ov_nght3_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght3_A.csv")

## file 4
ov_nght4 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 4.xls")
ov_nght4 = as.data.frame(ov_nght4)
ov_nght4_A = ov_nght4[-c(1:3),]
head(ov_nght4_A)

idc1 = which(str_detect(ov_nght4_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght4_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght4_A = ov_nght4_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght4_A[,1], "Average"))
idc4 = which(str_detect(ov_nght4_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght4_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght4_A = ov_nght4_A[-idcomb2,]

ov_nght4_A= ov_nght4_A[,!colnames(ov_nght4_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght4_A)

splt_let = strsplit(x = ov_nght4_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght4_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght4_A)
ov_nght4_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght4_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght4_A$`Test ID`)
}
colnames(ov_nght4_A)[1] = "farmers_name"
ov_nght4_A= ov_nght4_A[,-2]
head(ov_nght4_A)


write.csv(x =ov_nght4_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght4_A.csv")

# file 5
ov_nght5 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 5.xls")
ov_nght5 = as.data.frame(ov_nght5)
ov_nght5_A = ov_nght5[-c(1:3),]
head(ov_nght5_A)

idc1 = which(str_detect(ov_nght5_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght5_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght5_A = ov_nght5_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght5_A[,1], "Average"))
idc4 = which(str_detect(ov_nght5_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght5_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght5_A = ov_nght5_A[-idcomb2,]

ov_nght5_A= ov_nght5_A[,!colnames(ov_nght5_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght5_A)

splt_let = strsplit(x = ov_nght5_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght5_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght5_A)
ov_nght5_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght5_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght5_A$`Test ID`)
}
colnames(ov_nght5_A)[1] = "farmers_name"
ov_nght5_A= ov_nght5_A[,-2]
head(ov_nght5_A)


write.csv(x =ov_nght5_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght5_A.csv")

## 6
ov_nght6 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 6.xls")
ov_nght6 = as.data.frame(ov_nght6)
ov_nght6_A = ov_nght6[-c(1:3),]
head(ov_nght6_A)

idc1 = which(str_detect(ov_nght6_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght6_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght6_A = ov_nght6_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght6_A[,1], "Average"))
idc4 = which(str_detect(ov_nght6_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght6_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght6_A = ov_nght6_A[-idcomb2,]

ov_nght6_A= ov_nght6_A[,!colnames(ov_nght6_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght6_A)

splt_let = strsplit(x = ov_nght6_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght6_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght6_A)
ov_nght6_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght6_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght6_A$`Test ID`)
}
colnames(ov_nght6_A)[1] = "farmers_name"
ov_nght6_A= ov_nght6_A[,-2]
head(ov_nght6_A)


write.csv(x =ov_nght6_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght6_A.csv")

# file 7
ov_nght7 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 7.xls")
ov_nght7 = as.data.frame(ov_nght7)
ov_nght7_A = ov_nght7[-c(1:3),]
head(ov_nght7_A)

idc1 = which(str_detect(ov_nght7_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght7_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght7_A = ov_nght7_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght7_A[,1], "Average"))
idc4 = which(str_detect(ov_nght7_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght7_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght7_A = ov_nght7_A[-idcomb2,]

ov_nght7_A= ov_nght7_A[,!colnames(ov_nght7_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght7_A)

splt_let = strsplit(x = ov_nght7_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght7_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght7_A)
ov_nght7_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght7_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght7_A$`Test ID`)
}
colnames(ov_nght7_A)[1] = "farmers_name"
ov_nght7_A= ov_nght7_A[,-2]
head(ov_nght7_A)


write.csv(x =ov_nght7_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght7_A.csv")

## file 8
ov_nght8 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 8.xls")
ov_nght8 = as.data.frame(ov_nght8)
ov_nght8_A = ov_nght8[-c(1:3),]
head(ov_nght8_A)

idc1 = which(str_detect(ov_nght8_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght8_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght8_A = ov_nght8_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght8_A[,1], "Average"))
idc4 = which(str_detect(ov_nght8_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght8_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght8_A = ov_nght8_A[-idcomb2,]

ov_nght8_A= ov_nght8_A[,!colnames(ov_nght8_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght8_A)

splt_let = strsplit(x = ov_nght8_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght8_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght8_A)
ov_nght8_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght8_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght8_A$`Test ID`)
}
colnames(ov_nght8_A)[1] = "farmers_name"
ov_nght8_A= ov_nght8_A[,-2]
head(ov_nght8_A)


write.csv(x =ov_nght8_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght8_A.csv")

# file 9
ov_nght9 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 9.xls")
ov_nght9 = as.data.frame(ov_nght9)
ov_nght9_A = ov_nght9[-c(1:3),]
head(ov_nght9_A)

idc1 = which(str_detect(ov_nght9_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght9_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght9_A = ov_nght9_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght9_A[,1], "Average"))
idc4 = which(str_detect(ov_nght9_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght9_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght9_A = ov_nght9_A[-idcomb2,]

ov_nght9_A= ov_nght9_A[,!colnames(ov_nght9_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght9_A)

splt_let = strsplit(x = ov_nght9_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght9_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght9_A)
ov_nght9_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght9_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght9_A$`Test ID`)
}
colnames(ov_nght9_A)[1] = "farmers_name"
ov_nght9_A= ov_nght9_A[,-2]
head(ov_nght9_A)


write.csv(x =ov_nght9_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght9_A.csv")

## 10
ov_nght10 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 10.xls")
ov_nght10 = as.data.frame(ov_nght10)
ov_nght10_A = ov_nght10[-c(1:3),]
head(ov_nght10_A)

idc1 = which(str_detect(ov_nght10_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght10_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght10_A = ov_nght10_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght10_A[,1], "Average"))
idc4 = which(str_detect(ov_nght10_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght10_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght10_A = ov_nght10_A[-idcomb2,]

ov_nght10_A= ov_nght10_A[,!colnames(ov_nght10_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght10_A)

splt_let = strsplit(x = ov_nght10_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght10_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght10_A)
ov_nght10_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght10_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght10_A$`Test ID`)
}
colnames(ov_nght10_A)[1] = "farmers_name"
ov_nght10_A= ov_nght10_A[,-2]
head(ov_nght10_A)


write.csv(x =ov_nght10_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght10_A.csv")

## 11
ov_nght11 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 11.xls")
ov_nght11 = as.data.frame(ov_nght11)
ov_nght11_A = ov_nght11[-c(1:3),]
head(ov_nght11_A)

idc1 = which(str_detect(ov_nght11_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght11_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght11_A = ov_nght11_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght11_A[,1], "Average"))
idc4 = which(str_detect(ov_nght11_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght11_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght11_A = ov_nght11_A[-idcomb2,]

ov_nght11_A= ov_nght11_A[,!colnames(ov_nght11_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght11_A)

splt_let = strsplit(x = ov_nght11_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght11_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght11_A)
ov_nght11_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght11_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght11_A$`Test ID`)
}
colnames(ov_nght11_A)[1] = "farmers_name"
ov_nght11_A= ov_nght11_A[,-2]
head(ov_nght11_A)

write.csv(x =ov_nght11_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght11_A.csv")


##file12
ov_nght12 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 12.xls")
ov_nght12 = as.data.frame(ov_nght12)
ov_nght12_A = ov_nght12[-c(1:3),]
head(ov_nght12_A)

idc1 = which(str_detect(ov_nght12_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght12_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght12_A = ov_nght12_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght12_A[,1], "Average"))
idc4 = which(str_detect(ov_nght12_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght12_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght12_A = ov_nght12_A[-idcomb2,]

ov_nght12_A= ov_nght12_A[,!colnames(ov_nght12_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght12_A)

splt_let = strsplit(x = ov_nght12_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght12_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght12_A)
ov_nght12_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght12_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght12_A$`Test ID`)
}
colnames(ov_nght12_A)[1] = "farmers_name"
ov_nght12_A= ov_nght12_A[,-2]
head(ov_nght12_A)

write.csv(x =ov_nght12_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght12_A.csv")

## file 13
ov_nght13 <- readxl::read_xls("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/CHICHI GARRI 2022 OVERNIGHT 13.xls")
ov_nght13 = as.data.frame(ov_nght13)
ov_nght13_A = ov_nght13[-c(1:3),]
head(ov_nght13_A)

idc1 = which(str_detect(ov_nght13_A[,1], "Start")) #detect cells that start with "Start". These cells are empty
idc2 = which(str_detect(ov_nght13_A[,1], "End")) #detect cells that start with "End"
idcomb1 = c(idc1,idc2) # combining them and name the combination idcomb1
ov_nght13_A = ov_nght13_A[-idcomb1,] # remove the idcomb1 from the data frame

idc3 = which(str_detect(ov_nght13_A[,1], "Average"))
idc4 = which(str_detect(ov_nght13_A[,1], "S.D."))
idc5 = which(str_detect(ov_nght13_A[,1], "Coef. of Variation"))
idcomb2 = c(idc3,idc4, idc5)
ov_nght13_A = ov_nght13_A[-idcomb2,]

ov_nght13_A= ov_nght13_A[,!colnames(ov_nght13_A) %in% c("...3","Fracturability")] # remove "...3","Fracturability" from the data frame
head(ov_nght13_A)

splt_let = strsplit(x = ov_nght13_A$Batch, split = "-") # split the column containing strings
x = c()
for(i in 1:length(splt_let)){
  lt = splt_let[[i]][2] # the second element
  x = c(x,lt)
}
ov_nght13_A$variety_code = x
#extracting farmer name from test_id
head(ov_nght13_A)
ov_nght13_A$`Test ID`
pttrn = c("OVERNIGHT -A-1", "OVERNIGHT -A-2", "OVERNIGHT -A-3",
          "OVERNIGHT -B-1", "OVERNIGHT -B-2", "OVERNIGHT -B-3", "OVERNIGHT -C-1",
          "OVERNIGHT -C-2", "OVERNIGHT -C-3", "OVERNIGHT -D-1", "OVERNIGHT -D-2", "OVERNIGHT -D-3")
#bb = Txt_fresh1_A$`Test ID`
for( i in pttrn){
  ov_nght13_A$`Test ID` = gsub(pattern = i, replacement = "", x = ov_nght13_A$`Test ID`)
}
colnames(ov_nght13_A)[1] = "farmers_name"
ov_nght13_A= ov_nght13_A[,-2]
head(ov_nght13_A)

write.csv(x =ov_nght13_A,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght13_A.csv")


##input all the overnight data data
Txt_ov_night1 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght1_A.csv")
Txt_ov_night2 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght2_A.csv")
Txt_ov_night3 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght3_A.csv")
Txt_ov_night4 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght4_A.csv")
Txt_ov_night5 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght5_A.csv")
Txt_ov_night6 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght6_A.csv")
Txt_ov_night7 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght7_A.csv")
Txt_ov_night8 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght8_A.csv")
Txt_ov_night9 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght9_A.csv")
Txt_ov_night10 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght10_A.csv")
Txt_ov_night11 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght11_A.csv")
Txt_ov_night12 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght12_A.csv")
Txt_ov_night13 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/ov_nght13_A.csv")

over_night_txt = rbind(Txt_ov_night1,Txt_ov_night2, Txt_ov_night3, Txt_ov_night4,Txt_ov_night5, Txt_ov_night6,
                  Txt_ov_night7, Txt_ov_night8, Txt_ov_night9, Txt_ov_night10, Txt_ov_night11, Txt_ov_night12, Txt_ov_night13)

write.csv(x =over_night_txt,file = "/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Texture_profiling/over_night_texture/All_over_night_texture.csv")

