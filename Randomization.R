install.packages("agricolae")
library(agricolae)
#gn = read.csv("~/Downloads/tri_geno.csv")
#library(agricolae)
Genotype <- gn$Geno
fbook <- design.rcbd(trt = Genotype, r = 3, serie = 0, seed = 1, first = T, continue = T, randomization = T)
fbook$book
fbook$sketch
write.csv(x = fbook$book, file = "Randomization_RCBD.csv", row.names = F)

genotype = c("G1","G2","G3","G4", "G5", "G6", "G7")
field = design.rcbd(trt=genotype, r=3, seed=1, first = T, serie=0,continue=T)
field$book
field$sketch

