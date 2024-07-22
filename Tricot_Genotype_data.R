packages_used <- c("workflowr", "tidyverse", "ggplot2", "here", "stringr" ,
                   "sommer","lme4","metan", "rrBLUP","FactoMineR","corrplot",
                   "ASRgenomics", "adegenet", "circlize", "dendextend")
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




tricot_snps <- read_tsv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/Tricot_22/Tricot_snps/29_tricot_snps.tsv")
head(tricot_snps[,1:10])
dim(tricot_snps)
summary(tricot_snps[,1:10])
for(i in colnames(tricot_snps[,1:10])){  # looking at subset to see if there are NAs in the SNP file
  print(length(which(is.na(tricot_snps[,i]))))
}
tricot_snps = as.data.frame(tricot_snps)
rownames(tricot_snps) <- tricot_snps$Marker
tricot_snps = tricot_snps[,-1]
tri_snps<- t(tricot_snps)
head(tri_snps[1:10,1:10])
dim(tri_snps)

devtools::install_github("wolfemd/genomicMateSelectR", ref = 'master')
library(genomicMateSelectR)
trsnps_mf <- genomicMateSelectR::maf_filter(M = tri_snps, thresh = 0.05 )
dim(trsnps_mf)
head(trsnps_mf[1:10,1:10])

Tri_snp_num = trsnps_mf

Tri_pc = PCA(Tri_snp_num ,scale.unit = T)
snpsc_Tri = scale(Tri_snp_num, center = T)
#snpsc_Trisc = scale(snpsc_Tri)
ds_Tri = dist(snpsc_Tri)
hc_Tri = hclust(ds_Tri,method = "ward.D")
plot(hc_Tri, hang = -1)

cl_Tri = cutree(tree = hc_Tri, k = 5)
cl_Tri = as.factor(cl_Tri)
# factoextra::fviz_pca_ind(Tri_pc ,
#                          col.ind = "blue", addEllipses = F, habillage =cl_Tri,
#                          geom = "point")

library(dendextend)
library(circlize)

# Hierarchical clustering dendrogram
hc_Tri1 <- as.dendrogram(hc_Tri)
hc_Tri1 <-hc_Tri1 %>%
  color_branches(k = 3)%>%
  color_labels(k = 3)


dend <- as.dendrogram(hc_Tri1)
dend <- dend %>% set("labels_cex", 0.6 )

# Plot dendrogram
plot(dend)

par(cex=0.6, mar=c(1,1,1,1) )
 circlize_dendrogram(hc_Tri1,
                    labels_track_height = 0.3,
                    dend_track_height = 0.4)


dev.off()




# dim(tri_snps)
# tri_snps1<- as.matrix(tri_snps)
# tri_snpsFilter  <- qc.filtering(tri_snps1, base=FALSE, ref=NULL,
#                             marker.callrate=0.2,ind.callrate=0.2, maf=0.05,heterozygosity=0.95,
#                             Fis=1, impute=FALSE,  plots=FALSE,message=FALSE)$M.clean
#
# compute genomic relationship matrix from filtered SNPS marker
Gmat  <- G.matrix(M = snpsFilter, method = "VanRaden", na.string = NA,message=FALSE)$G

heatmap(Gmat)
