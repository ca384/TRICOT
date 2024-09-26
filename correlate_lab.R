chem22 <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_chem_Blue.csv" ) # the BLUE means for onfarm numeric data
head(chem22)
dim(chem22)
TPA_fresh <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_Blue.csv" ) # the BLUE means for onfarm numeric data
dim (TPA_fresh)
TPA_fresh2<-TPA_fresh%>%
dplyr:: rename("Adhesiveness_1"="Adhesiveness","Chewiness_1" = "Chewiness" , "Cohesiveness_1" = "Cohesiveness",
               "Gumminess_1"= "Gumminess" , "Resilience_1" = "Resilience", "Springiness_1" = "Springiness")%>%
  dplyr::select("Adhesiveness_1", "Cohesiveness_1",  "Gumminess_1", "Resilience_1" )
TPA_ov <- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_ov_Blue.csv" ) # the BLUE means for onfarm numeric data
dim(TPA_ov)

TPA_ov2= TPA_ov%>%
 dplyr:: rename("Adhesiveness_2"="Adhesiveness","Chewiness_2" = "Chewiness" , "Cohesiveness_2" = "Cohesiveness",
                "Gumminess_2"= "Gumminess" , "Resilience_2" = "Resilience", "Springiness_2" = "Springiness")%>%
  dplyr::select("Adhesiveness_2", "Cohesiveness_2",  "Gumminess_2", "Resilience_2" )

full_chem_blue<-cbind(chem22, TPA_fresh2, TPA_ov2 )
dim(full_chem_blue)
full_chem_blue=full_chem_blue%>%
  dplyr::rename("AMY_garri"="amy_garri","CF_garri" = "crude_fiber_garri",
                 "BD_garri" = "Bulk_Density", "STC_ch" ="Starch_ch",
                "STC_garri" = "starch_garri", "SC_garri"= "sugar_garri",
                "SI_garri"="SI" ,  "SP_garri"="SP", "WAC_garri"="WAC",
                "Adhes_1"= "Adhesiveness_1" , "Cohes_1"="Cohesiveness_1",
                "Gummi_1"="Gumminess_1" , "Res_1"=  "Resilience_1" ,
                "Adhes_2"= "Adhesiveness_2", "Cohes_2"="Cohesiveness_2",
                "Gummi_2"="Gumminess_2" ,   "Resil_2"= "Resilience_2" )
#%>%
#   dplyr::select ("X", "varieties", "AMY_ch",
#                  "BD_garri", "CF_ch",  "DMC", "AMY_garri","CF_garri",
#                  "SI_garri" ,  "SP_garri",  "STC_ch" , "STC_garri" ,
#                  "SC_garri" ,  "WAC_garri", "Adhes_1", "Cohes_1",
#                  "Gummi_1"  ,  "Res_1", "Adhes_2","Cohes_2",
#                 "Gummi_2"  ,  "Resil_2"  )
#######
cor_chem_Blue <- corTest(full_chem_blue[-c(1,2)])

cor_chem_Blue$r = round(cor_chem_Blue$r, 1)
cor_chem_Blue$p
cor_chem_Blue$p.adj
cor_chem_Blue$p


corrplot::corrplot(corr = cor_chem_Blue$r,
                   tl.cex = 0.5,p.mat = cor_chem_Blue$p,type = "lower",
                   method = 'color',addCoef.col ='black',diag = FALSE,
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.5,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
#Redo this was chem blues with TPA blup

write.csv(x=full_chem_blue, "/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_Chem_BLUE.csv")

All_Chem<-read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/All_TPA_Chem_BLUE.csv")
dim(All_Chem)
   All_eba_worth<- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/fres_overnight_eba_worth_estimate.csv")
dim(All_eba_worth)
All_eba_worth1<-All_eba_worth%>%
  dplyr::rename("Overall_Eba_2"="Overall.Eba_ov.Quality","Overall_Eba_1"= "Overall.Impression", "Stretchiness_1_R" = "Strechiness_1_R","Stretchiness_2_R"= "Strechiness_2_R",)%>%
  dplyr::select( "Mouldability_1_R", "Stretchiness_1_R", "Smoothness_1_R","Eba.color_1_R",
                 "Stickiness_1_R", "Swelling_1_R", "Taste_1_R", "Overall_Eba_1",
                "Mouldable_2_R", "Stretchiness_2_R", "Smoothness_2_R", "Overall_Eba_2", "Replant")

cor_rank_eba <- corTest(All_eba_worth1)

cor_rank_eba$r = round(cor_rank_eba$r, 2)
cor_rank_eba$p
cor_rank_eba$p.adj
cor_rank_eba$p


corrplot::corrplot(corr = cor_rank_eba$r,
                   tl.cex = 0.5,p.mat = cor_rank_eba$p,type = "lower",
                   method = 'color',addCoef.col ='black',diag = FALSE,
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.5,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))

#####################
#Eba tricot rank with chemical

cor_ranking_chem_eba <- corTest(All_eba_worth1, All_Chem[-c(1,2,3)])

cor_ranking_chem_eba$r = round(cor_ranking_chem_eba$r, 2)
cor_ranking_chem_eba$p
cor_ranking_chem_eba$p.adj
cor_ranking_chem_eba$p


corrplot::corrplot(corr =cor_ranking_chem_eba$r,
                   tl.cex = 0.5,p.mat = cor_ranking_chem_eba$p,type = "full",
                   method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.5,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
#R


cor_ranking_chem_eba2 <- corTest(All_eba_worth1, full_chem_blue[-c(1,2)])

cor_ranking_chem_eba2$r = round(cor_ranking_chem_eba2$r, 2)
cor_ranking_chem_eba2$p
cor_ranking_chem_eba2$p.adj
cor_ranking_chem_eba2$p


corrplot::corrplot(corr =cor_ranking_chem_eba2$r,
                   tl.cex = 0.5,p.mat = cor_ranking_chem_eba2$p,type = "full",
                   method = 'color',addCoef.col ='black',
                   sig.level = c(0.001,0.01, 0.05),
                   insig = "label_sig", pch.cex = 0.7,
                   number.cex = 0.5,
                   number.digits = 3, is.corr = T,outline = T, mar = c(0.5,0.5,0.5,0.5))
#R
#### New yield correlation with the model that has missing plot as covariate
onfarm22<- read.csv("/Users/ca384/Documents/ChinedoziRepo/TRICOT/output/2022_blues_model_missing_data.csv")
