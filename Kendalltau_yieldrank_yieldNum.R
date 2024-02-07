Traits_cor5 = c("ovmap9_rank", "gwrt9_rank", "weed9_rank","branch9_rank","suit9_rank")

rank_out_cor5 = list(ovmap9_rank= ovmap9_rank, gwrt9_rank=gwrt9_rank,weed9_rank=weed9_rank,branch9_rank=branch9_rank,suit9_rank= suit9_rank)

Traits_cor = c("HvOvi_rank","hvt12_rank", "density_rank", "Rootcol_rank", "Rootsiz_rank", "Rootshp_rank")

rank_out_cor = list(HvOvi_rank = HvOvi_rank, hvt12_rank = hvt12_rank, density_rank = density_rank, Rootcol_rank = Rootcol_rank, Rootsiz_rank = Rootsiz_rank, Rootshp_rank = Rootshp_rank)

Traits_cor6 = c("ov_Garri_Proc_rank", "woodyfil_rank","peeling_rank","ease_proc_garri_rank","Garri_mash_rank","water_rank","swellmash_rank","garri_yield_rank")

rank_out_cor6 = list(ov_Garri_Proc_rank= ov_Garri_Proc_rank, woodyfil_rank= woodyfil_rank,peeling_rank=peeling_rank,ease_proc_garri_rank= ease_proc_garri_rank,Garri_mash_rank= Garri_mash_rank,water_rank= water_rank,swellmash_rank= swellmash_rank,garri_yield_rank=garri_yield_rank)

Traits_cor7 = c("ov_Garri_quality_rank" , "garri_woodyfil_rank", "garri_col_rank", "garri_dryness_rank" , "Garri_heaviness_rank", "garri_smoothness_rank","garri_taste_rank")

rank_out_cor7 = list(ov_Garri_quality_rank =ov_Garri_quality_rank , garri_woodyfil_rank = garri_woodyfil_rank, garri_col_rank = garri_col_rank, garri_dryness_rank = garri_dryness_rank, Garri_heaviness_rank= Garri_heaviness_rank, garri_smoothness_rank=garri_smoothness_rank,garri_taste_rank= garri_taste_rank)

raits_cor8 = c("ov_eba_quality_rank","fresh_mould_rank","eba_soft_rank","eba_draw_rank","eba_smooth_rank","eba_color_rank","eba_sticky_rank","eba_swell_rank","eba_taste_rank")

rank_out_cor8 = list(ov_eba_quality_rank=ov_eba_quality_rank, fresh_mould_rank=fresh_mould_rank, eba_soft_rank=eba_soft_rank, eba_draw_rank=eba_draw_rank,  eba_smooth_rank=eba_smooth_rank, eba_color_rank=eba_color_rank, eba_sticky_rank=eba_sticky_rank, eba_swell_rank=eba_swell_rank, eba_taste_rank=eba_taste_rank)

Traits_cor11 = c("ov_eba_quality_rank","fresh_mould_rank","eba_soft_rank","eba_draw_rank","eba_smooth_rank","eba_color_rank","eba_sticky_rank","eba_swell_rank","eba_taste_rank", "ov_ebaAF_quality_rank","EbaAF_mould_rank","EbaAF_soft_rank","EbaAF_draw_rank","EbaAF_smooth_rank","EbaAF_odor_rank", "EbaAF_taste_rank", "EbaAF_color_rank")

rank_out_cor11 = list(ov_eba_quality_rank=ov_eba_quality_rank, fresh_mould_rank=fresh_mould_rank, eba_soft_rank=eba_soft_rank, eba_draw_rank=eba_draw_rank,  eba_smooth_rank=eba_smooth_rank, eba_color_rank=eba_color_rank, eba_sticky_rank=eba_sticky_rank, eba_swell_rank=eba_swell_rank, eba_taste_rank=eba_taste_rank,   ov_ebaAF_quality_rank=ov_ebaAF_quality_rank,EbaAF_mould_rank=EbaAF_mould_rank,EbaAF_soft_rank=EbaAF_soft_rank,EbaAF_draw_rank=EbaAF_draw_rank,EbaAF_smooth_rank=EbaAF_smooth_rank,EbaAF_odor_rank=EbaAF_odor_rank, EbaAF_taste_rank=EbaAF_taste_rank, EbaAF_color_rank=EbaAF_color_rank)

Numeric traits
  num_traits <- c("plt.arch", "rt.shape", "rt.colour", "rot","sell.rt.no","unsell.rt.no", "total.rt.no", "sellable.rt.wt","unsellable.rt.wt","total.rt.wt","sht.wt","initial.plant.vigor", "mean_ht","mean_brnch_ht","total_yield_plot",    
                  "mkt_yield_plot","efficient_yield","total_root_num_plot", "mkt_root_number_plot", "efficient_rootNum","shwt_plot") 
  num_trt_disease = c("cbbi", "cbbs", "cadi", "cads")
  
)

Harvest_num_rank = c(rank_out_cor5, rank_out_cor)
length(Harvest_num_rank)
names(Harvest_num_rank)
Num_trait_comb = c(num_traits, num_trt_disease)
names(R) <- Traits 


# correlation 
Tau_corMat_Har_comb_Num = matrix(nrow = length(Harvest_num_rank), ncol = length(R))
rownames(Tau_corMat_Har_comb_Num) <- names(Harvest_num_rank)
colnames(Tau_corMat_Har_comb_Num) <- names(R)

Tau_corPvalue_comb_Num = matrix(nrow = length(Harvest_num_rank), ncol = length(R))
rownames(Tau_corPvalue_comb_Num) <- names(Harvest_num_rank)
colnames(Tau_corPvalue_comb_Num) <- names(R)

for(i in names(Harvest_num_rank)){
  print(nrow(Harvest_num_rank[[i]]))
}
rxx = c()
for(j in names(R)){
  rxx = c(rxx, nrow(R[[j]]))
}


for(i in names(Harvest_num_rank)){
  for(j in names(R)){
    kt1 <- gosset::kendallTau(Harvest_num_rank[[i]][-c(1,4:6,8,17,38)], R[[j]][,-c(5,2)], null.rm = TRUE)
    Tau_corMat_Har_comb_Num[i,j] = round(kt1[1,1],3)
    Tau_corPvalue_comb_Num[i,j] = round(kt1[1,4],4)
  }
}

