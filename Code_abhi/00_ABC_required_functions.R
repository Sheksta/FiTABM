##########################################
# SUMMARY FUNCTION
##########################################

simulate_summary <- function(model_output){
  tot_inst_summ <- TSrepr::repr_paa(model_output$tot_inst_cap, q = 10, func = mean)
  avg_inst_summ <- TSrepr::repr_paa(model_output$avg_inst_cap, q = 10, func = mean)
  sim_summ <- c()
  sim_summ$tot <- tot_inst_summ
  sim_summ$avg <- avg_inst_summ
  return(sim_summ)
}

##########################################
# DISTANCE FUNCTION
##########################################

distance_calc_abc <- function(sim_summary_obj, obs_summary_obj){
  norm_obs_summ_tot <- obs_summary_obj$tot / max(obs_summary_obj$tot)
  norm_sim_summ_tot <- sim_summary_obj$tot / max(obs_summary_obj$tot)
  dist_tot <- dist(rbind(norm_sim_summ_tot, norm_obs_summ_tot))
  
  norm_obs_summ_avg <- obs_summary_obj$avg / max(obs_summary_obj$avg)
  norm_sim_summ_avg <- sim_summary_obj$avg / max(obs_summary_obj$avg)
  dist_avg <- dist(rbind(norm_sim_summ_avg, norm_obs_summ_avg))
  
  return(
    mean(cbind(dist_tot,dist_avg))
  )
}