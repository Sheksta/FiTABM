##########################################
# SUMMARY FUNCTION
##########################################

simulate_summary <- function(sim_model_output, ref_obs_model_output){
  sim_summ <- c(sim_model_output$tot_inst_cap/max(ref_obs_model_output$tot_inst_cap), 
                sim_model_output$avg/max(ref_obs_model_output$avg_inst_cap))
  return(sim_summ)
}

##########################################
# DISTANCE FUNCTION
##########################################

calc_distance_abc <- function(sim_summary_vec, obs_summary_vec){
  dist <- sum((sim_summary_vec - obs_summary_vec)^2)
  return(dist)
}

##########################################
# PRIOR FUNCTION
##########################################

#Declare prior
prior_transform <- function(n = 1){
  #PRIOR SIMULATE
  # dirichlet bit
  test <- sapply(X = seq(n),FUN = function(number_to_output){
    alpha_vec <- c(1,1,1,1)
    v <- mapply(X = alpha_vec, FUN = function(X){
      return(rgamma(1, X))
    })
    z <- log(v) %>% as.numeric()
    
    # uniform bit
    min_unif <- 0.5
    max_unif <- 1
    prop <- runif(1, min = min_unif, max = max_unif)
    Y <- log((prop - min_unif)/(max_unif - prop)) %>% 
      as.numeric()
    
    theta <- append( z , Y ) %>% 
      t() %>% 
      as.matrix()
    
    return(theta)
  })
  
  prior_out <- t(test)
  return(prior_out)
}

##########################################
# PRIOR DENSITY EVALUATION FUNCTION
##########################################

#prior evaluation
prior_eval_transform <-  function(theta){
  min_unif <- 0.5
  max_unif <- 1
  alpha_vec <- c(1,1,1,1)
  #PRIOR DENSITY
  z <- theta[1:4]
  Y <- theta[5]
  prop_pdf <- prod((1/alpha_vec)*exp(alpha_vec*z - exp(z)))
  unif_pdf <- ((max_unif - min_unif)*exp(Y))/((1+ exp(Y))^2)
  density <- prop_pdf*unif_pdf %>% 
    as.numeric()
  return(density)
}

###########################################
# DISTANCE OVERALL FUNCTION
##########################################
dist_abm_pv_trans <- function(theta, misc_args){
  min_unif = 0.5
  max_unif = 1
  # Detransform particle
  v_retr <- exp(theta[1:4])
  x <- v_retr/sum(v_retr)
  p_retr <- (min_unif*exp(-theta[5]) + max_unif)/(1 + exp(-theta[5]))
  
  # Set parameters
  w <- as.numeric(x)
  threshold <- as.numeric(p_retr)
  
  #Simulate model
  sim_output <- abm_pv_model_collapse(w = w, 
                                      threshold = threshold, 
                                      number_of_agents = misc_args$number_of_agents, 
                                      n_links = misc_args$number_of_links,
                                      adj = misc_args$adj,
                                      FiT = misc_args$datasets$FiT, 
                                      region_weights = misc_args$datasets$region_weights, 
                                      LF = misc_args$datasets$LF, 
                                      income_thresh = misc_args$datasets$income_thresh, 
                                      mus = misc_args$datasets$mus, 
                                      sigmas = misc_args$datasets$sigmas, 
                                      elec_price_time = misc_args$datasets$elec_price_time, 
                                      owner_occupiers = misc_args$datasets$owner_occupiers, 
                                      kW_price = misc_args$datasets$kW_price) # run the model once
  
  
  #Summarise cumulative uptake from model simulation
  sim_summary <- simulate_summary(sim_output, misc_args$obs_output)
  
  #Evaluate distance from observed data
  dist_out <- calc_distance_abc(sim_summary, misc_args$obs_summary)
  
  gc()
  return(dist_out)
}

# # ### ## ### ###### ### #
# For debugging only - outputs list.
# ## ## ### ### ### #### ### 
# dist_abm_pv_trans_debug <- function(theta, misc_args){
#   min_unif = 0.5
#   max_unif = 1
#   # Detransform particle
#   v_retr <- exp(theta[1:4])
#   x <- v_retr/sum(v_retr)
#   p_retr <- (min_unif*exp(-theta[5]) + max_unif)/(1 + exp(-theta[5]))
#   
#   # Set parameters
#   w <- as.numeric(x)
#   threshold <- as.numeric(p_retr)
#   
#   #Simulate model
#   all_res_rn <- abm_pv_model(w = w, threshold = threshold, number_of_agents = misc_args$number_of_agents, adj = misc_args$adj) # run the model once
#   
#   
#   #Summarise cumulative uptake from model simulation
#   sim_summary <- simulate_summary(all_res_rn)
#   
#   #Evaluate distance from observed data
#   dist_out <- distance_calc_abc(sim_summary, misc_args$obs_summ)
#   
#   output <- list(dist = dist_out, summ = sim_summary, full = all_res_rn, params = c(w, threshold)) 
#   return(output)
# }

## ### ### #### #### ## #
# Old model - for benchmarking.
# ### # # # ## ### #
# wrapper_model_dist_transform <- function(theta, obs_summ){
#   min_unif = 0.5
#   max_unif = 1
#   # Detransform particle
#   v_retr <- exp(theta[1:4])
#   x <- v_retr/sum(v_retr)
#   p_retr <- (min_unif*exp(-theta[5]) + max_unif)/(1 + exp(-theta[5]))
#   
#   # Set parameters
#   w <- as.numeric(x)
#   threshold <- as.numeric(p_retr)
#   number_of_agents <- 1000
#   
#   #Simulate model
#   all_res_rn <- run_model_abc(number_of_agents, w, threshold) # run the model once
#   
#   #Summarise cumulative uptake from model simulation
#   sim_summary <- simulate_summary(all_res_rn)
#   
#   #Evaluate distance from observed data
#   dist_out <- distance_calc_abc(sim_summary, obs_summ)
#   return(dist_out)
# }