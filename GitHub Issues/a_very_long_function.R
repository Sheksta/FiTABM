#------------------------------- Individual runs --------------------------------#

run_model <- function(number_of_agents, rn, w, threshold) {
  
  # Set up some parameters
  time_steps <- nrow(FiT) # number of months in time series
  
  agents <- rerun(number_of_agents, 
                  Household_Agent("N", assign_income(), assign_size(), assign_region()))
  
  n_links <- 10
  
  mean_income <- mean(extract(agents, "income"))
  agents %<>% map(assign_LF) %>% map(assign_elec_cons) %>% map(assign_u_inc, mean_inc = mean_income) %>% 
    map(assign_soc_network, n_ag = number_of_agents, n_l = n_links)
  
  adopters <- agents[map(agents, "status") == 1]
  
  if (length(adopters) > 0){
    n_owners <<- owner_occupiers[[1, 2]]
    init_cap <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
  }
  else{
    init_cap <- 0
  }
  
  # Create agents: all non-adopters, assign income, size and region randomly weighted by real data
  
  # assign further characteristics based on those previously assigned.
  
  # Set up data frame to put data in
  
  avg_u <- data.frame(time_series = FiT$time_series + months(1), 
                      run_number = as.factor(rep.int(rn, time_steps)),
                      mean_u_inc = vector(length = time_steps),
                      mean_u_ec = vector(length = time_steps),
                      mean_u_soc = vector(length = time_steps),
                      mean_u_cap = vector(length = time_steps),
                      mean_u_tot = vector(length = time_steps),
                      sd_u_inc = vector(length = time_steps),
                      sd_u_ec = vector(length = time_steps),
                      sd_u_soc = vector(length = time_steps),
                      sd_u_cap = vector(length = time_steps),
                      sd_u_tot = vector(length = time_steps),
                      frac_of_adopters = vector(length = time_steps),
                      avg_inst_cap = vector(length = time_steps),
                      tot_inst_cap = vector(length = time_steps),
                      inst_cap_diff = vector(length = time_steps)
  )
  
  #---------------------------------------------------------#
  
  # Time evolution! 
  
  if(run_w_cap == TRUE) {
    quarter_done <- 0 # how many quarters' capacity have already been used up?
    exceeded <- FALSE # has the total available capacity been exceeded?
  }
  
  for (i in 1:time_steps) {
    # set parameters for current time
    FiT_current_small <<- FiT$FiT[[i]]/100 # p to £
    FiT_current_large <<- FiT$FiT_large[[i]]/100
    exp_tar_current <<- FiT$exp_tar[[i]]/100 # p to £
    kW_price_current <<- kW_price$X2[i]
    current_date <<- FiT$time_series[i]
    elec_index <- which(sapply(elec_price_time$X1, function(x) grep(x, current_date)) == 1)
    elec_price <<- elec_price_time[[elec_index, 2]]/100
    n_owners <<- owner_occupiers[[elec_index, 2]]
    
    agents <- agents %>% map(assign_inst_cap) %>% map(utilities, w = w, ags = agents) %>% 
      map(decide, threshold = threshold)
    
    
    adopters <- agents[map(agents, "status") == 1]
    
    
    # Write data
    k <- extract(agents, "status") == "Y"
    avg_u$frac_of_adopters[i] <- length(k[k == TRUE])/number_of_agents
    avg_u$mean_u_ec[i] <- mean(extract(agents, "u_ec"))
    avg_u$mean_u_inc[i] <- mean(extract(agents, "u_inc"))
    avg_u$mean_u_soc[i] <- mean(extract(agents, "u_soc"))
    avg_u$mean_u_cap[i] <- mean(extract(agents, "u_cap"))
    avg_u$mean_u_tot[i] <- mean(extract(agents, "u_tot"))
    avg_u$sd_u_ec[i] <- sd(extract(agents, "u_ec"))
    avg_u$sd_u_inc[i] <- sd(extract(agents, "u_inc"))
    avg_u$sd_u_soc[i] <- sd(extract(agents, "u_soc"))
    avg_u$sd_u_cap[i] <- sd(extract(agents, "u_cap"))
    avg_u$sd_u_tot[i] <- sd(extract(agents, "u_tot"))
    
    if (length(adopters) > 0){
      avg_u$avg_inst_cap[i] <- mean(extract(adopters, "inst_cap"))
      avg_u$tot_inst_cap[i] <- sum(extract(adopters, "inst_cap"), na.rm = TRUE)*n_owners/(1000*number_of_agents)
    }
    else{
      avg_u$avg_inst_cap[i] <- NA
      avg_u$tot_inst_cap[i] <- 0
    }
    
    avg_u$inst_cap_diff[i] <- deployment$real_cap[i] - 
      avg_u$tot_inst_cap[i]
    
    ##### Deployment cap code - only runs if using a deployment cap scenario
    if (run_w_cap == TRUE){
      
      
      if ((avg_u$tot_inst_cap[i] - init_cap) > sum(dep_cap$orig_cap) && exceeded == FALSE) { # total available capacity has been exceeded;
        # all further FiTs are zero
        FiT[(i+1):nrow(FiT), 2:4] <<- 0
        exceeded <- TRUE
      }
      
      if (exceeded == FALSE && i < nrow(FiT)) { # Not yet exceeded all the caps & not in the final time step
        
        which_q <- max(which(current_date >= dep_cap$q_dates)) # which quarter are we in?
        
        ref_cap <- avg_u$tot_inst_cap[avg_u$time_series == dep_cap$q_dates[which_q]]
        if(is_empty(ref_cap)) ref_cap <- init_cap
        
        current_quarter <- avg_u$tot_inst_cap[i] - ref_cap # how much capacity has been installed so far in the quarter?
        
        dep_cap$inst_cap[which_q] <<- current_quarter
        
        current_month <- avg_u$tot_inst_cap[i] - avg_u$tot_inst_cap[i-1] # capacity installed this month
        
        excess_cap <- dep_cap$cap[which_q] - current_quarter # how much capacity is left over in the current quarter?
        
        if (current_month > 0 && which_q < nrow(dep_cap) && quarter_done == which_q) {
          # some capacity has been installed this month, not in the final quarter, and we aren't in next q's capacity already
          remaining <- current_month # the remaining installed capacity which must be assigned to this or next month
          
          for (j in 1:(nrow(dep_cap)-which_q)) { # 
            
            if (dep_cap$cap[which_q + j] - remaining < 0) { # if we are exceeding the available capacity in a quarter
              
              current_FiT_index <- min(which(FiT$FiT[i] == FiT_list))
              if (current_FiT_index == length(FiT_list)) {
                FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index]
              }
              else FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index + 1]
              
              remaining <- remaining - dep_cap$cap[which_q + j]
              dep_cap$cap[which_q + j] <<- 0
            } else if (dep_cap$cap[which_q + j] - remaining >= 0) { # all the capacity has been allocated; break out of the j loop
              dep_cap$cap[which_q + j] <<- dep_cap$cap[which_q + j] - remaining
              
              break
            }
          }
        }
        
        if (excess_cap < 0 && which_q < nrow(dep_cap) && quarter_done != which_q) {
          # have exceeded the allocated capacity for this quarter
          remaining <- current_quarter - dep_cap$cap[which_q]
          
          quarter_done <- which_q
          for (j in 1:(nrow(dep_cap)-which_q)) {
            
            if (dep_cap$cap[which_q + j] - remaining < 0) {
              
              current_FiT_index <- min(which(FiT$FiT[i] == FiT_list))
              if (current_FiT_index == length(FiT_list)) {
                FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index]
              }
              else FiT[(i+1):nrow(FiT), 2:3] <<- FiT_list[current_FiT_index + 1]
              
              remaining <- remaining - dep_cap$cap[which_q + j]
              dep_cap$cap[which_q + j] <<- 0
              
            } else if (dep_cap$cap[which_q + j] - remaining >= 0) {
              dep_cap$cap[which_q + j] <<- dep_cap$cap[which_q + j] - remaining
              break
            }
          }
        }
        
        if (current_date == dep_cap$q_dates[which_q] + months(2) && excess_cap > 0) {
          dep_cap$cap[which_q + 1] <<- dep_cap$cap[which_q + 1] + excess_cap
        }
        
      }
      
    }
    
    #### ^^^ End of deployment cap code
    
  }
  
  if (run_w_cap == TRUE && exceeded == TRUE) cat("Total available capacity was exceeded", "\n")
  
  FiT_outp <- cbind(FiT, run_number = rep(rn, nrow(FiT)))
  
  LCOE_data <- calc_LCOE(adopters, rn, number_of_agents)
  LCOE_avg <- LCOE_data[[1]]
  LCOE_data <- LCOE_data[[2]]
  cost_subs_res <- subs_cost(adopters, rn, number_of_agents)
  cost_priv_res <- priv_cost(adopters, rn, number_of_agents)
  tot_subs_cost <- cost_subs_res[[2]]
  ann_subs_cost <- cost_subs_res[[1]]
  
  tot_priv_cost <- cost_priv_res[[2]]
  cum_priv_cost <- cost_priv_res[[1]]
  all_results <- list(avg_u, ann_subs_cost, tot_subs_cost, cum_priv_cost, 
                      tot_priv_cost, LCOE_avg, LCOE_data)
  if (run_w_cap == TRUE) {
    all_results <- list(avg_u, ann_subs_cost, tot_subs_cost, cum_priv_cost, 
                        tot_priv_cost, LCOE_avg, LCOE_data, FiT_outp)
  }
  
  cat(length(adopters), "adopters in run", rn, "\n", sep = " ")
  
  return(all_results)
}
