# library(tidyverse)
library(igraph)
library(data.table)

################################################################
# RUN MODEL STARTS HERE.
################################################################


abm_pv_model <- function(w, threshold, number_of_agents, adj){
  time_steps <- nrow(FiT)
  agent_incomes <- rlnorm(number_of_agents, 10.06654, 0.574439)
  agent_size <- sample(1:5, size = number_of_agents,
                       prob = c(0.29, 0.35, 0.16, 0.13, 0.07),
                       replace = TRUE)
  
  # Get region and load factor attributes for agents
  region_indexes <- sample(seq(11), size = number_of_agents, prob = region_weights, replace = TRUE)
  regions <- as.vector(LF[region_indexes,4])
  load_factor <- as.vector(LF[region_indexes,2]/100)
  
  # Estimate electricity consumption
  index <- findInterval(agent_incomes, income_thresh)
  mus_agents <- mus[cbind(index, agent_size)]
  sigmas_agents <- sigmas[cbind(index, agent_size)]
  
  electricity_consumption <- vapply(1:number_of_agents, function(i) {
    rlnorm(1, mus_agents[i], sigmas_agents[i])
  }, numeric(1))
  
  # Get agent income utility
  mean_income <- mean(agent_incomes)
  u_inc <- 1/(1+exp((mean_income-agent_incomes)*0.0002))
  
  # Set adopter status
  adopter_status <- rep(FALSE, times = number_of_agents)
  
  # Initialise Agent attributes 
  Agents = data.table(
    inst_date = rep(0,number_of_agents),
    status = adopter_status,
    income = agent_incomes,
    size = agent_size,
    region = regions,
    load_factor = load_factor,
    elec_cons = electricity_consumption,
    feed_in_tariff = rep(0,number_of_agents),
    sys_total_output = rep(0,number_of_agents),
    inst_cap = rep(0,number_of_agents),
    annual_ret = rep(0,number_of_agents),
    u_inc = u_inc,
    u_ec = rep(0,number_of_agents),
    u_soc = rep(0,number_of_agents),
    u_cap = rep(0,number_of_agents),
    u_tot = rep(0,number_of_agents)
  )
  
  avg_u <- data.frame(
    time_series = FiT$time_series + months(1),
    avg_inst_cap = rep(0, time_steps),
    tot_inst_cap = rep(0, time_steps)
  )
  
  #---------------------------------------------------------#
  # Time evolution!
  
  # Get the right PV system costs for calculation in the time series
  system_cost_dates <- paste0(as.character(elec_price_time$X1), "0601") %>% 
    as_date() %>% 
    sort()
  
  elec_index <- vapply(1:length(FiT$time_series), function(i) {
    idx_date <- which.min(abs(FiT$time_series[i]-system_cost_dates))
    return(idx_date)}, numeric(1))
  
  # Electricity Price over Time
  elec_price_t <- elec_price_time[elec_index, 2] / 100
  
  # Number of owner occupiers over Time
  n_owners_time <- owner_occupiers[elec_index, 2]
  
  for (i in 1:time_steps) {
    # set parameters for current time
    current_date <- FiT$time_series[i]
    FiT_current_small <- as.numeric(FiT$FiT[[i]] / 100)# p to £ 
    FiT_current_large <- as.numeric(FiT$FiT_large[[i]] / 100)
    exp_tar_current <- as.numeric(FiT$exp_tar[[i]] / 100) # p to £
    kW_price_current <- as.numeric(kW_price$X2[i])
    elec_price <- as.numeric(elec_price_t[i,])
    n_owners <- as.numeric(n_owners_time[i,])
    
    # Assign installation capacity
    
    non_adopters <- Agents[status == FALSE]
    theo_inst_cap <- 0.3*non_adopters$income/kW_price_current
    mt_dmnd <- non_adopters$elec_cons/((non_adopters$load_factor.LF)*24*365) # I believe an error was made here!! LF Was not divided by 100.
    
    # Replace cases where kW req to meet demand exceeds installation capacity.
    idxs_to_replace <- mt_dmnd < theo_inst_cap
    theo_inst_cap[idxs_to_replace] <- mt_dmnd[idxs_to_replace]
    
    # Calculate annual return if inst_cap as is
    non_adopters[theo_inst_cap > 4]$feed_in_tariff <- FiT_current_large
    non_adopters[theo_inst_cap <= 4]$feed_in_tariff <- FiT_current_small
    non_adopters$sys_total_output <- theo_inst_cap*24*365*non_adopters$load_factor.LF
    R_FiT <- non_adopters$sys_total_output*non_adopters$feed_in_tariff
    displaced <- export <- 0.5*non_adopters$sys_total_output
    R_exp <- export*exp_tar_current
    R_sav <- displaced*elec_price
    R <- R_FiT + R_sav + R_exp
    
    # would systems larger than 4kw be better off choosing a smaller solar panel? Check and replace
    big_system_idxs <- theo_inst_cap > 4
    load_factors_4kw <- non_adopters[big_system_idxs]$load_factor.LF
    inst_cap_4kw <- rep(4, length(load_factors_4kw))
    solar_output_4kw <- inst_cap_4kw*24*365*load_factors_4kw
    R_FiT_4kw <- solar_output_4kw*FiT_current_small
    displaced_4kw <- export_4kw <- 0.5*solar_output_4kw
    R_exp_4kw <- export_4kw*exp_tar_current
    R_sav_4kw <- displaced_4kw*elec_price
    R_4kw <- R_FiT_4kw + R_sav_4kw + R_exp_4kw
    idx_to_replace <- R[big_system_idxs] < R_4kw
    
    # replace the FiT and Solar Output for Agents for which it is economic to go for a smaller system.
    non_adopters[big_system_idxs][which(idx_to_replace)]$feed_in_tariff <- FiT_current_small
    non_adopters[big_system_idxs][which(idx_to_replace)]$sys_total_output <- solar_output_4kw[idx_to_replace]
    theo_inst_cap[big_system_idxs][which(idx_to_replace)] <- 4
    non_adopters$inst_cap <- theo_inst_cap

    
    # Get the final annual return expected for each agent
    R[big_system_idxs][which(idx_to_replace)] <- R_4kw[which(idx_to_replace)]
    non_adopters$annual_ret <- R
    
    # Calculate payback period
    n <- 20 #economic life
    pp <- rep(0, nrow(non_adopters))
    pp[non_adopters$annual_ret > 0] <- non_adopters$inst_cap*kW_price_current/non_adopters$annual_ret
    pp[non_adopters$annual_ret < 0] <- n
    pp[pp > n] <- n
    
    
    # Calculate the utilities for all non_adopters
    
    # Economic Utility for agents
    non_adopters$u_ec <- (20-pp)/19
    
    # Social utility for agents
    neigh_w_solar <- as.numeric(adj %*% Agents$status) #TODO: Neigh with solar needs to be cut to the vector it's being applied to.
    non_adopters$u_soc <- 1/(1+exp(1.2*((n_links/4)-neigh_w_solar[Agents$status == FALSE])))
    
    # Capital utility for agents - U_cap <- needs to be fixed as well
    u_cap <- 1/(1+exp(-(0.2*non_adopters$income-non_adopters$inst_cap*kW_price_current)*0.0007))
    non_adopters$u_cap <- u_cap
    
    # Sum up all utilities with respective weights. Then establish purchase decision.
    u_tot <- w[1]*non_adopters$u_inc + w[2]*non_adopters$u_soc + w[3]*non_adopters$u_ec +  w[4]*non_adopters$u_cap
    non_adopters$u_tot <- u_tot
    
    # Decision threshold
    
    non_adopters[u_tot > threshold]$status <- TRUE
    non_adopters[u_tot > threshold]$inst_date <- current_date
    
    
    Agents[Agents$status == FALSE] <- non_adopters
    adopters <- sum(Agents$status == TRUE)

    
    # adopters <- Agents[Agents$status == TRUE]
    # Write data
    if(adopters > 0){
      avg_u$avg_inst_cap[i] <- mean(Agents[Agents$status == TRUE]$inst_cap)
      avg_u$tot_inst_cap[i] <- sum(Agents[Agents$status == TRUE]$inst_cap, na.rm = TRUE) * n_owners / (1000 * number_of_agents) %>% as.numeric() #MW
    }
  }
  
  return(avg_u)
}