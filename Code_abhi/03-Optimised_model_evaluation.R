source('01-required_functions.R')
source('02-run_functions.R')
source('Code_abhi/00-ABC_required_functions.R')
source('Code_abhi/02-Model_development.R')
load_data()
obs_summ_1000_avg <- readRDS(file = "Outputs/ABC/01A_prelim_dummy_obs_summ_1000sim_avg_ABC.rds")
n_agents <- 1000
n_links <- 10
adj_net <- readRDS(file ='Data/adjacency_matrix_1000n_10links.rds')
misc_args <- list(obs_summ = obs_summ_1000_avg,
                  number_of_agents = n_agents,
                  adj = adj_net)

############## Load above first #######################
theta_prop <- dummy_prior_transform(1)
  
time_taken_old <- system.time({
  wrapper_model_dist_transform(theta_prop, obs_summ_1000_avg)
})

time_taken_new <- system.time({
  dist_abm_pv_trans(theta_prop, misc_args)
})

expected_sims <- 1000000
exp_tot_t_old <- ((time_taken_old[3]*expected_sims)/60)/60/10
exp_tot_t_new <- ((time_taken_new[3]*expected_sims)/60)/60/10
percent_reduction <- (exp_tot_t_old - exp_tot_t_new)/exp_tot_t_old # 96.5 % REDUCTION !!! 
percent_reduction

###########################################
# Simulate a 1000 times
###########################################
library(igraph)
library(parallel)
cl <- makeCluster(10)

n_agents = 1000
n_links = 10
a_net <- sample_k_regular(n_agents, n_links, directed = FALSE, multiple = FALSE)
adj_net  <- get.adjacency(a_net)
saveRDS(adj_net,'Data/adjacency_matrix_1000n_10links.rds')

clusterEvalQ(cl, {
  source('01-required_functions.R')
  source('02-run_functions.R')
  source('Code_abhi/00-ABC_required_functions.R')
  source('Code_abhi/02-Model_development.R')
  load_data()
  obs_summ_1000_avg <- readRDS(file = "Outputs/ABC/01A_prelim_dummy_obs_summ_1000sim_avg_ABC.rds")
  n_agents <- 1000
  n_links <- 10
  adj_net <- readRDS(file ='Data/adjacency_matrix_1000n_10links.rds')
  misc_args <- list(obs_summ = obs_summ_1000_avg,
                    number_of_agents = n_agents,
                    adj = adj_net)
})



system.time({
  dists <- parSapply(cl, X = 1:1000, FUN = function(X){
    theta_prop <- dummy_prior_transform(1)
    sim_dist <- dist_abm_pv_trans_debug(theta_prop, misc_args)
    return(sim_dist)
  }, simplify = F)
})

#EXTRACT DISTANCES
all_dists <- sapply(dists, "[[", c(1))
hist(all_dists, breaks=100)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(all_dists)

zero_sims_idx <- (all_dists  == getmode(all_dists))
# high_sims_idx <- (all_dists > 2.5)

#PARAMS ANALYSIS
params <- sapply(dists, "[[", c(4))
par(mfrow=c(2,3))
for(i in 1:5){
  d<-hist(params[i,zero_sims_idx], breaks = 10)
}

pairs(t(params[,zero_sims_idx]), pch = 19, lower.panel = NULL)


#MODEL SIMULATIONS PLOT - AVG CAP
library(tidyverse)
library(reshape2)

obs_data <- readRDS("Outputs/ABC/01A_prelim_dummy_obs_1000sim_avg_ABC.rds")
model_ts_out <- sapply(dists, "[[", c(3))
list_avg_inst_cap <- model_ts_out[seq(1, length(model_ts_out), 3)+1]
df <-  matrix(unlist(list_avg_inst_cap), nrow=length(list_avg_inst_cap), byrow=TRUE) %>% 
  t() %>% 
  data.frame(
    time = 1:length(list_avg_inst_cap[[1]])
  )

obs_ts <- obs_data$avg_inst_cap %>% 
  data.frame(
    time = 1:length(list_avg_inst_cap[[1]])
  )

melted_df <- melt(df ,  id.vars = 'time', variable.name = 'series')
ggplot() + 
  geom_line(data = melted_df, aes(time,value, group = series), colour = "black", size = 1, alpha = 0.03)+
  geom_line(data = obs_ts, aes(x = time,y = .), colour = "blue", size = 2)+
  theme(legend.position = "none")+
  labs(title = "Average install capacity - 1000 simulations")
  
#MODEL SIMULATIONS PLOT - TOTAL CUMULATIVE CAP
list_tot_inst_cap <- model_ts_out[seq(1, length(model_ts_out), 3)+2]
df <-  matrix(unlist(list_tot_inst_cap), nrow=length(list_tot_inst_cap), byrow=TRUE) %>% 
  t() %>% 
  data.frame(
    time = 1:length(list_tot_inst_cap[[1]])
  )

obs_ts <- obs_data$tot_inst_cap %>% 
  data.frame(
    time = 1:length(list_tot_inst_cap[[1]])
  )

melted_df <- melt(df ,  id.vars = 'time', variable.name = 'series')
ggplot() + 
  geom_line(data = melted_df, aes(time,value, group = series), colour = "black", size = 1, alpha = 0.07)+
  geom_line(data = obs_ts, aes(x = time,y = .), colour = "blue", size = 2)+
  theme(legend.position = "none")+
  labs(title = "Total install capacity - 1000 simulations")

#########################################
# Checking a different distance measure
########################################
melted_df
diff_distances <- melted_df %>%
  group_by(series) %>%
  mutate_at(vars(value), list(~ .x - lag(.x)))

obs_time_series_tot_data <- obs_data$tot_inst_cap %>% 
  data.frame()

time_series_tot_data %>% 
  diff()


      
      