setwd("C:\\Users\\vargh\\OneDrive - Queensland University of Technology\\University Studies\\Year 5\\Thesis\\R Project\\FiTABM")
source('Code_abhi/00-ABC_required_functions.R')
source('Code_abhi/02-Model_development.R')
source('Code_abhi/02B-Model_development_Collapse.R')
source('Code_abhi/000-load_model_inputs.R')



library(igraph)
library(data.table)
library(TSrepr)
library(protoABC)
library(parallel)


getDTthreads(verbose=TRUE)
setDTthreads(1)

#Get summaries of the each simulation from the true data
sim_summ_posterior_sample <- lapply(1:1000, function(X, pp_model_outputs, pp_avg_model_outputs){
  tot_inst_summ <- TSrepr::repr_paa(pp_model_outputs[X,], q = 10, func = mean)
  avg_inst_summ <- TSrepr::repr_paa(pp_avg_model_outputs[X,], q = 10, func = mean)
  sim_summ <- c()
  sim_summ$tot <- tot_inst_summ
  sim_summ$avg <- avg_inst_summ
  return(sim_summ)
}, pp_model_outputs = t(pp_model_outputs), pp_avg_model_outputs = t(pp_avg_model_outputs))

sim_summ_true_sample <- lapply(1:1000, function(X, pp_model_outputs, pp_avg_model_outputs){
  tot_inst_summ <- TSrepr::repr_paa(pp_model_outputs[X,], q = 10, func = mean)
  avg_inst_summ <- TSrepr::repr_paa(pp_avg_model_outputs[X,], q = 10, func = mean)
  sim_summ <- c()
  sim_summ$tot <- tot_inst_summ
  sim_summ$avg <- avg_inst_summ
  return(sim_summ)
}, pp_model_outputs = t(tot_model_outputs), pp_avg_model_outputs = t(avg_model_outputs))

#Get distances of the each simulation from the true data
distance_calc_abc <- function(sim_summary_obj, obs_summary_obj){
  norm_obs_summ_tot <- obs_summary_obj$tot / max(obs_summary_obj$tot)
  norm_sim_summ_tot <- sim_summary_obj$tot / max(obs_summary_obj$tot)
  dist_tot <- dist(rbind(norm_sim_summ_tot, norm_obs_summ_tot))
  
  norm_obs_summ_avg <- obs_summary_obj$avg / max(obs_summary_obj$avg)
  norm_sim_summ_avg <- sim_summary_obj$avg / max(obs_summary_obj$avg)
  dist_avg <- dist(rbind(norm_sim_summ_avg, norm_obs_summ_avg))
  
  return(
    sum( cbind(dist_tot, dist_avg))
  )
}

# Calculate distances
obs_summ_1000_avg <- readRDS(file = "Outputs/ABC/01A_prelim_dummy_obs_summ_1000sim_avg_ABC.rds")

dists_pp_sample <- sapply(1:1000, function(X, sim_summary_list_obj, obs_summary_list_obj){
  distance_calc_abc(sim_summary_list_obj[[X]], obs_summary_list_obj)
}, sim_summary_list_obj = sim_summ_posterior_sample, obs_summary_list_obj = obs_summ_1000_avg)

dists_true_sample <- sapply(1:1000, function(X, sim_summary_list_obj, obs_summary_list_obj){
  distance_calc_abc(sim_summary_list_obj[[X]], obs_summary_list_obj)
}, sim_summary_list_obj = sim_summ_true_sample, obs_summary_list_obj = obs_summ_1000_avg)

#Histogram of distances
par(mfrow = c(1,2))
hist(dists_true_sample, main = "dist - true dummy parameter")
hist(dists_pp_sample, main = "dist - ABC posterior sampling")

#Top 1% distances
n <- 1#% quantile of lowest distances
top_1perc_dists <- dists_true_sample[dists_true_sample <= quantile(dists_true_sample, n/100)]

##################################################
# Check if distances are running correctly
##################################################
cl <- parallel::makeCluster(10)
clusterEvalQ(cl, {
  source('01-required_functions.R')
  source('02-run_functions.R')
  source('Code_abhi/00-ABC_required_functions.R')
  source('Code_abhi/02-Model_development.R')
  source('Code_abhi/01C-ABC_transform_sample_algorithms.R')
  load_data()
  x_true <- c(0.237, 0.233, 0.066, 0.464, 0.751) #<- These are the 'true' chosen vals simulating it.
  obs_summ_1000_avg <- readRDS(file = "Outputs/ABC/01A_prelim_dummy_obs_summ_1000sim_avg_ABC.rds")
  detrans_particles_df <- readRDS(file = "Outputs/ABC/posterior_detrans_ABC.rds")
  n_links <- 10
  n_agents <- 7000
  a_net <- sample_k_regular(n_agents, n_links, directed = FALSE, multiple = FALSE)
  adj_net  <- get.adjacency(a_net)
  misc_args <- list(obs_summ = obs_summ_1000_avg,
                    number_of_agents = n_agents,
                    adj = adj_net)
})


num_samps <- 10000

prior_samples_model_outputs <- parLapply(cl, 1:num_samps, function(X){
  trans_theta <- dummy_prior_transform()
  detrans_theta <- detransform_single_particle(trans_theta)
  abm_pv_model(detrans_theta[1:4], detrans_theta[5], number_of_agents = misc_args$number_of_agents, adj = misc_args$adj)
})

tot_prior_simulation_capacity <- lapply(prior_samples_model_outputs, "[[", 2) %>% {
  matrix(unlist(.), nrow=length(.), byrow=TRUE)
}

avg_prior_simulation_capacity <- lapply(prior_samples_model_outputs, "[[", 3) %>% {
 matrix(unlist(.), nrow=length(.), byrow=TRUE)
}

sim_summ_prior_sample <- lapply(1:num_samps, function(X, pp_model_outputs, pp_avg_model_outputs){
  tot_inst_summ <- TSrepr::repr_paa(pp_model_outputs[X,], q = 10, func = mean)
  avg_inst_summ <- TSrepr::repr_paa(pp_avg_model_outputs[X,], q = 10, func = mean)
  sim_summ <- c()
  sim_summ$tot <- tot_inst_summ
  sim_summ$avg <- avg_inst_summ
  return(sim_summ)
}, pp_model_outputs = tot_prior_simulation_capacity, pp_avg_model_outputs = avg_prior_simulation_capacity)

# Calculate distances
dists_prior_sample <- sapply(1:num_samps, function(X, sim_summary_list_obj, obs_summary_list_obj){
  distance_calc_abc(sim_summary_list_obj[[X]], obs_summary_list_obj)
}, sim_summary_list_obj = sim_summ_prior_sample, obs_summary_list_obj = obs_summ_1000_avg)

#Histogram of distances
par(mfrow = c(1,1))
dists_prior_top <- head(sort(dists_prior_sample),500)
hist(dists_prior_top, main = "dist - ABC prior sampling", breaks = 100)

#Histograms of the distances
post_dists <- readRDS(file = "Outputs/ABC/01A_prelim_trans_dummy_posterior_ABC_first_run.rds")
par(mfrow = c(2,2))
hist(dists_true_sample, main = "dist - true dummy parameter")
hist(dists_pp_sample, main = "dist - ABC posterior sampling")
hist(dists_prior_sample, main = "dist - ABC prior sampling", breaks = 100)
hist(post_dists[,ncol(post_dists)], breaks = 100, main = "ABC particle distance recorded")

























################################################
################################################
# Generate new observed data
################################################
source('Code_abhi/00-ABC_required_functions.R')
source('Code_abhi/02B-Model_development_Collapse.R')
source('Code_abhi/000-load_model_inputs.R')
library(igraph)
library(data.table)

x_true <- c(0.237, 0.233, 0.066, 0.464, 0.751) #<- These are the 'true' chosen vals simulating it.

n_agents = 10000
n_links = 10
social_network <- sample_k_regular(n_agents, n_links, directed = FALSE, multiple = FALSE)
social_network_adjacency <- get.adjacency(social_network)


misc_args <- list(obs_summary = NULL,
                  obs_output = NULL,
                  number_of_agents = n_agents,
                  number_of_links = n_links,
                  adjacency_matrix = social_network_adjacency,
                  datasets = load_model_input_data())
system.time({
  obs_output <- abm_pv_model_v2(x_true[1:4],
                                x_true[5],
                                misc_args$number_of_agents,
                                misc_args$number_of_links,
                                misc_args$adjacency_matrix,
                                misc_args$datasets)
})

obs_output <- abm_pv_model_v2(x_true[1:4],
                              x_true[5],
                              misc_args$number_of_agents,
                              misc_args$number_of_links,
                              misc_args$adjacency_matrix,
                              misc_args$datasets)

obs_summ <- simulate_summary(obs_output, obs_output)
plot(1:length(obs_summ), obs_summ)

################################################
# Run ABC with larger agents this time - decision made due to wayy lower stochasticity present in the model at this point.
################################################
source('Code_abhi/00-ABC_required_functions.R')
source('Code_abhi/02B-Model_development_Collapse.R')
source('Code_abhi/000-load_model_inputs.R')

library(igraph)
library(data.table)
library(protoABC)
library(parallel)

#Generate inputs required to run model
n_links <- 10
n_agents <- 10000
a_net <- sample_k_regular(n_agents, n_links, directed = FALSE, multiple = FALSE)
adj_net  <- get.adjacency(a_net)
obs_summ <- readRDS(file = "Data/large_agents_obs_summ_data.rds")
obs_output <- readRDS(file = "Data/large_agents_obs_output_data.rds")
model_inputs <- load_model_input_data()

misc_args <- list(obs_summary = obs_summ,
                  obs_output = obs_output,
                  number_of_agents = n_agents,
                  number_of_links = n_links,
                  adj = adj_net,
                  datasets = model_inputs)

#Generate parallel cluster
parallel::stopCluster(cl)
cl <- parallel::makeCluster(10, outfile = "debug.txt")

#Set appropriate data.table settings for parallel processing
getDTthreads(verbose=TRUE)
setDTthreads(1)

#Distribute required data to parallel cores
clusterEvalQ(cl, {
  source('Code_abhi/00-ABC_required_functions.R')
  source('Code_abhi/02B-Model_development_Collapse.R')
  library(data.table)
  gc(verbose = FALSE, reset = FALSE, full = TRUE)
})


dist_abm_pv_trans(theta = prior_transform(), misc_args = misc_args)

abc_output <- abc_start(
  prior = prior_transform,
  method = "RABC",
  distance = dist_abm_pv_trans,
  distance_args = misc_args,
  cl = cl,
  control = list(n = 1000, 
                 pacc_final = 0.01,
                 prior_eval = prior_eval_transform,
                 n_param = 5),
  output_control = list(
    include_dist = TRUE, 
    print_output = TRUE
  ))

saveRDS(abc_output, file = "Outputs/ABC/01B_Simple_Summary_Output.Rds")
stopCluster(cl)



































#############################
# Analyse Results from Large Run
##############################

abc_post_dmy_lrg_ag <- readRDS(file = "Outputs/ABC/01A_prelim_trans_dummy_posterior_abc_lrg_ag.rds")

detransform_single_particle <- function(theta){
  min_unif = 0.5
  max_unif = 0.9
  # Detransform particle
  v_retr <- exp(theta[1:4])
  x <- v_retr/sum(v_retr)
  p_retr <- (min_unif*exp(-theta[5]) + max_unif)/(1 + exp(-theta[5]))
  detransed_particle <- c(x, p_retr, theta[6])
  return(detransed_particle)
}


detransform_all_particles <- function(theta_df){
  apply(theta_df, MARGIN = 1, function(theta_df){
    true_theta <- detransform_single_particle(theta_df)
    return(true_theta)
  })
}

detrans_particles_df <- detransform_all_particles(abc_post_dmy_lrg_ag) %>% 
  t()
colnames(detrans_particles_df) <- c("w1", "w2", "w3", "w4","t", "dist")

saveRDS(detrans_particles_df, file = "Outputs/ABC/posterior_detrans_ABC_collapse_gc_test.rds")

detrans_particles_df <- readRDS(file = "Outputs/ABC/posterior_detrans_ABC_collapse_gc_test.rds")

# Plot Posteriors
par(mfrow=c(2,3))
plot(density(detrans_particles_df[,1]))
abline(v=x_true[1], col="blue")
plot(density(detrans_particles_df[,2]))
abline(v=x_true[2], col="blue")
plot(density(detrans_particles_df[,3]))
abline(v=x_true[3], col="blue")
plot(density(detrans_particles_df[,4]))
abline(v=x_true[4], col="blue")
plot(density(detrans_particles_df[,5]))
abline(v=x_true[5], col="blue")


# Histograms of the distances
par(mfrow = c(1,2))
hist(dists_true_sample, main = "dist - true dummy parameter")
hist(detrans_particles_df[,ncol(post_dists)], main = "ABC particle distance recorded")

pairs(detrans_particles_df[,1:5], pch = 21)


##########
# MODEL VALIDATION AND ABC POSTERIOR EVALUATION + ANALYSIS
##########
library(ggfan)
cl <- parallel::makeCluster(10)
clusterEvalQ(cl, {
  source('01-required_functions.R')
  source('02-run_functions.R')
  source('Code_abhi/00-ABC_required_functions.R')
  source('Code_abhi/02B-Model_development_Collapse.R')
  source('Code_abhi/01C-ABC_transform_sample_algorithms.R')
  source('Code_abhi/000-load_model_inputs.R')
  
  
  library(igraph)
  library(data.table)
  
  x_true <- c(0.237, 0.233, 0.066, 0.464, 0.751) #<- These are the 'true' chosen vals simulating it.
  detrans_particles_df <- readRDS(file = "Outputs/ABC/posterior_detrans_ABC_collapse_gc_test.rds")
  
  load_data()
  n_links <- 10
  n_agents <- 10000
  a_net <- sample_k_regular(n_agents, n_links, directed = FALSE, multiple = FALSE)
  adj_net  <- get.adjacency(a_net)
  misc_args <- list(obs_summ = NULL,
                    number_of_agents = n_agents,
                    adj = adj_net)
  
  gc(verbose = FALSE, reset = FALSE, full = TRUE)
})


x_true <- c(0.237, 0.233, 0.066, 0.464, 0.751) #<- These are the 'true' chosen vals simulating it.
n_links <- 10
n_agents <- 10000
a_net <- sample_k_regular(n_agents, n_links, directed = FALSE, multiple = FALSE)
adj_net  <- get.adjacency(a_net)

# obs_summ_lrg_ag <- readRDS(file = "Data/large_agents_obs_summ_data.rds")
misc_args <- list(obs_summ = NULL,
                  number_of_agents = n_agents,
                  adj = adj_net)

system.time({
  obs_data_ag_faster <- abm_pv_model_collapse(x_true[1:4], x_true[5], misc_args$number_of_agents, misc_args$adj)
  obs_data_summ_faster <- simulate_summary(obs_data_ag_fast)
})

saveRDS(obs_data_ag_faster, file = "Data/post_sim_true_data_long.rds")
saveRDS(obs_data_summ_faster, file = "Data/post_sim_true_data_long_summ.rds")

obs <- readRDS(file = "Data/post_sim_true_data_long.rds")

#Does total install Capacity match?
tot_model_outputs <- parSapply(cl, 1:1000, function(X){
  abm_pv_model_collapse(x_true[1:4], x_true[5], number_of_agents = misc_args$number_of_agents, adj = misc_args$adj)$tot_inst_cap
})
plot_tot_model_outputs <- melt(tot_model_outputs)

observed_data <- as.data.frame(cbind(tot = obs$tot))
tot_capacity <- ggplot()+
  geom_fan(data = plot_tot_model_outputs, aes(x = Var1, y = value))+
  geom_line(data = observed_data, aes(x = 1:82, y = tot), color = "blue", size=1)+
  stat_sample(data = plot_tot_model_outputs, aes(x = Var1, y = value, group=Var2), n_samples=1000, size=0.2, alpha=0.4)+
  theme_bw() + scale_fill_gradient(low="red", high="pink")+
  labs(title = "Total Capacity Simulations vs Dummy Observed data", x = "Time", y = "Total Capacity")
tot_capacity
ggsave(filename = "Diagrams/Graphs/faster_model_validation_tot_capacity.png", tot_capacity)
#Yes it does!

#Does average install capacity match?
avg_model_outputs <- parSapply(cl, 1:1000, function(X){
  abm_pv_model_collapse(x_true[1:4], x_true[5], number_of_agents = misc_args$number_of_agents, adj = misc_args$adj)$avg_inst_cap
})
plot_avg_model_outputs <- melt(avg_model_outputs)

observed_data <- as.data.frame(cbind(avg = obs$avg))
avg_capacity <- ggplot()+
  geom_fan(data = plot_avg_model_outputs, aes(x = Var1, y = value))+
  geom_line(data = observed_data, aes(x = 1:82, y = avg), color = "blue", size=1)+
  stat_sample(data = plot_avg_model_outputs, aes(x = Var1, y = value, group=Var2), n_samples=7, size=0.2, alpha=0.4)+
  theme_bw() + scale_fill_gradient(low="red", high="pink")+
  labs(title = "Average Capacity Simulations vs Dummy Observed data", x = "Time", y = "Average Capacity")
avg_capacity
ggsave(filename = "Diagrams/Graphs/faster_model_validation_avg_capacity.png", avg_capacity)

#Yes it does!

#posterior predictive 1 sim per particle

#Posterior Predictive - 1000 simulations - 1 simulation per particle, 10000 agents

pp_model_outputs <- parSapply(cl, 1:1000, function(X){
  # detrans_particles_df[X,6]
  abm_pv_model_collapse(detrans_particles_df[X, 1:4], detrans_particles_df[X, 5], number_of_agents = misc_args$number_of_agents, adj = misc_args$adj)$tot_inst_cap
})
plot_pp_model_outputs <- melt(pp_model_outputs)

observed_data <- as.data.frame(cbind(tot = obs$tot))
tot_pp_capacity <- ggplot()+
  # geom_fan(data = plot_pp_model_outputs, aes(x = Var1, y = value))+
  stat_sample(data = plot_pp_model_outputs, aes(x = Var1, y = value, group=Var2), n_samples=1000, size=0.2, alpha=0.4)+
  geom_line(data = observed_data, aes(x = 1:82, y = tot), color = "blue", size=1)+
  theme_bw() + scale_fill_gradient(low="yellow", high="lightyellow")+
  labs(title = "Posterior Predictive", x = "Time", y = "Total Capacity")
tot_pp_capacity
ggsave(filename = "Diagrams/Graphs/faster_model_validation_tot_pp_capacity.png", tot_pp_capacity)

pp_avg_model_outputs <- parSapply(cl, 1:1000, function(X){
  abm_pv_model_collapse(detrans_particles_df[X, 1:4], detrans_particles_df[X, 5] , number_of_agents = misc_args$number_of_agents, adj = misc_args$adj)$avg
})
plot_pp_avg_model_outputs <- melt(pp_avg_model_outputs)

observed_data <- as.data.frame(cbind(avg = obs$avg))
avg_pp_capacity <- ggplot()+
  geom_fan(data = plot_pp_avg_model_outputs, aes(x = Var1, y = value))+
  geom_line(data = observed_data, aes(x = 1:82, y = avg), color = "blue", size=1)+
  stat_sample(data = plot_pp_avg_model_outputs, aes(x = Var1, y = value, group=Var2), n_samples=200, size=0.2, alpha=0.4)+
  theme_bw() + scale_fill_gradient(low="yellow", high="lightyellow")+
  labs(title = "Posterior Predictive", x = "Time", y = "Avg Capacity")
avg_pp_capacity
# ggsave(filename = "Diagrams/Graphs/faster_model_validation_avg_pp_capacity.png", avg_pp_capacity)

