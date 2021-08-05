source('Code_abhi/00-ABC_required_functions.R')
source('01-required_functions.R')
source('02-run_functions.R')
load_data()


#Generate summary statistic of observed data.
real_obs <- c()
real_obs$tot_inst_cap <- deployment$real_cap
real_obs$avg_inst_cap <- deployment$avg_cap
real_obs_summ <- simulate_summary(real_obs)

library(TSrepr)
library(DirichletReg)
library(protoABC)#devtools::install_github("AnthonyEbert/protoABC")

##################################################################
# DUMMY with PROTO ABC
##################################################################

library(parallel)
cl <- makeCluster(10)

clusterEvalQ(cl, {
  source('01-required_functions.R')
  source('02-run_functions.R')
  source('Code_abhi/00-ABC_required_functions.R')
  load_data()
})

wrapper_run_model <- function(x){
  # Set parameters
  w <- x[1:4]
  threshold <- x[5]
  
  number_of_agents <- 7000
  
  all_res_rn <- run_model_abc(number_of_agents, w, threshold) # run the model once
  
  sim_summary <- simulate_summary(all_res_rn)
  return(sim_summary)
}

wrapper_model_dist <- function(theta, obs_summ){
  # Set parameters
  w <- theta[1:4]
  threshold <- theta[5]
  number_of_agents <- 7000
  
  #Simulate model
  all_res_rn <- run_model_abc(number_of_agents, w, threshold) # run the model once
  
  #Summarise cumulative uptake from model simulation
  sim_summary <- simulate_summary(all_res_rn)
  
  #Evaluate distance from observed data
  output <- distance_calc_abc(sim_summary, obs_summ)
  return(output)
}


#Generate summary statistic of observed data.
real_obs <- c()
real_obs$tot_inst_cap <- deployment$real_cap
real_obs$avg_inst_cap <- deployment$avg_cap
real_obs_summ <- simulate_summary(real_obs)

#Declare prior
prior <- function(n = 1){
  theta <- cbind( rdirichlet(n, c(1.5, 1, 1.3, 2)) , runif(n,0.5,1) ) %>% 
    data.frame()
  return(theta)
}

#Plot priors
prior(100000) %>% 
  gather() %>% 
  ggplot()+
  geom_density(aes(x = value, fill = key), alpha = 0.1)
  

#Check prior evaluation
prior_eval <-  function(theta){
  vals <- theta %>% 
    as.matrix()
  return(
    ifelse((sum(vals[1:4]) == 1) && (vals[5] > 0) && (vals[5] < 1), 
           TRUE,
           FALSE)
  )
}

#ABC-Runner
abc_post <- abc_start(
  dummy_prior,
  method = "RABC",
  distance = wrapper_model_dist,
  distance_args = obs_summ,
  cl = cl,
  control = list(n = 1000, 
                 pacc_final = 0.05,
                 prior_eval = dummy_prior_eval))

saveRDS(abc_post, file = "Outputs/ABC/01B_prelim_true_posterior_ABC.rds")

par(mfrow=c(2,3))
plot(density(abc_post$X1))
abline(v=x_true[1], col="blue")
plot(density(abc_post$X2))
abline(v=x_true[2], col="blue")
plot(density(abc_post$X3))
abline(v=x_true[3], col="blue")
plot(density(abc_post$X4))
abline(v=x_true[4], col="blue")
plot(density(abc_post$X5))
abline(v=x_true[5], col="blue")