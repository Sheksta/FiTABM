source('01-required_functions.R')
source('02-run_functions.R')
load_data()

library(EasyABC)
library(TSrepr)
library(DirichletReg)


# Set up
#Create a wrapper to receive a set of parameters and return summary statistics
wrapper_run_model <- function(x){
  set.seed(x[1])# so that each core is initialized with a different seed value.
  
  if(!exists("init")){
    source('01-required_functions.R')
    source('02-run_functions.R')
    load_data()
    init <- T
  }
  
  w <- c(0.1,0.2,0.3,0.4)
  threshold <- x[2]
  rn <- 1
  number_of_agents <- 500
  all_res_rn <- run_model(number_of_agents, rn, w, threshold) # run the model once
  
  sim_summary <- TSrepr::repr_paa(all_res_rn[[1]][["tot_inst_cap"]], q = 10, func = mean)
  
  return(sim_summary)
}



# Toy Model Testing
toy_prior <- list(c("unif",0,1),c("normal",1,2))

toy_model <- function(x){
  set.seed(x[1])
  c( x[2] + x[3] + rnorm(1,0,0.1) , x[2] * x[3] + rnorm(1,0,0.1) )
}

x_true <- c(12451346,1,2)
sum_stat_obs <- toy_model(x_true)

n = 600
pacc = 0.01
ABC_Results <- ABC_sequential(method="Lenormand", model=toy_model, 
                              prior=toy_prior, 
                              nb_simul=n, 
                              summary_stat_target=sum_stat_obs, 
                              p_acc_min=pacc,
                              verbose = T, 
                              n_cluster = 6, 
                              use_seed = T,
                              progress_bar = T)
#Plot results
ABC_Results$param %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_density(aes(x = V2)) +
  geom_density(aes(x = V1))

# The results clearly demonstrate that the ABC scheme is able to successfully 
# estimate the parameters of the distribution

#Now test with " Dummy " observed data for the ABM. First with constraints on parameters



# Dummy Data Testing

#Declare model
wrapper_run_model <- function(x){
  set.seed(x[1])# so that each core is initialized with a different seed value.
  
  #Set up core-workspace on init.
  if(!exists("init")){
    source('01-required_functions.R')
    source('02-run_functions.R')
    load_data()
    init <- T
  }
  # Set parameters
  w <- x[2:5]
  threshold <- x[6]
  rn <- 1
  
  number_of_agents <- 500
  
  all_res_rn <- run_model(number_of_agents, rn, w, threshold) # run the model once
  
  sim_summary <- TSrepr::repr_paa(all_res_rn[[1]][["tot_inst_cap"]], q = 10, func = mean)
  
  return(sim_summary)
}
x_true <- c(1551871191,0.237,0.233,0.066,0.464,0.751)
sum_stat_obs <- wrapper_run_model(x_true)


plot(1:length(sum_stat_obs), sum_stat_obs)

#Set priors
dummy_prior <- list(list(c("rdirichlet",1, c(1, 1, 1, 1)), c("ddirichlet", c(1, 1, 1, 1))), 
                    c("unif",0,1), c("unif",0,1),c("unif",0,1),c("unif",0,1))


#Set estimation parameters
n = 10
target_tol = 200
ABC_Results <- ABC_sequential(method="Drovandi", model=wrapper_run_model, 
                              prior=dummy_prior, 
                              nb_simul=n, 
                              summary_stat_target=sum_stat_obs, tolerance_tab = target_tol,
                              verbose = T, 
                              n_cluster = 10, 
                              use_seed = T,
                              progress_bar = T, 
                              prior_test="(X1 + X2 + X3 + X4) == 1")


 #Plot results
ABC_Results$param %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_density(aes(x = V2)) +
  geom_density(aes(x = V1))




















#Priors - just uniform for now. - Update to 
thresh_prior=list(c("unif",0,1))





obs_summary <- repr_paa(deployment$real_cap, q = 10, func = mean)

n = 1000
pacc = 0.05
# Note: defaults: alpha = 0.5, c = 0.01

ABC_Results <- ABC_sequential(method="Lenormand", model=wrapper_run_model, 
                              prior=toy_prior, 
                              nb_simul=n, 
                              summary_stat_target=obs_summary, 
                              p_acc_min=pacc,
                              verbose = T, n_cluster = 6, use_seed = T)

# sim_summary_stat <- wrapper_run_model(x = c(0.1,0.2,0.3,0.4,0.7)) #(How to run model)



## Plot original cum install vs actual
cbind.data.frame(ts = testobj[[1]][["time_series"]], cum_inst = testobj[[1]][["tot_inst_cap"]]) %>% 
  ggplot() + 
  geom_line(aes(x = ts, y = cum_inst))+
  geom_line(data = deployment, aes(x = time_series, y = real_cap))+
  scale_color_brewer(palette = "Dark2")

## Demonstration of time series dimensionality reduction for summaries
time <- 1:length(data_paa)
cbind.data.frame(time, data_paa, data_paa_obs) %>% 
  ggplot()+
  geom_line(aes(x = time, y = data_paa))+
  geom_line(aes(x = time, y = data_paa_obs))







#Dirichlet distribution
