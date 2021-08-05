source('Code_abhi/00-ABC_required_functions.R')
source('Code_abhi/00A-ABC_functions.R')
source('Code_abhi/02-Model_development.R')
source('01-required_functions.R')
source('02-run_functions.R')

load_data()

library(igraph)
library(data.table)
library(TSrepr)
library(protoABC)
library(parallel)


##################################################################
# TOY with PROTO ABC
##################################################################

library(parallel)
cl <- makeCluster(8)

# clusterEvalQ(cl, {
  # source('01-required_functions.R') IGNORE THIS
  # source('02-run_functions.R')
  # load_data()
  # 
  
  #Declare model in core
  toy_model <- function(x){
    c( x[1] + x[2] + rnorm(1,0,0.1) , x[1] * x[2] + rnorm(1,0,0.1) )
  }


toy_model <- function(x){
  c( x[1] + x[2] + rnorm(1,0,0.1) , x[1] * x[2] + rnorm(1,0,0.1) )
}

wrapper_toy_dist <- function(theta, obs_summ){
  # Set parameters

  
  #Simulate model
  sim_summary <- toy_model(theta) 
  
  #Evaluate distance from observed data
  output <- dist(rbind(sim_summary, obs_summ))
  return(output)
}

#Toy simulate and summary
x_true <- c(0.5,1.5)
sum_stat_obs <- c(x_true[1]+x_true[2],x_true[1]*x_true[2])#toy_model(x_true)
sum_stat_obs

#Declare prior
toy_prior <- function(n = 1){
  theta <- cbind( runif(n,0,1), runif(n,1,2) ) %>% 
    as.data.frame()
  return(theta)
}

#Check prior evaluation
toy_prior_eval = function(theta){
  return(
    ifelse(dunif(theta[1],0,1)*dunif(theta[2],1,2) == 0, 
           0,
           1)
      )
}

abc_post_toy <- abc_start(
  toy_prior,
  method = "RABC",
  distance = wrapper_toy_dist,
  distance_args = sum_stat_obs,
  cl = cl,
  control = list(n = 1000, 
                 pacc_final = 0.001,
                 prior_eval = toy_prior_eval))#function(x, y) {     return(1) }))

par(mfrow=c(1,2))
plot(density(abc_post_toy$V1))
abline(v=x_true[1], col="blue")
plot(density(abc_post_toy$V2))
abline(v=x_true[2], col="blue")

##################################################################
# DUMMY with PROTO ABC
##################################################################

# clusterEvalQ(cl, {
#   source('01-required_functions.R')
#   source('02-run_functions.R')
#   source('Code_abhi/00-ABC_required_functions.R')
#   load_data()
# })

wrapper_run_model <- function(x){
  # Set parameters
  w <- x[1:4]
  threshold <- x[5]
  
  number_of_agents <- 500
  
  all_res_rn <- run_model_abc(number_of_agents, w, threshold) # run the model once
  
  sim_summary <- simulate_summary(all_res_rn)
  return(sim_summary)
}

wrapper_model_dist <- function(theta, obs_summ){
  # Set parameters
  w <- theta[1:4]
  threshold <- theta[5]
  number_of_agents <- 500
  
  #Simulate model
  all_res_rn <- run_model_abc(number_of_agents, w, threshold) # run the model once
  
  #Summarise cumulative uptake from model simulation
  sim_summary <- simulate_summary(all_res_rn)
  
  #Evaluate distance from observed data
  output <- distance_calc_abc(sim_summary, obs_summ)
  return(output)
}

#Dummy simulate and summary
x_true <- c(0.237,0.233,0.066,0.464,0.751)
obs_summ <- wrapper_run_model(x_true)

#Declare prior
dummy_prior <- function(n = 1){
  theta <- cbind( rdirichlet(n, c(1, 1, 1, 1)) , runif(n,0.5,1) ) %>% 
    data.frame()
  return(theta)
}


#Check prior evaluation
dummy_prior_eval <-  function(theta){
  vals <- theta %>% 
    as.matrix()
  return(
    ifelse((sum(vals[1:4]) == 1) && (vals[5] > 0) && (vals[5] < 1), 
           TRUE,
           FALSE)
  )
}


#ABC-Runner
abc_post_dummy <- abc_start(
  dummy_prior,
  method = "RABC",
  distance = wrapper_model_dist,
  distance_args = obs_summ,
  cl = cl,
  control = list(n = 100, 
                 pacc_final = 0.2,
                 prior_eval = dummy_prior_eval))
# saveRDS(abc_post_dummy, file = "Outputs/ABC/01A_prelim_dummy_posterior_ABC.rds")

stopCluster(cl)

par(mfrow=c(2,3))
plot(density(abc_post_dummy$X1))
abline(v=x_true[1], col="blue")
plot(density(abc_post_dummy$X2))
abline(v=x_true[2], col="blue")
plot(density(abc_post_dummy$X3))
abline(v=x_true[3], col="blue")
plot(density(abc_post_dummy$X4))
abline(v=x_true[4], col="blue")
plot(density(abc_post_dummy$X5))
abline(v=x_true[5], col="blue")


##################################################################
# POSTERIOR PREDICTIVE ANALYSIS
##################################################################
cl <- parallel::makeCluster(8)
doParallel::registerDoParallel(cl)

clusterEvalQ(cl, {
  source('01-required_functions.R')
  source('02-run_functions.R')
  source('Code_abhi/00-ABC_required_functions.R')
  load_data()
})

abc_post_dummy <- readRDS("Outputs/ABC/01A_prelim_dummy_posterior_ABC.rds")
number_of_agents <- 1000
number_of_runs <- 1000
# posterior_samples <- head(abc_post_dummy,10)
sample_for_run <- abc_post_dummy#posterior_samples[sample(1:nrow(posterior_samples), number_of_runs, replace = TRUE), ]

foreach_wrapper_sim <- function() {
  all_res <- foreach(i1 = 1:number_of_runs, .combine='c', .export = ls(globalenv())) %dopar% {
    w <- unlist(sample_for_run[i1, 1:4])
    threshold <- unlist(sample_for_run[i1, 5])

    
    res <- run_model(number_of_agents, i1, w, threshold) # run the model once
    return(list(res))
 }
}
all_res_big <- foreach_wrapper_sim()


x_true <- c(0.237,0.233,0.066,0.464,0.751)
obs_summ <- run_model_abc(1000, x_true[1:4],x_true[5])


initialise_vars()
for(i in 1:number_of_runs){
  append_results(all_res_big[[i]])
}
summarise_results(avg_u, cost, cost_priv) # calculate averages of all runs

# Now do it for observed data 'dummy'
foreach_wrapper_obs <- function() {
  all_res <- foreach(i1 = 1:number_of_runs, .combine='c', .export = ls(globalenv())) %dopar% {
    w <- unlist(x_true[1:4])
    threshold <- unlist(x_true[5])
    
    
    res <- run_model(number_of_agents, i1, w, threshold) # run the model once
    return(list(res))
  }
}
obs_aggregate <- foreach_wrapper_obs()

initialise_obs_vars()
for(i in 1:number_of_runs){
  append_results_obs(obs_aggregate[[i]])
}
summarise_results_obs(avg_u_obs, cost_obs, cost_priv_obs) # calculate averages of all runs
averages_obs$avg_inst_cap[is.nan(averages_obs$avg_inst_cap)] <- 0
averages_obs_data <- list(time_series = averages_obs$time_series,
                     tot_inst_cap = averages_obs$tot_inst_cap, 
                     avg_inst_cap =  averages_obs$avg_inst_cap)
averages_obs_summ <- simulate_summary(averages_obs_data)
saveRDS(averages_obs_data, file = "Outputs/ABC/01A_prelim_dummy_obs_1000sim_avg_ABC.rds")
saveRDS(averages_obs_summ, file = "Outputs/ABC/01A_prelim_dummy_obs_summ_1000sim_avg_ABC.rds")

sum_abs_diff <- sum(abs(averages$inst_cap_diff)) # deviation from real data: sum of absolute values
# of deviation from installed FiT capacity < 10 kW

overall_tot_cost <- tot_cost_priv + tot_cost # total expenditure; used for calcul
overall_tot_cost_mean <- mean(overall_tot_cost)/1e9 # in billions
overall_tot_cost_sd <- sd(overall_tot_cost)/1e9 # in billions

prod_res <- calc_prod(LCOE_data, number_of_runs) # calculate total production from all installations at each date
cum_prod <<- prod_res[[1]]
cum_prod_avg <<- prod_res[[2]]

tot_p1 <- ggplot() + theme_bw() + 
        geom_line(data = averages_obs, aes(x = time_series, y = tot_inst_cap), color = "blue", size = 1) +
        geom_line(data = avg_u, aes(x = time_series, 
                                    y = tot_inst_cap, group = run_number), alpha = 0.2)+
        geom_line(data = averages, aes(x = time_series, y = tot_inst_cap), color = "black", size = 1)
tot_p1#+
        # annotate("text", x = dmy("01jul2011"), y = 2000, label = print_vars)
ggsave("Diagrams\\Figures\\dummy_testing\\tot_inst_cap_1000s_ag1000_rn20.svg", plot = tot_p1, device = "svg")

avg_p1 <- ggplot() + theme_bw() + 
  geom_line(data = averages_obs, aes(x = time_series, y = avg_inst_cap), color = "blue", size = 1) +
  geom_line(data = avg_u, aes(x = time_series, 
                              y = avg_inst_cap, group = run_number), alpha = 0.2)+
  geom_line(data = averages, aes(x = time_series, y = avg_inst_cap), color = "black", size = 1) #+
avg_p1
ggsave("Diagrams\\Figures\\dummy_testing\\avg_inst_cap_1000s_ag1000_rn20.svg", plot = avg_p1, device = "svg")
stopCluster(cl)


stoch_p1 <- ggplot() + theme_bw() + 
  geom_line(data = averages_obs, aes(x = time_series, y = tot_inst_cap), color = "blue", size = 1) +
  geom_line(data = avg_u_obs, aes(x = time_series, 
                              y = tot_inst_cap, group = run_number), color = "blue", alpha = 0.02) +
  labs(title = "stochasticity of model - 1000 sims, dummy 'true' params")
stoch_p1

stoch_p1_avg <- ggplot() + theme_bw() + 
  geom_line(data = averages_obs, aes(x = time_series, y = avg_inst_cap), color = "blue", size = 1) +
  geom_line(data = avg_u_obs, aes(x = time_series, 
                                  y = avg_inst_cap, group = run_number), color = "blue", alpha = 0.02) +
  labs(title = "stochasticity of model - 1000 sims, dummy 'true' params - avg install cap")
stoch_p1_avg


#######################################################################
# DIRICHLET PRIOR VS POSTERIOR DEMO
#######################################################################
prior_samp <- c()
prior_samp <- rdirichlet(10000, c(1,1,1,1))
prior_samp <- cbind(prior_samp, runif(10000, min = 0.5, max = 1)) %>% as.data.frame()

par(mfrow=c(2,3))
plot(density(prior_samp$V1), col = 2)
lines(density(abc_post_dummy$X1))
abline(v=x_true[1], col="blue")


plot(density(prior_samp$V2), col = 2)
lines(density(abc_post_dummy$X2))
abline(v=x_true[2], col="blue")


plot(density(prior_samp$V3), col = 2)
lines(density(abc_post_dummy$X3))
abline(v=x_true[3], col="blue")


plot(density(prior_samp$V4), col = 2)
lines(density(abc_post_dummy$X4))
abline(v=x_true[4], col="blue")


plot(density(prior_samp$V5), col = 2)
lines(density(abc_post_dummy$X5))
abline(v=x_true[5], col="blue")

##################################################################
# DUMMY with PROTO ABC WITH !!TRANSFORMS!!
##################################################################
cl <- parallel::makeCluster(11)
#Dummy simulate and summary (from 1000 simulations - just to minimise error from stochasticity and focus on calibration perf)
x_true <- c(0.237, 0.233, 0.066, 0.464, 0.751) #<- These are the 'true' chosen vals simulating it.
obs_summ_1000_avg <- readRDS(file = "Outputs/ABC/01A_prelim_dummy_obs_summ_1000sim_avg_ABC.rds")

#Declare model
wrapper_model_dist_transform <- function(theta, obs_summ){
  min_unif = 0.5
  max_unif = 1
  # Detransform particle
  v_retr <- exp(theta[1:4])
  x <- v_retr/sum(v_retr)
  p_retr <- (min_unif*exp(-theta[5]) + max_unif)/(1 + exp(-theta[5]))
  
  # Set parameters
  w <- as.numeric(x)
  threshold <- as.numeric(p_retr)
  number_of_agents <- 1000
  
  #Simulate model
  all_res_rn <- run_model_abc(number_of_agents, w, threshold) # run the model once
  
  #Summarise cumulative uptake from model simulation
  sim_summary <- simulate_summary(all_res_rn)
  
  #Evaluate distance from observed data
  dist_out <- distance_calc_abc(sim_summary, obs_summ)
  return(dist_out)
}

n_agents <- 1000
n_links <- 10
a_net <- sample_k_regular(n_agents, n_links, directed = FALSE, multiple = FALSE)
adj_net  <- get.adjacency(a_net)
misc_args <- list(obs_summ = obs_summ_1000_avg,
                number_of_agents = n_agents,
                adj = adj_net)

dist_abm_pv_trans <- function(theta, misc_args){
  min_unif = 0.5
  max_unif = 0.9
  # Detransform particle
  v_retr <- exp(theta[1:4])
  x <- v_retr/sum(v_retr)
  p_retr <- (min_unif*exp(-theta[5]) + max_unif)/(1 + exp(-theta[5]))
  
  # Set parameters
  w <- as.numeric(x)
  threshold <- as.numeric(p_retr)
  
  #Simulate model
  all_res_rn <- abm_pv_model(w = w, threshold = threshold, number_of_agents = misc_args$number_of_agents, adj = misc_args$adj) # run the model once
  
  
  #Summarise cumulative uptake from model simulation
  sim_summary <- simulate_summary(all_res_rn)
  
  #Evaluate distance from observed data
  dist_out <- distance_calc_abc(sim_summary, misc_args$obs_summ)
  return(dist_out)
}

#Declare prior
dummy_prior_transform <- function(n = 1){
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


#prior evaluation
dummy_prior_eval_transform <-  function(theta){
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

abc_post_dummy_trans <- abc_start(
  prior = dummy_prior_transform,
  method = "RABC",
  distance = wrapper_model_dist_transform,
  distance_args = obs_summ_1000_avg,
  cl = cl,
  control = list(n = 1000, 
                 pacc_final = 0.0001,
                 prior_eval = dummy_prior_eval_transform,
                 n_param = 5),
  output_control = list(
    include_dist = TRUE, 
    print_output = TRUE
  ))

saveRDS(abc_post_dummy_trans, file = "Outputs/ABC/01A_prelim_trans_dummy_posterior_ABC_first_run.rds")
stopCluster(cl)

par(mfrow=c(2,3))
plot(density(abc_post_dummy$X1))
abline(v=x_true[1], col="blue")
plot(density(abc_post_dummy$X2))
abline(v=x_true[2], col="blue")
plot(density(abc_post_dummy$X3))
abline(v=x_true[3], col="blue")
plot(density(abc_post_dummy$X4))
abline(v=x_true[4], col="blue")
plot(density(abc_post_dummy$X5))
abline(v=x_true[5], col="blue")



