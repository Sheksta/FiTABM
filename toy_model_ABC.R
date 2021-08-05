##################################################################
# TOY with PROTO ABC
##################################################################
library(parallel)
cl <- makeCluster(8)

clusterEvalQ(cl, {
  # source('01-required_functions.R') IGNORE THIS
  # source('02-run_functions.R')
  # load_data()
  # 
  
  
  #Declare model in core
  toy_model <- function(x){
    c( x[1] + x[2] + rnorm(1,0,0.1) , x[1] * x[2] + rnorm(1,0,0.1) )
  }
})


toy_model <- function(x){
  c( x[1] + x[2] + rnorm(1,0,0.1) , x[1] * x[2] + rnorm(1,0,0.1) )
}

wrapper_toy_dist <- function(theta, obs_summ){
  sim_summary <- toy_model(theta)
  
  #Evaluate distance from observed data
  output <- dist(rbind(sim_summary, obs_summ))
  return(output)
}

#Toy simulate and summary
x_true <- c(0.5,1.5)
sum_stat_obs <- toy_model(x_true)
sum_stat_obs

#Declare prior
toy_prior <- function(n = 1){
  theta <- cbind( runif(n,0,1), runif(n,1,2) ) %>% 
    as.data.frame()
  return(theta)
}
#Check if prior works
for(i in 1:20){
  print(paste(toy_prior()))
  }

abc_post_toy <- abc_start(
  toy_prior,
  method = "RABC",
  distance = wrapper_toy_dist,
  distance_args = sum_stat_obs,
  cl = cl,
  control = list(n = 1000, 
                 pacc_final = 0.01))

par(mfrow=c(1,2))
plot(density(abc_post_toy$V1))
abline(v=x_true[1], col="blue")
plot(density(abc_post_toy$V2))
abline(v=x_true[2], col="blue")