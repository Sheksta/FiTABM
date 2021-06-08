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


##############################################################
# Dummy Data Testing
##############################################################

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
rdirichlet(1, c(1, 1, 1, 1))

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
                              progress_bar = T)


 #Plot results
ABC_Results$param %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_density(aes(x = V2)) +
  geom_density(aes(x = V1))


##############################################################
# The following code tries to fix compatibility issues with enabling a dirichlet
# multivariate distribution (or any multivariate distribution) on easyABC.
##############################################################


#UNCHANGED!!!
.wrap_constants_in_model = function(prior, model, use_seed) {
  nb_parameters = length(prior)
  # detecting constants defined as c('unif',value,value)
  constants_mask = array(FALSE, nb_parameters)
  constants_values = list()
  for (i in 1:nb_parameters) {
    if ((prior[[i]][1] == "unif") && (as.numeric(prior[[i]][2]) == as.numeric(prior[[i]][3]))) {
      constants_mask[i] = TRUE
      constants_values = append(constants_values, as.numeric(prior[[i]][2]))
    }
  }
  constants_values = unlist(constants_values)
  new_prior = prior[!constants_mask]
  if (use_seed) {
    constants_mask = c(FALSE, constants_mask)
    nb_parameters = nb_parameters + 1
  }
  old_model = model
  # returning the prior without constants, and a new function that wraps the model
  # with the constants
  list(new_prior = new_prior, new_model = function(parameters) {
    param_with_constants = array(0, nb_parameters)
    param_with_constants[constants_mask] = constants_values
    param_with_constants[!constants_mask] = parameters
    old_model(param_with_constants)
  })
}


#UNCHANGED!!!
.is_included <- function(res, prior) {
  test = TRUE
  for (i in 1:length(prior)) {
    test = test && (prior[[i]]$density(res[i]) > 0)
  }
  test
}

#UNCHANGED!!!
.process_prior = function(prior) {
  if (!is.list(prior)) 
    stop("'prior' has to be a list")
  l = length(prior)
  new_prior = list()
  for (i in 1:l) {
    if (is.list(prior[[i]][1])) {
      if (length(prior[[i]]) != 2) {
        stop(paste("Incorrect prior specification for '", prior[[i]], "'. Please refer to the documentation.", 
                   sep = ""))
      }
      new_prior[[i]] = .create_dynamic_prior(prior[[i]][[1]][1], prior[[i]][[1]][-1], 
                                             prior[[i]][[2]][1], prior[[i]][[2]][-1])
    } else {
      if (any(prior[[i]][1] == c("unif", "normal", "lognormal", "exponential"))) {
        if (prior[[i]][1] == "exponential") {
          if (length(prior[[i]]) != 2) {
            stop(paste("Incomplete prior information for parameter ", i, 
                       sep = ""))
          }
        } else {
          if (length(prior[[i]]) != 3) {
            stop(paste("Incomplete prior information for parameter ", i, 
                       sep = ""))
          }
        }
        new_prior[[i]] = .create_legacy_prior(prior[[i]][1], prior[[i]][-1])
      } else {
        stop(paste("Only the methods 'unif', 'normal, 'lognormal' and 'exponential' are wrapped in EasyABC, unknown function '", 
                   prior[[i]][1], "'. Please refer to the documention for specifying your prior.", 
                   sep = ""))
      }
    }
  }
  new_prior
}


#DONE!!!
.create_dynamic_prior = function(sampleName, sampleArgs, densityName, densityArgs, 
                                 isUniform = FALSE) {
  # $sampling: sample function with the given arguments. The used function (given
  # by 'name' argument) takes the arguments 'args' for generating a sample.
  # $density: create the density function with the given arguments. The used
  # function (given by 'name' argument) takes as arguments the quantile value and
  # the given 'args' for computing the density.  $isUniform: indicates if this
  # prior follows an uniform distribution (condition for methods that need LHS)
  if(grepl("dirichlet", sampleName, fixed = TRUE)){
    list(sampling = function() {
      do.call(sampleName, list(as.numeric(sampleArgs[1]),as.vector(as.numeric(sampleArgs[2:length(sampleArgs)]))))
    }, density = function(value) {
      do.call(densityName, list(t(as.matrix(value)),as.vector(as.numeric(sampleArgs[1:length(densityArgs)]))))
    }, isUniform = isUniform || sampleName == "runif", sampleArgs = sampleArgs)
  }else{
    list(sampling = function() {
      do.call(sampleName, as.list(as.numeric(sampleArgs)))
    }, density = function(value) {
      do.call(densityName, as.list(as.numeric(c(value, densityArgs))))
    }, isUniform = isUniform || sampleName == "runif", sampleArgs = sampleArgs)
  }
}

#UNCHANGED!!!
.create_legacy_prior = function(name, args) {
  name = name
  args = args
  switch(
    EXPR = name,
    unif = .create_dynamic_prior("runif", c(1, args), "dunif", args, TRUE),
    normal = .create_dynamic_prior("rnorm", c(1, args), "dnorm", args, TRUE),
    lognormal = .create_dynamic_prior("rlnorm", c(1, args), "dlnorm",
                                      args, TRUE),
    exponential = .create_dynamic_prior("rexp", c(1, args), "dexp",
                                        args, TRUE)
  )
}
#UNCHANGED!!!
.is_in_parameter_constraints <- function(parameter, test) {
  is.null(test) || eval(parse(text = gsub("[xX]([0-9]+)", "parameter[\\1]", test)))
}

#DONE!!! 
#TODO: Density function to be changed as well.
.sample_prior <- function(prior, test) {
  test = NULL #Already defined somewhere I think
  l = length(hello)
  param = c()
  test_passed = FALSE
  while (!test_passed) {
    for (i in 1:l) {
      param = c(param, prior[[i]]$sampling())
    }
    test_passed = .is_in_parameter_constraints(param, test)
  }
  param
}



dummy_prior <- list(list(c("rdirichlet",1, c(1, 1, 1, 1)), c("ddirichlet", c(1, 1, 1, 1))), 
                    c("unif",0,1))
hello <- .process_prior(dummy_prior)
.sample_prior(hello)


##############################################################
#Can only proceed when the ABC functions are compatible with the dirichlet
#multivariate distribution priors
##############################################################

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
