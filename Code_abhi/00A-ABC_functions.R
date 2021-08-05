
##################################################################
# ABC FUNCTIONS
##################################################################

abc_algorithm <- function(prior, distance, distance_args, algorithm, control, output_control, lfunc){
  UseMethod("abc_algorithm", algorithm)
}

# Rejection

abc_algorithm.rejection <- function(prior, distance, distance_args, algorithm, control, output_control, lfunc){
  
  control <- do.call("abc_control.rejection", control)
  output_control <- do.call("abc_output.rejection", output_control)
  
  param <- prior(1)
  
  n <- control$n
  
  
  dist_col <- dim(param)[2] + 1
  output <- matrix(ncol = dist_col, nrow = 0)
  
  if(is.null(control$epsilon)){
    # Calculate number of samples to run so as to keep n samples
    m <- ceiling(n / control$quantile_keep)
    
    new_output <- rejection_core(m, prior, distance, lfunc, distance_args = distance_args)
    ord <- order(new_output[, dist_col])
    output <- new_output[ord, ][1:n, ]
  } else {
    
    # I didn't indent this because I didn't want to mess up the git commit.
    while(dim(output)[1] < control$n){
      
      new_output <- rejection_core(n, prior, distance, lfunc, distance_args = distance_args)
      
      #new_output <- matrix(unlist(new_output), ncol = dist_col, byrow = TRUE)
      
      new_output <- new_output[which(new_output[, dist_col] <= control$epsilon),]
      
      output <- rbind(output, new_output)
      
      n <- max(1, as.integer((control$n - (dim(output)[1]))/(dim(new_output)[1] / n + control$delta)))
      
      #print(dim(output)[1])
    }
  }
  
  output <- as.data.frame(output)
  output <- output[sample.int(control$n), ]
  rownames(output) <- seq(length=nrow(output))
  
  names(output) <- c(names(param), "distance")
  
  if(output_control$include_dist){
    return(output)
  } else {
    return(output[, - dist_col, drop = FALSE])
  }
  
}

rejection_core <- function(n, prior, distance, lfunc, distance_args){
  param <- prior(n)
  
  new_output <- lfunc(as.matrix(param), 1, function(i, distance_args, distance) {
    
    out <- distance(i, distance_args)
    
    return(c(i, out))
    
  }, distance_args = distance_args, distance = distance)
  
  return(t(new_output))
}

# RABC



abc_algorithm.RABC <- function(prior, distance, distance_args, algorithm, control, output_control, lfunc){
  
  param <- prior(1)
  control$n_param <- dim(param)[2]
  control <- do.call("abc_control.RABC", control)
  output_control <- do.call("abc_output.RABC", output_control)
  
  dist_col <- dim(param)[2] + 1
  output <- matrix(ncol = dist_col, nrow = 0)
  
  trial_run <- rejection_core(control$n, prior, distance, lfunc, distance_args = distance_args)
  
  input_params_s <- trial_run[order(trial_run[,dist_col]), ]
  
  dist_next <- input_params_s[control$num_keep, dist_col]
  dist_max <- input_params_s[control$n, dist_col]
  
  R = control$R
  
  while(dist_max > control$eps_final){
    
    rw_cov <- control$cov_func(input_params_s[1:control$num_keep, -dist_col])
    
    iacc <- 0
    
    index_resample_f <- sample(control$num_keep, control$n - control$num_keep, replace = TRUE)
    
    output <- lfunc(
      input_params_s[index_resample_f, ],
      1,
      RABC_core,
      distance_args = distance_args,
      dist_next = dist_next,
      num_keep = control$num_keep,
      R = R,
      rw_cov = rw_cov,
      distance = distance,
      prior_eval = control$prior_eval
    )
    
    output <- t(output)
    
    input_params_s[(control$num_keep + 1):control$n, ] <- output[,-dim(output)[2]]
    iacc <- sum(output[,dim(output)[2]])
    
    p_acc <- iacc / (control$num_drop * R)
    R = floor(log(control$c1) / log(1 - p_acc) + 1)
    
    if (p_acc < control$pacc_final){
      break;
    }
    
    if ((dist_max - dist_next)/dist_max < control$d_eps_final){
      break;
    }
    
    # order the particles according to the distance
    input_params_s <- input_params_s[order(input_params_s[,dist_col]), ]
    
    dist_next <- input_params_s[control$num_keep, dist_col]
    dist_max <- input_params_s[control$n, dist_col]
    
    if(output_control$print_output){
      
      message("********************************");
      
      message("Acceptance prob of MCMC was ",p_acc);
      message("Number of MCMC moves for next iteration is ",R);
      message("Number of unique particles is ", length(unique(input_params_s[,1])))
      
      message("dist_max is ",dist_max);
      message("dist_next is ",dist_next);
    }
    
    
  }
  
  if(output_control$include_dist){
    output <- as.data.frame(input_params_s)
    names(output) <- c(names(param), "distance")
  } else {
    output <- as.data.frame(input_params_s[,-dist_col])
    names(output) <- names(param)
  }
  
  return(output)
}

RABC_core <- function(
  input_params_s,
  distance_args,
  dist_next,
  num_keep,
  R,
  rw_cov,
  distance,
  prior_eval) {
  
  # resample from the particle population
  param_names <- names(input_params_s)
  
  
  input_params_s <- as.numeric(input_params_s)
  
  iacc <- 0
  input_params <- input_params_s[-length(input_params_s)]
  input_s      <- as.numeric(input_params_s[length(input_params_s)])
  
  # attempt to move particle i with MCMC kernel (R iterations)
  for (j in 1:R) {
    # repeat
    
    prop <- as.numeric(MASS::mvrnorm(n = 1, as.matrix(as.numeric(input_params), ncol = 1), rw_cov))
    
    names(prop) <- param_names[-length(input_params_s)]
    names(input_params) <- names(prop)
    
    #check if its within the prior distribution
    
    if (prior_eval(prop) == 0) {
      next
    }
    
    dist_prop = distance(prop, distance_args)
    
    
    if (dist_prop <= dist_next && prior_eval(prop) / prior_eval(input_params) < stats::runif(1)) {
      # Metropolis-Hastings Ratio
      iacc <- iacc + 1
      
      input_params <- prop
      
      input_s <- dist_prop
      
    }
  }
  return(c(input_params, input_s, sum(iacc)))
}


#' Start abc inference
#' @param prior function to sample from prior.
#' @param distance function to compute distance for proposal.
#' @param distance_args some R object (anything) which is passed as an argument to the distance function. Usually used to pass the observed data to the distance function.
#' @param method a character string specifying the algorithm to use.
#' @param control list of options to use in algorithm.
#' @param output_control list of options for controlling output of algorithm.
#' @param cl an object of class "cluster" to use \code{parApply} internally or the string "mclapply" to use \code{mclapply} internally. A NULL input (the default) will use \code{apply} internally, a single core.
#' @details
#' \code{abc_start} takes a function for the prior and a function for the distance as arguments. The \code{prior} function must take a single integer \eqn{n} as the argument and return a dataframe with \eqn{n} rows, the number of columns represents the number of parameters in your system. The \code{distance} function must take a numeric vector of length equal to the number of parameters in your system and return a single positive number, the distance.
#'
#' Note that other things normally considered part of ABC inference such as: observed data, simulated data and summary statistics are not seen by the algorithm but are defined beforehand by the user. For instance the distance returned by the distance function would normally be considered to be a distance between observed and simulated summary statistics, rather than a mapping from the space of parameters. In the design of this package we have simply skipped the intermediary steps so that the interface for the user is as flexible as possible.
#'
#' The \code{method} argument specifies the ABC algorithm to use to infer parameters, current options include \code{"rejection"} and \code{"RABC"}. The \code{control} argument takes a list of optional input values to be used in the ABC algorithm specified. The \code{output_control} argument takes a list of option input values to control how the output is returned to the user.
#'
#' The function can be parallelised in two ways. The first much the same way as \code{\link{parLapply}}. The user creates a cluster object \code{cl} with the \code{\link{makeCluster}} command and inputs \code{cl} as an argument to the function. The same troubleshooting procedures can be used as with \code{\link{parLapply}}, for instance if a node does not have access to objects in your environment use \code{\link{clusterExport}}. Alternatively if the character string "mclapply" is used as input to \code{cl} then \code{\link{mclapply}} will be used internally.
#'
#'
#' @export
abc_start <- function(prior, distance, distance_args = NULL, method = "rejection", control = list(), output_control = list(), cl = NULL){
  
  algorithm <- NA
  class(algorithm) <- method
  
  parallel <- ifelse(is.null(cl), 1,
                     ifelse("cluster" %in% class(cl), 2,
                            ifelse(cl == "mclapply", 3,
                                   ifelse(cl == "test", 4, 5)))
  )
  
  lfunc <- make_lfunc(parallel, cl)
  
  distance <- ifelse(is.null(distance_args), function(i, distance_args){distance(i)}, distance)
  
  output <- abc_algorithm(prior, distance, distance_args, algorithm, control, output_control, lfunc)
  
  return(output)
}


make_lfunc <- function(parallel, cl) {
  output <- switch(parallel,
                   apply,
                   function(X, MARGIN, FUN, ...) {
                     return(parallel::parLapply(cl, X, MARGIN, FUN, ...))
                   },
                   function(X, MARGIN, FUN, ...) {
                     Y <- split(t(X), rep(1:ncol(t(X)), each = nrow(t(X))))
                     Y <- lapply(Y, function(i, names_x) {
                       names(i) <- names_x
                       return(i)
                     }, names_x = colnames(X))
                     
                     output <- parallel::mclapply(Y, FUN, ...)
                     output_mat <-
                       matrix(unlist(output), length(output[[1]]))
                     rownames(output_mat) <- names(output[[1]])
                     
                     return(output_mat)
                   },
                   function(X, MARGIN, FUN, ...) {
                     Y <- split(t(X), rep(1:ncol(t(X)), each = nrow(t(X))))
                     Y <- lapply(Y, function(i, names_x) {
                       names(i) <- names_x
                       return(i)
                     }, names_x = colnames(X))
                     
                     output <- lapply(Y, FUN, ...)
                     output_mat <-
                       matrix(unlist(output), length(output[[1]]))
                     rownames(output_mat) <- names(output[[1]])
                     
                     return(output_mat)
                   })
  return(output)
}

# Control functions --------

## Rejection --------

#' abc_control.rejection
#' @description Create list of control parameters for rejection ABC sampler
#' @export
#' @param n numeric number of proposals in output
#' @param epsilon numeric proposals corresponding to distances under this number are kept. If you intend to use quantile_keep , set this to NULL.
#' @param delta numeric control number of new proposals to be resampled if distances are greater than epsilon
#' @param quantile_keep numeric quantile at which proposals are kept
abc_control.rejection <- function(
  n = 1000,
  epsilon = 0.05,
  delta = 1e-3,
  quantile_keep = NULL){
  
  stopifnot(n > 0 & n %% 1 == 0)
  stopifnot(epsilon >= 0 %% is.numeric(epsilon))
  stopifnot(delta >= 0 %% is.numeric(delta))
  
  stopifnot(quantile_keep >= 0)
  stopifnot(quantile_keep <= 1)
  if(!is.null(quantile_keep)){
    stopifnot(is.null(epsilon))
  }
  
  return(as.list(environment(), all=TRUE))
}

abc_output.rejection <- function(
  include_dist = FALSE){
  
  return(as.list(environment(), all=TRUE))
}

## RABC -----------

#' abc_control.RABC
#' @description Create list of control parameters for Replenishment ABC sampler
#' @export
#' @param n numeric number of proposals in output
#' @param a numeric fraction of proposals to drop at each iteration
#' @param c1 controls number of MCMC steps to make at each iteration
#' @param R starting value of number of MCMC steps to make at each iteration
#' @param eps_final numeric algorithm will finish when proposals have distances below this number
#' @param pacc_final numeric algorithm will finish when acceptance proportion is below this number
#' @param d_eps_final numeric algorithm will stop when percent difference in epsilon between iterations is below this number
#' @param num_drop numeric number of proposals to drop at each iteration
#' @param num_keep numeric number of proposals to keep at each iteration
#' @param n_param numeric for internal use, don't change
#' @param cov_func function method for computing covariance
#' @param prior_eval evaluator of prior function
abc_control.RABC <- function(
  n = 1000,
  a = 0.5,
  c1 = 0.01,
  R = 10,
  eps_final = 0,
  pacc_final = 0.02,
  d_eps_final = 0,
  num_drop = ceiling(n * a),
  num_keep = n - num_drop,
  n_param = NA,
  cov_func = ifelse(n_param == 1, stats::var, stats::cov),
  prior_eval = function(x,y){return(1)}){
  
  stopifnot(n > 2 & n %% 1 == 0)
  stopifnot(a > 0 & a < 1)
  stopifnot(is.numeric(c1))
  stopifnot(R %% 1 == 0)
  stopifnot(eps_final >= 0 %% is.numeric(eps_final))
  stopifnot(d_eps_final >= 0 %% is.numeric(d_eps_final))
  stopifnot(num_drop %% 1 == 0)
  stopifnot(num_keep >= 2 && num_keep %% 1 == 0)
  stopifnot(num_keep == n - num_drop)
  
  return(as.list(environment(), all=TRUE))
}

abc_output.RABC <- function(
  include_dist = FALSE,
  print_output = TRUE){
  
  return(as.list(environment(), all=TRUE))
}


