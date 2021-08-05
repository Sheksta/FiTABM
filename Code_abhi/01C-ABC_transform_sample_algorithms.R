
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
    max_unif <- 0.9
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
  max_unif <- 0.9
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


detransform_single_particle <- function(theta){
  min_unif = 0.5
  max_unif = 0.9
  # Detransform particle
  v_retr <- exp(theta[1:4])
  x <- v_retr/sum(v_retr)
  p_retr <- (min_unif*exp(-theta[5]) + max_unif)/(1 + exp(-theta[5]))
  detransed_particle <- c(x, p_retr)
  return(detransed_particle)
}


detransform_all_particles <- function(theta_df){
  apply(theta_df, MARGIN = 1, function(theta_df){
    true_theta <- detransform_single_particle(theta_df)
    return(true_theta)
  })
}
