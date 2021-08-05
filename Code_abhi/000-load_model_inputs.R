# After Running load_data() we save each of the input datasets
# source("01-required_functions.R")
# load_data()

# saveRDS(FiT, file = "Input_Data_For_Model/feed_in_tariff.rds")
# saveRDS(region_weights, file = "Input_Data_For_Model/region_population_weights.rds")
# saveRDS(LF, file = "Input_Data_For_Model/load_factors_per_region.rds")
# saveRDS(income_thresh, file = "Input_Data_For_Model/income_thresholds.rds")
# saveRDS(mus, file = "Input_Data_For_Model/mus.rds")
# saveRDS(sigmas, file = "Input_Data_For_Model/sigmas.rds")
# saveRDS(elec_price_time, file = "Input_Data_For_Model/elec_price_time.rds")
# saveRDS(owner_occupiers, file = "Input_Data_For_Model/owner_occupiers.rds")
# saveRDS(kW_price, file = "Input_Data_For_Model/solar_sys_price_per_kw.rds")

load_model_input_data <- function(){
  FiT <- readRDS(file = "Input_Data_For_Model/feed_in_tariff.rds")
  region_weights <- readRDS(file = "Input_Data_For_Model/region_population_weights.rds")
  LF <- readRDS(file = "Input_Data_For_Model/load_factors_per_region.rds")
  income_thresh <- readRDS(file = "Input_Data_For_Model/income_thresholds.rds")
  mus <- readRDS(file = "Input_Data_For_Model/mus.rds")
  sigmas <- readRDS(file = "Input_Data_For_Model/sigmas.rds")
  elec_price_time <- readRDS(file = "Input_Data_For_Model/elec_price_time.rds")
  owner_occupiers <- readRDS(file = "Input_Data_For_Model/owner_occupiers.rds")
  kW_price <- readRDS(file = "Input_Data_For_Model/solar_sys_price_per_kw.rds")
  return(
    mget(ls())  
  )
}
