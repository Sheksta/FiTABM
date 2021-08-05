
run_model_abc <- function(number_of_agents, w, threshold) {
    # Set up some parameters
    time_steps <- nrow(FiT) # number of months in time series

    agents <- rerun(
        number_of_agents,
        Household_Agent("N", assign_income(), assign_size(), assign_region())
    )
#1: Set all adopters to zero
#2: Assign income (how?)
#3: Assign size (how?)
#4: Assign region (how?)

    n_links <- 10

    mean_income <- mean(extract(agents, "income"))
    agents %<>% map(assign_LF) %>%
        map(assign_elec_cons) %>%
        map(assign_u_inc, mean_inc = mean_income) %>%
        map(assign_soc_network, n_ag = number_of_agents, n_l = n_links)

    adopters <- agents[map(agents, "status") == 1]

    # Create agents: all non-adopters, assign income, size and region randomly weighted by real data
    # assign further characteristics based on those previously assigned.
    # Set up data frame to put data in

    avg_u <- data.frame(
        time_series = FiT$time_series + months(1),
        avg_inst_cap = vector(length = time_steps),
        tot_inst_cap = vector(length = time_steps)
    )

    #---------------------------------------------------------#
    # Time evolution!

    for (i in 1:time_steps) {
        # set parameters for current time
        FiT_current_small <<- FiT$FiT[[i]] / 100 # p to £
        FiT_current_large <<- FiT$FiT_large[[i]] / 100
        exp_tar_current <<- FiT$exp_tar[[i]] / 100 # p to £
        kW_price_current <<- kW_price$X2[i]
        current_date <<- FiT$time_series[i]
        elec_index <- which(sapply(elec_price_time$X1, function(x) grep(x, current_date)) == 1)
        elec_price <<- elec_price_time[[elec_index, 2]] / 100
        n_owners <<- owner_occupiers[[elec_index, 2]]

        agents <- agents %>%
            map(assign_inst_cap) %>%
            map(utilities, w = w, ags = agents) %>%
            map(decide, threshold = threshold)


        adopters <- agents[map(agents, "status") == 1]


        # Write data
        if (length(adopters) > 0) {
            avg_u$avg_inst_cap[i] <- mean(extract(adopters, "inst_cap"))
            avg_u$tot_inst_cap[i] <- sum(extract(adopters, "inst_cap"), na.rm = TRUE) * n_owners / (1000 * number_of_agents)
        }
        else {
            avg_u$avg_inst_cap[i] <- 0
            avg_u$tot_ins_cap[i] <- 0
        }
    }
    return(avg_u)
}