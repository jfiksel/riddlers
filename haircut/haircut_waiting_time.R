#### simulate one waiting time
sim_wait_time <- function(prob_hold_for_tiffany = .25) {
    ### Decide whether other customer will wait for tiffany
    will_hold_for_tiffany <- rbinom(1, 1, prob_hold_for_tiffany)
    ### simulate uniform current times into haircuts for barbers
    ### first one is Tiffany
    current_haircut_times <- runif(4, 0, 15)
    ### Time left in each of the current haircuts
    time_left <- 15 - current_haircut_times
    ### which barber has the shortest remaining time in current haircut
    shortest_wait_index <- which.min(time_left)
    ### First case: other person will not hold out for for tiffany
    if(will_hold_for_tiffany == 0) {
        ### Case 1: tiffany the next one done
        if(shortest_wait_index == 1) {
            ### add time remaining for current cut and 15 minutes for next haircut
            return(time_left[1] + 15)
        } else {
            ### if not, just return time left for Tiffany's haircut
            return(time_left[1])
        }
    ### Other case: other person will hold out for Tiffany
    } else {
        ### add time remaining for current cut and 15 minutes for next haircut
        return(time_left[1] + 15)
    }
}

### Now simulate waiting times over 1,000,000 iterations
S <- 1000000
set.seed(123)
wait_times <- sapply(1:S, function(i) sim_wait_time())
### Average wait time
mean(wait_times)
