### Below is a function that will simulate one round (the number of days until
### all of the drivers have 3 tickets). We can then repeat this simulation
### many times and take the expected value of the results

simulation <- function(){
    ### initialize the number of tickets for each person
    num_tickets <- rep(0, 4)
    
    ### vector for the percent chance of getting a ticket
    prob_ticket <- c(.1, .15, .2, .25)
    
    ### initialize number of days the carpool has lasted
    num_days <- 0
    
    ### continue the simulation until all drivers have 3 tickets
    while(any(num_tickets < 3)) {
        ### pick a driver for the morning
        eligible_drivers_morning <- which(num_tickets < 3)
        morning_driver <- sample(eligible_drivers_morning, 1)
        
        ### simulate whether driver gets a ticket
        prob_ticket_morning <- prob_ticket[morning_driver]
        gets_ticket <- rbinom(1, 1, prob_ticket_morning)
        if(gets_ticket){
            num_tickets[morning_driver] <- num_tickets[morning_driver] + 1
        }
        
        ### add half a day
        num_days <- num_days + 0.5
        
        ### see if any more drivers are eligible for drive in evening
        if(any(num_tickets < 3)) {
            ### pick a driver for the evening
            eligible_drivers_evening <- which(num_tickets < 3)
            evening_driver <- sample(eligible_drivers_evening, 1)
            
            ### simulate whether driver gets a ticket
            prob_ticket_evening <- prob_ticket[evening_driver]
            gets_ticket <- rbinom(1, 1, prob_ticket_evening)
            if(gets_ticket){
                num_tickets[evening_driver] <- num_tickets[evening_driver] + 1
            }
            
            ### another another half a day
            num_days <- num_days + 0.5
        }
    }
    
    ### return the number of days until all drivers had 3 tickets
    return(num_days)
}


### Repeat the simulation 100,000 times and take the expectation
n_simulations <- 1E5
sim_results <- rep(NA, n_simulations)

### set seed so we get the same result each time
set.seed(5212017)
for(i in 1:n_simulations){
    sim_results[i] <- simulation()
}

### take expectation
mean(sim_results)
