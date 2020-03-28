### Function to run one simulation of this

n_rolls_until_all_equal <- function() {
    ### First roll
    my_die <- sample(1:6, 6, replace = TRUE)
    nrolls <- 1
    ### Until all numbers on the die are the same, keep re-rolling
    while(length(unique(my_die)) > 1) {
        my_die <- sample(my_die, 6, replace = TRUE)
        nrolls <- nrolls + 1
    }
    ### return number of rolls
    return(nrolls)
}

### number of simulations
Nsims <- 1E6
### record rolls
set.seed(123)
my_rolls <- rep(NA, Nsims)
for(i in 1:Nsims) {
    my_rolls[i] <- n_rolls_until_all_equal()
}
### Return the mean number of rolls
mean(my_rolls)
