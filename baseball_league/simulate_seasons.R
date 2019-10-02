### First write function to simulate the number of runs scored for a given team in each of 9 innings
simulate_runs <- function(team) {
    if(team == "moonwalkers") {
        ### Simulate the number of walks from a negative binomial
        pstrikeout <- .6
        nwalks <- rnbinom(n = 9, size = 3, prob = pstrikeout)
        ### Number of runs = max(nwalks - 3, 0)
        nruns <- nwalks - 3
        nruns[nruns < 0] <- 0
    }
    if(team == "doubloons") {
        ### Simulate the number of doubles from a negative binomial
        pstrikeout <- .8
        ndoubles <- rnbinom(n = 9, size = 3, prob = pstrikeout)
        ### Number of runs = max(ndoubles - 1, 0)
        nruns <- ndoubles - 1
        nruns[nruns < 0] <- 0
    }
    if(team == "taters") {
        pstrikeout <- .9
        nhomeruns <- rnbinom(n = 9, size = 3, prob = pstrikeout)
        ### Number of runs = nhr
        nruns <- nhomeruns
    }
    return(nruns)
}

simulate_game <- function(team1, team2) {
    team1runs <- sum(simulate_runs(team1))
    team2runs <- sum(simulate_runs(team2))
    ### return which team wins (if we have winner)
    if(team1runs > team2runs) {
        return(team1)
    } else if (team1runs < team2runs) {
        return(team2)
    } else{
        ### what to do if we have a tie
        winner <- 0
        while(winner == 0) {
            team1extras <- simulate_runs(team1)
            team2extras <- simulate_runs(team2)
            rundiff <- team1extras - team2extras
            if(identical(rundiff, rep(0, length(rundiff)))) {
                winner <- 0
            } else {
                winner <- 1
                winning_inning <- min(which(rundiff != 0))
                if(rundiff[winning_inning] > 0) {
                    return(team1)
                } else {
                    return(team2)
                }
            }
        }
    }
}

simulate_season <- function() {
    teams <- c("taters", "moonwalkers", "doubloons")
    games <- data.frame(t1 = c('taters', 'taters', 'moonwalkers'),
                        t2 = c('moonwalkers', 'doubloons', 'doubloons'))
    ### replicate data frame (multiple games in a season)
    games <- data.frame(sapply(games, rep.int, times=10))
    wins <- rep(0, 3)
    winners <- sapply(1:nrow(games), function(i) {
        row <- games[i,]
        return(as.character(simulate_game(row$t1, row$t2)))
    })
    for(i in 1:length(teams)) {
        wins[i] <- sum(winners == teams[i])
    }
    ### Check to see if number of wins is same
    if(all(wins - mean(wins) == 0)) {
        return('tie')
    } else{
        return(teams[which.max(wins)])
    }
}

### Simulate many seasons
nsims <- 10000
set.seed(123)
seeds <- sample(-1e6:1e6, nsims, replace = F)
season_winners <- sapply(1:nsims, function(i) {
    set.seed(seeds[i])
    simulate_season()
})
table(season_winners)