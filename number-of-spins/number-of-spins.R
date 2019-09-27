library(rjags)
library(ggmcmc)
library(gtools)
library(matrixStats)
num_freq_obs = c(18, 19, 22, 19, 20, 22)

#### Find appropriate dirichlet distribution
prob_ratio <- function(delta) {
    samples <- rdirichlet(10000, alpha = rep(delta, 8))
    rowmins <- rowMins(samples)
    rowmaxs <- rowMaxs(samples)
    return(mean(rowmaxs / rowmins > 2))
}

deltas <- seq(1, 100, by = 1)
ratios <- sapply(deltas, prob_ratio)
### use delta such that prob(max > min) approx 1%
deltas[which.min(abs(ratios - .01))]

data <- list(num_freq_obs = num_freq_obs, nspins_known = sum(num_freq_obs),
             delta = deltas[which.min(abs(ratios - .01))])
model <- jags.model("number-of-spins.txt", data = data, n.chains = 3)
### Get samples
update(model, 5000)
samples <- coda.samples(model, variable.names = c("nspins", "n_4", "n_5", "pordered"),
                        n.iter = 5000)
samples_all <- ggs(samples)
### Rhat
ggs_Rhat(samples_all)
samples_ggs <- ggs(samples, family = c("n_[4|5]"))
### Check traceplot for number of 4s & 5s
ggs_traceplot(samples_ggs)
### Joint posterior
ggs_pairs(samples_ggs)
### Posterior means
samples_ggs %>%
    group_by(Parameter) %>%
    summarize(posterior_mean = mean(value))


