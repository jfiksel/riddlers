library(doParallel)
cl <- makeCluster(7)
registerDoParallel(cl)
ntrials <- 100000
set.seed(1)
trialresults <- foreach(i=1:ntrials) %dopar% {
  consec.makes <- 0
  nshots <- 0
  while(consec.makes < 17) {
    shot <- sample(c(0, 1), size = 1, prob = c(.3, .7))
    if(shot == 1){
      consec.makes <- consec.makes + 1
    } else {
      consec.makes <- 0
    }
    nshots <- nshots + 1
  }
  nshots
}
stopCluster(cl)
mean(unlist(trialresults))
