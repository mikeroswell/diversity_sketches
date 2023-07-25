# devtools::install_github("mikeroswell/MeanRarity")
library(MeanRarity)
library(tidyverse)

aba<-fit_SAD() # approximately lognormal SAD with richness 50, Hill-Simpson 40
abb<-fit_SAD(rich = 60, distr = "gamma") # apporximately gamma-distributed SAD with richness 60, Hill-Simpson 40

Hurlbert<-function(ab, ss_vec){ # return a table of rarefied richness and ENS_hurlbert
  ss_vec = c(ss_vec, sum(ab))
  map_dfr(ss_vec, function(ss){
    s_k = hRare(ab, ss)
    ENS_k = hurl(ab, ss)
    return(data.frame(s_k, ENS_k, k = ss))
  })
}

ss_vec <- c(10, 20, 50) # some low reference samples (i.e. n in the paper)

inits<- c(100, 500, 1000) # sample sizes to take from the two SADs (i.e. N in the paper)

set.seed(100)
aba_samples = map(inits, function(ss){
  sample_infinite(aba$rel_abundances, ss)
})

abb_samples = map(inits, function(ss){
  sample_infinite(abb$rel_abundances, ss)
})

res <- map_dfr(c("aba_samples", "abb_samples"), function(orig){
  map_dfr(get(orig), function(sample){
    data.frame(Hurlbert(sample, ss_vec), orig, start_size = sum(sample))
  })
})

ENS_part = res %>% 
  group_by(orig, start_size) %>% 
  filter(k >20) %>% 
  mutate(SAD =min(ENS_k), N = ENS_k-lag(ENS_k)) %>% 
  group_by(orig, start_size) %>% summarize(SAD = mean(SAD), N = mean(N, na.rm =T))

ENS_part
