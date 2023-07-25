remotes::install_github("mikeroswell/MeanRarity")
library(purrr)
library(ggplot2)
library(dplyr)



even_com <- c(rep(200, 30), rep(10000, 8))
abundant_com <- c(rep(300, 30), rep (20000, 8))
sum(even_com)
sum(abundant_com)


Evenness_Abundance <- map_dfr(1:100, function(reps){
  map_dfr(c(0.001, 0.005, 0.01), function(intense){
    map_dfr(c("even_com", "abundant_com"), function(com){
  my_samp <- MeanRarity::sample_infinite(get(com), size = intense*sum(get(com)))
  SampleHillShannon <- MeanRarity::rarity(my_samp, l = 0)
  SampleRichness <- sum(my_samp>0)
  HillShannon <- MeanRarity::rarity(get(com), l=0)
  Richness <- sum(get(com)>0)
  Abundance <- sum(my_samp)
  return(data.frame(sampling_intensity = intense, community = com, SampleHillShannon, SampleRichness, HillShannon, Richness, Abundance))
    })
  })
})


Evenness_Abundance %>% 
  ggplot(aes(SampleRichness, SampleHillShannon, color = community))+ 
  geom_point()+
  facet_wrap(~sampling_intensity)+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()

head(Evenness_Abundance)

Evenness_Abundance %>% 
  group_by(community, sampling_intensity) %>% 
  summarize(across(.fns = mean))

