library(baad.data)
library(ggplot2)
library(dplyr)
library(brms)
library(modelr)


# baad_data_version_current()
baad <- baad_data("1.0.1")


# asymptotic height with age
baad.age.ht <- 
  baad$data %>% 
  filter(!is.na(age),
         !is.na(h.t)) %>% 
  droplevels()

baad.age.ht.candidate <- 
  baad.age.ht %>% 
  group_by(speciesMatched) %>% 
  summarise(N = n(),
            age.range = diff(range(age)),
            age.unique = length(unique(age))) %>% 
  # selection criteria
  filter(N >= 30,
         age.range >= 5,
         age.unique >= 10)

baad.age.ht <- 
  baad.age.ht %>% 
  filter(speciesMatched %in% baad.age.ht.candidate$speciesMatched)


ggplot(baad.age.ht, aes(age, h.t)) +
  geom_point(aes(colour = location)) +
  facet_wrap(~ speciesMatched) +
  theme(legend.position = "none")



# Analysis
mod <- 
  brm(
    bf(h.t ~ (a * age) / (b + age),
       a ~ 1 + (1 | speciesMatched) + (1 | location), 
       b ~ 1 + (1 | speciesMatched) + (1 | location),
       nl = TRUE),
    data = baad.age.ht,
    prior = c(
      prior(normal(30, 1), nlpar = "a", lb = 0),    
      prior(normal(15, 1), nlpar = "b", lb = 0)
    ),
    warmup = 2000,
    iter = 4000,
    thin = 2,
    chains = 4,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 15),
    cores = 4
  )



# prediction
new.dat <- 
  baad.age.ht %>% 
  group_by(speciesMatched) %>% 
  data_grid(age = seq_range(age, 100)) %>% 
  bind_cols(predict(mod, newdata = new.dat, re_formula = ~ (1 | speciesMatched)) %>% as.data.frame())

ggplot(new.dat, aes(age, Estimate)) +
  geom_line(aes(colour = speciesMatched)) +
  theme(legend.position = "none")
