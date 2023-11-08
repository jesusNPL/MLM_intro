##### Libraries #####
library(tidyverse) # Easily Install and Load the 'Tidyverse' 
library(cmdstanr) # R Interface to 'CmdStan' 
library(posterior) # Tools for Working with Posterior Distributions 
library(bayesplot) # Plotting for Bayesian Models 
library(brms) # Bayesian Regression Models using 'Stan' 
library(ape) # Analyses of Phylogenetics and Evolution 

bayesplot::color_scheme_set("brightblue")
theme_set(bayesplot::theme_default(base_family = "sans"))

##### Data ##### 

### Trait data and predictors
pana_trt <- read_csv("data/pana_traits.csv")

glimpse(pana_trt)

pana_trt %>% 
  view()

### Phylogenetic tree 
pana_phy <- read.nexus("data/pana_phylo.nex")

plot(pana_phy, type = "phylogram", show.tip.label = FALSE)
axisPhylo()

##### Model  ##### 

# Click on File -> New file -> Stan file

##### Explore the data #####

### Number of observations per KG 
pana_trt %>% 
  count(KG_classes)

### Number of observations per species
pana_trt %>% 
  count(Species)

##### Model building - Part One #####

### Explore the distribution of the variate AKA "y" 

# SLA
pana_trt %>% 
  ggplot(aes(x = sla)) + 
  geom_histogram()

# log(SLA)
pana_trt %>% 
  mutate(slaLog = log(sla)) %>% 
  ggplot(aes(x = slaLog)) + 
  geom_histogram()

### Build a simple regression model with flat priors in beta

## Model 0 - no prior information in beta
mod0_FlatPrior <- brm(formula = log(sla) ~ CHELSA_bio10_1 + CHELSA_bio10_12, 
                    family = gaussian(), 
                    data = pana_trt, 
                    chains = 4, 
                    backend = "cmdstanr", 
                    cores = 10)

# Explore convergence 
plot(mod0_FlatPrior, ask = FALSE, N = 4)

summary(mod0_FlatPrior)
conditional_effects(mod0_FlatPrior)
bayes_R2(mod0_FlatPrior)

# see the prior that was used in the model
get_prior(log(sla) ~ CHELSA_bio10_1 + CHELSA_bio10_12, 
          family = gaussian(), 
          data = pana_trt)

### Build a model with some priors in beta and sigma

## Prior
prior_m0 <- get_prior(log(sla) ~ CHELSA_bio10_1 + CHELSA_bio10_12, 
                      family = gaussian(),
                      data = pana_trt)

# print the default prior
prior_m0 

?StudentT
prior_m0$prior[1] <- "normal(0,10)" # slopes 
prior_m0$prior[5] <- "normal(0,1)" # sigma

prior_m0

## Model
mod0_WeakPrior <- brm(formula = log(sla) ~ CHELSA_bio10_1 + CHELSA_bio10_12, 
                      prior = prior_m0, 
                      family = gaussian(), 
                      data = pana_trt, 
                      chains = 4, 
                      backend = "cmdstanr", 
                      cores = 10)

summary(mod0_WeakPrior)
conditional_effects(mod0_WeakPrior)
bayes_R2(mod0_WeakPrior)

# Both models returned similar estimations, let's compare them 

### Add criterion for comparison
mod0_FlatPrior <- add_criterion(mod0_FlatPrior, c("loo", "waic"))

print(mod0_FlatPrior$criteria$loo)
print(mod0_FlatPrior$criteria$waic)

mod0_WeakPrior <- add_criterion(mod0_WeakPrior, c("loo", "waic"))

print(mod0_WeakPrior$criteria$loo)
print(mod0_WeakPrior$criteria$waic)

### compare both models
loo_compare(mod0_FlatPrior, mod0_WeakPrior, criterion = "loo")

### Test the hypothesis that the predictors deviates from zero

# Mean Annual Temperature
ht <- hypothesis(x = mod0_WeakPrior, 
                 hypothesis = "CHELSA_bio10_1 > 0", 
                 alpha = 0.05, 
                 robust = TRUE)

plot(ht)

# Mean Annual Precipitation
hp <- hypothesis(x = mod0_WeakPrior, 
                 hypothesis = "CHELSA_bio10_12 < 0", 
                 alpha = 0.05, 
                 robust = TRUE)
plot(hp)

##### Posterior predictive checks #####

# Plots comparing the empirical distribution of data y to the distributions of individual simulated datasets (rows) in yrep.
pp_check(mod0_WeakPrior, ndraws = 100)  # shows dens_overlay plot by default

# Scatterplots of the observed data y vs. simulated/replicated data yrep from the posterior predictive distribution
pp_check(mod0_WeakPrior, type = "scatter_avg", ndraws = 100) 

# Test statistics
pp_check(mod0_WeakPrior, type = "stat", stat = "median", ndraws = 1000) 

# Scatterplot of two test statistics.
pp_check(mod0_WeakPrior, type = "stat_2d") 

# Leave-One-Out (LOO) predictive checks. PIT = probability integral transformation
pp_check(mod0_WeakPrior, type = "loo_pit")
# The LOO PIT values are asymptotically uniform (for continuous data) if the model is calibrated. The ppc_loo_pit_overlay() function creates a plot comparing the density of the LOO PITs (thick line) to the density estimates of many simulated data sets from the standard uniform distribution (thin lines)

################## --------------------------------- #####################

##### Model building - Part Two #####

### Build a model in which we allow variation within the species

## Prior
prior_m1 <- get_prior(log(sla) ~ CHELSA_bio10_1 + CHELSA_bio10_12 + (1 | Species), 
                      family = gaussian(),
                      data = pana_trt)

prior_m1$prior[1] <- "normal(0,10)" # slopes 

prior_m1

## Model
mod1_WeakPrior <- brm(formula = log(sla) ~ CHELSA_bio10_1 + CHELSA_bio10_12 + (1 | Species), 
                      prior = prior_m1, 
                      family = gaussian(), 
                      data = pana_trt, 
                      chains = 4, 
                      backend = "cmdstanr", 
                      cores = 10)

summary(mod1_WeakPrior)
conditional_effects(mod1_WeakPrior)
bayes_R2(mod1_WeakPrior)

### Recalculate R2 including and excluding the group-level  
round(data.frame(bayes_R2(mod1_WeakPrior, re_formula = NA)), 3) # include no group-level effects.
round(data.frame(bayes_R2(mod1_WeakPrior, re_formula = NULL)), 3) # include all group-level effects

mod1_WeakPrior <-  add_criterion(mod1_WeakPrior, c("loo", "waic"))

loo(mod1_WeakPrior)

### compare both models
loo_compare(mod0_WeakPrior, mod1_WeakPrior, criterion = "loo")

### Build a model using the phylogenetic relationship 
# we can use the same prior

# Get the matrix of variance-covariance
pana_vcv <- vcv.phylo(pana_phy)

## Model
mod1b_WeakPrior <- brm(formula = log(sla) ~ CHELSA_bio10_1 + CHELSA_bio10_12 + (1 | gr(Species, cov = pana_vcv)),
                      prior = prior_m1, 
                      family = gaussian(), 
                      data = pana_trt, 
                      data2 = list(pana_vcv = pana_vcv), # phylogenetic relationhsip
                      chains = 4, 
                      backend = "cmdstanr", 
                      cores = 10)

summary(mod1b_WeakPrior)
conditional_effects(mod1b_WeakPrior)
bayes_R2(mod1b_WeakPrior)

### Recalculate R2 including and excluding the group-level  
round(data.frame(bayes_R2(mod1b_WeakPrior, re_formula = NA)), 3) # include no group-level effects.
round(data.frame(bayes_R2(mod1b_WeakPrior, re_formula = NULL)), 3) # include all group-level effects

mod1b_WeakPrior <-  add_criterion(mod1b_WeakPrior, c("loo", "waic"))

loo(mod1b_WeakPrior)

### compare both models
loo_compare(mod0_WeakPrior, mod1_WeakPrior, mod1b_WeakPrior, criterion = "loo")

### Now let's complicate our lives and build a more complex model including both sources of variation

## Prior
prior_m2 <- get_prior(log(sla) ~ CHELSA_bio10_1 + CHELSA_bio10_12 + (1 | Species) + (1 | gr(Species_model, cov = pana_vcv)), 
                      family = gaussian(),
                      data = pana_trt, 
                      data2 = list(pana_vcv = pana_vcv))

prior_m2$prior[1] <- "normal(0,10)" # slopes 

prior_m2

## Model
mod2_WeakPrior <- brm(formula = log(sla) ~ CHELSA_bio10_1 + CHELSA_bio10_12 + (1 | Species) + (1 | gr(Species_model, cov = pana_vcv)), 
                      prior = prior_m2, 
                      family = gaussian(), 
                      data = pana_trt, 
                      data2 = list(pana_vcv = pana_vcv),
                      chains = 4, 
                      backend = "cmdstanr", 
                      cores = 10)

summary(mod2_WeakPrior)
conditional_effects(mod2_WeakPrior)
bayes_R2(mod2_WeakPrior)

### Recalculate R2 including and excluding the group-level  
round(data.frame(bayes_R2(mod1_WeakPrior, re_formula = NA)), 3) # include no group-level effects.
round(data.frame(bayes_R2(mod1_WeakPrior, re_formula = NULL)), 3) # include all group-level effects

mod2_WeakPrior <-  add_criterion(mod2_WeakPrior, c("loo", "waic"))

loo(mod2_WeakPrior)

### compare both models
loo_compare(mod0_WeakPrior, mod1_WeakPrior, mod2_WeakPrior, criterion = "loo")

### Estimate the phylogenetic signal of SLA
h_phy <- paste(
  "sd_Species_model__Intercept^2 /",
  "(sd_Species_model__Intercept^2 + sd_Species__Intercept^2 + sigma^2) = 0"
)

ps <- hypothesis(mod2_WeakPrior, h_phy, class = NULL)
ps

plot(ps)

### Get the population-level effects or fixed effects
fixef(mod2_WeakPrior)

### Get group-level effects or random effects 
ranef(mod2_WeakPrior)

##### Visualization #####

### Plot the species-group-level effect 
## Aux theme
theme_new <- function() {
  theme_bw() +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold", size = 15), 
          legend.text = element_text(size = 12))
}

## Bar colors
barCOLS <- c(scales::alpha("red", 0.7), 
             scales::alpha("darkblue", 0.7), 
             scales::alpha("darkgray", 0.7))


### Get Species group-level effects (SD)
species <- ranef(mod2_WeakPrior, probs = c(0.025, 0.05, 0.11, 0.89, 0.95, 0.975))$Species

selSPP <- unique(pana_trt$Species)[1:50]

# plot
dt_spp <-  species[, , "Intercept"] %>%  
  data.frame() %>% 
  rownames_to_column() %>%
  rename(species = rowname) %>% 
  as_tibble() %>% 
  mutate(species2 = gsub("_", " ", species)) %>% 
  mutate(direction = ifelse(Q11 > 0, "Positive", 
                            ifelse(Q89 < 0, "Negative", "Null"))) %>% 
  arrange(species2) %>% 
  filter(species %in% selSPP)

p_species <- dt_spp %>% 
  ggplot(aes(x = species2, y = Estimate, color = direction)) +  
  geom_pointrange(aes(ymin = Q5, ymax = Q95), 
                  fatten = 1, linewidth = 1.5, alpha = 0.4) + 
  geom_pointrange(aes(ymin = Q11, ymax = Q89), 
                  fatten = 2, linewidth = 3.5, alpha = 0.7) + 
  #coord_cartesian(ylim = c(-1, 1.5)) + 
  geom_hline(yintercept = 0, linewidth = 1, colour = "darkgray", linetype = "dashed") + 
  scale_colour_manual(values = c("Positive" = barCOLS[2], 
                                 "Negative" = barCOLS[1], 
                                 "Null" = barCOLS[3])
  ) + 
  coord_flip() + 
  labs(x = NULL, 
       y = "Species-level effect (SD)") + 
  theme_new() + 
  theme(
    legend.position = "none", 
    axis.text.x = element_text(vjust = 0.5, hjust = 0.7), 
    axis.text.y = element_text(face = "italic"),
    axis.title = element_text(size = 18), 
    axis.text = element_text(size = 15, colour = "black")
  ) 

p_species 

### Plot Population-level effects 
library(tidybayes)

dd <- mod1_WeakPrior$data

dd %>% 
  add_predicted_draws(mod1_WeakPrior, re_formula = NA) %>%  # adding the posterior distribution
  ggplot(aes(x = CHELSA_bio10_1, y = `log(sla)`)) +  
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                  alpha = 0.7, colour = "black") +
  geom_point(data = dd, colour = "black", size = 4, alpha = 0.5) +   # raw data
  scale_fill_brewer(palette = "Greys", name = "Level") +
  labs(x = "Mean Annual Temperature", 
       y = "Specific leaf area [log]") +  
  scale_y_continuous(n.breaks = 6) + 
  scale_x_continuous(n.breaks = 7) + 
  theme_new() +
  theme(legend.position = c(0.85, 0.70), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 20),
        axis.text = element_text(size = 20, colour = "black"),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))


