##### Libraries #####
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(cmdstanr) # R Interface to 'CmdStan' 
library(posterior) # Tools for Working with Posterior Distributions
library(bayesplot) # Plotting for Bayesian Models

bayesplot::color_scheme_set("brightblue")
theme_set(bayesplot::theme_default(base_family = "sans"))

##### Explore data #####
pana_trt <- read_csv("data/pana_traits.csv")

glimpse(pana_trt)

pana_trt %>% 
  view()

##### Step 1 - build a linear model from scratch ##### 

# Click on File -> New file -> Stan file

##### Step 2 - Prepare the toy data #####
toy <- pana_trt %>% 
  filter(!is.na(sla)) %>% 
  filter(!is.na(CHELSA_bio10_1))

# Covariate
x <- toy$CHELSA_bio10_1 
# Variate or variable
y <- log(toy$sla)

##### Step 3 - Run the model ##### 

### Call the Stan model 
mod <- cmdstan_model("stan/simple_LM.stan")

### Check the location of the model
mod$exe_file()

### Create a data object to be used by our Stan model
# names correspond to the data block in the Stan program
data_list <- list(N = length(x), x = x, y = y)

### Run the linear model 
fit_cmdstanr <- mod$sample(
  data = data_list, # data list
  seed = 12345, # random seed number
  chains = 4, # number of chains
  parallel_chains = 2, # number of cores used to run the model
  refresh = 500 # print update every 500 iterations
)

##### Step 3 - Explore the results #####
fit_cmdstanr$summary()

# summarize all variables with default and additional summary measures
fit_cmdstanr$summary(
  variables = NULL,
  posterior::default_summary_measures(),
  extra_quantiles = ~posterior::quantile2(., probs = c(.0275, .975))
)

### Get posterior samples 
# default is a 3-D draws_array object from the posterior package
# iterations x chains x variables
draws_arr <- fit_cmdstanr$draws() # or format="array"
str(draws_arr)

### Transform the array to a data frame
# draws x variables data frame
draws_df <- fit_cmdstanr$draws(format = "df")
str(draws_df)

draws_df

# this should be identical to draws_df created via draws(format = "df")
draws_df_2 <- as_draws_df(draws_arr)
identical(draws_df, draws_df_2)

### Plot the parameter estimations
mcmc_hist(fit_cmdstanr$draws("beta"))

### Build a simple linear model using the "lm" function of {base} R
obj <- lm(y ~ x)

# Likelihood versus Bayes 
summary(obj)

fit_cmdstanr$summary()

##### Using brms #####
library(brms) # Bayesian Regression Models using 'Stan'

### Use {brms} to create a Stan model
make_stancode(log(sla) ~ CHELSA_bio10_1,
              data = toy, 
              family = gaussian())

### Verify that the priors indeed found their way into Stan's model code
myprior <- get_prior(log(sla) ~ CHELSA_bio10_1,
                     data = toy, 
                     family = gaussian())

# print the priors
myprior

### define priors according our Stan model
myprior$prior[1] <- "normal(0,10)" # slopes 
myprior

myprior$prior[3] <- "normal(0,10)" # intercept
myprior

myprior$prior[4] <- "normal(0,1)" # sigma
myprior

### Run model using brms
fit_brms <- brm(log(sla) ~ CHELSA_bio10_1, # formula
            data = toy, # data to be used
            prior = myprior, # prior information
            family = gaussian(), # family used for modeling
            backend = "cmdstanr", # engine
            chains = 4, 
            cores = 4
)

### Explore the results
summary(fit_brms)

### Plot the association between y ~ x
conditional_effects(fit_brms)

### Estimate R2
bayes_R2(fit_brms)

### Get effects
fixef(fit_brms)

## extract posterior draws in an array format
draws_fit <- as_draws_array(fit_brms)
posterior::summarize_draws(draws_fit)

### Plot samples of slopes

# data
d <- fit_brms$data

# estimations
post <- as.data.frame(x = fit_brms, # fit object
                     regex = TRUE) # transform the parameters to a data frame

# estimate posterior means
post_means <- colMeans(post)

# take a sample from the posterior draws
keep <- sample(nrow(post), 100)

# select 100 samples from posterior draws
post_draws <- post[keep, ] 

### Use {ggplot2} to plot the results
d %>% 
  ggplot(aes(x = CHELSA_bio10_1, y = log(sla))) + 
  geom_point(alpha = 1 / 10) + # add datapoints
  geom_abline( # 100 posterior draws of the regression line
    intercept = post_draws[, 1],
    slope = post_draws[, 2],
    color = "#9497eb", 
    linewidth = 0.5, 
    alpha = 0.25
  ) + 
  geom_abline( # posterior mean regression line
    intercept = post_means[1],
    slope = post_means[2],
    color = "#1c35c4", 
    linewidth = 1.5, 
    alpha = 0.8
  ) + 
  ylim(2, 3) + # adjust the y axis to match the conditional_effect() plot from {brms}
  labs(x = "Mean Annual Temperature", 
       y = "Specific leaf area [log]") 

### Cool!