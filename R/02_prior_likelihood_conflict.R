# Code belongs to Richard McElreath 
# https://gist.github.com/rmcelreath/39dd410fc6bb758e54d79249b11eeb2f
# Original X post https://twitter.com/rlmcelreath/status/1701165075493470644

# https://github.com/rmcelreath/stat_rethinking_2023#calendar--topical-outline

# If you want to install {rethinking} run the next lines of code
install.packages(c("coda", "mvtnorm", "devtools", "loo", "dagitty"))
devtools::install_github("rmcelreath/rethinking@slim")

##### Prior - Likelihood conflict #####

library(rethinking)

yobs <- 0

### Models 

# student prior, normal likelihood
mtn <- ulam(
  alist(
    y ~ dstudent(2, mu, 1),
    mu ~ dnorm(10, 1)
  ), 
  data = list(y = yobs), chains = 4, iter = 2000) 

# student prior, student likelihood
mtt <- ulam(
  alist(
    y ~ dstudent(2, mu, 1),
    mu ~ dstudent(2, 10, 1)
  ), 
  data = list(y = yobs), chains = 4, iter = 2000)

# normal prior, normal likelihood
mnn <- ulam(
  alist(
    y ~ dnorm(mu, 1),
    mu ~ dnorm(10, 1)
  ), 
  data = list(y = yobs), chains = 4, iter = 2000) 

# normal prior, student likelihood
mnt <- ulam(
  alist(
    y ~ dnorm(mu, 1),
    mu ~ dstudent(2, 10, 1)
  ), 
  data = list(y = yobs), chains = 4, iter = 2000)

### Plots
par(mfrow = c(2, 2), cex = 1.05)
ymax <- 0.53
xlwd <- 1.5
postcol <- 2
xadj <- 0.8

p1 <- extract.samples(mnn) 
# The classic flavor of Bayesian updating - the posterior is a compromise between the prior and likelihood
dens(p1$mu, # posterior distribution
     xlim = c(-5, 15), 
     ylim = c(0, ymax), 
     lwd = xlwd + 1, 
     col = postcol, 
     xlab = "", 
     adj = xadj)
mtext("normal prior, normal likelihood")
curve(dnorm(yobs, x, 1), add = TRUE, lty = 1, lwd = xlwd) # likelihood
curve(dnorm(x, 10, 1), add = TRUE, lty = 2, lwd = xlwd) # prior

text(0, 0.42, "likelihood")
text(10, 0.42, "prior")

p2 <- extract.samples(mtt) 
# The two modes persist - the extra mass in the tails means each distribution finds the other's mode more plausible and so the average isn't the best "compromise"
dens(p2$mu,  # posterior distribution
     xlim = c(-5, 15), 
     ylim = c(0, ymax), 
     lwd = xlwd + 1, 
     col = postcol, 
     xlab = "", 
     adj = xadj)
mtext("t prior, t likelihood")
curve(dstudent(yobs, 2, x, 1), add = TRUE, lty = 1, lwd = xlwd) # likelihood
curve(dstudent(x, 2, 10, 1), add = TRUE, lty = 2, lwd = xlwd) # prior

text(0, 0.42, "likelihood")
text(10, 0.42, "prior")

p3 <- extract.samples(mnt)
# Now the likelihood dominates - it's thin tails are very skeptical of the prior, but the prior's thick tails not so surprised by the likelihood
dens(p3$mu,  # posterior distribution
     xlim = c(-5, 15), 
     ylim = c(0, ymax), 
     lwd = xlwd + 1, 
     col = postcol, 
     xlab = "", 
     adj = xadj)
mtext("t prior, normal likelihood")
curve(dnorm(yobs, x, 1), add = TRUE, lty = 1, lwd = xlwd) # likelihood
curve(dstudent(x, 2, 10, 1), add = TRUE, lty = 2, lwd = xlwd) # prior

text(0, 0.42, "likelihood")
text(10, 0.42, "prior")

p4 <- extract.samples(mtn) 
# Now the prior dominates, so reason as previous example but in reverse
dens(p4$mu,  # posterior distribution
     xlim = c(-5, 15), 
     ylim = c(0, ymax), 
     lwd = xlwd + 1, 
     col = postcol, 
     xlab = "", 
     adj = xadj)
mtext("normal prior, t likelihood")
curve(dstudent(yobs, 2, x, 1), add = TRUE, lty = 1, lwd = xlwd) # likelihood
curve(dnorm(x, 10, 1), add = TRUE, lty = 2, lwd = xlwd) # prior

text(0, 0.42, "likelihood")
text(10, 0.42, "prior")

##### This chunk was extracted from the book: Regressions and Other Stories ##### 
# You can download a copy of the book from here: https://avehtari.github.io/ROS-Examples/

### Background 
# Suppose an election is coming up, and a previously fitted model using economic and political conditions gives a forecast 
# that the Democratic candidate will receive 52.4% of the two-party vote, with a predictive uncertainty of 4.1%. 
# Using the notation above, theta-prior = 0.524 and se-prior = 0.041.
# We now conduct a survey of 400 people, of whom 190 say they will vote for the Democratic
# candidate and 210 support the Republican. If the survey was a simple random sample of voters
# with no nonresponse, with voters who will not change their minds between the time of the
# survey and the election, then the data estimate is theta-data = 190/400 = 0.475 with standard error
# sqrt(0.475 ∗ (1 − 0.475)/400 = 0.025).


#' Prior based on a previously-fitted model using economic and political condition.
theta_hat_prior <- 0.524
se_prior <- 0.041

#' Survey of 400 people, of whom 190 say they will vote for the Democratic candidate
n <- 400
y <- 190 

#' #### Data estimate
theta_hat_data <- y/n 
theta_hat_data

se_data <- sqrt((y/n)*(1-y/n)/n) 
se_data

#' #### Bayes estimate
theta_hat_bayes <- (theta_hat_prior/se_prior ^ 2 + theta_hat_data/se_data ^ 2) / (1/se_prior ^ 2 + 1/se_data ^ 2) 
theta_hat_bayes

se_bayes <- sqrt(1/(1/se_prior ^ 2 + 1/se_data ^ 2))
se_bayes

#' ## Figures

# Prior versus Likelihood
par(mar = c(3, 1, 1, 1), mgp = c(1.5, 0.5, 0), tck = -.02)

plot(0, 0, xlim = c(0.37, 0.67), 
     ylim = c(0, 20), 
     xlab = expression(theta), 
     xaxt = "n", ylab = "", yaxs = "i", yaxt = "n", bty = "n", cex.lab = 1.2)

axis(1, seq(0.3, 0.7, 0.1))
curve(dnorm(x, theta_hat_prior, se_prior), n = 1000, add = TRUE, col = "red")
text(0.588, 5, "Prior")
curve(dnorm(x, theta_hat_data, se_data), n = 1000, add = TRUE, col = "blue")
text(0.420, 8, "Likelihood")

# In this example, the prior standard error is 4.1% and the data standard error is 2.5%, 
# so the data are more informative than the prior, 
# and the Bayes estimate will be closer to the data.

# Indeed - the posterior or Bayes estimation is between the prior and data estimates, but closer to the data.

# Prior versus Likelihood versus Posterior
plot(0, 0, xlim = c(0.37, 0.67), 
     ylim = c(0, 20), 
     xlab = expression(theta), 
     xaxt = "n", ylab = "", yaxs = "i", yaxt = "n", bty = "n", cex.lab = 1.2) 

axis(1, seq(0.3, 0.7, 0.1))
curve(dnorm(x, theta_hat_prior, se_prior), n = 1000, add = TRUE, col = "red")
text(0.588, 5, "Prior")
curve(dnorm(x, theta_hat_data, se_data), n = 1000, add = TRUE, col = "blue")
text(0.420, 8, "Likelihood")
curve(dnorm(x, theta_hat_bayes, se_bayes), n = 1000, add = TRUE, col = "green")
text(0.525, 15, "Posterior")

# Now consider the same situation but with much more uncertain data. 
# Just take the same point estimate and increase the data standard error se-data from 0.025 to, say, 0.075. 
# The Bayes estimate then becomes 0.512; it is now closer to the prior.

#' #### Data estimate 
se_data2 <- 0.075
se_data2

#' #### Bayes estimate
theta_hat_bayes2 <- (theta_hat_prior/se_prior ^ 2 + theta_hat_data/se_data2 ^ 2) / (1/se_prior ^ 2 + 1/se_data2 ^ 2) 
theta_hat_bayes2

se_bayes2 <- sqrt(1/(1/se_prior ^ 2 + 1/se_data2 ^ 2))
se_bayes2

### Figure 
# Prior versus Likelihood versus Posterior versus Posterior 2
plot(0, 0, xlim = c(0.37, 0.67), 
     ylim = c(0, 20), 
     xlab = expression(theta), 
     xaxt = "n", ylab = "", yaxs = "i", yaxt = "n", bty = "n", cex.lab = 1.2) 

axis(1, seq(0.3, 0.7, 0.1))
curve(dnorm(x, theta_hat_prior, se_prior), n = 1000, add = TRUE, col = "red")
text(0.588, 5, "Prior")
curve(dnorm(x, theta_hat_data, se_data), n = 1000, add = TRUE, col = "blue")
text(0.420, 8, "Likelihood")
curve(dnorm(x, theta_hat_bayes, se_bayes), n = 1000, add = TRUE, col = "green")
text(0.525, 15, "Posterior - low uncertainty")
curve(dnorm(x, theta_hat_bayes2, se_bayes2), n = 1000, add = TRUE, col = "purple")
text(0.525, 11, "Posterior - high uncertainty")
