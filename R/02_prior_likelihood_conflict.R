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
