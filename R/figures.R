
# Make sure that working directory is set properly
# setwd("~/Dropbox/projects/strategic-mobilization/")

# Clear workspace
rm(list = ls())

# Load packages
library(compactr) # for plotting

# Load simulations
load("output/mcmc-sims.RData")

# Load the individual-level (with missingness), district-level, and country-level data.
mis.data <- read.csv("output/mi-data.csv")
district.data <- read.csv("output/district-data.csv")
country.data <- read.csv("output/country-data.csv")


#################################################################
## Figure Showing Pr(Conctact) as Competitiveness Varies in 
##   Both SMDP and PR Systems
#################################################################

# Choose the set of sims to use.
sims <- sims.weak

# Set values of predictors
smdp0 <- 1
competitiveness0 <- seq(0, 1, length.out = 100)

# Transform parameter simulations to QI simulations when SMDP = 1
pr.sims.left <- matrix(NA, nrow = nrow(sims), ncol = length(competitiveness0))
for (i in 1:nrow(sims)) {
  pr.sims.left[i, ] <- plogis(sims[i, "delta0[1]"] + sims[i, "delta0[2]"]*smdp0 +  # gamma_0k
                                (sims[i, "delta1[1]"] + sims[i, "delta1[2]"]*smdp0)*competitiveness0)
  
}
q.pr.sims.left <- apply(pr.sims.left, 2, quantile, c(0.05, .5, .95))

# Transform parameter simulations to QI simulations when SMDP = 0
smdp0 <- 0
pr.sims.right <- matrix(NA, nrow = nrow(sims), ncol = length(competitiveness0))
for (i in 1:nrow(sims)) {
  pr.sims.right[i, ] <- plogis(sims[i, "delta0[1]"] + sims[i, "delta0[2]"]*smdp0 +  # gamma_0k
                                 (sims[i, "delta1[1]"] + sims[i, "delta1[2]"]*smdp0)*competitiveness0)
  
}
q.pr.sims.right <- apply(pr.sims.right, 2, quantile, c(0.05, .5, .95))

# Prepare the data for the background histogram.
h.left <- hist(district.data$District.Competitiveness[district.data$SMDP == 1], plot = FALSE)
h.right <- hist(district.data$District.Competitiveness[district.data$SMDP == 0], plot = FALSE)
h.left$counts <- .8*h.left$counts/(1.1*max(c(h.left$counts)))
h.right$counts <- .8*h.right$counts/(1.1*max(c(h.right$counts)))

pdf("doc/figs/prob.pdf", height = 3, width = 8, family = "serif")
par(mfrow = c(1,2), family = "serif", 
    xaxs = "i", yaxs = "i",
    mar = c(.5, 1, .5, 1), oma = c(2.5,2.5,1,1))
plot(h.left, xlim = c(0, 1), ylim = c(0, .8),
     axes = FALSE, border = NA, main = NA, col = "grey80")
par(new = TRUE)
eplot(xlim = c(0, 1), ylim = c(0, .8),
      xlab = "District Competitiveness",
      ylab = "Pr(Contact)", ylabpos = 2,
      main = "SMDP")
lines(competitiveness0, q.pr.sims.left[2,], lwd = 3)
lines(competitiveness0, q.pr.sims.left[3,], lwd = 1, lty = 3)
lines(competitiveness0, q.pr.sims.left[1,], lwd = 1,lty = 3)

plot(h.right, xlim = c(0, 1), ylim = c(0, .8),
     axes = FALSE, border = NA, main = NA, col = "grey80")
par(new = TRUE)
aplot("PR")
lines(competitiveness0, q.pr.sims.right[2,], lwd = 3)
lines(competitiveness0, q.pr.sims.right[3,], lwd = 1, lty = 3)
lines(competitiveness0, q.pr.sims.right[1,], lwd = 1,lty = 3)
dev.off()


#################################################################
## Figure Showing the Effect of Disproportionality as 
##   Competitivness Varies
#################################################################

pr.sims.dif <- pr.sims.left - pr.sims.right
q.pr.sims.dif <- apply(pr.sims.dif, 2, quantile, c(0.05, .5, .95))

pdf("doc/figs/fd_smdp.pdf", height = 3.5, width = 5, family = "serif")
par(mfrow = c(1,1), family = "serif", 
    xaxs = "i", yaxs = "i",
    mar = rep(0, 4), oma = c(3,4,1,1))
eplot(xlim = c(0, 1), ylim = c(-.3, .7),
      xlab = "District Competitiveness",
      ylab = "Pr(Contact | SMDP) - Pr(Contact | PR)", ylabpos = 2)
abline(h = 0)
lines(competitiveness0, q.pr.sims.dif[2,], lwd = 3)
lines(competitiveness0, q.pr.sims.dif[3,], lwd = 1, lty = 3)
lines(competitiveness0, q.pr.sims.dif[1,], lwd = 1,lty = 3)
dev.off()

#################################################################
## Figure Showing the ME of Competitiveness on Pr(Conctact) as 
##   Competitiveness Varies in Both SMDP and PR Systems
#################################################################

# Set values of SMDP
smdp0 <- 1

# Transform parameter simulations to QI simulations when SMDP = 1
me.sims.left <- matrix(NA, nrow = nrow(sims), ncol = length(competitiveness0))
for (i in 1:nrow(sims)) {
  me.sims.left[i, ] <- dlogis(sims[i, "delta0[1]"] + sims[i, "delta0[2]"]*smdp0 +  # gamma_0k
                                (sims[i, "delta1[1]"] + sims[i, "delta1[2]"]*smdp0)*competitiveness0)*
    (sims[i, "delta1[1]"] + sims[i, "delta1[2]"]*smdp0)
  
}
q.me.sims.left <- apply(me.sims.left, 2, quantile, c(0.05, .5, .95))

# Transform parameter simulations to QI simulations when SMDP = 0
smdp0 <- 0
me.sims.right <- matrix(NA, nrow = nrow(sims), ncol = length(competitiveness0))
for (i in 1:nrow(sims)) {
  me.sims.right[i, ] <- dlogis(sims[i, "delta0[1]"] + sims[i, "delta0[2]"]*smdp0 +  # gamma_0k
                                 (sims[i, "delta1[1]"] + sims[i, "delta1[2]"]*smdp0)*competitiveness0)*
    (sims[i, "delta1[1]"] + sims[i, "delta1[2]"]*smdp0)
  
}
q.me.sims.right <- apply(me.sims.right, 2, quantile, c(0.05, .5, .95))

# Prepare the data for the background histogram.
h.left <- hist(district.data$District.Competitiveness[district.data$SMDP == 1], plot = FALSE)
h.right <- hist(district.data$District.Competitiveness[district.data$SMDP == 0], plot = FALSE)
h.left$counts <- h.left$counts/(1.1*max(c(h.left$counts)))
h.right$counts <- h.right$counts/(1.1*max(c(h.right$counts)))

pdf("doc/figs/me_competitiveness.pdf", height = 3, width = 8, family = "serif")
par(mfrow = c(1,2), family = "serif", 
    xaxs = "i", yaxs = "i",
    mar = c(.5, 1, .5, 1), oma = c(2.5,3.5,1,1))
plot(h.left, xlim = c(0, 1), ylim = c(0, 1),
     axes = FALSE, border = NA, main = NA, col = "grey80")
par(new = TRUE)
eplot(xlim = c(0, 1), ylim = c(-.1, 1.2),
      xlab = "District Competitiveness",
      ylab = "Marginal Effect of\nCompetitiveness on Pr(Contact)", ylabpos = 2,
      main = "SMDP")
abline(h = 0)
lines(competitiveness0, q.me.sims.left[2,], lwd = 3)
lines(competitiveness0, q.me.sims.left[3,], lwd = 1, lty = 3)
lines(competitiveness0, q.me.sims.left[1,], lwd = 1,lty = 3)

plot(h.right, xlim = c(0, 1), ylim = c(0, 1),
     axes = FALSE, border = NA, main = NA, col = "grey80")
par(new = TRUE)
aplot("PR")
lines(competitiveness0, q.me.sims.right[2,], lwd = 3)
lines(competitiveness0, q.me.sims.right[3,], lwd = 1, lty = 3)
lines(competitiveness0, q.me.sims.right[1,], lwd = 1,lty = 3)
abline(h = 0)
dev.off()

#################################################################
## Figure Showing the Difference in the ME Across SMDP and PR
##   Systems as Competitiveness Varies
#################################################################

me.sims.dif <- me.sims.left - me.sims.right
q.me.sims.dif <- apply(me.sims.dif, 2, quantile, c(0.05, .5, .95))

pdf("doc/figs/interaction.pdf", height = 3.5, width = 5, family = "serif")
par(mfrow = c(1,1), family = "serif", 
    xaxs = "i", yaxs = "i",
    mar = rep(0, 4), oma = c(3,4,1,1))
eplot(xlim = c(0, 1), ylim = c(-.1, 1.1),
      xlab = "District Competitiveness",
      ylab = "Difference in Marginal Effect\nAcross SMDP and PR Systems", ylabpos = 2)
abline(h = 0)
lines(competitiveness0, q.me.sims.dif[2,], lwd = 3)
lines(competitiveness0, q.me.sims.dif[3,], lwd = 1, lty = 3)
lines(competitiveness0, q.me.sims.dif[1,], lwd = 1, lty = 3)
dev.off()

#################################################################
## Figure Showing Pr(Conctact) as Competitiveness Varies in 
##   Across All Countries in the Data Set
#################################################################

country.indices <- c(3,1,4,2,5)
country.names <- c("Canada (SMDP)",
                   "Finland (PR)",
                   "Great Britain (SMDP)",
                   "Portugal 2002 (PR)",
                   "Portugal 2005 (PR)")

pdf("doc/figs/CbyC.pdf", height = 4, width = 8, family = "serif")
par(mfcol = c(2,3), family = "serif", 
    xaxs = "i", yaxs = "i",
    mar = rep(1, 4), oma = c(3,3,1,1))
for (country.index in country.indices) {
  eplot(xlim = c(0, 1), ylim = c(0,.8), 
        xlab = "District Competitiveness", ylabpos = 2,
        ylab = "Pr(Contact)",
        main = country.names[country.index])
  current.district.data <- district.data[district.data$Country == country.index, ]
  # add the lines first
  for (j in 1:nrow(current.district.data)) {
    alpha.sims <- sims[, paste("alpha[", current.district.data$District[j], "]", sep = "")]
    q.pr.sims <- quantile(plogis(alpha.sims), c(0.05, 0.5, 0.95))
    comp0 <- current.district.data$District.Competitiveness[j]
    lines(c(comp0, comp0), q.pr.sims[c(1,3)], col = "grey80")
  }
  # then the points
  for (j in 1:nrow(current.district.data)) {
    alpha.sims <- sims[, paste("alpha[", current.district.data$District[j], "]", sep = "")]
    q.pr.sims <- quantile(plogis(alpha.sims), c(0.05, 0.5, 0.95))
    comp0 <- current.district.data$District.Competitiveness[j]
    points(comp0, q.pr.sims[2], pch = 19, col = "grey50", cex = .9)
  }
  # now add the estimates and CIs
  gamma0k <- sims[, paste("gamma0[", country.index, "]", sep = "")]
  gamma1k <- sims[, paste("gamma1[", country.index, "]", sep = "")]
  pr.sims <- matrix(NA, nrow = nrow(sims), ncol = length(competitiveness0))
  for (i in 1:nrow(pr.sims)) {
    pr.sims[i, ] <- plogis(gamma0k[i] + gamma1k[i]*competitiveness0)
  }
  q.pr.sims <- apply(pr.sims, 2, quantile, c(0.05, .5, 0.95))
  lines(competitiveness0, q.pr.sims[2, ], lwd = 3)
  lines(competitiveness0, q.pr.sims[1, ], lwd = 1, lty = 3)
  lines(competitiveness0, q.pr.sims[3, ], lwd = 1, lty = 3)
} 
addxaxis()
dev.off()

#################################################################
## Plots of Variance Priors and Posteriors
#################################################################

# Show Priors from 0 to 9
pdf("doc/figs/variance_priors.pdf", height = 3, width = 6, family = "serif")
par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(2.5, 3, 1,1),
    family = "serif", 
    xaxs = "i", yaxs = "i")
eplot(xlim = c(0, 9), ylim = c(0, 0.5),
      xlab = "Standard Deviation Parameter",
      ylab = "Prior Density", ylabpos = 2,
      xat = c(0, 3, 6, 9))
polygon(c(0, 3, 3, 0), c(0, 0, 1, 1),
        border = NA, col = "grey80")
polygon(c(3, 10, 10, 3), c(0, 0, 1, 1),
        border = NA, col = "grey60")
text(1.5, .5, "Reasonable Range", pos = 1)
text(6, .5, "Uneasonable Range", pos = 1)
curve(2*dcauchy(x, 0, 5), add = TRUE, lwd = 2, lty = 1)
curve(dunif(x, 0, 100), add = TRUE, lwd = 2, lty = 2)
curve(2*dnorm(x, 0, 2), add = TRUE, lwd = 2, lty = 3)
text(1, .05, "uniform(0, 100)\n(i.e. \"flat\")", cex = .8)
text(1, .16, "half-Cachy(5)\n(i.e. \"weak\")", cex = .8)
text(2, .37, "half-normal(2)\n(i.e. \"informative\")", cex = .8)
box()
dev.off()

# Plot of posteriors of variance parameters

pdf("doc/figs/variance_posteriors.pdf", height = 2, width = 8, family = "serif")
par(mfrow = c(1,3), family = "serif", 
    xaxs = "i", yaxs = "i",
    mar = c(.5, 1, .5, 1), oma = c(2.5,3.5,2,1))
eplot(xlim = c(0, 2), ylim = c(0, 6),
      xlab = expression(sigma[alpha]),
      ylab = "Posterior Density",
      main = "Standard Deviation of\nDistrict-Level Intercept")
lines(density(sims.weak[, "sigma.alpha"]), lwd = 2, lty = 1)
lines(density(sims.flat[, "sigma.alpha"]), lwd = 2, lty = 2, col = "red")
lines(density(sims.info[, "sigma.alpha"]), lwd = 2, lty = 3, col = "blue")
text(1.2, 4, "Posteriors overlap\nsubstantially.")

eplot(xlim = c(0, 2), ylim = c(0, 6),
      xlab = expression(sigma[gamma[0]]),
      ylab = "Posterior Density",
      main = "Standard Deviation of\nCountry-Level Intercept")
lines(density(sims.weak[, "sigma.gamma0"]), lwd = 2, lty = 1)
lines(density(sims.flat[, "sigma.gamma0"]), lwd = 2, lty = 2, col = "red")
lines(density(sims.info[, "sigma.gamma0"]), lwd = 2, lty = 3, col = "blue")

eplot(xlim = c(0, 2), ylim = c(0, 6),
      xlab = expression(sigma[gamma[1]]),
      ylab = "Posterior Density",
      main = "Standard Deviation of\nCountry-Level Effect")
lines(density(sims.weak[, "sigma.gamma1"]), lwd = 2, lty = 1)
lines(density(sims.flat[, "sigma.gamma1"]), lwd = 2, lty = 2, col = "red")
lines(density(sims.info[, "sigma.gamma1"]), lwd = 2, lty = 3, col = "blue")
legend(2, 6, xjust = 1, yjust = 1, legend = c("half-Cauchy(5)", "uniform(0, 100)", "half-normal(2)"),
       lty = c(1,2,3), col = c("black", "red", "blue"), bty = "n", lwd = 2)
dev.off()

# #################################################################
# ## Plots of Posterior Distributions (Not Used in MS)
# #################################################################
# 
# par(mfrow = c(2,2), family = "serif", 
#     xaxs = "i", yaxs = "i",
#     mar = c(.5, 1, 1, 1), oma = c(2.5,3.5,2,1))
# eplot(xlim = c(-5, 5), ylim = c(0, 1),
#       xlab = "Parameter", 
#       ylab = "Posterior Density",
#       main = expression(delta["00"]))
# lines(density(sims.weak[, "delta0[1]"]))
# abline(v = 0, lty = 3)
# 
# aplot(expression(delta["01"]))
# lines(density(sims.weak[, "delta0[2]"]))
# abline(v = 0, lty = 3)
# 
# aplot(expression(delta[10]))
# lines(density(sims.weak[, "delta1[1]"]))
# abline(v = 0, lty = 3)
# 
# aplot(expression(delta[11]))
# lines(density(sims.weak[, "delta1[2]"]))
# abline(v = 0, lty = 3)
# 
# par(mfrow = c(3,3), family = "serif", 
#     xaxs = "i", yaxs = "i",
#     mar = c(.5, 1, 1, 1), oma = c(2.5,3.5,2,1))
# eplot(xlim = c(-1, 2), ylim = c(0, 8),
#       xlab = "Parameter", 
#       ylab = "Posterior Density",
#       main = expression(beta[age]))
# lines(density(sims.weak[, "beta.age"]))
# abline(v = 0, lty = 3)
# 
# aplot(expression(beta[male]))
# lines(density(sims.weak[, "beta.male"]))
# abline(v = 0, lty = 3)
# 
# aplot(expression(beta[edu]))
# lines(density(sims.weak[, "beta.edu"]))
# abline(v = 0, lty = 3)
# 
# aplot(expression(beta[union]))
# lines(density(sims.weak[, "beta.union"]))
# abline(v = 0, lty = 3)
# 
# aplot(expression(beta[income]))
# lines(density(sims.weak[, "beta.income"]))
# abline(v = 0, lty = 3)
# 
# aplot(expression(beta[urban]))
# lines(density(sims.weak[, "beta.urban"]))
# abline(v = 0, lty = 3)
# 
# aplot(expression(beta[close]))
# lines(density(sims.weak[, "beta.close"]))
# abline(v = 0, lty = 3)