
# Make sure that working directory is set properly
# setwd("~/Dropbox/projects/strategic-mobilization/")

# Clear workspace
rm(list = ls())

# Load Data
d <- read.csv("output/mi-data.csv")

# standardize (min = 0, max = 1)
stdz <- function(x) {
  x.temp <- x - min(x, na.rm = TRUE)
  x.temp <- x.temp/max(x.temp, na.rm = TRUE)
  return(x.temp)
}

d$Age <- stdz(d$Age)
d$Education <- stdz(d$Education)
d$Household.Income <- stdz(d$Household.Income)
d$ENEP <- stdz(d$ENEP)

library(arm)
library(blme)
library(texreg)
library(compactr)

# Use hierarchical models and logits to check if number of parties changes
#   the results
m1 <- bglmer(Contacted ~ District.Competitiveness*PR + (1 | District) + (1 | Country) + (0 + District.Competitiveness | Country), data = d, family = binomial, cov.prior="gamma(shape = 2, rate = 0.001, posterior.scale='sd')")
m2 <- bglmer(Contacted ~ District.Competitiveness*PR + ENEP + (1 | District) + (1 | Country) + (0 + District.Competitiveness | Country), data = d, family = binomial, cov.prior="gamma(shape = 2, rate = 0.001, posterior.scale='sd')")
m3 <- bglmer(Contacted ~ District.Competitiveness*PR + ENEP*PR + (1 | District) + (1 | Country) + (0 + District.Competitiveness | Country), data = d, family = binomial, cov.prior="gamma(shape = 2, rate = 0.001, posterior.scale='sd')")

m4 <- bglmer(Contacted ~ District.Competitiveness*PR + (1 | District) + (1  | Country) + (0 + District.Competitiveness | Country) + Age + Male + Education + Married + Union.Member + Household.Income + Close.To.Party, data = d, family = binomial, cov.prior="gamma(shape = 2, rate = 0.001, posterior.scale='sd')")
m5 <- bglmer(Contacted ~ District.Competitiveness*PR + ENEP + (1 | District) + (1  | Country) + (0 + District.Competitiveness | Country) + Age + Male + Education + Married + Union.Member + Household.Income + Close.To.Party, data = d, family = binomial, cov.prior="gamma(shape = 2, rate = 0.001, posterior.scale='sd')")
m6 <- bglmer(Contacted ~ District.Competitiveness*PR + ENEP*PR + (1 | District) + (1  | Country) + (0 + District.Competitiveness | Country)  + Age + Male + Education + Married + Union.Member + Household.Income + Close.To.Party, data = d, family = binomial, cov.prior="gamma(shape = 2, rate = 0.001, posterior.scale='sd')")

anova(m1, m2, m3)
anova(m1, m3)
anova(m4, m5, m6)
anova(m4, m6)

BIC(m1, m2, m3, m4, m5, m6)


# Check that the results are robust to complete pooling.
m7 <- glm(Contacted ~ District.Competitiveness*PR, data = d, family = binomial)
m8 <- glm(Contacted ~ District.Competitiveness*PR + ENEP, data = d, family = binomial)
m9 <- glm(Contacted ~ District.Competitiveness*PR + ENEP*PR , data = d, family = binomial)

m10 <- glm(Contacted ~ District.Competitiveness*PR + Age + Male + Education + Married + Union.Member + Household.Income + Close.To.Party , data = d, family = binomial)
m11 <- glm(Contacted ~ District.Competitiveness*PR + ENEP + Age + Male + Education + Married + Union.Member + Household.Income + Close.To.Party , data = d, family = binomial)
m12 <- glm(Contacted ~ District.Competitiveness*PR + ENEP*PR  + Age + Male + Education + Married + Union.Member + Household.Income + Close.To.Party , data = d, family = binomial)

### Model Comparisons
# notice that m4, the specification I present in the paper, has the smallest BIC.
BIC(m7, m8, m9, m10, m11, m12)
# notice that adding number of parties does not significantly improve the fit
anova(m7, m8, m9, test = "Chisq")
anova(m7, m9, test = "Chisq")
# adding number of parties (m5) does significantly improve the fit when 
#   individual-level control variables are included. Including does not change
#   the results, so I've gone with the model suggested by the BIC. Also notice
#   that the estimate is small and changed directions depending on whether the 
#   individual
anova(m10, m11, m12, test = "Chisq")
anova(m10, m12, test = "Chisq")

texreg(list(m1, m2, m3, m4, m5, m6), stars = 0.05,
          custom.model.names = c("Model 1",
                                 "Model 2",
                                 "Model 3",
                                 "Model 4",
                                 "Model 5",
                                 "Model 6"),
          custom.coef.names = c("Intercept",
                                "Competitiveness",
                                "PR",
                                "Competitiveness $\\times$ PR",
                                "ENEP",
                                "ENEP $\\times$ PR",
                                "Age",
                                "Male",
                                "Education",
                                "Married",
                                "Union Member",
                                "Household Income",
                                "Close to a Party"),
          #reorder.gof = 2,
          dcolumn = TRUE,
       scriptsize = TRUE,
       float.pos = "h!",
          label = "tab:enep_partial",
       include.aic = FALSE,
       include.loglik = FALSE,
       include.deviance = FALSE,
       use.packages = FALSE,
       include.variance = FALSE,
       caption = "This table shows that the findings presented in the main text are robust to the inclusion of ENEP. Notice also that excluding ENEP leads to the smallest BIC, although some models including ENEP fit the data statistically significantly better (see Table \\ref{tab:F_partial} for the details).",
       file = "doc/tabs/enep_partial.tex"
       )

texreg(list(m7, m8, m9, m10, m11, m12), stars = 0.05,
       custom.model.names = c("Model 7",
                              "Model 8",
                              "Model 9",
                              "Model 10",
                              "Model 11",
                              "Model 12"),
       custom.coef.names = c("Intercept",
                             "Competitiveness",
                             "PR",
                             "Competitiveness $\\times$ PR",
                             "ENEP",
                             "ENEP $\\times$ PR",
                             "Age",
                             "Male",
                             "Education",
                             "Married",
                             "Union Member",
                             "Household Income",
                             "Close to a Party"),
       #reorder.gof = 2,
       dcolumn = TRUE,
       scriptsize = TRUE,
       float.pos = "h!",
       label = "tab:enep_complete",
       include.aic = FALSE,
       include.loglik = FALSE,
       include.deviance = FALSE,
       use.packages = FALSE,
       include.variance = FALSE,
       caption = "This table shows that the findings presented in the main text are robust to the inclusion of ENEP and complete pooling. Notice again that excluding ENEP leads to the smallest BIC, although some models including ENEP fit the data statistically significantly better (see Table \\ref{tab:F_complete} for the details).",
       file = "doc/tabs/enep_complete.tex"
)


# estimate logits in each country with only competitiveness
cntry <- c(3,1,2,4,5)
country.names <- c("Canada (SMDP)",
                   "Finland (PR)",
                   "Great Britain (SMDP)",
                   "Portugal 2002 (PR)",
                   "Portugal 2005 (PR)")
m <- list()
for (i in 1:5) {
  m[[i]] <- glm(Contacted ~ District.Competitiveness, data = d, family = "binomial",
                subset = Country == cntry[i])
}

texreg(list(m[[1]], m[[2]], m[[3]], m[[4]], m[[5]]), stars = 0.05,
       custom.model.names = country.names[cntry],
       custom.coef.names = c("Intercept",
                             "Competitiveness"),
       #reorder.gof = 2,
       dcolumn = TRUE,
       scriptsize = TRUE,
       float.pos = "h!",
       label = "tab:CbyC_componly",
       include.aic = FALSE,
       include.loglik = FALSE,
       include.deviance = FALSE,
       use.packages = FALSE,
       include.variance = FALSE,
       include.bic = FALSE,
       caption = "This table shows that the findings presented in the main text are robust to estimating separate models in each country with no individual-level covariates. Notice that the estimated effect of competitiveness is much larger in SMDP countries than in PR countries.",
       file = "doc/tabs/CbyC_componly.tex"
)


# estimate logits in each country with individual-level covariates
m <- list()
for (i in 1:5) {
  m[[i]] <- glm(Contacted ~ District.Competitiveness + Age + Male + Education + Married + Union.Member + Household.Income + Close.To.Party,
                family = "binomial", data = d,
                subset = Country == cntry[i])
}
texreg(list(m[[1]], m[[2]], m[[3]], m[[4]], m[[5]]), stars = 0.05,
       custom.model.names = country.names[cntry],
       custom.coef.names = c("Intercept",
                             "Competitiveness",
                             "Age",
                             "Male",
                             "Education",
                             "Married",
                             "Union Member",
                             "Household Income",
                             "Close to a Party"),
       #reorder.gof = 2,
       dcolumn = TRUE,
       scriptsize = TRUE,
       float.pos = "h!",
       label = "tab:CbyC_full",
       include.aic = FALSE,
       include.loglik = FALSE,
       include.deviance = FALSE,
       use.packages = FALSE,
       include.variance = FALSE,
       include.bic = FALSE,
       caption = "This table shows that the findings presented in the main text are robust to estimating separate models in each country with individual-level covariates. Notice that the estimated effect of competitiveness is much larger in SMDP countries than in PR countries.",
       file = "doc/tabs/CbyC_full.tex"
)


# for clarity, replicate the Country-by-Country figure using separate regressions
competitiveness0 <- seq(0, 1, length.out = 100)
pdf("doc/figs/CbyC_sep.pdf", height = 4, width = 8, family = "serif")
par(mfcol = c(2,3), family = "serif", 
    xaxs = "i", yaxs = "i",
    mar = rep(1, 4), oma = c(3,3,1,1))
for (i in 1:5) {
  eplot(xlim = c(0, 1), ylim = c(0,.8), 
        xlab = "District Competitiveness", ylabpos = 2,
        ylab = "Pr(Contact)",
        main = country.names[cntry[i]])
  # now add the estimates and CIs
  m <- glm(Contacted ~ District.Competitiveness + Age + Male + Education + Married + Union.Member + Household.Income + Close.To.Party, family = "binomial",
           subset = Country == cntry[i], data = d)
  sims <- coef(sim(m, 1000))
  gamma0k <- sims[, 1]
  gamma1k <- sims[, 2]
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
