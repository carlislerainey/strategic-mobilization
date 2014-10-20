
# Make sure that working directory is set properly
# setwd("~/Dropbox/projects/strategic-mobilization/")

# Clear workspace
rm(list = ls())

# Load packages
library(Amelia) # for multiple imputation
library(R2jags) # run JAGS from R

# Load the individual-level (with missingness), district-level, and country-level data.
mis.data <- read.csv("output/mi-data.csv")
district.data <- read.csv("output/district-data.csv")
country.data <- read.csv("output/country-data.csv")

n.chains <- 10 # Each chain starts by multiply imputing the data sets and then running the JAGS model
sims.weak <- NULL  # place to hold mcmc simulations
sims.flat <- NULL  # place to hold mcmc simulations
sims.info <- NULL  # place to hold mcmc simulations
for (chain in 1:n.chains) {
  ###########################################################
  ## Start by multiply imputing the individual-level data set
  ###########################################################
  # Create data sets from each country to be separately multiply imputed
  mis.Canada <- mis.data[mis.data$Alpha.Polity == "Canada", ]
  mis.Finland <- mis.data[mis.data$Alpha.Polity == "Finland", ]
  mis.GreatBritain <- mis.data[mis.data$Alpha.Polity == "Great Britain", ]
  mis.Portugal2002 <- mis.data[mis.data$Alpha.Polity == "Portugal 2002", ]
  mis.Portugal2005 <- mis.data[mis.data$Alpha.Polity == "Portugal 2005", ]
  
  # Impute the datasets
  allmis <- names(mis.Canada) %in% c("Religious.Attendance")
  mis.Canada <- mis.Canada[!allmis] # drop variables that are entirely missing from the process
  mi.Canada <- amelia(mis.Canada, m = 1,
                      idvars = c("X", "Alpha.Polity", "District", "PR", "Number.Seats", "Country"),
                      lgstc = c("District.Competitiveness"),
                      logs = c("ENEP") ,                 
                      ords = c("Male", "Education", "Married", "Union.Member", 
                               "Household.Income", "Urban", "Campaign.Activities",
                               "Freq.Campaign", "Contacted", "Cast.Ballot", 
                               "Vote.Matters", "Cast.Ballot.Previous", "Close.To.Party",
                               "Ideology", "Know1", "Know2", "Know3"))
  
  allmis <- names(mis.Finland) %in% c("Religious.Attendance")
  mis.Finland <- mis.Finland[!allmis] # drop variables that are entirely missing from the process
  mi.Finland <- amelia(mis.Finland,  m = 1,
                       idvars = c("X", "Alpha.Polity", "District", "PR", "Country"),
                       lgstc = c("District.Competitiveness"),
                       logs = c("ENEP"),
                       ords = c("Male", "Education", "Married", "Union.Member", 
                                "Household.Income", "Urban", "Campaign.Activities",
                                "Freq.Campaign", "Contacted", "Cast.Ballot", 
                                "Vote.Matters", "Cast.Ballot.Previous", "Close.To.Party",
                                "Ideology", "Know1", "Know2", "Know3"))
  
  mi.GreatBritain <- amelia(mis.GreatBritain,  m = 1,
                            idvars = c("X", "Alpha.Polity", "District", "PR", "Number.Seats", "Country"),
                            lgstc = c("District.Competitiveness"),
                            logs = c("ENEP"),
                            ords = c("Male", "Education", "Married", "Union.Member", 
                                     "Household.Income", "Urban", "Campaign.Activities",
                                     "Freq.Campaign", "Religious.Attendance", "Contacted", "Cast.Ballot", 
                                     "Vote.Matters", "Cast.Ballot.Previous", "Close.To.Party",
                                     "Ideology", "Know1", "Know2", "Know3"))
  
  mi.Portugal2002 <- amelia(mis.Portugal2002,  m = 1,
                            idvars = c("X", "Alpha.Polity", "District", "PR", "Country"),
                            lgstc = c("District.Competitiveness"),
                            logs = c("ENEP"),
                            ords = c("Male", "Education", "Married", "Union.Member", 
                                     "Household.Income", "Urban", "Campaign.Activities",
                                     "Freq.Campaign", "Religious.Attendance", "Contacted", "Cast.Ballot", 
                                     "Vote.Matters", "Cast.Ballot.Previous", "Close.To.Party",
                                     "Ideology", "Know1", "Know2", "Know3"))
  
  allmis <- names(mis.Portugal2005) %in% c("Campaign.Activities", "Freq.Campaign")
  mis.Portugal2005 <- mis.Portugal2005[!allmis] # drop variables that are entirely missing from the process
  mi.Portugal2005 <- amelia(mis.Portugal2005,  m = 1,
                            idvars = c("X", "Alpha.Polity", "District", "PR", "Country"),
                            lgstc = c("District.Competitiveness"),
                            logs = c("ENEP"),
                            ords = c("Male", "Education", "Married", "Union.Member", 
                                     "Household.Income", "Urban", "Religious.Attendance", "Contacted", "Cast.Ballot", 
                                     "Vote.Matters", "Cast.Ballot.Previous", "Close.To.Party",
                                     "Ideology", "Know1", "Know2", "Know3"))    
  
  analysis.vars <- c("Contacted", "Age", "Male", "Education", "Union.Member", "Household.Income", "Urban", "Close.To.Party", "District.Competitiveness", "ENEP", "PR", "Alpha.Polity", "District")
  
  # Stack the (individual-level) multiply imputed data sets
  individual.data <- rbind(mi.Canada[[1]]$imp1[, analysis.vars],
                           mi.GreatBritain[[1]]$imp1[, analysis.vars],
                           mi.Finland[[1]]$imp1[, analysis.vars],
                           mi.Portugal2002[[1]]$imp1[, analysis.vars],
                           mi.Portugal2005[[1]]$imp1[, analysis.vars])
  
  ####################################################################
  ## Now use the multiply imputed data to estimate the model with JAGS
  ####################################################################
  
  # set the variables as objects in the environment
  n <- nrow(individual.data)
  n.districts <- nrow(district.data)
  n.countries <- nrow(country.data)
  contacted <- individual.data$Contacted
  
  stdz <- function(x) {
    x.temp <- x - min(x)
    x.temp <- x.temp/max(x.temp)
    x.temp <- x.temp - median(x.temp)
    return(x.temp)
  }
  age <- stdz(individual.data$Age)
  male <- stdz(individual.data$Male)
  education <- stdz(individual.data$Education)
  union <- stdz(individual.data$Union.Member)
  income <- stdz(individual.data$Household.Income)
  urban <- stdz(individual.data$Urban)
  close <- stdz(individual.data$Close.To.Party)
  competitiveness <- district.data$District.Competitiveness
  smdp <- country.data$SMDP
  district <- individual.data$District
  country <- district.data$Country
  X <- cbind(age, male, education, union, income, urban, close)
  
  # Set up prior parameters
  d0 <- rep(0, 2)
  D0 <- matrix(0, ncol = 2, nrow = 2)
  diag(D0) <- .001
  d1 <- rep(0, 2)
  D1 <- matrix(0, ncol = 2, nrow = 2)
  diag(D1) <- .001
  
  # Set up objects for jags call
  data <- list(contacted=contacted, 
               n=n, 
               n.districts=n.districts, 
               n.countries=n.countries, 
               district=district, 
               country=country,
               age=age,
               male=male,
               education=education,
               union=union,
               income=income,
               urban=urban,
               close=close,
               competitiveness=competitiveness,
               smdp=smdp,
               d0=d0,
               D0=D0,
               d1=d1,
               D1=D1)
  
  param <- c("alpha", 
             "beta.age", "beta.male", "beta.edu", "beta.union", "beta.income",
             "beta.urban", "beta.close",
             "gamma0", "gamma1",
             "delta0", "delta1", 
             "sigma.alpha", "sigma.gamma0", "sigma.gamma1", "rho",
             "p.track")
  inits <- function() {
    list ("alpha" = rnorm(n.districts), 
          "Gamma" = array(rnorm(2*n.countries), c(n.countries, 2)), 
          "beta.age" = rnorm(1, 0, 2), 
          "beta.male" = rnorm(1, 0, 2), 
          "beta.edu" = rnorm(1, 0, 2), 
          "beta.union" = rnorm(1, 0, 2), 
          "beta.income" = rnorm(1, 0, 2), 
          "beta.urban" = rnorm(1, 0, 2), 
          "beta.close" = rnorm(1, 0, 2), 
          "delta0" = rnorm(2), 
          "delta1" = rnorm(2), 
          "sigma.alpha" = runif(1, 0, 10), 
          "sigma.gamma0" = runif(1, 0, 10), 
          "sigma.gamma1" = runif(1, 0, 10), 
          "rho" = runif(1, -1, .1))   
  }
  n.thin <- 10
  n.iter <- 15000
  n.burnin <- 5000
  # Weakly informative variance priors
  m.weak <- jags(model.file = "bugs/weak.bugs",
                 data = data,
                 inits = inits,
                 param = param,
                 n.chains = 1,
                 n.thin = n.thin,
                 n.burnin = n.burnin,#n.burnin,
                 n.iter = n.iter,#n.iter,
                 DIC = FALSE)
  sims.weak <- rbind(sims.weak, m.weak$BUGSoutput$sims.matrix)
  # flat variance priors
  m.flat <- jags(model.file = "bugs/flat.bugs",
                 data = data,
                 inits = inits,
                 param = param,
                 n.chains = 1,
                 n.thin = n.thin,
                 n.burnin = n.burnin,#n.burnin,
                 n.iter = n.iter,#n.iter,
                 DIC = FALSE)
  sims.flat <- rbind(sims.flat, m.flat$BUGSoutput$sims.matrix)
  # informative variance priors
  m.info <- jags(model.file = "bugs/info.bugs",
                 data = data,
                 inits = inits,
                 param = param,
                 n.chains = 1,
                 n.thin = n.thin,
                 n.burnin = n.burnin,#n.burnin,
                 n.iter = n.iter,#n.iter,
                 DIC = FALSE)
  sims.info <- rbind(sims.info, m.info$BUGSoutput$sims.matrix)
}

save(sims.weak, sims.info, sims.flat, file = "output/mcmc-sims.RData")

