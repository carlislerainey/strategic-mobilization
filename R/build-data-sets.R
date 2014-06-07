
# Make sure that working directory is set properly
# setwd("~/Dropbox/projects/strategic-mobilization/")

# Clear workspace
rm(list = ls())

# Read in the raw data from the CSES Module 2 data set
cses2 <- read.csv("data/cses2_rawdata.txt")

#  Pull out variables of interest
mycses2 <- c("B1004", "B2001", "B2002", "B2003", "B2004", "B2005", "B2020", "B2023", "B2030", "B2031", "B3001_2", "B3002_2", "B3003", "B3004_1", "B3014", "B3016", "B3028", "B3045", "B3047_1", "B3047_2", "B3047_3", "B4001", "B4002", "B4003", "B4004_A", "B4004_B", "B4004_C", "B4004_D", "B4004_E", "B4004_F", "B4005", "B5043_1")
cses2  <- cses2[, mycses2]

# 	Change the variable names
names(cses2) <- c("Alpha.Polity", "Age", "Male", "Education", "Married", "Union.Member", "Household.Income", "Religious.Attendance", "Urban", "District", "Campaign.Activities", "Freq.Campaign", "Contacted", "Cast.Ballot", "Vote.Matters", "Cast.Ballot.Previous", "Close.To.Party", "Ideology", "Know1", "Know2", "Know3", "Number.Seats", "Number.Candidates", "Number.Lists", "VoteA",  "VoteB", "VoteC", "VoteD", "VoteE", "VoteF", "District.Turnout", "Electoral.Formula")

# Drop countries for which there is not information about the electoral district
cses2 <- cses2[cses2$District!= 99999, ]
cses2 <- cses2[cses2$Number.Seats != 999, ]


####	Recode and Create Variables

# Alpha.Polity

cses2$Alpha.Polity <- as.character(cses2$Alpha.Polity)

cses2$Alpha.Polity[cses2$Alpha.Polity=="CAN_2004"] <- "Canada"
cses2$Alpha.Polity[cses2$Alpha.Polity=="FIN_2003"] <- "Finland"
cses2$Alpha.Polity[cses2$Alpha.Polity=="GBR_2005"] <- "Great Britain"
cses2$Alpha.Polity[cses2$Alpha.Polity=="PRT_2002"] <- "Portugal 2002"
cses2$Alpha.Polity[cses2$Alpha.Polity=="PRT_2005"] <- "Portugal 2005"

cses2 <- cses2[cses2$Alpha.Polity == "Canada" 	|
                 cses2$Alpha.Polity == "Finland"	|
                 cses2$Alpha.Polity == "Great Britain"	|
                 cses2$Alpha.Polity == "Portugal 2002"|
                 cses2$Alpha.Polity == "Portugal 2005", ]

cses2$Alpha.Polity <- as.factor(cses2$Alpha.Polity)

# Age
cses2$Age[cses2$Age>=997] <- NA
summary(cses2$Age)

# Sex
cses2$Male[cses2$Male==1] <- 1
cses2$Male[cses2$Male==2] <- 0
cses2$Male[cses2$Male>2] <- NA
summary(cses2$Male)

# Education
cses2$Education[cses2$Education>=9] <- NA
summary(cses2$Educatioin)

# Marital Status
cses2$Married[cses2$Married==1] <- 1
cses2$Married[cses2$Married>1 & cses2$Married<7] <- 0
cses2$Married[cses2$Married>6] <- NA
summary(cses2$Marital.Status)

# Union Membership
cses2$Union.Member[cses2$Union.Member==1] <-1
cses2$Union.Member[cses2$Union.Member==2] <-0
cses2$Union.Member[cses2$Union.Member>2] <-NA
summary(cses2$Union.Member)

# Household.Income
cses2$Household.Income[cses2$Household.Income==9] <- NA
cses2$Household.Income[cses2$Household.Income==6] <- NA
cses2$Household.Income[cses2$Household.Income==7] <- NA
cses2$Household.Income[cses2$Household.Income==8] <- NA
summary(cses2$Household.Income)

# Religious.Attendance
cses2$Religious.Attendance[cses2$Religious.Attendance >= 7] <- NA
cses2$Religious.Attendance[cses2$Religious.Attendance < 5] <- 0
cses2$Religious.Attendance[cses2$Religious.Attendance >= 5] <- 1
summary(cses2$Religious.Attendance)

#Rural or Urban Residence
cses2$Urban[cses2$Urban >= 7] <- NA
cses2$Urban[cses2$Urban < 4] <- 0
cses2$Urban[cses2$Urban == 4] <- 1
summary(cses2$Urban)

# Campaign.Activites
cses2$Campaign.Activities[cses2$Campaign.Activities == 9] <- NA
cses2$Campaign.Activities[cses2$Campaign.Activities == 8] <- NA
cses2$Campaign.Activities[cses2$Campaign.Activities == 7] <- NA
cses2$Campaign.Activities[cses2$Campaign.Activities == 2] <- 0
summary(cses2$Campaign.Activities)

# Freq.Campaign
cses2$Freq.Campaign[cses2$Freq.Campaign >= 7] <- NA
cses2$Freq.Campaign[cses2$Campaign.Activities == 0] <- 0
summary(cses2$Freq.Campaign)

# Contacted
cses2$Contacted[cses2$Contacted == 9] <- NA
cses2$Contacted[cses2$Contacted == 8] <- NA
cses2$Contacted[cses2$Contacted == 7] <- NA
cses2$Contacted[cses2$Contacted == 2] <- 0
summary(cses2$Contacted)

# Cast.Ballot
cses2$Cast.Ballot.Alt <- cses2$Cast.Ballot
cses2$Cast.Ballot[cses2$Cast.Ballot==9] <- NA
cses2$Cast.Ballot[cses2$Cast.Ballot==8] <- 0
cses2$Cast.Ballot[cses2$Cast.Ballot==7] <- 0
cses2$Cast.Ballot[cses2$Cast.Ballot==6] <- NA
cses2$Cast.Ballot[cses2$Cast.Ballot==4] <- 0
cses2$Cast.Ballot[cses2$Cast.Ballot==2] <- 0
summary(cses2$Cast.Ballot)

# Vote.Matters
cses2$Vote.Matters[cses2$Vote.Matters>5] <- NA
summary(cses2$Vote.Matters)

# Cast.Ballot. Previous
cses2$Cast.Ballot.Previous[cses2$Cast.Ballot.Previous==9] <- NA
cses2$Cast.Ballot.Previous[cses2$Cast.Ballot.Previous==8] <- 0
cses2$Cast.Ballot.Previous[cses2$Cast.Ballot.Previous==7] <- 0
cses2$Cast.Ballot.Previous[cses2$Cast.Ballot.Previous==6] <- NA
cses2$Cast.Ballot.Previous[cses2$Cast.Ballot.Previous==4] <- 0
cses2$Cast.Ballot.Previous[cses2$Cast.Ballot.Previous==2] <- 0
summary(cses2$Cast.Ballot.Previous)

# Close.To.Party
cses2$Close.To.Party[cses2$Close.To.Party==9] <- NA
cses2$Close.To.Party[cses2$Close.To.Party==8] <- NA
cses2$Close.To.Party[cses2$Close.To.Party==7] <- NA
cses2$Close.To.Party[cses2$Close.To.Party==2] <- 0
summary(cses2$Close.To.Party)

#Ideology
cses2$Ideology.Alt <- cses2$Ideology
cses2$Ideology[cses2$Ideology==99] <- 5
cses2$Ideology[cses2$Ideology==98] <- 5
cses2$Ideology[cses2$Ideology==97] <- 5
cses2$Ideology[cses2$Ideology==96] <- 5

#Political.Knowledge
cses2$Know1[cses2$Know1==9]  <- NA
cses2$Know1[cses2$Know1>1] <- 0
cses2$Know2[cses2$Know2==9]  <- NA
cses2$Know2[cses2$Know2>1] <- 0
cses2$Know3[cses2$Know3==9]  <- NA
cses2$Know3[cses2$Know3>1] <- 0
cses2$Political.Knowledge <- cses2$Know1+cses2$Know2+cses2$Know3

# Number.Seats
cses2$Number.Seats[cses2$Number.Seats==999] <- NA
cses2$District.Magnitude <- cses2$Number.Seats
cses2$log.District.Magnitude <- log(cses2$District.Magnitude)

# Number.Candidates
cses2$Number.Candidates[cses2$Number.Candidates==9999] <- NA
cses2$Number.Candidates[cses2$Number.Candidates==0000] <- NA

# Number.Lists
cses2$Number.Lists[cses2$Number.Lists==999] <- NA
cses2$Number.Lists[cses2$Number.Lists==000] <- NA

# District.Turnout
cses2$District.Turnout[cses2$District.Turnout==999] <- NA

# Electoral.Formula
cses2$Electoral.Formula[cses2$Electoral.Formula==99] <- NA
cses2$Electoral.Formula[cses2$Electoral.Formula==98] <- NA
cses2$Electoral.Formula[cses2$Electoral.Formula==00] <- NA
cses2$PR[cses2$Electoral.Formula==11] <- 0
cses2$PR[cses2$Electoral.Formula==12] <- 0
cses2$PR[cses2$Electoral.Formula==21] <- 0
cses2$PR[cses2$Electoral.Formula==22] <- 0
cses2$PR[cses2$Electoral.Formula==31] <- 1
cses2$PR[cses2$Electoral.Formula==32] <- 1
cses2$PR[cses2$Electoral.Formula==33] <- 1
cses2$PR[cses2$Electoral.Formula==34] <- 1
cses2$Electoral.Formula <- factor(cses2$Electoral.Formula, levels = c(11,12,21,22,31,32,33,34), labels = c("SMDP", "MMDP","Majority Runoff", "Majority Alternative", "d'Hondt", "Droop", "Hare", "STE"))

## ENEP
cses2$VoteA[cses2$VoteA == 999] <- 0
cses2$VoteB[cses2$VoteB == 999] <- 0
cses2$VoteC[cses2$VoteC == 999] <- 0
cses2$VoteD[cses2$VoteD == 999] <- 0
cses2$VoteE[cses2$VoteE == 999] <- 0
cses2$VoteF[cses2$VoteF == 999] <- 0
cses2$ENEP <- 1/((cses2$VoteA/100)^2 + (cses2$VoteB/100)^2 + (cses2$VoteC/100)^2 + (cses2$VoteD/100)^2 + (cses2$VoteE/100)^2 + (cses2$VoteF/100)^2)
cses2$ENEP[cses2$ENEP == Inf] <- NA

########################################
## Compute District Competitiveness		##
########################################

# Canada and Britain

# Vote_

cses2$VoteA[cses2$VoteA==999] <- 0
cses2$VoteB[cses2$VoteB==999] <- 0
cses2$VoteC[cses2$VoteC==999] <- 0
cses2$VoteD[cses2$VoteD==999] <- 0
cses2$VoteE[cses2$VoteE==999] <- 0
cses2$VoteF[cses2$VoteF==999] <- 0

subset1 <- cses2[cses2$Electoral.Formula == "SMDP", ]
for (i in levels(subset1$Alpha.Polity)) {
  print(i)
  subset2 <- subset1[subset1$Alpha.Polity == i, ]
  for (j in unique(subset2$District)) {
    subset3 <- subset2[subset2$District == j, ]
    
    A <- subset3[1,"VoteA"]
    B <- subset3[1,"VoteB"]
    C <- subset3[1,"VoteC"]
    D <- subset3[1,"VoteD"]
    E <- subset3[1,"VoteE"]
    F <- subset3[1,"VoteF"]
    R <- 1-(A+B+C+D+E+F)
    
    vote.vector2 <- sort(c(A, B, C, D, E, F), decreasing = TRUE)
    vote.vector2[is.na(vote.vector2)] <- 0
    vote.vector <- vote.vector2/sum(vote.vector2)
    
    
    TE <- .5
    
    loss.A <- (-vote.vector[2] + vote.vector[1])/2
    gain.B <- .5 - vote.vector[2]
    gain.C <- .5 - vote.vector[3]
    gain.D <- .5 - vote.vector[4]
    gain.E <- .5 - vote.vector[5]
    gain.F <- .5 - vote.vector[6]
    
    c.A <- (TE - loss.A)/TE
    c.B <- (TE - gain.B)/TE
    c.C <- (TE - gain.C)/TE
    c.D <- (TE - gain.D)/TE
    c.E <- (TE - gain.E)/TE
    c.F <- (TE - gain.F)/TE
    
    C <- (c.A*vote.vector[1] + c.B*vote.vector[2] + c.C*vote.vector[3] + c.D*vote.vector[4] + c.E*vote.vector[5] + c.F*vote.vector[6])
    cses2$District.Competitiveness[cses2$Alpha.Polity == i & cses2$District == j] <- C
    cses2$M[cses2$Alpha.Polity == i & cses2$District == j] <- 1
  }
}

# Finland

Finland.Data <- read.table("data/finland-2003.csv", header=T, sep=",")

for (i in unique(Finland.Data$District)) {
  Finland.Data2 <- Finland.Data[Finland.Data$District == i, ] 
  a.v <- Finland.Data2$Party.A.Votes
  b.v <- Finland.Data2$Party.B.Votes
  c.v <- Finland.Data2$Party.C.Votes
  d.v <- Finland.Data2$Party.D.Votes
  e.v <- Finland.Data2$Party.E.Votes
  f.v <- Finland.Data2$Party.F.Votes
  g.v <- Finland.Data2$Party.G.Votes
  h.v <- Finland.Data2$Party.H.Votes
  
  a.s <- Finland.Data2$Party.A.Seats
  b.s <- Finland.Data2$Party.B.Seats
  c.s <- Finland.Data2$Party.C.Seats
  d.s <- Finland.Data2$Party.D.Seats
  e.s <- Finland.Data2$Party.E.Seats
  f.s <- Finland.Data2$Party.F.Seats
  g.s <- Finland.Data2$Party.G.Seats
  h.s <- Finland.Data2$Party.H.Seats
  
  m <- a.s + b.s + c.s + d.s + e.s + f.s + g.s + h.s
  seats.vector <- c(a.s, b.s, c.s, d.s, e.s, f.s, g.s, h.s)
  
  votes <- a.v + b.v + c.v + d.v + e.v + f.v + g.v + h.v
  votes.vector <- c(a.v, b.v, c.v, d.v, e.v, f.v, g.v, h.v)/votes
  
  r <- votes.vector			## r = "remainder vector"
  d <- c(1, 1, 1, 1, 1, 1, 1, 1)			## d = "divisor vector"
  s <- c(0, 0, 0, 0, 0, 0, 0, 0)			## s = "seats vector"
  for (k in 1:m) {
    assignment <- which(r/d == max(r/d))
    s[assignment] = s[assignment] + 1
    d[assignment] = d[assignment] + 1
  }
  seats.assigned <- s
  
  TE <- 1/(m + 1)
  
  a.v <- a.v/votes
  b.v <- b.v/votes
  c.v <- c.v/votes
  d.v <- d.v/votes
  e.v <- e.v/votes
  f.v <- f.v/votes
  g.v <- g.v/votes
  h.v <- h.v/votes
  
  a.s <- s[1]
  b.s <- s[2]
  c.s <- s[3]
  d.s <- s[4]
  e.s <- s[5]
  f.s <- s[6]
  g.s <- s[7]
  h.s <- s[8]
  
  gain.A <- ((a.s + 1)/(m + 1)) - (a.v)
  gain.A[gain.A > TE] <- NA
  if (a.s >= m) gain.A <- NA
  gain.B <- ((b.s + 1)/(m + 1)) - (b.v)
  gain.B[gain.B > TE] <- NA
  if (b.s >= m) gain.B <- NA
  gain.C <- ((c.s + 1)/(m + 1)) - (c.v)
  gain.C[gain.C > TE] <- NA
  if (c.s >= m) gain.C <- NA
  gain.D <- ((d.s + 1)/(m + 1)) - (d.v)
  gain.D[gain.D > TE] <- NA
  if (d.s >= m) gain.D <- NA
  gain.E <- ((e.s + 1)/(m + 1)) - (e.v)
  gain.E[gain.E > TE] <- NA
  if (e.s >= m) gain.E <- NA
  gain.F <- ((f.s + 1)/(m + 1)) - (f.v)
  gain.F[gain.F > TE] <- NA
  if (f.s >= m) gain.F <- NA
  gain.G <- ((g.s + 1)/(m + 1)) - (g.v)
  gain.G[gain.G > TE] <- NA
  if (g.s >= m) gain.G <- NA
  gain.H <- ((h.s + 1)/(m + 1)) - (h.v)
  gain.H[gain.H > TE] <- NA
  if (h.s >= m) gain.H <- NA
  
  ru.votes <- r[which(max(r/d) == r/d)]
  ru.seats <- s[which(max(r/d) == r/d)]
  
  loss.A <- (-a.s*ru.votes + ru.seats*a.v + a.v)/(a.s + ru.seats + 1)
  if (a.s <= 0) loss.A <- NA
  loss.B <- (-b.s*ru.votes + ru.seats*b.v + b.v)/(b.s + ru.seats + 1)
  if (b.s <= 0) loss.B <- NA
  loss.C <- (-c.s*ru.votes + ru.seats*c.v + c.v)/(c.s + ru.seats + 1)
  if (c.s <= 0) loss.C <- NA
  loss.D <- (-d.s*ru.votes + ru.seats*d.v + d.v)/(d.s + ru.seats + 1)
  if (d.s <= 0) loss.D <- NA
  loss.E <- (-e.s*ru.votes + ru.seats*e.v + e.v)/(e.s + ru.seats + 1)
  if (e.s <= 0) loss.E <- NA
  loss.F <- (-f.s*ru.votes + ru.seats*f.v + f.v)/(f.s + ru.seats + 1)
  if (f.s <= 0) loss.F <- NA
  loss.G <- (-g.s*ru.votes + ru.seats*g.v + g.v)/(g.s + ru.seats + 1)
  if (g.s <= 0) loss.G <- NA
  loss.H <- (-h.s*ru.votes + ru.seats*h.v + h.v)/(h.s + ru.seats + 1)
  if (h.s <= 0) loss.H <- NA
  
  close.A <- max((TE - gain.A), (TE - loss.A), na.rm = TRUE)/TE
  if (is.na(gain.A) & is.na(loss.A)) close.A <- 0
  close.B <- max((TE - gain.B), (TE - loss.B), na.rm = TRUE)/TE
  if (is.na(gain.B) & is.na(loss.B)) close.B <- 0
  close.C <- max((TE - gain.C), (TE - loss.C), na.rm = TRUE)/TE
  if (is.na(gain.C) & is.na(loss.C)) close.C <- 0
  close.D <- max((TE - gain.D), (TE - loss.D), na.rm = TRUE)/TE
  if (is.na(gain.D) & is.na(loss.D)) close.D <- 0
  close.E <- max((TE - gain.E), (TE - loss.E), na.rm = TRUE)/TE
  if (is.na(gain.E) & is.na(loss.E)) close.E <- 0
  close.F <- max((TE - gain.F), (TE - loss.F), na.rm = TRUE)/TE
  if (is.na(gain.F) & is.na(loss.F)) close.F <- 0
  close.G <- max((TE - gain.G), (TE - loss.G), na.rm = TRUE)/TE
  if (is.na(gain.G) & is.na(loss.G)) close.G <- 0
  close.H <- max((TE - gain.H), (TE - loss.H), na.rm = TRUE)/TE
  if (is.na(gain.H) & is.na(loss.H)) close.H <- 0
  
  C <- a.v*close.A + b.v*close.B + c.v*close.C + d.v*close.D + e.v*close.E  + f.v*close.F + g.v*close.G + h.v*close.H
  
  cses2$District.Competitiveness[cses2$District == i & cses2$Alpha.Polity == "Finland"] <- C
  cses2$M[cses2$District == i & cses2$Alpha.Polity == "Finland"] <- m
}

## NOTE: I had an extreme outlier in the Finland District level turnout data, which I checked and found to be incorrect.

cses2$District.Turnout[cses2$Alpha.Polity == "Finland" & cses2$District == 7] <- 64.6

# Portugal 2002

Portugal2002.Data <- read.table("data/portugal-2002.csv", header=T, sep=",")
for (i in unique(Portugal2002.Data$District)) {
  
  Portugal2002.Data2 <- Portugal2002.Data[Portugal2002.Data$District == i, ] 
  
  a.v <- Portugal2002.Data2$Party.A.Votes
  b.v <- Portugal2002.Data2$Party.B.Votes
  c.v <- Portugal2002.Data2$Party.C.Votes
  d.v <- Portugal2002.Data2$Party.D.Votes
  e.v <- Portugal2002.Data2$Party.E.Votes
  o.v <- Portugal2002.Data2$Others.Votes
  
  a.s <- Portugal2002.Data2$Party.A.Seats
  b.s <- Portugal2002.Data2$Party.B.Seats
  c.s <- Portugal2002.Data2$Party.C.Seats
  d.s <- Portugal2002.Data2$Party.D.Seats
  e.s <- Portugal2002.Data2$Party.E.Seats
  o.s <- Portugal2002.Data2$Others.Seats
  
  votes <- a.v + b.v + c.v + d.v + e.v
  votes.vector <- c(a.v, b.v, c.v, d.v, e.v)/votes
  
  m <- a.s + b.s + c.s + d.s + e.s
  seats.vector <- c(a.s, b.s, c.s, d.s, e.s)
  
  r <- votes.vector			## r = "remainder vector"
  d <- c(1, 1, 1, 1, 1)			## d = "divisor vector"
  s <- c(0, 0, 0, 0, 0)			## s = "seats vector"
  for (k in 1:m) {
    assignment <- which(r/d == max(r/d))
    s[assignment] = s[assignment] + 1
    d[assignment] = d[assignment] + 1
  }
  seats.assigned <- s
  
  TE <- 1/(m + 1)
  
  a.v <- a.v/votes
  b.v <- b.v/votes
  c.v <- c.v/votes
  d.v <- d.v/votes
  e.v <- e.v/votes
  o.v <- o.v/votes
  
  a.s <- s[1]
  b.s <- s[2]
  c.s <- s[3]
  d.s <- s[4]
  e.s <- s[5]
  
  gain.A <- ((a.s + 1)/(m + 1)) - (a.v)
  gain.A[gain.A > TE] <- NA
  if (a.s >= m) gain.A <- NA
  gain.B <- ((b.s + 1)/(m + 1)) - (b.v)
  gain.B[gain.B > TE] <- NA
  if (b.s >= m) gain.B <- NA
  gain.C <- ((c.s + 1)/(m + 1)) - (c.v)
  gain.C[gain.C > TE] <- NA
  if (c.s >= m) gain.C <- NA
  gain.D <- ((d.s + 1)/(m + 1)) - (d.v)
  gain.D[gain.D > TE] <- NA
  if (d.s >= m) gain.D <- NA
  gain.E <- ((e.s + 1)/(m + 1)) - (e.v)
  gain.E[gain.E > TE] <- NA
  if (e.s >= m) gain.E <- NA
  
  gain.vector <- c(gain.A, gain.B, gain.C, gain.D, gain.E)
  
  ru.votes <- r[which(max(r/d) == r/d)]
  ru.seats <- s[which(max(r/d) == r/d)]
  
  loss.A <- (-a.s*ru.votes + ru.seats*a.v + a.v)/(a.s + ru.seats + 1)
  if (a.s <= 0) loss.A <- NA
  loss.B <- (-b.s*ru.votes + ru.seats*b.v + b.v)/(b.s + ru.seats + 1)
  if (b.s <= 0) loss.B <- NA
  loss.C <- (-c.s*ru.votes + ru.seats*c.v + c.v)/(c.s + ru.seats + 1)
  if (c.s <= 0) loss.C <- NA
  loss.D <- (-d.s*ru.votes + ru.seats*d.v + d.v)/(d.s + ru.seats + 1)
  if (d.s <= 0) loss.D <- NA
  loss.E <- (-e.s*ru.votes + ru.seats*e.v + e.v)/(e.s + ru.seats + 1)
  if (e.s <= 0) loss.E <- NA
  
  loss.vector <- c(loss.A, loss.B, loss.C, loss.D, loss.E)
  
  close.A <- max((TE - gain.A), (TE - loss.A), na.rm = TRUE)/TE
  if (is.na(gain.A) & is.na(loss.A)) close.A <- 0
  close.B <- max((TE - gain.B), (TE - loss.B), na.rm = TRUE)/TE
  if (is.na(gain.B) & is.na(loss.B)) close.B <- 0
  close.C <- max((TE - gain.C), (TE - loss.C), na.rm = TRUE)/TE
  if (is.na(gain.C) & is.na(loss.C)) close.C <- 0
  close.D <- max((TE - gain.D), (TE - loss.D), na.rm = TRUE)/TE
  if (is.na(gain.D) & is.na(loss.D)) close.D <- 0
  close.E <- max((TE - gain.E), (TE - loss.E), na.rm = TRUE)/TE
  if (is.na(gain.E) & is.na(loss.E)) close.E <- 0
  
  C <- a.v*close.A + b.v*close.B + c.v*close.C + d.v*close.D + e.v*close.E 
  
  cses2$District.Competitiveness[cses2$District == i & cses2$Alpha.Polity == "Portugal 2002"] <- C
  cses2$M[cses2$District == i & cses2$Alpha.Polity == "Portugal 2002"] <- m
}

# Portugal 2005

Portugal2005.Data <- read.table("data/portugal-2005.csv", header=T, sep=",")

for (i in unique(Portugal2005.Data$District)) {
  Portugal2005.Data2 <- Portugal2005.Data[Portugal2005.Data$District == i, ] 
  
  a.v <- Portugal2005.Data2$Party.A.Votes
  b.v <- Portugal2005.Data2$Party.B.Votes
  c.v <- Portugal2005.Data2$Party.C.Votes
  d.v <- Portugal2005.Data2$Party.D.Votes
  e.v <- Portugal2005.Data2$Party.E.Votes
  o.v <- Portugal2005.Data2$Others.Votes
  
  a.s <- Portugal2005.Data2$Party.A.Seats
  b.s <- Portugal2005.Data2$Party.B.Seats
  c.s <- Portugal2005.Data2$Party.C.Seats
  d.s <- Portugal2005.Data2$Party.D.Seats
  e.s <- Portugal2005.Data2$Party.E.Seats
  o.s <- Portugal2005.Data2$Others.Seats
  
  votes <- a.v + b.v + c.v + d.v + e.v
  votes.vector <- c(a.v, b.v, c.v, d.v, e.v)/votes
  
  m <- a.s + b.s + c.s + d.s + e.s
  seats.vector <- c(a.s, b.s, c.s, d.s, e.s)
  
  r <- votes.vector			## r = "remainder vector"
  d <- c(1, 1, 1, 1, 1)			## d = "divisor vector"
  s <- c(0, 0, 0, 0, 0)			## s = "seats vector"
  for (k in 1:m) {
    assignment <- which(r/d == max(r/d))
    s[assignment] = s[assignment] + 1
    d[assignment] = d[assignment] + 1
  }
  seats.assigned <- s
  
  TE <- 1/(m + 1)
  
  a.v <- a.v/votes
  b.v <- b.v/votes
  c.v <- c.v/votes
  d.v <- d.v/votes
  e.v <- e.v/votes
  o.v <- o.v/votes
  
  a.s <- s[1]
  b.s <- s[2]
  c.s <- s[3]
  d.s <- s[4]
  e.s <- s[5]
  
  
  gain.A <- ((a.s + 1)/(m + 1)) - (a.v)
  gain.A[gain.A > TE] <- NA
  if (a.s >= m) gain.A <- NA
  gain.B <- ((b.s + 1)/(m + 1)) - (b.v)
  gain.B[gain.B > TE] <- NA
  if (b.s >= m) gain.B <- NA
  gain.C <- ((c.s + 1)/(m + 1)) - (c.v)
  gain.C[gain.C > TE] <- NA
  if (c.s >= m) gain.C <- NA
  gain.D <- ((d.s + 1)/(m + 1)) - (d.v)
  gain.D[gain.D > TE] <- NA
  if (d.s >= m) gain.D <- NA
  gain.E <- ((e.s + 1)/(m + 1)) - (e.v)
  gain.E[gain.E > TE] <- NA
  if (e.s >= m) gain.E <- NA
  
  gain.vector <- c(gain.A, gain.B, gain.C, gain.D, gain.E)
  
  ru.votes <- r[which(max(r/d) == r/d)]
  ru.seats <- s[which(max(r/d) == r/d)]
  
  loss.A <- (-a.s*ru.votes + ru.seats*a.v + a.v)/(a.s + ru.seats + 1)
  if (a.s <= 0) loss.A <- NA
  loss.B <- (-b.s*ru.votes + ru.seats*b.v + b.v)/(b.s + ru.seats + 1)
  if (b.s <= 0) loss.B <- NA
  loss.C <- (-c.s*ru.votes + ru.seats*c.v + c.v)/(c.s + ru.seats + 1)
  if (c.s <= 0) loss.C <- NA
  loss.D <- (-d.s*ru.votes + ru.seats*d.v + d.v)/(d.s + ru.seats + 1)
  if (d.s <= 0) loss.D <- NA
  loss.E <- (-e.s*ru.votes + ru.seats*e.v + e.v)/(e.s + ru.seats + 1)
  if (e.s <= 0) loss.E <- NA
  
  loss.vector <- c(loss.A, loss.B, loss.C, loss.D, loss.E)
  
  close.A <- max((TE - gain.A), (TE - loss.A), na.rm = TRUE)/TE
  if (is.na(gain.A) & is.na(loss.A)) close.A <- 0
  close.B <- max((TE - gain.B), (TE - loss.B), na.rm = TRUE)/TE
  if (is.na(gain.B) & is.na(loss.B)) close.B <- 0
  close.C <- max((TE - gain.C), (TE - loss.C), na.rm = TRUE)/TE
  if (is.na(gain.C) & is.na(loss.C)) close.C <- 0
  close.D <- max((TE - gain.D), (TE - loss.D), na.rm = TRUE)/TE
  if (is.na(gain.D) & is.na(loss.D)) close.D <- 0
  close.E <- max((TE - gain.E), (TE - loss.E), na.rm = TRUE)/TE
  if (is.na(gain.E) & is.na(loss.E)) close.E <- 0
  
  C <- a.v*close.A + b.v*close.B + c.v*close.C + d.v*close.D + e.v*close.E 
  
  cses2$District.Competitiveness[cses2$District == i & cses2$Alpha.Polity == "Portugal 2005"] <- C
  cses2$M[cses2$District == i & cses2$Alpha.Polity == "Portugal 2005"] <- m
}

## District Variable


cses2$District.Country <- paste(cses2$Alpha.Polity, cses2$District, sep = "")
cses2$District.Country <- as.factor(cses2$District.Country)

District.Names <- sort(unique(cses2$District.Country))
for (i in 1:length(District.Names)) {
  cses2$District[cses2$District.Country == District.Names[i]] <- i
}

######################################
## Save datasets as .csv files		##
######################################
cses2$District <- as.numeric(as.character(cses2$District))
cses2$Country <- as.numeric(cses2$Alpha.Polity)

# Save a listwise-deleted data set.
ld.vars <- c("Contacted", "Age", "Male", "Education", "Married", "Union.Member", "Household.Income", "Urban", "Close.To.Party", "District.Competitiveness", "ENEP", "PR", "Alpha.Polity", "District", "Country", "District.Country")
ld.data <- cses2[, ld.vars]
ld.data <- na.omit(ld.data)
write.csv(ld.data, "output/ld-data.csv")

# Save a data set with missing values for multiple imputation.
mi.vars <- c("Alpha.Polity", "Age", "Male", "Education", "Married", "Union.Member", "Household.Income", "Religious.Attendance", "Urban", "District", "Campaign.Activities", "Freq.Campaign", "Contacted", "Cast.Ballot", "Vote.Matters", "Cast.Ballot.Previous", "Close.To.Party", "Ideology", "Know1", "Know2", "Know3", "District.Competitiveness", "PR", "Number.Seats", "ENEP", "Country", "District")
mi.data <- cses2[, mi.vars]
write.csv(mi.data, "output/mi-data.csv")



# Create the district-level data
get.first <- function(x) {
  return(x[1])
}

district.data <- cses2[, c("Alpha.Polity", "Country", "District", "District.Competitiveness", "PR")]
district.data <- aggregate(district.data, by = list(cses2$District), FUN = get.first)
district.data$SMDP <- 1 - district.data$PR
write.csv(district.data, "output/district-data.csv")

# Create the country-level data
country.data <- cses2[, c("Alpha.Polity", "Country", "PR")]
country.data <- aggregate(country.data, by = list(cses2$Country), FUN = get.first)
country.data$SMDP <- 1 - country.data$PR
write.csv(country.data, "output/country-data.csv")

