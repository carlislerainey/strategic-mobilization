# Make sure that working directory is set properly
# setwd("~/Dropbox/projects/strategic-mobilization/")

# clear workspace
rm(list = ls())


library(arm)
library(blme)

cses2 <- read.csv("output/ld-data.csv")


img.ht <- 7
img.wdth <- img.ht*(6/3)
font <- "Helvetica"

cex1 <- 2
cex2 <- 2
lwd1 <- 5
lwd2 <- 1
my.red <- rgb(228, 26, 28, maxColorValue = 255)
my.blue <- rgb(55, 126, 184, maxColorValue = 255)
 
# # Country-by-Country Mixed-Effects Logis
png(file = "present/fig/results.png", width = img.wdth, height = img.ht, bg = "transparent", res = 300, units = "in")

par(mfcol = c(2, 3), oma = c(5,6,2,0), mar = c(1,1,2,4), bg = NA, family = font)

f <- function(x) { .1 + .9*x^2 }


# SMDP Prediction
plot(NULL, axes = F, xlab = NA, ylab = NA, xlim = c(0,1), ylim = c(0,1))
box(lwd = lwd2)
#axis(side = 2, at = c(.1, .9), labels = c("Low", "High"), 
#     lwd = 0, line = 2, las = 1, cex.axis = cex2)
mtext(side = 2, "Mobilization", line = 4, cex = cex1)
#Lit. Prop
curve(f(.9*x) + .1, xlim = c(.5, 1), col = my.blue, add = T, lwd = lwd1, lty = 2)
text(.25, f(.9*.5) + .16, "The Literature's\nPrediction", col = my.blue, lwd = lwd1, cex = cex2)
# My Prop
curve(f(x - .5), xlim = c(.5, 1), col = my.blue, add = T, lwd = lwd1, lty = 2)
text(.25, f(0), "My Prediction", col = my.blue, lwd = lwd1, cex = cex2)
mtext("Proportional Rules", line = .2, cex = cex1)


plot(NULL, axes = F, xlab = NA, ylab = NA, xlim = c(0,1), ylim = c(0,1))
box(lwd = lwd2)
axis(side = 1, at = c(.1, .9), labels = c("Low", "High"), 
     lwd = 0, line = -.4, cex.axis = cex2)
mtext(side = 1, "Competitiveness", line = 3.5, cex = cex1)
axis(side = 2, at = c(.1, .9), labels = c("Low", "High"), 
     lwd = 0, line = -.8, las = 1, cex.axis = cex2)
mtext(side = 2, "Mobilization", line = 4, cex = cex1)
# Plurality
curve(f(x), xlim = c(0,1), lwd = lwd1, col = my.red, add = T)
text(.45, .75, "My and the Literature's\nPrediction",  col = my.red, cex = cex2)
mtext("Plurality Rules", line = .2, cex = cex1)

par(mar = c(1,4.5,2,.5))

countries <- unique(cses2$Alpha.Polity)
cntry.names <- c("Canada", "Finland", "Great Britain", "Portugal (2002)")
cntry.names2 <- c("Canada", "Finland", "Great Britain", "Portugal")

for (i in c(2,1,4,3)) {
  m0 <- glmer(Contacted ~ District.Competitiveness + (1 | District), data = cses2, 
              subset = Alpha.Polity == countries[i], family = binomial)
  sims <-  sim(m0, n = 100)
  fixed.sims <- fixef(sims)
  random.sims <- ranef(sims)$District[,,1]

  d <- cses2[cses2$Alpha.Polity == countries[i], ]
  dc <- seq(min(d$District.Competitiveness),max(d$District.Competitiveness), length.out = 100)
  x0 <- cbind(1,dc)
  y.star <- x0%*%t(fixed.sims)
  pr50 <- apply(plogis(y.star), 1, quantile, .5)
  pr95 <- apply(plogis(y.star), 1, quantile, .95)
  pr05 <- apply(plogis(y.star), 1, quantile, .05)
  plot(NA, xlim = c(min(cses2$District.Competitiveness),max(cses2$District.Competitiveness)), ylim = c(-.1, 1.1),
       xlab = NA, ylab = NA, axes = F)
  box()
  districts <- sort(unique(d$District))
  for (j in 1:length(districts)) {
    comp <- d$District.Competitiveness[d$District == districts[j]][1]
    alpha <- plogis(fixed.sims[, "(Intercept)"] + 
      fixed.sims[, "District.Competitiveness"]*comp + 
      random.sims[, j])
    q.alpha <- quantile(alpha, c(.05, .5, .95))
    points(comp, q.alpha[2], pch = 19, cex = .8, col = "grey50")
    lines(c(comp, comp), c(q.alpha[1], q.alpha[3]), col = "grey50", lwd = 1)
    }
  
  mtext(cntry.names2[i], line = .2, cex = cex1)
  col = my.blue
  if (cntry.names[i] == "Canada" | cntry.names[i] == "Great Britain") {
    mtext(side = 1, "Competitiveness", line = 3.5, cex = cex1)
    axis(side = 1, cex.axis = cex2)
    col = my.red
  }
  if (cntry.names[i] == "Finland" | cntry.names[i] == "Canada") {
    mtext(side = 2, "Pr(Contact)", line = 5, cex = cex1)
    axis(side = 2, at = 0:4/4, las = 1, cex.axis = cex2)
  }  
  lines(dc,pr50,lwd=4, , col = col)
  lines(dc,pr05,lty=3, col = col, lwd = 3)
  lines(dc,pr95,lty=3, col = col, lwd = 3)
  if (i == 1) { par(mar = c(1,1,2, 4))}
}
dev.off()

