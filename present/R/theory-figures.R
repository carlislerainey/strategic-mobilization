# Make sure that working directory is set properly
# setwd("~/Dropbox/projects/strategic-mobilization/")

# clear workspace
rm(list = ls())

img.ht <- 5
img.wdth <- img.ht*(4/3)

lum <- function(col) {
  col <- col2rgb(col)
  R <- col[1]
  G <- col[2]
  B <- col[3]
  x <- sqrt( 0.241*R^2 + 0.691*G^2 + 0.068*B^2 )
  return(x)
}

cex1 <- 2
cex2 <- 1.5
lwd1 <- 5
lwd2 <- 1

plurality.col <- rgb(228, 26, 28, maxColorValue = 255)
prop.col<- rgb(55, 126, 184, maxColorValue = 255)
axis.col <- "grey20"#rgb(120,120,120, maxColorValue = 255)
       lum(axis.col)         
font <- "Helvetica"

f <- function(x) { .1 + .9*x^2 }

## Plurality Line
png("present/fig/full.png", height = img.ht, width = img.wdth, bg = "transparent", units = "in", res = 300)
par(family = font, bg = "transparent", mar = c(4,5,1,1), oma = c(0,0,0,0))
# Basic Plot and Axes
plot(NULL, axes = F, xlab = NA, ylab = NA, xlim = c(0,1), ylim = c(0,1))
box(lwd = lwd2, col = axis.col)
axis(side = 1, at = c(.1, .9), labels = c("Low", "High"), 
    lwd = 0, line = -.6, cex.axis = cex2, col.axis = axis.col)
mtext(side = 1, "Competitiveness", line = 2.2, cex = cex1, col = axis.col)
axis(side = 2, at = c(.1, .9), labels = c("Low", "High"), 
    lwd = 0, line = -.8, las = 1, cex.axis = cex2, col.axis = axis.col)
mtext(side = 2, "Mobilization", line = 3.2, cex = cex1, col = axis.col)
# Plurality
curve(f(x), xlim = c(0,1), lwd = lwd1, col = plurality.col, add = T)
text(.3 -.03, f(.3) + .03, "Plurality", pos = 2,  col = plurality.col, cex = cex2)
#Lit. Prop
curve(f(.9*x) + .1, xlim = c(.5, 1), col = prop.col, add = T, lwd = lwd1)
text(.55, f(.9*.6) + .1, "Proportional", pos = 2, col = prop.col, lwd = lwd1, cex = cex2)
# My Prop
curve(f(x - .5), xlim = c(.5, 1), col = prop.col, add = T, lwd = lwd1)
text(.5, f(0)-.07, "Proportional", pos = 4, col = prop.col, lwd = lwd1, cex = cex2)
dev.off()


## Axes Only
png("present/fig/axes-only.png", height = img.ht, width = img.wdth, bg = "transparent", units = "in", res = 300)
par(family = font, bg = "transparent", mar = c(4,5,1,1), oma = c(0,0,0,0))
# Basic Plot and Axes
plot(NULL, axes = F, xlab = NA, ylab = NA, xlim = c(0,1), ylim = c(0,1))
box(lwd = lwd2, col = axis.col)
axis(side = 1, at = c(.1, .9), labels = c("Low", "High"), 
     lwd = 0, line = -.6, cex.axis = cex2, col.axis = axis.col)
mtext(side = 1, "Competitiveness", line = 2.2, cex = cex1, col = axis.col)
axis(side = 2, at = c(.1, .9), labels = c("Low", "High"), 
     lwd = 0, line = -.8, las = 1, cex.axis = cex2, col.axis = axis.col)
mtext(side = 2, "Mobilization", line = 3.2, cex = cex1, col = axis.col)
dev.off()

## Plurality Only
png("present/fig/plurality-only.png", height = img.ht, width = img.wdth, bg = "transparent", units = "in", res = 300)
par(family = font, bg = "transparent", mar = c(4,5,1,1), oma = c(0,0,0,0))
# Basic Plot and Axes
plot(NULL, axes = F, xlab = NA, ylab = NA, xlim = c(0,1), ylim = c(0,1))
box(lwd = lwd2, col = axis.col)
axis(side = 1, at = c(.1, .9), labels = c("Low", "High"), 
     lwd = 0, line = -.6, cex.axis = cex2, col.axis = axis.col)
mtext(side = 1, "Competitiveness", line = 2.2, cex = cex1, col = axis.col)
axis(side = 2, at = c(.1, .9), labels = c("Low", "High"), 
     lwd = 0, line = -.8, las = 1, cex.axis = cex2, col.axis = axis.col)
mtext(side = 2, "Mobilization", line = 3.2, cex = cex1, col = axis.col)
# Plurality
curve(f(x), xlim = c(0,1), lwd = lwd1, col = plurality.col, add = T)
text(.3 -.03, f(.3) + .03, "Plurality", pos = 2,  col = plurality.col, cex = cex2)
dev.off()

## Literature
png("present/fig/literature.png", height = img.ht, width = img.wdth, bg = "transparent", units = "in", res = 300)
par(family = font, bg = "transparent", mar = c(4,5,1,1), oma = c(0,0,0,0))
# Basic Plot and Axes
plot(NULL, axes = F, xlab = NA, ylab = NA, xlim = c(0,1), ylim = c(0,1))
box(lwd = lwd2, col = axis.col)
axis(side = 1, at = c(.1, .9), labels = c("Low", "High"), 
     lwd = 0, line = -.6, cex.axis = cex2, col.axis = axis.col)
mtext(side = 1, "Competitiveness", line = 2.2, cex = cex1, col = axis.col)
axis(side = 2, at = c(.1, .9), labels = c("Low", "High"), 
     lwd = 0, line = -.8, las = 1, cex.axis = cex2, col.axis = axis.col)
mtext(side = 2, "Mobilization", line = 3.2, cex = cex1, col = axis.col)
# Plurality
curve(f(x), xlim = c(0,1), lwd = lwd1, col = plurality.col, add = T)
text(.3 -.03, f(.3) + .03, "Plurality", pos = 2,  col = plurality.col, cex = cex2)
#Lit. Prop
curve(f(.9*x) + .1, xlim = c(.5, 1), col = prop.col, add = T, lwd = lwd1)
text(.55, f(.9*.6) + .1, "Proportional", pos = 2, col = prop.col, lwd = lwd1, cex = cex2)
dev.off()

## Me
png("present/fig/me.png", height = img.ht, width = img.wdth, bg = "transparent", units = "in", res = 300)
par(family = font, bg = "transparent", mar = c(4,5,1,1), oma = c(0,0,0,0))
# Basic Plot and Axes
plot(NULL, axes = F, xlab = NA, ylab = NA, xlim = c(0,1), ylim = c(0,1))
box(lwd = lwd2, col = axis.col)
axis(side = 1, at = c(.1, .9), labels = c("Low", "High"), 
     lwd = 0, line = -.6, cex.axis = cex2, col.axis = axis.col)
mtext(side = 1, "Competitiveness", line = 2.2, cex = cex1, col = axis.col)
axis(side = 2, at = c(.1, .9), labels = c("Low", "High"), 
     lwd = 0, line = -.8, las = 1, cex.axis = cex2, col.axis = axis.col)
mtext(side = 2, "Mobilization", line = 3.2, cex = cex1, col = axis.col)
# Plurality
curve(f(x), xlim = c(0,1), lwd = lwd1, col = plurality.col, add = T)
text(.3 -.03, f(.3) + .03, "Plurality", pos = 2,  col = plurality.col, cex = cex2)
# My Prop
curve(f(x - .5), xlim = c(.5, 1), col = prop.col, add = T, lwd = lwd1)
text(.5, f(0)-.07, "Proportional", pos = 4, col = prop.col, lwd = lwd1, cex = cex2)
dev.off()