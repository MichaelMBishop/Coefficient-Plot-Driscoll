# Code to create 'coefficient plots' as an alternative to
#   tables for regression models.
# Written by Carlisle Rainey. 
#            http://blog.carlislerainey.com/2012/07/03/an-improvement-to-coefficient-plots/
# Inspired by Amanda Driscoll
#            http://driscoll.wustl.edu/Amanda_Driscoll-Knight/Welcome.html
# Maintained by Michael Metcalf Bishop

# If any errors are discovered, please let me know.


setwd("~/Desktop/Blogs/DriscollPlots")
require(arm)
png("driscollplot.png", height = 400, width = 500)

d <- read.csv("http://www.carlislerainey.com/Files/anes1992.csv")  # call the data set d for convenience

m <- glm(Turnout ~ PartyID.Folded + Age + Female + Black + Union.Member + Education, 
  family = binomial, data = d, x = T)

var.names <- c("partisan strength", "age, in years",
  "female", "African-American", "union member", "education level")


d0 <- m[["x"]][, -1] 
lo <- apply(d0, 2, quantile, 0)
md <- apply(d0, 2, quantile, .5)
hi <- apply(d0, 2, quantile, 1)

sims <- coef(sim(m, n = 1000))

par(
  family = "serif",
  oma = c(0,0,0,0),
  mar = c(5,2,4,2)
  )
# create an empty plot for total customization
plot(NULL, col = "grey10",
  xlim = c(-.5, 1),                    
  ylim = c(.7, length(var.names) + .4), 
  axes = F, xlab = NA, ylab = NA, xaxs = "i", yaxs = "i")  

polygon(c(-.5, 5, 1, -.5), c(.7, .7, 6.4, 6.4), border = NA, col = "grey")
abline(v = 0, col = "white", lwd = 3)
text(-.5, .7, "-0.50", pos = 1, xpd = T)
text(-.25, .7, "-0.25", pos = 1, xpd = T)
text(0, .7, "0.00", pos = 1, xpd = T)
text(.25, .7, "0.25", pos = 1, xpd = T)
text(.5, .7, "0.50", pos = 1, xpd = T)
text(.75, .7, "0.75", pos = 1, xpd = T)
text(1, .7, "1.00", pos = 1, xpd = T)

abline(v = -.25, lty = 3, col = "white")
abline(v = .25, lty = 3, col = "white")
abline(v = .5, lty = 3, col = "white")
abline(v = .75, lty = 3, col = "white")

polygon(c(0, 1, 1, 0), c(5.9, 5.9, 6.1, 6.1), border = "white", col = "white")
polygon(c(0, 1, 1, 0), c(4.9, 4.9, 5.1, 5.1), border = "white", col = "white")
polygon(c(0, -1, -1, 0), c(3.9, 3.9, 4.1, 4.1), border = "white", col = "white")
polygon(c(0, -1, -1, 0), c(2.9, 2.9, 3.1, 3.1), border = "white", col = "white")
polygon(c(0, 1, 1, 0), c(1.9, 1.9, 2.1, 2.1), border = "white", col = "white")
polygon(c(0, 1, 1, 0), c(0.9, 0.9, 1.1, 1.1), border = "white", col = "white")

# add the data
for (i in 1:(length(coef(m)) - 1)) {  # loop over a counter the length of the estimate vector
  x.lo <- md; x.lo[i] <- lo[i]; x.lo <- c(1, x.lo)
  x.hi <- md; x.hi[i] <- hi[i]; x.hi <- c(1, x.hi)
  sim.lo <- plogis(sims%*%x.lo)                        # be sure to convert the linear predictor using the link function
  sim.hi <- plogis(sims%*%x.hi)                        # be sure to convert the linear predictor using the link function
  sims.dif <- sim.hi - sim.lo
  q <- quantile(sims.dif, c(.05, .25, .5, .75, .95))
  points(q[3], i, pch = 19, cex = .5)
  lines(c(q[1], q[5]), c(i,i))
  lines(c(q[2], q[4]), c(i,i), lwd = 3)
  text(q[3], i, var.names[i], xpd = T, cex = .8, pos = 3)
}
  
mtext(side = 1, "First Difference in Predicted Probabilities as a Variable\nMoves from Its Minimum to Its Maximum", line = 3)
mtext(side = 3, "Logistic Regression Model of\n(Self-Reported) Voting", line = 1)
dev.off()
