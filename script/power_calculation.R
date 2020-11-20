#Last edited by - Deus Thindwa
#Date - 28/10/2019

#determine the power given effect size and equal sample sizes in age classes
powerd3d <- pwr.t2n.test(d=0.3, n2=180, n1=c(180, 180, 180, 180, 180, 180))

#sample size versus effect size
ptab <- cbind(NULL, NULL)
for (i in c(.3, .4, .5, .6)){
  powerA <- pwr.t2n.test(d=i, n2=180, power=.81, sig.level = .05)
  ptab <- rbind(ptab, cbind(powerA$d, powerA$n1))
}

#plot sample size against effect size for given power 81%
par(mfrow = c(1,1), mai = c(1, 1, 0.5, 0.2))
plot(ptab[,1]*100, ptab[,2], type = "b", col = "red", lwd = 2, xlab = '% difference in effect size', ylab = 'Sample size ('*italic(n[i])~')')
abline(v = 30, col = "black", lty = 2)
mtext("", side = 3, line = 0, cex = 1.3, adj = 0)
legend(45, 150, legend = c("Power: 81%", "Significance level: 0.05", "30% difference in effect size", "6 equal-sized age groups", "180 people per age group"),
col = c("black", "black"), lty = c(1, 1, 2, 1, 1), cex = 0.8)
