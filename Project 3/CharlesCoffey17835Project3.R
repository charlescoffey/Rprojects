## ----setup, include=FALSE--------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#Charles Coffey
#17.835 PSET 3
#10.6.20

## ----1.1-------------------------------------------------------------
rm(list = ls(all.names = TRUE))
library(rddtools)
#KEPT
#TRUE = 1; FALSE = 0;

#ALLOC_TYPE
#Flat = 1;
#Late_High = 0;

#MESSAGE
#YES = 1;
#NO = 0;

Retro_Vote = read.csv("retro_vote.csv")

Retro_Vote[Retro_Vote$kept,"kept"] = 1
Retro_Vote[Retro_Vote$alloc_type == "Flat", "alloc_type"] = 0 #control
Retro_Vote[Retro_Vote$alloc_type == "Late_High", "alloc_type"] = 1 #treated
Retro_Vote[Retro_Vote$message == "NO", "message"] = 0 #control
Retro_Vote[Retro_Vote$message == "YES", "message"] = 1 #treated



## ----1.2-------------------------------------------------------------

retention_rates = tapply(Retro_Vote$kept, list(Retro_Vote$alloc_type, Retro_Vote$message), mean)

#alloc_type is the rows & message is the columns
print(retention_rates)


## ----1.3-------------------------------------------------------------
#standard error function defined below

std_error = function(X){
  #function that computes standard error
  #INPUT: vector object
  #OUTPUT: standard error scalar value for vector object
  
  sqrt(var(X)/length(X))
}

standard_errors = tapply(Retro_Vote$kept, list(Retro_Vote$alloc_type, Retro_Vote$message), std_error)

flat_nomes_interval = c(retention_rates["0", "0"] - 1.96*standard_errors["0", "0"], retention_rates["0", "0"] + 1.96*standard_errors["0", "0"])
flat_yesmes_interval = c(retention_rates["0", "1"] - 1.96*standard_errors["0", "1"], retention_rates["0", "1"] + 1.96*standard_errors["0", "1"])
lahi_nomes_interval = c(retention_rates["1", "0"] - 1.96*standard_errors["1", "0"], retention_rates["1", "0"] + 1.96*standard_errors["1", "0"])
lahi_yesmes_interval = c(retention_rates["1", "1"] - 1.96*standard_errors["1", "1"], retention_rates["1", "1"] + 1.96*standard_errors["1", "1"])

print(paste("The 95% confidence interval for Flat & No Message is", toString(flat_nomes_interval[1]), "< x <", 
            toString(flat_nomes_interval[2]), sep = " "))
print(paste("The 95% confidence interval for Flat & Yes Message is", toString(flat_yesmes_interval[1]), "< x <", 
            toString(flat_yesmes_interval[2]), sep = " "))
print(paste("The 95% confidence interval for Late High & No Message is", toString(lahi_nomes_interval[1]), "< x <", 
            toString(lahi_nomes_interval[2]), sep = " "))
print(paste("The 95% confidence interval for Late High & Yes Message is", toString(lahi_yesmes_interval[1]), "< x <", toString(lahi_yesmes_interval[2]), sep = " "))


## ----1.4-------------------------------------------------------------

plot(c(1:4), retention_rates, 
     xaxt='n',
     ylim = c(0.35, 0.95),
     ylab = "Retention Rates", 
     xlab = "Groups",
     main = "Retention Rates of Each Group")
arrows(c(1:4), retention_rates-1.96*standard_errors, c(1:4), retention_rates+1.96*standard_errors, length=0.05, angle=90, code=3)
axis(1, at=c(1:4), labels=c("FN", "LHN", "FY", "LHY"))
legend(1, 0.95, c("FN = Flat, No MSG", "LHN = Late High, No MSG", "FY = Flat, Yes Message", "LHY = Late High, Yes MSG"), cex = 0.6)



## ----1.5-------------------------------------------------------------

#calculating ATE of receiving high payment in the second 30 days (effect on kept among all respondents)

alloc_treated = Retro_Vote[Retro_Vote$alloc_type == 1, ]
alloc_control = Retro_Vote[Retro_Vote$alloc_type == 0, ]

#ATE calculation
dif_mean_alloc = mean(alloc_treated$kept) - mean(alloc_control$kept)

ols_alloc = lm(Retro_Vote$kept ~ Retro_Vote$alloc_type)

#ATE from regression
Beta_alloc = summary(ols_alloc)$coefficients[2,1]

print(paste("Difference-in-means method: ", toString(dif_mean_alloc)))
print(paste("OLS regression: ", toString(Beta_alloc)))



## ----1.6-------------------------------------------------------------

#calculating ATE of receiving campaign primer (effect on kept among all respondents)
mes_treated = Retro_Vote[Retro_Vote$message == 1, ]
mes_control = Retro_Vote[Retro_Vote$message == 0, ]

#ATE calculation
dif_mean_mes = mean(mes_treated$kept) - mean(mes_control$kept)

ols_mes = lm(Retro_Vote$kept ~ Retro_Vote$message)

#ATE from regression
Beta_mes = summary(ols_mes)$coefficients[2,1]

print(paste("Difference-in-means method: ", toString(dif_mean_mes)))
print(paste("OLS regression: ", toString(Beta_mes)))



## ----1.7-------------------------------------------------------------

ols_alloc_se = summary(ols_alloc)$coefficients[2,2]
ols_mes_se = summary(ols_mes)$coefficients[2,2]

#confidence intervals
alloc_Beta_interval = c(Beta_alloc - 1.96*ols_alloc_se, Beta_alloc + 1.96*ols_alloc_se)
mes_Beta_interval = c(Beta_mes - 1.96*ols_mes_se, Beta_mes + 1.96*ols_mes_se)

print(paste("The 95% confidence interval for the alloc_type Beta value is", toString(alloc_Beta_interval[1]), "< x <", toString(alloc_Beta_interval[2]), sep = " "))
print(paste("The 95% confidence interval for the message Beta value is", toString(mes_Beta_interval[1]), 
            "< x <", toString(mes_Beta_interval[2]), sep = " "))

plot(c(1,2), c(Beta_alloc, Beta_mes), col = "blue", xaxt = "n", 
     xlab = "Treatments", ylab = "Beta values", ylim = c(-0.3, 0.3), main = "Treatment Beta Values")
arrows(c(1:2), c(alloc_Beta_interval[1], mes_Beta_interval[1]), c(1:2), c(alloc_Beta_interval[2], mes_Beta_interval[2]), 
       col = "blue", length=0.05, angle=90, code=3)
axis(1, at=c(1:2), labels=c("alloc_type", "message"))
abline(h=0, lty = 2)



## ----1.8-------------------------------------------------------------

#same code as 1.5

print(paste("The conditional treatment affect of the Late_High allocator: ", toString(dif_mean_alloc)))

plot(c(1,1,2,2), retention_rates, 
     xaxt='n',
     ylim = c(0.35, 0.95),
     ylab = "Retention Rates", 
     xlab = "Groups",
     pch = 19,
     col = c("blue", "red", "blue", "red"),
     main = "Retention Rates")
arrows(c(1,1,2,2), retention_rates-1.96*standard_errors , c(1,1,2,2), retention_rates+1.96*standard_errors, 
       length=0.05, 
       angle=90, 
       code=3, 
       col = c("blue", "red", "blue", "red"),)
axis(1, at=c(1,2), labels=c("No Message", "Yes Message"))

tr_points = lm(c(retention_rates[2,1], retention_rates[2,2]) ~ c(1,2))
cl_points = lm(c(retention_rates[1,1], retention_rates[1,2]) ~ c(1,2))

curve(tr_points$coefficients[1] + tr_points$coefficients[2]*x, 1, 2, add = T, lwd = 3, lty = 1, col = "red")
curve(cl_points$coefficients[1] + cl_points$coefficients[2]*x, 1, 2, add = T, lwd = 3, lty = 1, col = "blue")

legend("topleft", c("Late High", "Flat"), col = c("red", "blue"), lty = 1, lwd = 3, cex = 0.8)



## ----2.2-------------------------------------------------------------

brazil_mayor = read.csv("brazil_mayor.csv")

#only looking at margin for 1996 vote because if we looked at both it would literally be only 30 observations??
brazil_mayor = brazil_mayor[abs(brazil_mayor$vote_margin_pct) < 2,]

#subsetting treated and control groups
brazil_mayor_treated = brazil_mayor[brazil_mayor$incumbent == 1,]
brazil_mayor_control = brazil_mayor[brazil_mayor$incumbent == 0,]

reg_treated = lm(brazil_mayor_treated$margin_2000 ~ brazil_mayor_treated$vote_margin_pct)
reg_control = lm(brazil_mayor_control$margin_2000 ~ brazil_mayor_control$vote_margin_pct)

#intercept, slope
mod.tr = c(summary(reg_treated)$coefficients[1,1], summary(reg_treated)$coefficients[2,1])
mod.cl = c(summary(reg_control)$coefficients[1,1], summary(reg_control)$coefficients[2,1])

summary(reg_treated)
summary(reg_control)



## ----2.3-------------------------------------------------------------

plot(brazil_mayor$vote_margin_pct, brazil_mayor$margin_2000,
     xlab = "Vote Margin 1996 (%)",
     ylab = "Vote Margin 2000 (%)",
     main = "Incumbency Advantage",
     cex = 0.5,
     pch = 16)

curve(reg_treated$coefficient[1] + reg_treated$coefficient[2] * x, 0, 2, add = T, lwd = 3, lty = 1, col = "red")
curve(reg_control$coefficient[1] + reg_control$coefficient[2] * x, -2, 0, add = T, lwd = 3, lty = 1, col = "blue")
abline(v = 0, lty = 2)

legend("topleft", legend = c("Barely lost", "Incumbent"), col = c("blue", "red"), bty = "n", lty = 1, lwd = 3, cex = 0.8)



## ----2.4-------------------------------------------------------------

tau = as.numeric(reg_control$coefficient[1] - reg_treated$coefficient[1])

print(paste("The difference of the fitted values of the two lines at x=0 is: ", toString(tau), "%", sep=""))

