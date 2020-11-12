library(ggplot2)
library(matrixStats)

#QUESTION 1

#1.1
load("boas_hidalgo_2011.RData")

pctVV_mean = mean(br$pctVV)
pctVV_median = median(br$pctVV)
pctVV_max = max(br$pctVV)
pctVV_min = min(br$pctVV)

naive_estimator = mean(br[br$treat == 1,"pctVV"]) - mean(br[br$treat == 0,"pctVV"])

print(paste("Percent of Valid Votes Won Mean: ", toString(pctVV_mean)))
print(paste("Percent of Valid Votes Won Median: ", toString(pctVV_median)))
print(paste("Percent of Valid Votes Won Max: ", toString(pctVV_max)))
print(paste("Percent of Valid Votes Won Min: ", toString(pctVV_min)))
print(paste("Difference between Treated and Control Mean Percent of Valid Votes:", toString(naive_estimator)))

#1.2

#1.3

vars = names(br)[3:length(names(br))] # vector of covariate names

br_match_var = br[,vars] # subset of covariates to match against

std = colSds(as.matrix(br[,3:ncol(br)]))

treated_means = colMeans(br[br$treat == 1,3:ncol(br)]) #find the mean covariate values for treated units
control_means = colMeans(br[br$treat == 0,3:ncol(br)]) #find mean covariate values for contorl units

mean_differences_std = data.frame((treated_means-control_means)/std) #standardized mean differences

names(mean_differences_std) = "mean_diff"
mean_differences_std$var <- c("Log(Valid Votes)", "% of Presidential Vote Share by PT in 1998", "Male", "Log (Total Assets)", "Log(Number of Applicants", "Media Career")

#plot standardized mean differences
ggplot(mean_differences_std, aes(y=mean_diff, x=var)) + geom_hline(yintercept=0) + geom_point(size=2) + coord_flip() + ylab("Standardized Difference in Means") + xlab("Covariates") + ggtitle("Standardized Differences in Means before Matching") + theme(plot.title = element_text(size = 14, hjust = 0.5), axis.title=element_text(size=14))

#1.4 

#intitialization of mahalonobis function
MDist <- function(X, idx1, idx2){
  x1 = X[idx1,]
  x2 = X[idx2,]
  
  vcov = var(X)
  
  dist = sqrt(t(x1-x2) %*% solve(vcov) %*% (x1-x2))
  
  return(dist)
}

#1.5
treated_unit_idxs = which(br$treat == 1) #indices of treated units
control_unit_idxs = which(br$treat == 0) #indices of control units

att.comb = NULL #treated and matched controls differences vector
used_control_matches = NULL #matched controls indices

for (i in treated_unit_idxs){
  #loop through treated values
  
  dist_M = NULL #Mahalanobis distances for each control
  
  for (j in control_unit_idxs){
    #get mahalonobis distnace values between one treated and all controls 
    dist = MDist(as.matrix(br_match_var), i, j)
    dist_M =  append(dist_M, dist)
  }
  
  # distance measures for each of the controls
  control_matches = data.frame(cbind(control_unit_idxs, dist_M))
  
  #top two best matches
  best_matches = control_matches[order(dist_M), ][1:2, ]
  
  used_control_matches = append(used_control_matches, best_matches$control_unit_idxs)
  
  att = br[i,"pctVV"] - mean(br[best_matches$control_unit_idxs, "pctVV"])
  
  att.comb = append(att.comb, att)
  
}

#average treatment effect of radio licenses on vote share
ATT = mean(att.comb)
print(paste("The Average Treatment Effect on the Treated Units is:", toString(ATT)))

#1.6

used_control_matches_unique = unique(used_control_matches)
not_used_controls = control_unit_idxs[!control_unit_idxs %in% used_control_matches_unique ]

br_subset = br[-not_used_controls,] #this subset contains just the treated values and controls used for matching

std_match = colSds(as.matrix(br_subset[,3:ncol(br_subset)])) #std of treated and matched controls

control_means_match = colMeans(br_subset[br_subset$treat == 0,3:ncol(br_subset)]) #means of all matched control units

mean_differences_std_match = data.frame((treated_means-control_means_match)/std_match) #standardized mean differences after matching
names(mean_differences_std_match) = "mean_diff_matched"

mean_differences_combined = cbind(mean_differences_std, mean_differences_std_match)

#plot old differences before and after matching
ggplot(mean_differences_combined, aes(x=var, y = mean_diff)) + geom_point(aes(y=mean_diff_matched), color = "green") + geom_hline(yintercept=0) + geom_point(size=1.5) + coord_flip() + ylab("Standardized Difference in Means") + xlab("Covariates") + ggtitle("Standardized Differences in Means after Matching") + theme(plot.title = element_text(size = 14, hjust = 0.5), axis.title=element_text(size=14))

#################################################################3

#QUESTION 2



#2.1
cces_2012 <- read.csv("cces_2012.csv")

#calculate mean GOP vote share per state
#this could be done in one step by just setting the function to mean
gop_state_vote = tapply(cces_2012$vote_gop, INDEX = cces_2012$state_abb, FUN = sum)
gop_state_n = tapply(cces_2012$vote_gop, INDEX = cces_2012$state_abb, FUN = length)
gop_state_share = gop_state_vote/gop_state_n

#calculate mean income per state
income_state = tapply(cces_2012$income, INDEX = cces_2012$state_abb, FUN=mean)


plot(income_state, gop_state_share, ylim = c(0, 0.8), "n", 
     main = "GOP State Share vs Mean State Income", 
     ylab = "GOP State Share",
     xlab = "Mean Income")

text(income_state, gop_state_share, names(gop_state_share), col="red", cex = 0.65)

abline(lm(gop_state_share ~ income_state))

#2.2
#calculate GOP vote share across incomes
gop_income_vote = tapply(cces_2012$vote_gop, INDEX = cces_2012$income, FUN = sum)
gop_income_n = tapply(cces_2012$vote_gop, INDEX = cces_2012$income, FUN = length)
gop_income_share = gop_income_vote/gop_income_n

barplot(gop_income_share, names.arg= names(gop_income_share), ylim = c(0, 0.6), 
        main = "GOP Income Share vs Income", 
        xlab = "Income Level",
        ylab = "GOP State Share")

points(c(1:length(gop_income_share)), gop_income_share, pch = 16, cex = 2, ylim = c(0, 0.6))
lines(c(1:length(gop_income_share)), gop_income_share)

#2.3

#creat subset data for each state
MA_subset = cces_2012[cces_2012$state_abb == "MA", c("vote_gop", "income")]
WI_subset = cces_2012[cces_2012$state_abb == "WI", c("vote_gop", "income")]
MS_subset = cces_2012[cces_2012$state_abb == "MS", c("vote_gop", "income")]


plot(0,
     type = 'n',
     xlim = c(1, 5),
     ylim = c(0, 1),
     main = "Income level and Support within states",
     xlab = "Income level",
     ylab = "Support for GOP",
     cex.lab = 1.2)

abline(lm(MA_subset), col="blue", lty = 1, lwd = 4)
abline(lm(WI_subset), col=" purple", lty = 2, lwd = 4)
abline(lm(MS_subset), col="red", lty = 3, lwd = 4)

legend("topleft", 
       legend = c("MA", "WI", "MS"),
       lwd = c(1.5,1.5,1.5),
       lty = 1:3,
       cex = 0.9,
       col = c("blue", "purple", "red"),
       bty = "n")

#find average gop vote share and income values for each state
state_average_values = data.frame()
state_average_values = rbind(state_average_values, colMeans(MA_subset), colMeans(WI_subset), colMeans(MS_subset))
names(state_average_values) = c("gop_vote_mean", "income_mean")

points(state_average_values[,2], state_average_values[,1], pch = 16, cex = 2)

lines(state_average_values[,2], state_average_values[,1], lwd = 4)

#2.4

MA_corr = cor(MA_subset$income, MA_subset$vote_gop)
MS_corr = cor(MS_subset$income, MS_subset$vote_gop)

print(paste("MA indv. income to indv. support for GOP candidate correlation coefficient: ", toString(MA_corr)))
print(paste("MS indv. income to indv. support for GOP candidate correlation coefficient: ", toString(MS_corr)))
