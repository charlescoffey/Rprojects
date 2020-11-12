# Charles Coffey
# 17.835 PSET 1
# 9.16.20
#

library(ISLR)
library(MASS)

#1.1 load csv files

cities_info = read.csv("cities_info.csv");
distance_to_wuhan = read.csv("distance_to_wuhan.csv");

#1.2 merged data set with all info
cities_all <- merge(cities_info, distance_to_wuhan, by = "city_name");

head(cities_all)

#1.3 making the data easier to work with because it is skewed

log.wuhan.outflow <- log(cities_all$wuhan_outflow.Jan1.to.Jan24.);
log.cum.cases <- log(cities_all$cumulative_confirmed_cases.Feb19.);

#1.4 plotting outflow from wuhan vs number of cases

plot(log.wuhan.outflow, log.cum.cases, pch = 16, col = "red", 
     main = "Wuhan Outflow vs. Cummulative Number of Cases", 
     xlab = "Log of Outflow from Wuhan",
     ylab = "Log of Cummulative Cases");

#1.5 plot texts instead of points
plot(log.wuhan.outflow, log.cum.cases, type="n")
text(log.wuhan.outflow, log.cum.cases, cities_all$city_name, cex = 0.45)

abline(a=-1.4193, b=0.5662)

#1.6 checking for cities with potential to be seriously affected
cities.with.risk <- character();

for (row in 1:nrow(cities_all)){
  
  if ((cities_all[row, 3]*10^4 > 10^7) && (cities_all[row, 8] > 10^4)){
    
    cities.with.risk <- c(cities.with.risk, cities_all[row,1])
  }
}

print(cities.with.risk)

#2.1
results_germany_2017 <- read.csv("results_germany_2017.csv", sep = ";");

print(results_germany_2017[c(3,5),])

#2.2
ESS_Germany_2018 <- read.csv("ESS_Germany_2018.csv");

  # initialize zero vectors for count number of votes for each party
num_votes_1 = integer(length(results_germany_2017$party_name)) 
num_votes_2 = integer(length(results_germany_2017$party_name))

  #create holding place for keeping track of votes for each party
party_vote_estimates = data.frame(num_votes_1, num_votes_2, row.names = results_germany_2017$party_name)

  #check to see which voters voted and which parties they voted for
for (respondent in 1:nrow(ESS_Germany_2018)){
  if ((ESS_Germany_2018[respondent, "vote"] == "Yes") && (!is.na(ESS_Germany_2018[respondent, "prtvede1"]))){
    party_voted_1 = ESS_Germany_2018[respondent, "prtvede1"]
    party_vote_estimates[party_voted_1, "num_votes_1"] = party_vote_estimates[party_voted_1, "num_votes_1"] + 1
  }
  if ((ESS_Germany_2018[respondent, "vote"] == "Yes") && (!is.na(ESS_Germany_2018[respondent, "prtvede2"]))){
    party_voted_2 = ESS_Germany_2018[respondent, "prtvede2"]
    party_vote_estimates[party_voted_2, "num_votes_2"] = party_vote_estimates[party_voted_2, "num_votes_2"] + 1
  }
}

  #find percentage of voters each party received
percentage_votes_1 = party_vote_estimates$num_votes_1/sum(party_vote_estimates$num_votes_1)*100
percentage_votes_2 = party_vote_estimates$num_votes_2/sum(party_vote_estimates$num_votes_2)*100
names(percentage_votes_1) = c("Percentage Voters 1")
names(percentage_votes_2) = c("Percentage Voters 2")

  #add percentage information to storage data frame
party_vote_estimates <- cbind(party_vote_estimates, percentage_votes_1, percentage_votes_2)

  # calculated difference between expected outcome vs actual outcome 
estimated_minus_result_1 = party_vote_estimates$percentage_votes_1 - results_germany_2017$vote1_perc
estimated_minus_result_2 = party_vote_estimates$percentage_votes_2 - results_germany_2017$vote2_perc

  # add differences
party_vote_estimates <- cbind(party_vote_estimates, estimated_minus_result_1, estimated_minus_result_2)

  # create 7 column data table with all necessary information
data_table = cbind(results_germany_2017$party_name, 
                   party_vote_estimates$percentage_votes_1,
                   results_germany_2017$vote1_perc, 
                   party_vote_estimates$estimated_minus_result_1,
                   party_vote_estimates$percentage_votes_2,
                   results_germany_2017$vote2_perc,
                   party_vote_estimates$estimated_minus_result_2)

  # converting data table to data frame to be able to work with it
data_table = as.data.frame(data_table)
names(data_table) = c("Party Name", "Estimated First Vote", "Actual First Vote","Estimated1-Actual1",
                      "Estimated Second Vote", "Actual Second Vote", "Estimated2-Actual2")
  #converting all numbers to numerics instead of characters
data_table$`Estimated First Vote` = as.numeric(data_table$`Estimated First Vote`)
data_table$`Actual First Vote` = as.numeric(data_table$`Actual First Vote`)
data_table$`Estimated1-Actual1` = as.numeric(data_table$`Estimated1-Actual1`)
data_table$`Estimated Second Vote` = as.numeric(data_table$`Estimated Second Vote`)
data_table$`Actual Second Vote` = as.numeric(data_table$`Actual Second Vote`)
data_table$`Estimated2-Actual2` = as.numeric(data_table$`Estimated2-Actual2`)

  #order data table by most actual first votes
data_table[order(data_table$`Actual First Vote`),]

  #prepare data to be plotted and extract top 5 parties' information
plot_data = t(data_table[c(1:5), c(2:3)])
colnames(plot_data) <- c(data_table$`Party Name`[c(1:5)])

  #create bar plot for data
colors.names = c("cadetblue","chocolate")
par(cex.axis = 0.8)
barplot(as.matrix(plot_data),
        col = colors.names, 
        beside = TRUE,
        legend=rownames(plot_data),
        xlab = "Party",
        ylab = "Percentage",
        ylim = c(0,40),
        main = "Percentage of Voters per Party in 2017 Bundestag Election")

print(data_table)

#build up the table column by column
#calculate all of the percentages for each round on their own
  # find denominator : how many people are eligible to vote and also voted? -> vote1 you just want people that said yes and voted
  # find all parties that you have, for each party look at how many people voted
  # vector with party name, percentage voted for in first vote, percentage voted for in second vote
  # combine estimated and results -> merge these by party name
  # from here, create a new column with the difference between estimated and actual results
  # then order the summary table and grab the top 5 to not worry about the rest of the it

#2.3

#3.1
library(foreign)
LupPon_data <- read.dta("LupPon_APSR.dta");

country_names <- unique(LupPon_data$country);
years <- unique(LupPon_data$year);

redist <- na.omit(LupPon_data$redist)
ratio9050 <- na.omit(LupPon_data$ratio9050)
ratio5010 <- na.omit(LupPon_data$ratio5010)
country_3_obs_count <- 0; #count of countries with all 3 observations

for (row in 1:nrow(LupPon_data)){
  
  if (all(!is.na(LupPon_data[row, c("redist","ratio9050","ratio5010")]))){
    country_3_obs_count = country_3_obs_count + 1;
  }
}

print(paste("There are",toString(length(country_names)),"countries in the dataset.", sep = " "))
print(paste("There are",toString(length(country_names)),"years in the dataset.", sep = " "))
print("Years:")
print(years)
print(paste("There are",toString(country_3_obs_count),"country-observations with all three variables in the dataset.", sep = " "))

#3.2

top_ordered9050 <- LupPon_data[order(LupPon_data$ratio9050, decreasing = TRUE)[c(1:5)], c("country", "ratio9050")]

bottom_ordered9050 <- LupPon_data[order(LupPon_data$ratio9050, decreasing = FALSE)[c(1:5)], c("country", "ratio9050")]

#3.3

top_ordered5010 <- LupPon_data[order(LupPon_data$ratio5010, decreasing = TRUE)[c(1:5)], c("country", "ratio5010")]

bottom_ordered5010 <- LupPon_data[order(LupPon_data$ratio5010, decreasing = FALSE)[c(1:5)], c("country", "ratio5010")]

#3.4

count1 = 0
count2 = 0

  # get rid of country-year pairs that have incomplete data
LupPon_data = LupPon_data[complete.cases(LupPon_data[c('ratio9050','ratio5010')]),]
  
skew = c()
  # calculate ratios for each country-year pair
for (row in 1:nrow(LupPon_data)){
  # variable for 9050/5010
  r9overr5 = LupPon_data[row, "ratio9050"]/LupPon_data[row, "ratio5010"]
  
  skew = append(skew, r9overr5)
  
  if(r9overr5 > 1){
    count1 = count1 + 1
  }
  else{
    count2 = count2 + 1
  }
}

  #add skew to original data frame
LupPon_data = cbind(LupPon_data, skew)

  #box plot data
par(las = 2)
par(cex.axis=0.8)
boxplot(LupPon_data$skew ~ LupPon_data[["country"]], boxwex=0.6, ylab="Ratio between 90-50 and 50-10 percentiles (skew)")

print(paste("There are", toString(count1),"country-year observations with skew>1.", sep = " "))
print(paste("There are", toString(count2),"country-year observations with skew<1.", sep= " "))
