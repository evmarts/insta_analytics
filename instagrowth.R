##########################################################################################
##############################       HELPER FUNCTIONS       ##############################
##########################################################################################

# function foldList sums all the entries in a list with their previous entries
# and returns a list with each entry as the sum of those previous entries
foldList <- function(list){
  sum_so_far = 0
  folded_list = c(sum_so_far)
  n = length(list)
  for (i in 1:n){
    sum_so_far = folded_list[i] + list[i]
    folded_list= c(folded_list, sum_so_far)
  }
  return(folded_list[-1])
}

##########################################################################################
##############################             DATA            ##############################
##########################################################################################

# reading in the data
lhu_dat = read.csv("LHU_stats.csv", header = T)
mmo_dat = read.csv("MMO_stats.csv", header = T)
ftm_dat = read.csv("FTM_stats.csv", header = T)
dru_dat = read.csv("DRU_stats.csv", header = T)
lmo_dat = read.csv("LMO_stats.csv", header = T)

# mmo_dat has the largest interval of observations
n = length(mmo_dat$Date)

# creating dates
mmo_dat$Date <- as.Date(mmo_dat$Date, "%m/%d/%Y")
lhu_dat$Date <- as.Date(lhu_dat$Date, "%m/%d/%Y")
ftm_dat$Date <- as.Date(ftm_dat$Date, "%m/%d/%Y")
dru_dat$Date <- as.Date(dru_dat$Date, "%m/%d/%Y")
lmo_dat$Date <- as.Date(lmo_dat$Date, "%m/%d/%Y")

##########################################################################################
##############################       FOLLOWER  GAINS        ##############################
##########################################################################################

# Plotting the data in fancy Instagram colours
plot(Followers ~ Date, mmo_dat,type = "l", ann = FALSE, col = 'darkgoldenrod1', lwd = 3, ylim = c(0,35000))
lines(Followers ~ Date, lhu_dat,type = "l", col = 'orangered', lwd = 3)
lines(Followers ~ Date, ftm_dat,type = "l", col = 'magenta', lwd = 3)
lines(Followers ~ Date, dru_dat,type = "l", col = 'purple', lwd = 3)
lines(Followers ~ Date, lmo_dat,type = "l", col = 'blue2', lwd = 3)
title(main="Follower growth Aug 28, 2015 - Sept 7, 2016", xlab="date", ylab="followers", lwd = 3)
grid(nx = NULL, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
legend("topleft", legend = c( "@MMO","@LHU", "@FMT", "@DRU", "@LMO"), col = c('darkgoldenrod1', 'orangered', 'magenta', "purple", "blue2"), lwd = 4, lty= 1:1)

##########################################################################################
##############################      FOLLOW BACK RATIO       ##############################
##########################################################################################
# plotting visualization of follow-back ratios

# follow-back for @MMO
plot(log(foldList(Followed)) ~ Date, ann = FALSE, mmo_dat,type = "l", col = 'darkgoldenrod1', lwd = 3)
lines(log((Followers - Followers[1])) ~ Date, mmo_dat, col = "orange", lwd = 3)
title(main = "Follow back rate, @MMO", xlab = "date", ylab = "log(users)")
legend("topleft", legend = c( "Followed","Followers"), col = c('darkgoldenrod1', 'orange'), lwd = 4, lty= 1:1)
# math to determine follow-back ratio
total_followed_lhu = tail(foldList(lhu_dat$Followed), n = 1)
total_followers_lhu = tail(lhu_dat$Followers, n = 1)  - lhu_dat$Followers[1]
lhu_fbr = total_followers_lhu/total_followed_lhu

# follow-back for @LHU
plot(log(foldList(Followed)) ~ Date, ann = FALSE, lhu_dat,type = "l", col = 'orangered', lwd = 3)
lines(log((Followers- Followers[1])) ~ Date, lhu_dat, col = "red3", lwd = 3)
title(main = "Follow back rate, @LHU", xlab = "date", ylab = "log(users)")
legend("topleft", legend = c( "Followed","Followers"), col = c('orangered', 'red3'), lwd = 4, lty= 1:1)
# math to determine follow-back ratio
total_followed_lhu = tail(foldList(lhu_dat$Followed), n = 1)
total_followers_lhu = tail(lhu_dat$Followers, n = 1)  - lhu_dat$Followers[1]
lhu_fbr = total_followers_lhu/total_followed_lhu

# follow-back for @DRU
plot(log(foldList(Followed)) ~ Date, ann = FALSE, dru_dat,type = "l", col = 'purple', lwd = 3)
lines(log((Followers-Followers[1])) ~ Date, dru_dat, col = "purple3", lwd = 3)
title(main = "Follow back rate, @DRU", xlab = "date", ylab = "log(users)")
legend("topleft", legend = c( "Followed","Followers"), col = c('purple', 'purple3'), lwd = 4, lty= 1:1)
# math to determine follow-back ratio
total_followed_dru = tail(foldList(dru_dat$Followed), n = 1)
total_followers_dru = tail(dru_dat$Followers, n = 1)  - dru_dat$Followers[1]
dru_fbr = (total_followers_dru)/total_followed_dru

# follow-back for @FTM
plot(log(foldList(Followed)) ~ Date, ann = FALSE, ftm_dat,type = "l", col = 'magenta', lwd = 3)
lines(log((Followers- Followers[1])) ~ Date, ftm_dat, col = "magenta3", lwd = 3)
title(main = "Follow back rate, @FTM", xlab = "date", ylab = "log(users)")
legend("topleft", legend = c( "Followed","Followers"), col = c('magenta', 'magenta3'), lwd = 4, lty= 1:1)
# math to determine follow-back ratio
total_followed_ftm = tail(foldList(ftm_dat$Followed), n = 1)
total_followers_ftm = tail(ftm_dat$Followers, n = 1)  - ftm_dat$Followers[1]
ftm_fbr = total_followers_ftm/total_followed_ftm

# follow-back for @LMO
plot(log(foldList(Followed)) ~ Date, ann = FALSE, lmo_dat,type = "l", col = 'blue', lwd = 3)
lines(log((Followers- Followers[1])) ~ Date, lmo_dat, col = "blue3", lwd = 3)
title(main = "Follow back rate, @LMO", xlab = "date", ylab = "log(users)")
legend("topleft", legend = c( "Followed","Followers"), col = c('blue', 'blue3'), lwd = 4, lty= 1:1)
# math to determine follow-back ratio
total_followed_lmo = tail(foldList(lmo_dat$Followed), n = 1)
total_followers_lmo = tail(lmo_dat$Followers, n = 1)  - lmo_dat$Followers[1]
lmo_fbr = total_followers_lmo/total_followed_lmo

# plotting daily follower gains
plot(Gains ~ Date, mmo_dat,type = "l", ann = FALSE, col = 'darkgoldenrod1', lwd = 3)
abline(mean(dru_dat$Gains),0, col = "orange")
title(main="Daily follower gains, @MMO", xlab="date", ylab="followers", lwd = 3)

## TODO: figure out a way to smmoth data to give observations for each day
## TODO: do a plot of followback ratio 





