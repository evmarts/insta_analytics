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

fb_plot <- function(dat, acc, col1, col2){
  total_followed = tail(foldList(dat$Followed), n = 1)
  total_followers = tail(dat$Followers, n = 1)  - dat$Followers[1]
  fbr = total_followers/total_followed
  
  plot(log(foldList(Followed)) ~ Date, ann = FALSE, dat,type = "l", col = col1, lwd = 3)
  lines(log((Followers - Followers[1])) ~ Date, dat, col = col2, lwd = 3)
  title = "Follow back rate,"
  title(main = paste(title, acc), xlab = "date", ylab = "log(users)", sub = paste("follow-back ratio =", round(fbr,3)))
  legend("topleft", legend = c( "Followed","Followers"), col = c(col1, col2), lwd = 4, lty= 1:1)
  
  f = paste(acc, ".png", sep = "")
  dev.copy(jpeg,filename=paste("figs/", f, sep = ""),width = 500, height = 500);
  dev.off ();
}

##########################################################################################
##############################             DATA            ##############################
##########################################################################################

# reading in the data
lhu_dat = read.csv("data/LHU_stats.csv", header = T)
mmo_dat = read.csv("data/MMO_stats.csv", header = T)
ftm_dat = read.csv("data/FTM_stats.csv", header = T)
dru_dat = read.csv("data/DRU_stats.csv", header = T)
lmo_dat = read.csv("data/LMO_stats.csv", header = T)

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

# save the fig
dev.copy(jpeg,filename="figs/growth.png",width = 1000, height = 500);
dev.off ();

##########################################################################################
##############################      FOLLOW BACK RATIO       ##############################
##########################################################################################
# plotting visualization of follow-back ratios

fb_plot(mmo_dat,'@MMO', 'darkgoldenrod1', 'orange')
fb_plot(lhu_dat,'@LHU', 'orangered', 'red3')
fb_plot(dru_dat,'@DRU', 'purple', 'purple3')
fb_plot(ftm_dat,'@FTM', 'magenta', 'magenta3')
fb_plot(lmo_dat,'@LMO', 'blue', 'blue3')

# plotting daily follower gains
plot(Gains ~ Date, mmo_dat,type = "l", ann = FALSE, col = 'darkgoldenrod1', lwd = 3)
abline(mean(dru_dat$Gains),0, col = "orange")
title(main="Daily follower gains, @MMO", xlab="date", ylab="followers", lwd = 3)

## TODO: figure out a way to smmoth data to give observations for each day
## TODO: do a plot of followback ratio 





