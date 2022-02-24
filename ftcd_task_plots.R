## Script to create fTCD timecourse plots for each of the 6 language tasks. ZW Feb 2022

## The script uses data produced during the fTCD preprocessing. 
## For each participant, there are 6 .csv files, one for each task,
## containing the averaged, preprocessed fTCD signal for the task.

## The script reads in all of the data files and averages over all participants,
## to produce one grand mean plot for each task.

## 1. Prepare packages
require(tidyverse)
require(yarrr)

tasks <- c('WG','SG','PD','WC','SC','SD')
ntasks <- length(tasks)

basestart=-5 # baseline start
baseend=2 # baseline end
poistart=6 # period of interest start
poiend=23 # period of interest end

## Read in data
# ftcddata is processed data; subjects in rows, various ftcd data in cols.
# includes mean LIs per task
datadir <- "~/Dropbox/COLA_RR_Analysis/02-data/02.2_ftcd/ftcd_task_means/"
ftcddata <- read.csv("~/Dropbox/COLA_RR_Analysis/02-data/02.2_ftcd/ftcd_data.csv")
ftcddata$exclude <- ftcddata$A_exclude + ftcddata$B_exclude + ftcddata$C_exclude + ftcddata$D_exclude + ftcddata$E_exclude + ftcddata$F_exclude
ftcddata <- ftcddata %>% filter(exclude <= 1)

## Get data for left-handers (h=0) and right-handers (h=1)

# Load in the first one to find out how big it is
tmpfile <- read.csv(paste0(datadir, '3151603_WG.csv'))

# Get the x-axis (time) information from tmpfile
mytime <- tmpfile$time

for (t in 1:ntasks){
  taskmean <- matrix(data=NA, nrow=dim(tmpfile)[1], ncol=4)
  colnames(taskmean) <- c('LeftHand_L', 'LeftHand_R', 'RightHand_L', 'RightHand_R')
  for (h in 0:1){
    mysubj <- which(ftcddata$Hand_R == h)
    nsubj <- length(mysubj)
    
    # Create matrices for the data we're going to read in (Ldata and Rdata)
    Ldata <- Rdata <- matrix(data=NA, nrow=dim(tmpfile)[1], ncol=nsubj)
    
    for (s in 1:nsubj){
      # Read in the data
      mysubname <- ftcddata$Gorilla_ID[s]
      myfile <- paste0(datadir,mysubname,'_',tasks[t],'.csv')
      if (file.exists(myfile)){
        mydat <- read.csv(myfile)
        Ldata[ , s] <- mydat$Lmean
        Rdata[ , s] <- mydat$Rmean
      }
    } # end subject loop
    
    # Average over participants (rows)
    Lmean <- rowMeans(Ldata, na.rm=TRUE)
    Rmean <- rowMeans(Rdata, na.rm=TRUE)
    
    taskmean[ , (h*2+1)] <- Lmean
    taskmean[ , (h*2+2)] <- Rmean
    
  } # end handedness loop
  
  # Create plot for this task
  # Make data longer
  taskmean <- as.data.frame(taskmean)
  taskmean$Time <- mytime
  
  # Crop to -5 to 25 seconds

  start_index <- which(mytime==-5)
  end_index <- which(mytime==25)
  xtime <- mytime[start_index:end_index]
  taskmean <- taskmean[start_index:end_index, ]
  taskmean_long <- pivot_longer(data=taskmean, cols=c(1:4), names_to = 'Condition', values_to='Velocity')
  
  if (t == 1 | t == 2) {poiend <- 17}
  if (t > 2) {poiend <- 23}
  
  myplot = ggplot(data=taskmean_long, aes(x=Time, y=Velocity, group=Condition)) +
    geom_line(aes(colour=Condition)) +
    geom_hline(yintercept = 100, linetype="solid", alpha = 0.8) +
    geom_vline(xintercept = 0, linetype="solid", alpha = 0.8) +
    scale_y_continuous(breaks = seq(94, 104, by=2)) +
    geom_vline(xintercept = basestart, linetype = "dashed", alpha = 0.8) + # Start of Baseline
    geom_vline(xintercept = baseend, linetype = "dashed", alpha = 0.8) + # End of Baseline
    geom_vline(xintercept = poistart, linetype = "dashed", alpha = 0.8) + # Start of POI
    geom_vline(xintercept = poiend, linetype = "dashed", alpha = 0.8) +  # End of POI
    ggtitle(tasks[t])
  
  ggsave(paste0('~/Dropbox/COLA_RR_Analysis/03-graphic-outputs/ftcd_grandmean_', tasks[t],'.png'))
} # end task loop




  
  # The POI ends earlier for WG and SG tasks, due to REPORT phase

  





## Also a pirate plot of LI values

plotdata <- ftcddata %>% select(Gorilla_ID, Hand_R, A_mean_LI, B_mean_LI, 
                                C_mean_LI, D_mean_LI, E_mean_LI, F_mean_LI)
colnames(plotdata) <- c('ID','Hand','A','B','C','D','E','F')
longdata <- pivot_longer(data = plotdata, cols = c(3:8), names_to = 'Task', values_to = 'LI')
pirateplot(data = longdata, LI ~ Hand * Task)
abline(h=0)
title(main=paste0('Pirate Plot of LI Data, n=',length(ftcddata$Gorilla_ID)))

