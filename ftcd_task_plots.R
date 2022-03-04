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
longtasks<-c('Word Generation','Sentence Generation','Phonological Decision','Word Comprehension','Sentence Comprehension','Syntactic Decision')
ntasks <- length(tasks)

basestart=-5 # baseline start
baseend=2 # baseline end
poistart=6 # period of interest start
poiend=23 # period of interest end

## Read in data
datadir <- "~/Dropbox/COLA_RR_Analysis/02-data/02.2_ftcd/ftcd_task_means/"
ftcddata <- read.csv("~/Dropbox/COLA_RR_Analysis/02-data/02.2_ftcd/ftcd_data.csv")
w<-which(is.na(ftcddata$A_exclude)) #all cases with no signal
ftcddata <- ftcddata[-w,] 

ftcddata$Nexclude <- ftcddata$A_exclude + ftcddata$B_exclude + ftcddata$C_exclude + ftcddata$D_exclude + ftcddata$E_exclude + ftcddata$F_exclude
ftcddata <- ftcddata %>% filter(Nexclude <= 1)

#NB; some are NA on Nexclude - I checked and these are both cases with just one missing



# Load in the first one to find out how big it is
tmpfile <- read.csv(paste0(datadir, '3151603_WG.csv'))

# Get the x-axis (time) information from tmpfile
mytime <- tmpfile$time

## Get data for left-handers (h=0) and right-handers (h=1)
for (t in 1:ntasks){
  taskmean <- matrix(data=NA, nrow=dim(tmpfile)[1], ncol=6)
  colnames(taskmean) <- c('LeftHand_L', 'LeftHand_R', 'RightHand_L', 
  'RightHand_R','LeftHand_L-R','RightHand_L-R')
  for (h in 0:1){
    mysubj <- which(ftcddata$Hand_R == h) #rows of subs with this handedness
    nsubj <- length(mysubj)
     # Create matrices for the data we're going to read in (Ldata and Rdata)
    Ldata <- Rdata <-diffdata<-matrix(data=NA, nrow=dim(tmpfile)[1], ncol=nsubj)
    
    for (s in 1:nsubj){
      # Read in the data
      mysubname <- ftcddata$Gorilla_ID[mysubj[s]]
      myfile <- paste0(datadir,mysubname,'_',tasks[t],'.csv')
      if (file.exists(myfile)){
        mydat <- read.csv(myfile)
        Ldata[ , s] <- mydat$Lmean
        Rdata[ , s] <- mydat$Rmean
        diffdata[,s]<-100+mydat$meanDiff #NB we add 100 to the difference so we can put it on the same plot
      }
    } # end subject loop
    
    # Average over participants (rows)
    Lmean <- rowMeans(Ldata, na.rm=TRUE)
    Rmean <- rowMeans(Rdata, na.rm=TRUE)
    diff <- rowMeans(diffdata,na.rm=TRUE)
    
    taskmean[ , (h*2+1)] <- Lmean
    taskmean[ , (h*2+2)] <- Rmean
    taskmean[,(h+5)] <- diff #diffs in cols 5 and 6
    
  } # end handedness loop
  
  # Create plot for this task
  # Make data longer
  taskmean <- as.data.frame(taskmean)
  taskmean$Time <- mytime
  
  # Crop to -10 to 30 seconds
  start_index <- which(mytime==-10)
  end_index <- which(mytime==24)
  xtime <- mytime[start_index:end_index]
  taskmean <- taskmean[start_index:end_index, ]
  taskmean_long <- pivot_longer(data=taskmean, cols=c(1:6), 
                                names_to = 'Condition', values_to='Velocity')
  
  if (t == 1 | t == 2) {poiend <- 17}
  if (t > 2) {poiend <- 23}
  
  #chaos caused by R's ordering of factors, 
  # so we need to change some names before factorising.
  
  uniques<-unique(taskmean_long$Condition)
  #These are actually in the right order! Presumably because of order in original file.
  #But they are character strings that get alphabetised if factored.
  #So!
  for (i in 1:6){
    w<-which(taskmean_long$Condition==uniques[i])
    n<-c(1,2,3,4,5,6)
    taskmean_long$Condition[w]<-paste0(n[i],uniques[i]) #stick a number on the front
  }

  #Total nightmare getting correct allocation of condition to linetype and colour!
  taskmean_long$Handed_Side <- as.factor(taskmean_long$Condition)
  levels(taskmean_long$Handed_Side)<- c('L hander/L side', 'L hander/R side', 
                                        'R hander/L side','Rhander/R side',
                                        'L hander/L-R','R hander/L-R')
#Make a little file that allocates linetype/color to condition, but NB whether this works depends also on code below, as order of factors v important
  color_lty_cross = expand.grid(
    ltypes = c(1,2),
    colors = c('red','blue','black'),
    stringsAsFactors = F)
  color_lty_cross$condition<-levels(taskmean_long$Handed_Side)
#Change the order, as preferable to have solid for all R and dotted for all L
  color_lty_cross$ltypes <- c(2,2,1,1,2,1)
  color_lty_cross$colors <- c("red","blue","red","blue","black","black")
  
  thisplot<-ggplot(taskmean_long, aes(x=Time, y=Velocity, group=Handed_Side)) + 
    geom_line(aes(linetype=Handed_Side,colour=Handed_Side,)) +
    ylim(94,108)+
    xlim(-5,25)+
    scale_y_continuous(breaks = seq(94, 108, by=2),name = "Blood flow velocity", sec.axis = sec_axis(~.-100, name = "L-R"))+
    scale_x_continuous(breaks = seq(-5, 25, by=5),name = "Time (s)")+
    scale_color_manual(values = color_lty_cross$colors) +
    scale_linetype_manual(values = color_lty_cross$ltypes[1:6]) +
    theme_bw()+
    geom_hline(yintercept = 100, linetype="solid", alpha = 0.8) +
    geom_vline(xintercept = 0, linetype="solid", alpha = 0.8) +
    annotate(geom="label", x=-1, y=95, label="Baseline",hjust=0.5,size=3,fill='black',colour='white')+
    annotate(geom="label", x=(poistart+poiend)/2, y=95, label="Period of Interest",hjust=0.5,size=3,fill='black',colour='white')+
    geom_vline(xintercept = basestart, linetype = "dotted", alpha = 0.8) + # Start of Baseline
    geom_vline(xintercept = baseend, linetype = "dotted", alpha = 0.8) + # End of Baseline
    geom_vline(xintercept = poistart, linetype = "dotted", alpha = 0.8) + # Start of POI
    geom_vline(xintercept = poiend, linetype = "dotted", alpha = 0.8) +  # End of POI
    ggtitle(longtasks[t])
  thisplot
  ggsave(paste0('~/Dropbox/COLA_RR_Analysis/03-graphic-outputs/ftcd_grandmeanx_', tasks[t],'.png'),
         width = 6,
         height = 3,)
  d6<-thisplot
  if(t==1){d1<-thisplot}
  if(t==2){d2<-thisplot}
  if(t==3){d3<-thisplot}
  if(t==4){d4<-thisplot}
  if(t==5){d5<-thisplot}
  
}
alldens <- ggarrange(d1,d2,d3,d4,d5,d6, ncol = 2, nrow = 3,common.legend=TRUE)
ggsave("~/Dropbox/COLA_RR_Analysis/03-graphic-outputs/alltimecourse.png",width = 7, height = 7)

ggsave("alltimecourse.png",width = 7, height = 7)



  
  # The POI ends earlier for WG and SG tasks, due to REPORT phase

  





## Also a pirate plot of LI values
# 
# plotdata <- ftcddata %>% select(Gorilla_ID, Hand_R, A_mean_LI, B_mean_LI, 
#                                 C_mean_LI, D_mean_LI, E_mean_LI, F_mean_LI)
# colnames(plotdata) <- c('ID','Hand','A','B','C','D','E','F')
# longdata <- pivot_longer(data = plotdata, cols = c(3:8), names_to = 'Task', values_to = 'LI')
# pirateplot(data = longdata, LI ~ Hand * Task)
# abline(h=0)
# title(main=paste0('Pirate Plot of LI Data, n=',length(ftcddata$Gorilla_ID)))

