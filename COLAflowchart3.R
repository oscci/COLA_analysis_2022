
#Making the flowchart with DiagrammeR
#Won't work with markdown, so create flowchart separately

#have combdat loaded.

usePackage("DiagrammeR") #for participant recruitment flowchart
usePackage('DiagrammeRsvg')
usePackage('magrittr')
usePackage('rsvg')


mydir <- "~/Dropbox/COLA_RR_Analysis"
figdir <-"03-graphic-outputs" 


# readfile<-paste0(mydir,"/02-data/combined_data.csv")
# combdat <- read.csv(readfile)
# 
# 
# #One subject was wrongly coded as ftcd=1: they came for testing but could not get signal and so we reset ftcd to zero for them.
# w<-which(combdat$ID==3495951)
# combdat$ftcd[w]<-9
# 
# combdat$lang2exclude <- 0
# w<- which(combdat$gramerr12>1)
# combdat$lang2exclude[w]<-10
# 
# w<- which(combdat$lexTALE<80)
# combdat$lang2exclude[w]<-combdat$lang2exclude[w]+1
# #For main paper, we set 'excluded' for any who were below 80 on LexTale or more than 1 error on short grammar AND are not native English speakers. I.e. all native English are included, regardless of language tests
# #We also do this when supplementary =1


testtab <- table(combdat$Rhanded,combdat$ftcd)

rownames(testtab)<-c('Left-handed','Right-handed')


testtab-table(combdat$Rhanded,combdat$ftcd)


LH1 <- testtab[1,1]+testtab[1,2]+testtab[1,3]
RH1 <-testtab[2,1]+testtab[2,2]+testtab[2,3]

#Need to divide into Group 1 and 2 according to language
#Group 1 has native speakers and non-native who passed screen
#Group 2 has (a) nonnative who failed prereg only and (b) nonnative who failed new screen

natives <- table(combdat$nativeEnglish,combdat$Rhanded)
LH1a <- natives[2,1]
RH1a <-natives[2,2]

nonnatives<-filter(combdat,nativeEnglish==0)
nonnatives$failprereg <- 0
nonnatives$failprereg[nonnatives$gramerrs>3]<-1
nntab<-table(nonnatives$lang2exclude,nonnatives$failprereg,nonnatives$Rhanded)

LH1b <- nntab[1,1,1]
RH1b <- nntab[1,1,2]

LH1c <- nntab[1,2,1] #failed prereg screen only
RH1c <- nntab[1,2,2]

LH1d <- sum(nntab[,,1])-LH1b-LH1c
RH1d <- sum(nntab[,,2])-RH1b-RH1c

LH2<- testtab[1,2]
RH2<- testtab[2,2]

thisdat<-combdat[combdat$ftcd>0,] #all with 1 or 9 for ftcd
w<-which(thisdat$ftcd==9)
thisdat$DopExclude[w]<-9
testtab2<-table(thisdat$Rhanded,thisdat$DopExclude)

thisdat2 <- thisdat[thisdat$DopExclude==0,]
thisdat3<-thisdat2[thisdat2$nativeEnglish==1,]
testtab3<-table(thisdat3$Rhanded)

#nonnative only
thisdat4<-thisdat2[thisdat2$nativeEnglish==0,]
thisdat4$preregx <- 0
w<-which(thisdat4$gramerrs>3)
thisdat4$preregx[w]<-1
testtab4<-table(thisdat4$preregx,thisdat4$lang2exclude,thisdat4$Rhanded)



LH4 <- testtab2[1,1]
LH5 <- testtab2[1,3]
LH5a <- testtab2[1,2]
LH6 <- testtab3[1]
LH7 <- testtab4[1,1,1]
LH7b <- testtab4[2,1,1]
LH7a <- sum(testtab4[,,1])-LH7-LH7b

RH4 <- testtab2[2,1]
RH5 <- testtab2[2,3]
RH5a <- testtab2[2,2]
RH6 <- testtab3[2]
RH7 <- testtab4[1,1,2]
RH7b <- testtab4[2,1,2]
RH7a <- sum(testtab4[,,2])-RH7-RH7b


myflow<-grViz("digraph flowchart {
  node [fontname = arial, shape = plaintext]
          X[label='Screening\\nquestions']
          Y[label= 'Step 1\\nOnline testing']
          Q[label = 'Language screen']
          Z[label = 'Step2\\nfTCD testing for subset']
          U[label= 'fTCD quality']
          V[label = 'Language screen']
   node [fontname = arial, shape = rectangle, color = lightblue, style = filled]
  A [label = '@@1']
  B[label='@@2']
  C [label='@@3']
  D [label='@@4']
  E [label='@@5']
  F [label='@@6']
  G [label='@@7']
  H [label='@@8']
  I [label='@@9']

  A -> B 
  B->{C,D}
  C -> E
  D -> E
  E ->{F,G}
  F -> {H,I}

  
      X -> Y [alpha=0,color='white']
      Y -> Q [alpha=0,color='white']
      Q -> Z [alpha=0,color='white']
      Z -> U [alpha=0,color='white']
      U -> V [alpha=0,color='white']
              }
}
  
  [1]: paste0('Aged 16-50; Hearing and vision;','\\n','Native language; Neurological','\\n', 'history; Developmental disorders;','\\n', 'fMRI screening; Willingness to','\\n','attend Session 2')
  [2]: paste0('Demographic questionnaire','\\n','Language proficiency measures','\\n','Handedness/footedness/eyedness','\\n','4 lateralisation tasks','\\n','Planned: 300 L-handed + 150 R-handed', '\\n','Actual: ',LH1,' L-handed + ',RH1,' R-handed' )  
  [3]: paste0('Group 1','\\n','Native speaker:','\\n', LH1a,' L-handed + ',RH1a,' R-handed','\\n','Non-native speaker, passed','\\n', LH1b,' L-handed + ',RH1b,' R-handed')
  [4]: paste0('Group 2','\\n','Non-native speaker:','\\n','a) Failed prereg only:','\\n', LH1c,' L-handed + ',RH1c,' R-handed','\\n','b) Failed new screen:','\\n', LH1d,' L-handed + ',RH1d,' R-handed','\\n')
  [5]: paste0('At Oxford, Bangor, London','\\n',' Lincoln, Lancaster, or UWA','\\n','6 language tasks','\\n','Planned: 112 L-handed + 112 R-handed','\\n','Actual: ', LH2,' L-handed + ',RH2,' R-handed')
  [6]: paste0('Included','\\n', LH4,' L-handed + ',RH4,' R-handed')
  [7]: paste0('No signal: ','\\n', LH5,' L-handed + ',RH5,' R-handed','\\n','Poor quality:','\\n ',LH5a,' L-handed + ',RH5a,' R-handed')
  [8]: paste0('Group 1','\\n','Native speaker:','\\n', LH6,' L-handed + ',RH6,' R-handed','\\n','Non-native speaker:Passed:','\\n', LH7,' L-handed + ',RH7,' R-handed')
  [9]: paste0('Group 2','\\n','Non-native speaker:','\\n','a) Failed prereg only:','\\n', LH7b,' L-handed + ',RH7b,' R-handed','\\n','b) Failed new screen:','\\n', LH7a,' L-handed + ',RH7a,' R-handed','\\n')
")

makefile<-1
if(makefile==1){
myflow %>% export_svg %>% charToRaw %>% rsvg %>% png::writePNG("COLA_flow.png")
}


