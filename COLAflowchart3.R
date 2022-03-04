
#Making the flowchart with DiagrammeR
#Won't work with markdown, so create flowchart separately

#have combdat loaded.

require(DiagrammeR) #for participant recruitment flowchart
require(DiagrammeRsvg)
require(magrittr)
require(rsvg)

combdat<-read.csv('combdat.csv')
mydir <- "~/Dropbox/COLA_RR_Analysis"
figdir <-"03-graphic-outputs" 


testtab <- table(combdat$Rhanded,combdat$ftcd)

rownames(testtab)<-c('Left-handed','Right-handed')


testtab-table(combdat$Rhanded,combdat$ftcd)


LH1 <- testtab[1,1]+testtab[1,2]+testtab[1,3]
RH1 <-testtab[2,1]+testtab[2,2]+testtab[2,3]

#Need to divide into Group 1 and 2 according to language
#Group 1 has native speakers and non-native who passed screen
#Group 2 has (a) nonnative who failed prereg only and (b) nonnative who failed new screen

langtab<-table(combdat$Rhanded,combdat$langgroup)

LH1a <- langtab[1,1]
RH1a <-langtab[2,1]
LH1b <- langtab[1,2]
RH1b <- langtab[2,2]
LH1c <- langtab[1,3]
RH1c <- langtab[2,3]
LH1d <-langtab[1,4]
RH1d <-langtab[2,4]


thisdat<-combdat[combdat$ftcd>0,] #all with 1 or 9 for ftcd
w<-which(thisdat$ftcd==9)
thisdat$DopExclude[w]<-9
testtab2<-table(thisdat$Rhanded,thisdat$DopExclude)

LH2 <- testtab2[1,1]+testtab2[1,3] #here we include those who had unusable data
RH2 <- testtab2[2,1]+testtab2[2,3]



dopdat<-combdat[combdat$ftcd==1,]

LH4 <- testtab2[1,1]
LH5 <- testtab2[1,3]
LH5a <- testtab2[1,2]
RH4 <- testtab2[2,1]
RH5 <- testtab2[2,3]
RH5a <- testtab2[2,2]

langtab<-table(dopdat$Rhanded,dopdat$langgroup)

LH6a <- langtab[1,1]
RH6a <-langtab[2,1]
LH6b <- langtab[1,2]
RH6b <- langtab[2,2]
LH6c <- langtab[1,3]
RH6c <- langtab[2,3]
LH6d <-langtab[1,4]
RH6d <-langtab[2,4]



myflow<-grViz("digraph flowchart {
  node [fontname = arial, shape = plaintext]
          X[label='Screening\\nquestions']
          Y[label= 'Step 1\\nOnline testing']
          Q[label = 'Language screen']
          Z[label = 'Step2\\nfTCD for subset']
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
  [2]: paste0('Demographic questionnaire','\\n','Language proficiency measures','\\n','Handedness/footedness/eyedness','\\n','4 lateralisation tasks','\\n','Planned: 300 L-handed + 150 R-handed', '\\n','Actual: ',LH1,' L-handed + ',RH1,' R-handed','\\n','(Retested: 28 L-handed + 25 R-handed)' )  
  [3]: paste0('Group 1','\\n','Native speaker:','\\n', LH1a,' L-handed + ',RH1a,' R-handed','\\n','Non-native speaker, passed','\\n', LH1b,' L-handed + ',RH1b,' R-handed')
  [4]: paste0('Group 2','\\n','Non-native speaker:','\\n','a) Failed prereg only:','\\n', LH1c,' L-handed + ',RH1c,' R-handed','\\n','b) Failed new screen:','\\n', LH1d,' L-handed + ',RH1d,' R-handed','\\n')
  [5]: paste0('At Oxford, Bangor, London','\\n',' Lincoln, Lancaster, or UWA','\\n','6 language tasks','\\n',LH2,' L-handed + ',RH2,' R-handed')
  [6]: paste0('Included','\\n', LH4,' L-handed + ',RH4,' R-handed','\\n','(Planned: 112 L-handed + 112 R-handed)')
  [7]: paste0('No signal: ','\\n', LH5,' L-handed + ',RH5,' R-handed','\\n','Poor quality:','\\n ',LH5a,' L-handed + ',RH5a,' R-handed')
  [8]: paste0('Group 1','\\n','Native speaker:','\\n', LH6a,' L-handed + ',RH6a,' R-handed','\\n','Non-native speaker, passed:','\\n', LH6b,' L-handed + ',RH6b,' R-handed')
  [9]: paste0('Group 2','\\n','Non-native speaker:','\\n','a) Failed prereg only:','\\n', LH6c,' L-handed + ',RH6c,' R-handed','\\n','b) Failed new screen:','\\n', LH6d,' L-handed + ',RH6d,' R-handed','\\n')
")

makefile<-1
if(makefile==1){
  filename<-paste0(mydir,"/",figdir,"/","COLAflow.jpg")
myflow %>% export_svg %>% charToRaw %>% rsvg %>% jpeg::writeJPEG(filename)
}


