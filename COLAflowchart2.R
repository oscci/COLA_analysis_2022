
#Making the flowchart with DiagrammeR
#Won't work with markdown, so create flowchart separately

#have combdat loaded.

usePackage("DiagrammeR") #for participant recruitment flowchart
usePackage('DiagrammeRsvg')
usePackage('magrittr')
usePackage('rsvg')

testtab-table(combdat$Rhanded,combdat$ftcd)


LH1 <- testtab[1,1]+testtab[1,2]+testtab[1,3]
RH1 <-testtab[2,1]+testtab[2,2]+testtab[2,3]
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
          Z[label = 'Step2\\nfTCD testing']
          U[label= 'fTCD quality']
          V[label = 'Language screen']
   node [fontname = arial, shape = rectangle, color = grey, style = filled]
  tab1 [label = '@@1']
  tab2 [label='@@2']
  tab3 [label='@@3']
  A [label='@@4']
  B [label='@@5']
  C [label='@@6']
  D [label='@@7']
  tab1-> tab2 -> tab3
  tab3-> {A,B}
  A -> {C,D}
  
      X -> Y [alpha=0,color='white']
      Y -> Z [alpha=0,color='white']
      Z -> U [alpha=0,color='white']
      U -> V [alpha=0,color='white']
              }
}
  
  [1]: paste0('Aged 16-50; Hearing and vision;','\\n','Native language; Neurological','\\n', 'history; Developmental disorders;','\\n', 'fMRI screening; Willingness to','\\n','attend Session 2')
  [2]: paste0('Demographic questionnaire','\\n','Language proficiency measures','\\n','Handedness/footedness/eyedness','\\n','4 lateralisation tasks','\\n','Planned: 300 L-handed + 150 R-handed', '\\n','Actual: ',LH1,' L-handed + ',RH1,' R-handed' )  
  [3]: paste0('At Oxford, Bangor, London','\\n',' Lincoln, Lancaster, or UWA','\\n','6 language tasks','\\n','Planned: 112 L-handed + 112 R-handed','\\n','Actual: ', LH2,' L-handed + ',RH2,' R-handed')
  [4]: paste0('Included','\\n', LH4,' L-handed + ',RH4,' R-handed')
  [5]: paste0('No signal: ','\\n', LH5,' L-handed + ',RH5,' R-handed','\\n','Poor quality:','\\n ',LH5a,' L-handed + ',RH5a,' R-handed')
  [6]: paste0('Native speaker:','\\n', LH6,' L-handed + ',RH6,' R-handed')
  [7]: paste0('Non-native speaker:','\\n','Passed all:','\\n', LH7,' L-handed + ',RH7,' R-handed','\\n','Failed new screen only:','\\n', LH7a,' L-handed + ',RH7a,' R-handed','\\n','Failed prereg screen:','\\n', LH7b,' L-handed + ',RH7b,' R-handed')
")



myflow %>% export_svg %>% charToRaw %>% rsvg %>% png::writePNG("COLA_flow.png")


