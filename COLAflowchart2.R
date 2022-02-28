

usePackage("DiagrammeR") #for participant recruitment flowchart
usePackage('DiagrammeRsvg')
usePackage('magrittr')
usePackage('rsvg')




LH1 <- testtab[1,1]+testtab[1,2]
RH1 <-testtab[2,1]+testtab[2,2]
LH2<- testtab[1,2]
RH2<- testtab[2,2]
#LH2 <- langtab[1,1,1]+langtab[1,1,2]+langtab[1,2,2]+langtab[1,3,2]+langtab[1,4,2]
#RH2 <- langtab[2,1,1]+langtab[2,1,2]+langtab[2,2,2]+langtab[2,3,2]+langtab[2,4,2]


myflow<-grViz("digraph flowchart {
  node [fontname = arial, shape = rectangle, color = grey, style = filled]
  tab1 [label = '@@1']
  tab2 [label='@@2']
  tab3 [label='@@3']
  tab1-> tab2 -> tab3
}
  
  [1]: paste0('Screening questions','\\n', 'Aged 16-50; Hearing and vision;','\\n','Native language; Neurological','\\n', 'history; Developmental disorders;','\\n', 'fMRI screening; Willingness to','\\n','attend Session 2')
  [2]: paste0('Step 1: Online battery','\\n','Demographic questionnaire','\\n','Language proficiency measures','\\n','Handedness/footedness/eyedness','\\n','4 lateralisation tasks','\\n','Planned: 300 L-handed + 150 R-handed', '\\n','Actual: ',LH1,' L-handed + ',RH1,' R-handed' )  
  [3]: paste0('Step 2:   Functional TCD', '\\n','At Oxford, Bangor, London','\\n',' Lincoln, Lancaster, or UWA','\\n','6 language tasks','\\n','Planned: 112 L-handed + 112 R-handed','\\n','Actual: ', LH2,' L-handed + ',RH2,' R-handed')
")



myflow %>% export_svg %>% charToRaw %>% rsvg %>% png::writePNG("COLA_flow.png")


