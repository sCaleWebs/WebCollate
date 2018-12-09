# sCaleWebs
# script to import literature, feeding trial and gut analysis data from googlesheets

library(googlesheets)
library(dplyr)


#import the literature and feeding trial 

lit <- gs_key("1IBOURUMrSwO488_KjpNI5-q7sgv-R-GAa4seGTzNVqE")

lit%>%gs_ws_ls()

cons<-lit %>% 
  gs_read(ws = "literature_consumer_info", range = cell_cols(1:7))

res<-lit %>% 
  gs_read(ws = "literature_resource_info", range = cell_cols(1:7))

ref<-lit %>% 
  gs_read(ws = "literature_sources", range = cell_cols(1:4))

trial<-lit %>% 
  gs_read(ws = "feeding_trials")


#import the gut analyses, note duplication of Culicidae col

gut<-gs_key("1cntlV8HDqUck2VZiFiOkCgUlE4L2TcXwOugj5FMIuEY")

gut%>%gs_ws_ls()

prgut<-gut %>% 
  gs_read(ws = "ElVerde")

cogut<-gut %>% 
  gs_read(ws = "Colombia")

mogut<-gut %>% 
  gs_read(ws = "Monteverde")

cagut<-gut %>% 
  gs_read(ws = "Cardoso")

magut<-gut %>% 
  gs_read(ws = "Macae")

##assemble the literature matrix and remove surplus columns

lit.matrix<-left_join(cons, res)
lit.m<-left_join(lit.matrix, ref)%>%select(-who,-X4)
lit.m<-mutate(lit.m, habitat=as.factor(habitat))

tapply(lit.m$consumer_lowest_taxon_name, lit.m$habitat, length)%>%as.matrix()


##assemble the gut information

prgut$Culicidae_1<-0 #just to give the code below something to work with

#in which we correct the commas in numbers, class of variables, duplicate Culicidae
#more efficient here than in googlesheets

numbermaker<-function(a){
  as.integer(a)
}

gsfixer<-function(b){
  
  new<-b%>%
    select(FPOM:ncol(b))%>%
    apply(2, numbermaker)%>%
    as.data.frame
  
  full<-b%>%
    select(-(FPOM:ncol(b)))%>%
    cbind(new)
  
  fixed<-full%>%
    mutate(body.length.mm=gsub("\\,", ".", body.length.mm)%>%as.numeric(),
           vial.code = as.character(vial.code),
           Nina.Mairead = as.character(Nina.Mairead),
           slide.abbr = as.character(slide.abbr),
           individual.id = as.integer(individual.id),
           Culicidae=Culicidae+Culicidae_1)%>%
    select(-Culicidae_1)
  
  return(fixed)
}

prgut<-gsfixer(prgut)
cogut<-gsfixer(cogut)
cagut<-gsfixer(cagut)
mogut<-gsfixer(mogut)
magut<-gsfixer(magut)

gut1<-full_join(prgut, cogut)%>%full_join(magut)%>%full_join(cagut)%>%full_join(mogut)

write.csv(gut1, "guts.csv")

write.csv(lit.m, "lit.csv")

write.csv(trial, "trial.csv")
