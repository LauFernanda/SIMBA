library(tidyverse)
setwd('./Dropbox/Andes/SIMBA/')

directory<-getwd()

files<-list.files(directory, full.names= TRUE,pattern="*.txt") ##hace lista de csv en carpeta

filesdata<- data.frame()
#i=2

for(i in 1:length(files)){
  file <- read.table(files[i])
  names(file)<-'desempeno'
  file$t <- 1:nrow(file) 
  type<-sub('strat.*', "", files[i])
  type<-sub(".*type=", "",type)
  file$network_type<-type
  if(grepl('S=3',files[i])){
    file$scope<-3
  } else  if(grepl('S=9',files[i])){
    file$scope<-9
  }
  
  if(grepl('best',files[i])){
    file$strategy<-'best member'
  }  else if(grepl('conformity',files[i])){
    file$strategy<-'conformity'
  }
  if(grepl('k=0',files[i])){
    file$env<-'simple'
  } else  if(grepl('k=7',files[i])){
    file$env<-'complex'
  }
  filesdata<- rbind(filesdata,file)
}

filesdata$scope_strat<-paste(filesdata$strategy,"_S=",filesdata$scope,sep="")
filesdatat<-filter(filesdata, t<=200)

write.csv(filesdatat, './resultados_consolidados.csv')
data<-filter(filesdatat, env=='complex')

mean_data <- group_by(data,scope_strat,t) %>%
  summarise(desempeno = mean(desempeno, na.rm = TRUE))
unique_solutions<-
  
p<-ggplot(data=mean_data, aes(x=t, y=desempeno, group=scope_strat)) +
  geom_line(aes(color=scope_strat))+
  geom_point(aes(color=scope_strat))
p
