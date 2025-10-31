{#LIBRARY
  library(foreach)
  library(doSNOW)
  library(tidyverse)
  library(data.table)
  library(magrittr)
  library(roperators)
  library(readxl)
  library(Rmisc)
}

###key function to standardize raw data according to interval
Slice.by<-function(filepath, interval){
  
  center_coor <-readxl::read_xlsx(filepath, sheet = "data") %>% 
    transform(center_x=(x_normalised-0.5)/3*4,center_y=y_normalised-0.5,
              group="")%>%setDT
  
  #centre the coordinate system
  center_coor[which(center_coor$type=="zone"|center_coor$type=="screen start"),
              c('center_x','center_y')]<-0
  center_coor[, c("center_x", "center_y") := 
                .(
                  fifelse(type == "screen finish", lag(center_x), center_x),
                  fifelse(type == "screen finish", lag(center_y), center_y)
                )]
  
  #total slices
  .nslice<-ceiling((center_coor$time_stamp[nrow(center_coor)]-center_coor$time_stamp[1])/interval)
  sliced_coor<-vector("list", length = (.nslice+1))
  sliced_coor[[1]]<-data.table(participant.id=center_coor$participant_id[1],
                               spreadsheet_row=center_coor$spreadsheet_row[1],
                               timeslice=0,x.center=center_coor[type=='mouse']$x_normalised[1],
                               y.center=center_coor[type=='mouse']$y_normalised[1])
  
  for(i in 1:.nslice){
    temp<-list(timeStart=0, timeEnd=0,rows=0,x=0,y=0)
    temp$timeStart<-center_coor$time_stamp[1]
    temp$timeEnd<-temp$timeStart+i*interval
    temp$rows<-which(center_coor$time_stamp<=temp$timeEnd &
                       center_coor$time_stamp>(temp$timeEnd-interval))

    ###recover coordinates using a weighted interpolation method
    if(length(temp$rows)>=1){#if points exist in current slice
      #if not the first in a trial, take the point from the last slice at the first, and * first duration
      if(temp$rows[1]==1) temp$rows<- temp$rows[-1] 
      temp$x<-center_coor$center_x[temp$rows[1]-1]*
        (center_coor$time_stamp[temp$rows[1]]-temp$timeEnd+interval)#slice head
      temp$y<-center_coor$center_y[temp$rows[1]-1]*
        (center_coor$time_stamp[temp$rows[1]]-temp$timeEnd+interval)#head
      
      temp$x %+=% Reduce("+",center_coor$center_x[temp$rows]*diff(center_coor$time_stamp[temp$rows]%>%c(temp$timeEnd)))
      temp$y %+=% Reduce("+",center_coor$center_y[temp$rows]*diff(center_coor$time_stamp[temp$rows]%>%c(temp$timeEnd)))
      
      sliced_coor[[i+1]] = data.table(participant.id=center_coor$participant_id[1],
                                      spreadsheet_row=center_coor$spreadsheet_row[1],
                                      timeslice=i*interval,x.center=temp$x/interval,y.center=temp$y/interval
      )
    }else{ 
      sliced_coor[[i+1]] = sliced_coor[[i]]
    }
    
  }#for end, combine into a data table and fix `timeslice`
  sliced_coor%<>%rbindlist()
  sliced_coor[, timeslice:= ..interval*(.I-1)]
  
  ###generate common metrics
  sliced_coor[,
              `:=`(distance = ifelse(seq_len(.N)==1, 0, sqrt((x.center-lag(x.center))^2+(y.center-lag(y.center))^2)), 
                   x_velocity = ifelse(seq_len(.N)==1, 0, (x.center-lag(x.center))/interval), 
                   y_velocity = ifelse(seq_len(.N)==1, 0, (y.center-lag(y.center))/interval))
  ][,
    `:=`(distance = cumsum(distance),
         ifmovement = ifelse(seq_len(.N)>1 & distance>Rmisc::CI(distance,ci=0.95)[3], 1, 0)
    )]
  
  return(sliced_coor)
}

###if needed, right align mouse movement data (for certain analysis see Dr Mirman's <<Growth Curve Analysis and Visualization Using R>>)
AlignR<-function(merged_slice,setMax){
  tlist<-vector("list", length = length(unique(merged_slice$spreadsheet_row)))
  .maxSlice<-max(merged_slice$timeslice)/100
  .maxSlice<-min(setMax,.maxSlice)
  
  system.time(merged_slice_R<-foreach (ssrow = sort(unique(merged_slice$spreadsheet_row)),.export=c('timeslice'),
                                       .packages = c("doSNOW",'data.table'),.verbose = T)%dopar%{#, .errorhandling="pass".combine=rbind,
                                         foreach(id = unique(merged_slice$participant.id))%do%{
                                           if(!any(merged_slice[participant.id==id]$spreadsheet_row==ssrow)) return()
                                           
                                           selected<-merged_slice[merged_slice$participant.id==id&
                                                                    merged_slice$spreadsheet_row==ssrow,]
                                           if(nrow(selected)<.maxSlice){
                                             selected<-rbind(selected, selected[rep(nrow(selected), .maxSlice+1-nrow(selected)), ],make.row.names=F)
                                             
                                             selected$timeslice<-as.numeric(rownames(selected))*timeslice-timeslice
                                           }
                                           
                                           return(selected[0:.maxSlice+1])
                                         }})
  merged_slice_R<-unlist(merged_slice_R, recursive = F)
  merged_slice_R<-do.call('rbind',merged_slice_R)
  return(merged_slice_R)
}
