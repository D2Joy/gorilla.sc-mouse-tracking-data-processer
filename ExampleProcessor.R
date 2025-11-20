###example code with parallel processing
# read behavioural data from the source folder & create a list of all the file paths
filepaths_base <- list.files(#basic data files
  getwd(),
  pattern = ".*_task-.*.csv",
  full.names = TRUE
)

baseT<-foreach(i = 1 :length(filepaths_base),.combine=rbind,.verbose = F)%do%{#
  filepath<-filepaths_base[i]
  # status message (handy for debugging)
  cat("processing", filepath, "...","\n")
  
  current_data <-fread(filepath,fill = F,verbose = F, check.names=T,na.strings="NA")#,header=T
  #current_data <-read.csv(filepath)
  
  #score incorrect as -1
  current_data[Attempt==1 &Incorrect==1]$Correct<--1
  current_data[Response==""]$Response<-NA
  
  return(current_data)
}

sst<-fread(paste(getwd(), "/spreadsheet.csv", sep = ""))

#read trajectory data file paths into a list
filepaths <- list.files(
  paste0(getwd(),"/uploads"),
  pattern = "*.xlsx",
  full.names = TRUE
)

progress <- function(n) setTxtProgressBar(txtProgressBar(max = length(filepaths), style = 3), n)

registerDoSNOW(.cl<-makeCluster(9))

system.time(merged_slice<-foreach (i = 1:length(filepaths),.combine=rbind,.verbose = F,
                                   .packages = c("dplyr",'data.table','roperators'), 
                                   .options.snow = list(progress = progress))%dopar%{
                                     
                                     filepath<-filepaths[i]

                                     sliced_coor<-Slice.by(filepath,interval<-100)
                                     
                                     return(sliced_coor)
                                   })

