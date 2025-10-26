###example code with parallel processing
filepaths <- list.files(#read trajectory data file path
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
