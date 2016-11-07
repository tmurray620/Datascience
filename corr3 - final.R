corr <- function(directory, threshold = 0) { 
  files <- list.files(directory, full.names = TRUE)     
        final_data <- c()
                for (i in 1:length(files)){
                         holding_space <- read.csv(files[i])
                                 clean.data <- na.omit(holding_space)
        if(nrow(clean.data) > threshold){
    final_data <- c(final_data, cor(clean.data$sulfate, clean.data$nitrate))
  
        }
        
}
  final_data
}