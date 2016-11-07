complete <- function(directory, id = 1:332) {
        file_list <- list.files(directory, full.names = TRUE)
        files_clean <- file_list[id]
        dat <- data.frame(id = id, nobs = 0)
                for(i in 1:length(id)){
                         holding <- read.csv(files_clean[i])
                                nobs <- sum(complete.cases(holding))
                                        dat[i, "nobs"] <- nobs 
        }
        dat
        
}