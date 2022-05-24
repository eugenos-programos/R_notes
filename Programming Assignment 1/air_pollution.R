pollutantmean <- function(directory, pollutant, id = 1:332){
    file_list <- list.files(path="../input/programming-assignment-1-air-pollution/specdata", pattern='csv', all.files=TRUE, full.names=TRUE)
    file_list <- file_list[id]
    values_list <- c()
    for (file_name in file_list){
        data <- read.csv(file_name)
        for (poll_value in data[[pollutant]]){
            if (!is.na(poll_value)){
                values_list <- append(values_list, poll_value)
            }
        }
    }
    mean(values_list)
}

# print results for pollutantmean function
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

complete <- function(directory, id = 1:332){
    file_list <- list.files(path="../input/programming-assignment-1-air-pollution/specdata", pattern='csv', all.files=TRUE, full.names=TRUE)
    file_list <- file_list[id]
    nobs = c()
    for (file_name in file_list){
        data <- read.csv(file_name)
        nobs = append(nobs, sum(!is.na(data[, 2]) & !is.na(data[, 3])))
    }
    data.frame(id, nobs)
}

# print results for complete function
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

corr <- function(directory, threshold = 0){
    cor_results <- numeric(0)
    
    complete_cases <- complete(directory)
    complete_cases <- complete_cases[complete_cases$nobs>=threshold, ]

    if(nrow(complete_cases)>0){
        for(monitor in complete_cases$id){
            path <- paste("../input/programming-assignment-1-air-pollution/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
            #print(path)
            monitor_data <- read.csv(path)
            #print(monitor_data)
            interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
            interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
            sulfate_data <- interested_data["sulfate"]
            nitrate_data <- interested_data["nitrate"]
            cor_results <- c(cor_results, cor(sulfate_data, nitrate_data))
        }
    }
    cor_results
}

# print results for 3rd function
cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
