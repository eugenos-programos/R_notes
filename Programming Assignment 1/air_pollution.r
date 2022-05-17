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

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
