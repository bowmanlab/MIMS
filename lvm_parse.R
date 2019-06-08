setwd('Data/PreTempData')

## create a function to parse an input lvm file

parse.lvm <- function(lvm, columns = c('j_day', 'H2O', 'N2', 'O2', 'Ar', 'DMS', 'O2_Ar', 'N2_Ar', 'Total')){
  temp.lvm <- read.csv(lvm, skip = 32, col.names = columns, sep = "", as.is = T)
  return(temp.lvm)
}

## get a list of files

files <- list.files(pattern = '.lvm')

## create a list to hold each day

lvm.days <- list()

## loop across files

for(f in files){
  f.name <- strsplit(f, '_.lvm')[[1]]
  lvm.days[[f.name]] <- parse.lvm(f)
}

## now you have a list holding each days file as a dataframe, which could be convenient, but
## lets try converting to zoo object

library(zoo)

## initialize a zoo object with the first lvm file

lvm.zoo <- zoo(lvm.days[[1]], order.by = lvm.days[[1]]$j_day)

## add remaining lvm files

for(day in names(lvm.days)[2:length(names(lvm.days))]){
  temp.zoo <- zoo(lvm.days[[day]], order.by = lvm.days[[day]]$j_day)
  lvm.zoo <- rbind.zoo(lvm.zoo, temp.zoo)
}

lvm.zoo <- as.data.frame(lvm.zoo)

lvm.zoo$seconds <- lvm.zoo$j_day*24*60*60

lvm.zoo$acutal_day <- as.POSIXct(lvm.zoo$seconds, origin = "2018-12-31 07:00:00") 

## everything works fine to here, though for some reason the values are getting picked up as character strings despite as.is = T

write.table(lvm.zoo, "MIMS.data.txt", sep = ',')


