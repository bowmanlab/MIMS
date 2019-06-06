## create a function to parse an input lvm file

parse.lvm <- function(lvm, columns = c('j_day', 'H2O', 'N2', 'O2', 'Ar', 'DMS', 'O2_Ar', 'N2_Ar', 'Total')){
  temp.lvm <- read.csv(lvm, skip = 22, col.names = columns, sep = "", as.is = T)
  temp.lvm$j_day <- strptime(temp.lvm$j_day, format = '%j') + as.difftime(temp.lvm$j_day - floor(temp.lvm$j_day), units = 'days', format = '%j')
  return(temp.lvm)
}

## get a list of files

files <- list.files(path = 'test_data', pattern = '.lvm')

## create a list to hold each day

lvm.days <- list()

## loop across files

for(f in files){
  f <- paste0('test_data/', f)
  try({
    f.name <- strsplit(f, '_.lvm')[[1]]
    lvm.days[[f.name]] <- parse.lvm(f)
  }, silent = T)
}

## now you have a list holding each days file as a dataframe, which could be convenient, but
## lets try converting to zoo object

library(zoo)

## initialize a zoo object with the first lvm file

temp.datetime <- lvm.days[[1]]$j_day
lvm.temp <- lvm.days[[1]]
lvm.temp$j_day <- NULL

lvm.zoo <- zoo(lvm.temp, order.by = temp.datetime)

## add remaining lvm files

for(day in names(lvm.days)[2:length(names(lvm.days))]){
  temp.datetime <- lvm.days[[day]]$j_day
  lvm.temp <- lvm.days[[day]]
  lvm.temp$j_day <- NULL
  temp.zoo <- zoo(lvm.temp, order.by = temp.datetime)
  lvm.zoo <- rbind.zoo(lvm.zoo, temp.zoo)
}

plot(lvm.zoo$O2_Ar)
