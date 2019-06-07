## create a function to parse an input lvm file

parse.lvm <- function(lvm){
  max.columns <- c('j_day', 'T', 'H2O', 'N2', 'O2', 'Ar', 'DMS', 'O2_Ar', 'N2_Ar', 'Total', 'Bromoform_173', 'Bromoform_171', 'Bromoform_175', 'Isoprene_67', 'Isoprene_68', 'Isoprene_52', 'Isoprene_39')
  
  temp.lvm <- read.csv(lvm, skip = 22, sep = "", as.is = T, header = F)
  if(length(colnames(temp.lvm)) == 9){
    colnames(temp.lvm) <- c('j_day', 'H2O', 'N2', 'O2', 'Ar', 'DMS', 'O2_Ar', 'N2_Ar', 'Total')}
  if(length(colnames(temp.lvm)) == 10){
    colnames(temp.lvm) <- c('j_day', 'T', 'H2O', 'N2', 'O2', 'Ar', 'DMS', 'O2_Ar', 'N2_Ar', 'Total')}
  if(length(colnames(temp.lvm)) == 17){
    colnames(temp.lvm) <- max.columns}
  temp.lvm$j_day <- strptime(temp.lvm$j_day, format = '%j') + as.difftime(temp.lvm$j_day - floor(temp.lvm$j_day), units = 'days', format = '%j')
  temp.lvm <- temp.lvm[!duplicated(temp.lvm$j_day),]
  
  for(col in max.columns){
    if(!col %in% colnames(temp.lvm)){
      temp.lvm[col] <- NA
    }
  }
  
  temp.lvm <- temp.lvm[max.columns]
  
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

print(day)

## add remaining lvm files

for(day in names(lvm.days)[2:length(names(lvm.days))]){
  print(day)
  lvm.temp <- lvm.days[[day]]
  
  ## Rough QC on the O2/Ar values.
  
  lvm.temp[which(lvm.temp$O2_Ar > 50), 'O2_Ar'] <- NA
  lvm.temp[which(lvm.temp$O2_Ar == 0), 'O2_Ar'] <- NA
  
  temp.datetime <- lvm.temp$j_day
  lvm.temp$j_day <- NULL

  ## make sure no duplicate timestamps
  
  try({
    temp.zoo <- zoo(lvm.temp[which(!temp.datetime %in% index(lvm.zoo)),], order.by = temp.datetime)
    lvm.zoo <- rbind.zoo(lvm.zoo, temp.zoo)
  }, silent = T)
}

plot(lvm.zoo$O2_Ar)

## apply a smoothing filter to the O2_AR data, default number here is arbitrary

window.size <- 30
lvm.zoo.rm <- rollapply(as.numeric(lvm.zoo$O2_Ar), width = window.size, mean)
lvm.zoo.rm <- zoo(lvm.zoo.rm, order.by = index(lvm.zoo)[window.size:(length(index(lvm.zoo)) - window.size)])

plot(lvm.zoo.rm,
     ylab = 'O2/Ar',
     pch = 19,
     cex = 0.6)
