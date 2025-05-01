#Veronika Tsishetska, group 2
#project 2

######### functions to read data ###############################################################
get_header <- function(file, relevant = c('DATASET', 'VERSION ', 'SERIES ', 'DATE', 'TIME', 'RATE', 
                                          'VERT_UNITS', 'HORZ_UNITS', 'COMMENT','NUM_SERIES', 'STORAGE_MODE', 'FILE_TYPE', 
                                          'SLOPE', 'X_OFFSET', 'Y_OFFSET', 'NUM_SAMPS')){
  header = readLines(file)
  elements = lapply(relevant, function(x){ which(grepl(x, header))[1]})
  elements = unlist(elements)
  header = header[elements]
  
  positions = regexpr(' ',header )
  result = as.list(substr(header,positions+1, nchar(header) ))
  names(result) = substr(header, 1,positions-1 )
  
  for(vars in  c('Y_OFFSET', 'X_OFFSET', 'SLOPE', 'NUM_SERIES', 'RATE', 'VERSION', 'NUM_SAMPS')){
    result[[vars]] <- eval(parse(text = paste0('c(', result[[vars]], ')')) )
  }
  
  for(vars in c('VERT_UNITS', 'SERIES')){
    result[[vars]]  <- gsub(' ', '',  result[[vars]]  )
    result[[vars]]  <- strsplit( result[[vars]] , ",")[[1]]
  }
  
  return(result)
}


get_data <- function(file, header){
  sens_dat = readBin(file, integer(), n = (header$NUM_SERIES * header$NUM_SAMPS), size = 2, endian = 'little')
  
  mat = t(matrix(sens_dat, nrow = header$NUM_SERIES))
  for(i in 1:header$NUM_SERIES){
    mat[,i] <- mat[,i] * header$SLOPE[i] + header$Y_OFFSET[i]
  }
  colnames(mat) <- header$SERIES
  return(mat)
}

#############        read data #################################################
data2 <- get_data('V2_00001.DAT', get_header('V2_00001.HDR'))
data10 <- get_data('V10_0001.DAT', get_header('V10_0001.HDR'))
data6 <- get_data('V6_00001.dat', get_header('V6_00001.hdr'))  
data17 <- get_data('V17_0001.dat', get_header('V17_0001.hdr'))  
data20 <- get_data('V20_0001.dat', get_header('V20_0001.hdr'))  
data6 <- get_data('V6_00001.dat', get_header('V6_00001.hdr'))  
data24 <- get_data('V24_0001.dat', get_header('V24_0001.hdr'))  
data25a <- get_data('V25a_001.dat', get_header('V25a_001.hdr'))  


###############################################################################
    #############    plot data ###############################################
library('tuneR')

par(mfrow = c(3, 3))
outer = FALSE

pl <- Wave(data2[, 1], samp.rate=20000)
plot(pl, ylab= 'torque (in Nm)', cex.lab = 0.001)

plot(Wave(data25a[, 1], samp.rate=20000), ylab= 'torque (in Nm)', xlab = 'tine (in sec)') #moment
#axis(1,cex.axis=0.1)
plot(Wave(data25a[, 2], samp.rate=20000), ylab= 'force (in N)' , xlab = 'tine (in sec)') # kraft
plot(Wave(data25a[, 3], samp.rate=20000), ylab= 'SyncSig (in V)'  , xlab = 'tine (in sec)') #syncsig
plot(Wave(data25a[, 4], samp.rate=20000), ylab= 'acoustics (in Pa)' , xlab = 'tine (in sec)') #akusik
plot(Wave(data25a[, 5], samp.rate=20000),  ylab= 'WSAF (in m/s^2)' , xlab = 'tine (in sec)') #wsaf
plot(Wave(data25a[, 6], samp.rate=20000), ylab= 'WSAS (in m/s^2)' , xlab = 'tine (in sec)' ) #wsas
plot(Wave(data25a[, 7], samp.rate=20000), ylab= 'BOZA (in m/s^2)' , xlab = 'tine (in sec)' ) # boza


par(mfrow = c(3, 3))

plot(Wave(data10[, 1], samp.rate=20000)) #moment
plot(Wave(data10[, 2], samp.rate=20000)) # kraft
plot(Wave(data10[, 3], samp.rate=20000)) #syncsig
plot(Wave(data10[, 4], samp.rate=20000)) #akusik
plot(Wave(data10[, 5], samp.rate=20000)) #wsaf
plot(Wave(data10[, 6], samp.rate=20000)) #wsas
plot(Wave(data10[, 7], samp.rate=20000)) # boza



par(mfrow = c(3, 3))

plot(Wave(data17[, 1], samp.rate=20000)) #moment
plot(Wave(data17[, 2], samp.rate=20000)) # kraft
plot(Wave(data17[, 3], samp.rate=20000)) #syncsig
plot(Wave(data17[, 4], samp.rate=20000)) #akusik
plot(Wave(data17[, 5], samp.rate=20000)) #wsaf
plot(Wave(data17[, 6], samp.rate=20000)) #wsas
plot(Wave(data17[, 7], samp.rate=20000)) # boza


par(mfrow = c(3, 3))

plot(Wave(data6[, 1], samp.rate=20000)) #moment
plot(Wave(data6[, 2], samp.rate=20000)) # kraft
plot(Wave(data6[, 3], samp.rate=20000)) #syncsig
plot(Wave(data6[, 4], samp.rate=20000)) #akusik
plot(Wave(data6[, 5], samp.rate=20000)) #wsaf
plot(Wave(data6[, 6], samp.rate=20000)) #wsas
plot(Wave(data6[, 7], samp.rate=20000)) # boza

par(mfrow = c(3, 3))
plot(Wave(data20[, 1], samp.rate=20000)) #moment
plot(Wave(data20[, 2], samp.rate=20000)) # kraft
plot(Wave(data20[, 3], samp.rate=20000)) #syncsig
plot(Wave(data20[, 4], samp.rate=20000)) #akusik
plot(Wave(data20[, 5], samp.rate=20000)) #wsaf
plot(Wave(data20[, 6], samp.rate=20000)) #wsas
plot(Wave(data20[, 7], samp.rate=20000)) # boza



par(mfrow = c(3, 3))
plot(Wave(data24[, 1], samp.rate=20000)) #moment
plot(Wave(data24[, 2], samp.rate=20000)) # kraft
plot(Wave(data24[, 3], samp.rate=20000)) #syncsig
plot(Wave(data24[, 4], samp.rate=20000)) #akusik
plot(Wave(data24[, 5], samp.rate=20000)) #wsaf
plot(Wave(data24[, 6], samp.rate=20000)) #wsas
plot(Wave(data24[, 7], samp.rate=20000)) # boza


########################## data 2 ###########################################

par(mfrow = c(2, 1))
plot(Wave(data2[3900000:3901000, 1], samp.rate=20000), ylab= 'torque (in Nm)', xlab='time (in sec)')
plot(Wave(data2[4000000:4001000, 1], samp.rate=20000), ylab= 'torque (in Nm)', xlab = 'time (in sec)')




plot(Wave(data2[3994000:3995000, 1], samp.rate=20000),  xlab= 'time (in sec)', ylab= 'torque (in Nm)')
plot(Wave(data2[3996000:3997000, 1], samp.rate=20000),  xlab= 'time (in sec)', ylab= 'torque (in Nm)')
plot(Wave(data2[3997000:3998000, 1], samp.rate=20000),  xlab= 'time (in sec)', ylab= 'torque (in Nm)')
plot(Wave(data2[3998000:3999000, 1], samp.rate=20000),  xlab= 'time (in sec)', ylab= 'torque (in Nm)')
plot(Wave(data2[3999000:4000000, 1], samp.rate=20000),  xlab= 'time (in sec)', ylab= 'torque (in Nm)')
plot(Wave(data2[4000000:4001000, 1], samp.rate=20000),  xlab= 'time (in sec)', ylab= 'torque (in Nm)')

obj <- periodogram(Wave(data2[1:4000000, 1], samp.rate=20000))
par(mfrow = c(1, 1))
obj <- periodogram(Wave(data2[3994000:3995000, 1], samp.rate=20000))

obj1 <- periodogram(Wave(data2[3996000:3997000, 1], samp.rate=20000))

obj2 <- periodogram(Wave(data2[3997000:3998000, 1], samp.rate=20000))

obj4 <- periodogram(Wave(data2[3998000:3999000, 1], samp.rate=20000))
obj5 <- periodogram(Wave(data2[3999000:4000000, 1], samp.rate=20000))
obj6 <- periodogram(Wave(data2[4000000:4001000, 1], samp.rate=20000))

par(mfrow = c(2, 3))
plot(obj,xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)' )
plot(obj1, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj2,xlab= 'Frequency (in Hz)', ylab = 'normalized periodogram (V^2/Hz)' )

plot(obj4, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj5, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj6, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')


max_spec_idx <- which.max(obj@spec[[1]])
max_spec_value <- obj@spec[[1]][which.max(obj@spec[[1]])]

# Corresponding frequency
obj@freq[which.max(obj@spec[[1]])]
obj1@freq[which.max(obj@spec[[1]])]
obj2@freq[which.max(obj@spec[[1]])]
obj4@freq[which.max(obj@spec[[1]])]
obj5@freq[which.max(obj@spec[[1]])]
obj6@freq[which.max(obj@spec[[1]])]


#########################    data 6 ############################################
plot(Wave(data6[943001:1000000, 1], samp.rate=20000))



plot(Wave(data6[, 1], samp.rate=20000))
#21s


plot(Wave(data6[941000:942000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')

plot(Wave(data6[942000:943000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data6[944000:945000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')

plot(Wave(data6[945000:946000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data6[946000:947000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data6[947000:948000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')


obj <- periodogram(Wave(data6[941000:942000, 1], samp.rate=20000))
obj1 <- periodogram(Wave(data6[942000:943000, 1], samp.rate=20000))
obj2 <- periodogram(Wave(data6[944000:945000, 1], samp.rate=20000))



obj4 <- periodogram(Wave(data6[945000:946000, 1], samp.rate=20000))
obj5 <- periodogram(Wave(data6[946000:947000, 1], samp.rate=20000))
obj6 <- periodogram(Wave(data6[947000:948000, 1], samp.rate=20000))

freq_idx <- which(obj4@freq >= 1171 & obj@freq <= 1192)
max_spec <- which.max(obj@spec[[1]][freq_idx])
freq_idx[max_spec]

obj@freq[which.max(obj@spec[[1]])]
obj1@freq[which.max(obj@spec[[1]])]
obj2@freq[which.max(obj@spec[[1]])]
obj4@freq[which.max(obj@spec[[1]])]
obj5@freq[which.max(obj@spec[[1]])]
obj6@freq[which.max(obj@spec[[1]])]



par(mfrow = c(1, 1))
par(mfrow = c(2, 3))
plot(obj,xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)' )
plot(obj1, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj2,xlab= 'Frequency (in Hz)', ylab = 'normalized periodogram (V^2/Hz)' )

plot(obj4, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj5, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj6, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')



####################      data 10 ###############################################

plot(Wave(data10[943000:1100000, 1], samp.rate=20000))

obj <- periodogram(Wave(data6[432000:500000, 1], samp.rate=20000))


plot(Wave(data10[, 1], samp.rate=20000))
#21s


plot(Wave(data10[941000:942000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')

plot(Wave(data10[942000:943000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data10[944000:945000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')

plot(Wave(data10[945000:946000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data10[946000:947000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data10[947000:948000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')


obj <- periodogram(Wave(data10[941000:942000, 1], samp.rate=20000))
obj1 <- periodogram(Wave(data10[942000:943000, 1], samp.rate=20000))
obj2 <- periodogram(Wave(data10[944000:945000, 1], samp.rate=20000))



obj4 <- periodogram(Wave(data10[945000:946000, 1], samp.rate=20000))
obj5 <- periodogram(Wave(data10[946000:947000, 1], samp.rate=20000))
obj6 <- tuneR::periodogram(Wave(data10[947000:948000, 1], samp.rate=20000))


obj@freq[which.max(obj@spec[[1]])]
obj1@freq[which.max(obj@spec[[1]])]
obj2@freq[which.max(obj@spec[[1]])]
obj4@freq[which.max(obj@spec[[1]])]
obj5@freq[which.max(obj@spec[[1]])]
obj6@freq[which.max(obj@spec[[1]])]


par(mfrow = c(1, 1))
par(mfrow = c(2, 3))
plot(obj,xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)' )
plot(obj1, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj2,xlab= 'Frequency (in Hz)', ylab = 'normalized periodogram (V^2/Hz)' )

plot(obj4, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj5, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj6, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')


############################    data 17 #############################################
plot(Wave(data17[, 1], samp.rate=20000))
plot(Wave(data17[681000:1000000, 1], samp.rate=20000))



plot(Wave(data17[, 1], samp.rate=20000))
#21s



plot(Wave(data17[680000:681000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data17[681000:682000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data17[682000:683000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data17[683000:684000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data17[684000:685000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data17[689000:690000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')


obj <- periodogram(Wave(data17[680000:681000, 1], samp.rate=20000))
obj1 <- periodogram(Wave(data17[681000:682000, 1], samp.rate=20000))
obj2 <- periodogram(Wave(data17[682000:683000, 1], samp.rate=20000))



obj4 <- periodogram(Wave(data17[683000:684000, 1], samp.rate=20000))
obj5 <- periodogram(Wave(data17[684000:685000, 1], samp.rate=20000))
obj6 <- periodogram(Wave(data17[689000:690000, 1], samp.rate=20000))

obj@freq[which.max(obj@spec[[1]])]
obj1@freq[which.max(obj@spec[[1]])]
obj2@freq[which.max(obj@spec[[1]])]
obj4@freq[which.max(obj@spec[[1]])]
obj5@freq[which.max(obj@spec[[1]])]
obj6@freq[which.max(obj@spec[[1]])]



par(mfrow = c(1, 1))
par(mfrow = c(2, 3))
plot(obj,xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)' )
plot(obj1, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj2,xlab= 'Frequency (in Hz)', ylab = 'normalized periodogram (V^2/Hz)' )

plot(obj4, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj5, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj6, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')



################################  data 20 #######################################

plot(Wave(data20[1050000:1100000, 1], samp.rate=20000))


plot(Wave(data20[1040000:1041000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')

plot(Wave(data20[1041000:1042000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data20[1042000:1043000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')

plot(Wave(data20[1043000:1044000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data20[1044000:1045000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data20[1050000:1051000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')


obj <- periodogram(Wave(data20[1040000:1041000, 1], samp.rate=20000))
obj1 <- periodogram(Wave(data20[1041000:1042000, 1], samp.rate=20000))
obj2 <- periodogram(Wave(data20[1042000:1043000, 1], samp.rate=20000))



obj4 <- periodogram(Wave(data20[1043000:1044000, 1], samp.rate=20000))
obj5 <- periodogram(Wave(data20[1044000:1045000, 1], samp.rate=20000))
obj6 <- periodogram(Wave(data20[1050000:1051000, 1], samp.rate=20000))


obj@freq[which.max(obj@spec[[1]])]
obj1@freq[which.max(obj@spec[[1]])]
obj2@freq[which.max(obj@spec[[1]])]
obj4@freq[which.max(obj@spec[[1]])]
obj5@freq[which.max(obj@spec[[1]])]
obj6@freq[which.max(obj@spec[[1]])]


par(mfrow = c(1, 1))
par(mfrow = c(2, 3))
plot(obj,xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)' )
plot(obj1, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj2,xlab= 'Frequency (in Hz)', ylab = 'normalized periodogram (V^2/Hz)' )

plot(obj4, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj5, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj6, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')






###########################  data 24 ############################################
obj <- periodogram(Wave(data24[432000:500000, 1], samp.rate=20000))
plot(Wave(data24[, 1], samp.rate=20000))

plot(Wave(data24[, 1], samp.rate=20000))
#21s
plot(Wave(data24[430000:431000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')

plot(Wave(data24[432000:433000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')

plot(Wave(data24[433000:434000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data24[500000:501000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')

plot(Wave(data24[501000:502000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data24[503000:504000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')

#21.5-21,55
obj <- periodogram(Wave(data24[430000:431000, 1], samp.rate=20000))
#21.6 s-21.65s
obj1 <- periodogram(Wave(data24[432000:433000, 1], samp.rate=20000))
#21.65
obj2 <- periodogram(Wave(data24[433000:434000, 1], samp.rate=20000))



obj4 <- periodogram(Wave(data24[500000:501000, 1], samp.rate=20000))
obj5 <- periodogram(Wave(data24[501000:502000, 1], samp.rate=20000))
obj6 <- periodogram(Wave(data24[503000:504000, 1], samp.rate=20000))

obj@freq[which.max(obj@spec[[1]])]
obj1@freq[which.max(obj@spec[[1]])]
obj2@freq[which.max(obj@spec[[1]])]
obj4@freq[which.max(obj@spec[[1]])]
obj5@freq[which.max(obj@spec[[1]])]
obj6@freq[which.max(obj@spec[[1]])]



par(mfrow = c(1, 1))
par(mfrow = c(2, 3))
plot(obj,xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)' )
plot(obj1, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj2,xlab= 'Frequency (in Hz)', ylab = 'normalized periodogram (V^2/Hz)' )

plot(obj4, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj5, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj6, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')


#############    data 25a ######################################################
plot(Wave(data25a[, 1], samp.rate=20000))
plot(Wave(data25a[468500:1000000, 1], samp.rate=20000))


plot(Wave(data25a[464000:465000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data25a[465000:466000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')

plot(Wave(data25a[466000:467000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data25a[467000:468000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data25a[468000:469000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')
plot(Wave(data25a[469000:470000, 1], samp.rate=20000), xlab= 'time (in sec)', ylab = 'torque (in Nm)')





#21.5-21,55
obj <- periodogram(Wave(data25a[464000:465000, 1], samp.rate=20000))
#21.6 s-21.65s
obj1 <- periodogram(Wave(data25a[465000:466000, 1], samp.rate=20000))
#21.65
obj2 <- periodogram(Wave(data25a[466000:467000, 1], samp.rate=20000))



obj4 <- periodogram(Wave(data25a[467000:468000, 1], samp.rate=20000))
obj5 <- periodogram(Wave(data25a[468000:469000, 1], samp.rate=20000))
obj6 <- periodogram(Wave(data25a[469000:470000, 1], samp.rate=20000))

obj@freq[which.max(obj@spec[[1]])]
obj1@freq[which.max(obj@spec[[1]])]
obj2@freq[which.max(obj@spec[[1]])]
obj4@freq[which.max(obj@spec[[1]])]
obj5@freq[which.max(obj@spec[[1]])]
obj6@freq[which.max(obj@spec[[1]])]



par(mfrow = c(1, 1))
par(mfrow = c(2, 3))
plot(obj,xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)' )
plot(obj1, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj2,xlab= 'Frequency (in Hz)', ylab = 'normalized periodogram (V^2/Hz)' )

plot(obj4, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj5, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')
plot(obj6, xlab= 'Frequency (in Hz)', ylab = 'norm. periodogram (V^2/Hz)')




#################     early alarm ##############################################


# Function to compute periodogram with overlapping windows
compute_periodogram <- function(data, window_size, overlap, target_freq, threshold) {
  num_obs <- length(data)
  num_windows <- floor((num_obs - window_size) / overlap) + 1
  
  for (i in 1:num_windows) {
    start_idx <- (i - 1) * overlap + 1
    end_idx <- start_idx + window_size - 1
    if (end_idx > num_obs) {
      end_idx <- num_obs
    }
    time_series <- data[start_idx:end_idx]
    
    # Compute the periodogram
    per <- periodogram(Wave(time_series, samp.rate = 20000))
    
    # Check if target frequency (1183 Hz) has value greater than threshold (0.3)
    freq_idx <- which(per@freq >= 1171 & per@freq <= 1192)
    max <- which.max(obj@spec[[1]][freq_idx])
    if (length(freq_idx) > 0) {
      max_spec <- which.max(per@spec[[1]][freq_idx])
      freq_idx <- freq_idx[max_spec]
      if (per@spec[[1]][freq_idx] > threshold) {
        message("Frequency at ", per@freq[freq_idx], " Hz has value ", per@spec[[1]][freq_idx], " which is larger than ", threshold,
                " in a time window ", start_idx, " to ", end_idx, " with freq_idx ", freq_idx)
        return()
      }
    }
  }
  
  message("No frequency at ", target_freq, " Hz has a value larger than ", threshold, " in any of the time windows.")
}


window_size <- 1000
overlap <- 500
target_freq <- 1183
threshold <- 0.15

compute_periodogram(data10[1:5000000, 1], window_size, overlap, target_freq, threshold)

compute_periodogram(data6[1:5000000, 1], window_size, overlap, target_freq, threshold)
compute_periodogram(data17[1:5000000, 1], window_size, overlap, target_freq, threshold)
compute_periodogram(data20[1:5000000, 1], window_size, overlap, target_freq, threshold)
compute_periodogram(data24[1:5000000, 1], window_size, overlap, target_freq, threshold)
compute_periodogram(data25a[1:5000000, 1], window_size, overlap, target_freq, threshold)



compute_periodogram <- function(data, window_size, overlap, target_freq, threshold) {
  num_obs <- length(data)
  num_windows <- floor((num_obs - window_size) / overlap) + 1
  
  for (i in 1:num_windows) {
    start_idx <- (i - 1) * overlap + 1
    end_idx <- start_idx + window_size - 1
    if (end_idx > num_obs) {
      end_idx <- num_obs
    }
    time_series <- data[start_idx:end_idx]
    
    # Compute the periodogram
    per <- periodogram(Wave(time_series, samp.rate = 20000))
    
    # Check if target frequency (1183 Hz) has value greater than threshold (0.3)
    freq_idx <- which(per@freq >= 703 & per@freq <= 723)
    max <- which.max(obj@spec[[1]][freq_idx])
    if (length(freq_idx) > 0) {
      max_spec <- which.max(per@spec[[1]][freq_idx])
      freq_idx <- freq_idx[max_spec]
      if (per@spec[[1]][freq_idx] > threshold) {
        message("Frequency at ", per@freq[freq_idx], " Hz has value ", per@spec[[1]][freq_idx], " which is larger than ", threshold,
                " in a time window ", start_idx, " to ", end_idx, " with freq_idx ", freq_idx)
        return()
      }
    }
  }
  
  message("No frequency at ", target_freq, " Hz has a value larger than ", threshold, " in any of the time windows.")
}


compute_periodogram(data2[1:5000000, 1], window_size, overlap, target_freq, threshold)






