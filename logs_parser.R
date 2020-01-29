# setwd("C:/Users/Aneta/Projects/RShinyApp")

# Parsing Mobi C responses and SensDxApp logs.



# Reading in a log-file
processFile <- function(filepath) {
  con = file(filepath, "r")
  
  # data =  vector("list")
  
  while (TRUE) {
    line = readLines(con, n = 1)

    if (length(line) == 0) {
      break
    } else if (grepl('^#', line) == TRUE) {
      iToRpt <- unlist(strsplit(line, " "))[3]
      iToRpt <- unlist(strsplit(iToRpt, "="))[2]
      iToRpt
    } else if (grepl('^<<', line) == TRUE) {
      out <- unlist(strsplit(line, " "))
      out <- out[4:length(out)]
      out
    }
    
    # data <- c(data, iToRpt, out)
  }
  
  # return(data)
  
  return(list(iToRpt=iToRpt, out=out))
  
  close(con)
}


# Extracting channel number
channelNumber <- function(out){
  return(out[3])
}

# Extracting cmd number
cmdNumber <- function(out){
  cmd <- substr(out[1], 2, nchar(out[1]))
  return(cmd)
}

# Extracting frequency
frequencyLevel <- function(out){
  freq <- out[4]
  return(freq)
}

# Extracting impedance
impedanceRe  <- function(out){
  z_re <- out[5]
  return(z_re)
}

impedanceIm  <- function(out){
  z_im <- out[6]
  return(z_im)
}

# Creating data frame
log_df <- function(){
  p <- processFile("C:/Users/Aneta/Projects/RShinyApp/A_rep2.log")
  str(p)
  
  channel <- channelNumber(p$out)
  frequency <- frequencyLevel(p$out)
  cycle <- p$iToRpt
  z_im <- impedanceIm(p$out)
  z_re <- impedanceRe(p$out)

  results <- data.frame(channel, frequency, z_re, z_im, cycle)
  print(results)
}


log_df()




# # Reverting cycle number
# revertCycle <- function(iToRpt){
#   
#   cycles <- seq(1, 7)
#   names(cycles) <- seq(7, 1, by = -1)
#   print(cycles)
#   # iToRpt <- as.numeric(iToRpt)
#   
#   for(c in 1:length(cycles)){
#     if (iToRpt == cycles[c]) {
#       iToRpt <- c
#     }
#   }
#   print(iToRpt)
# }
