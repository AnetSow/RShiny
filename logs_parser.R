# Parsing Mobi C responses and SensDxApp logs.


# Reading in a log-file

processFile <- function(filepath) {
  con = file(filepath, "r")
    
  linesInFile = readLines(con) 
  # print(length(linesInFile))
  
  iToRpt_list <- list()
  out_list <- list()
  iToRpt_idx = 1
  out_idx = 1

  for (line in linesInFile){

     if (length(line) == 0) {

       break

     } else if (grepl('^# iCmd', line) == TRUE) {
       
       iToRpt <- unlist(strsplit(line, " "))[3]
       iToRpt <- unlist(strsplit(iToRpt, "="))[2]
       # print(paste(" >>> iToRtp:", iToRpt, sep=" "))

       iToRpt_list[[iToRpt_idx]] <- iToRpt
       iToRpt_idx <- iToRpt_idx + 1

     } else if (grepl('^<<', line) == TRUE) {

       out <- unlist(strsplit(line, " "))
       out <- out[4:length(out)]
       out <- paste(out, collapse=" ")
       # print(paste(" >>> out: ", out, sep=" "))
       
       out_list[[out_idx]] <- out
       out_idx <- out_idx + 1
     }
  }

  data <- list(iToRpt_list, out_list)
  names(data) <- c("iToRpt", "out")
  return(data)

  close(con)
}


# Extracting channel number

channelNumber <- function(out){
  channel_vec <- vector()
  channel_idx <- 1
  
  for (i in 1:length(out)){
    channel_vec[channel_idx] <- unlist(strsplit(out[[i]], " "))[3]
    i = i + 1
    channel_idx = channel_idx + 1
  }
  return(channel_vec)
}


# Extracting cmd number

cmdNumber <- function(out){
  cdm_vec <- vector()
  cdm_idx <- 1
  
  for (i in 1:length(out)){
    cdm_vec[cdm_idx] <- substr(unlist(strsplit(out[[i]], " "))[1], 2, nchar(unlist(strsplit(out[[i]], " "))[1]))
    i = i + 1
    cdm_idx = cdm_idx + 1
  }
  return(cdm_vec)
}

# cmdNumber(p$out)


# Extracting frequency

frequencyLevel <- function(out){
  freq_vec <- vector()
  freq_idx <- 1
  
  for (i in 1:length(out)){
    freq_vec[freq_idx] <- unlist(strsplit(out[[i]], " "))[4]
    i = i + 1
    freq_idx = freq_idx + 1
  }
  return(freq_vec)
}


# Extracting impedance

impedanceRe  <- function(out){
  z_re_vec <- vector()
  z_re_idx <- 1
  
  for (i in 1:length(out)){
    z_re_vec[z_re_idx] <- unlist(strsplit(out[[i]], " "))[5]
    i = i + 1
    z_re_idx = z_re_idx + 1
  }
  return(z_re_vec)
}

impedanceIm  <- function(out){
  z_im_vec <- vector()
  z_im_idx <- 1
  
  for (i in 1:length(out)){
    z_im_vec[z_im_idx] <- unlist(strsplit(out[[i]], " "))[6]
    i = i + 1
    z_im_idx = z_im_idx + 1
  }
  return(z_im_vec)
}


# Creating data frame with results

log_df <- function(){
  p <- processFile(file.choose())
  
  channel <- channelNumber(p$out)
  frequency <- frequencyLevel(p$out)
  cycle <- rev(unlist(p$iToRpt)) # Reverting cycle number
  z_re <- impedanceRe(p$out)
  z_im <- impedanceIm(p$out)

  # results <- 
  data.frame(channel, frequency, z_re, z_im, cycle)
  # print(results)
}


log_df()


# library(jsonlite)
# x <- toJSON(log_df())
# cat(x)
# write_json("C:/Users/Aneta/Projects/RShinyApp", x)

