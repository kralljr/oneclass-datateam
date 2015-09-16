


#' Functions to apply attack labels
#'
#' \code{labelall} Label all datasets
#' 
#' This function labels all datasets, and appends them togther
#' 
#' @param valid1 list of filenames for validation datasets
#' @param test1 list of filenames for test datasets
labelall <- function(valid1, test1) {
  for(i in 1 : length(valid1)) {
    dat1 <- getlabel(valid1[i])
    if(i == 1) {
      valid <- dat1
    }else{
      valid <- full_join(dat1, valid)
    }
  }


  for(i in 1 : length(test1)) {
    
    dat1 <- getlabel(test1[i], type = "test" )
    if(i == 1) {
      test <- dat1
    }else{
      test <- full_join(dat1, test)
    }

  }
  return(list(test= test, valid = valid))
}


#' 
#' \code{getlabel} Read in data and apply labels
#'
#' This function takes the filename, and some other information
#' and outputs the data with applied attack (1) and normal (0) labels
#'
#' @param filename name of validation or test data
#' @param type validation or test data, default "valid"
#' @param plusminus number of seconds in window (+/-)
getlabel <- function(filename, type = "valid", plusminus = 30, test = F) {
  # read in data
  dat <- read.csv(filename, header = F, stringsAsFactors = F)
  colnames(dat)[1 : 2] <- c("date", "ip")

  # fix date format
  dat$date <- ymd_hms(dat$date)
  
  # get 30 second window
  dat <- mutate(dat, min1 = date - plusminus, max1 = date + plusminus)

  # first load attack data
  if(type == "valid") {
    attack <- read.csv("validation-labels.csv", stringsAsFactors = F)
  }else if(type == "test") {
    attack <- read.csv("test-labels.csv", stringsAsFactors = F)
  }else{
    stop("Cannot load attack data")
  }


  # convert date
  attack$datetime <- ymd_hms(attack$datetime)
  attvector <- attack$datetime
  ips <- attack$ip


  
  # get connection data
  connect1 <- get(sapply(strsplit(gsub("-", "_", filename), "\\."),
    function(x) x[1]))
  convector <- connect1$datetime
  conip <- connect1[, c("src", "dst")]
  conip$src <- as.character(conip$src)
  conip$dst <- as.character(conip$dst)


  # find which entries are attacks from attack data
  other1 <- list(attack = attack, connect1 = connect1)
  att1 <- mapply(windowfun, dat$min1, dat$max1, dat$ip, MoreArgs = other1)

  # If not test, TRUE for normal
  if(!test) {
    att1 <- !(att1 == "attack" | att1 == "attack connectivity")
    # Drop min1, max1 columns, add labels
    dat <- dplyr::select(dat, -min1, -max1)
    out <- mutate(dat, label = att1)

  }else{
    out <- att1
  }

  return(out)
}




#' \code{windowfun} Function to identify attacks in window
#'
#' This function takes the start and end of the window, and the IP
#' and outputs whether the time should be labelled as an attack
#'
#' @param min1 Start of window (datetime)
#' @param max1 End of window (datetime)
#' @param ip1 IP address in data
windowfun <- function(min1, max1, ip1, attack, connect1) {
  
  # Set up attack data
  attvector <- attack$datetime
  ips <- attack$ip
 
  # Number of attacks in window
  wh1 <- which((attvector < max1) & (attvector > min1))

  # these are all the IP addresses involved in attacks during the time window specified
  ipatt <- ips[wh1]

  # if our IP is in that vector
  ssip1 <- substr(ip1, 1, 11)
  if(ip1 %in% ipatt | ssip1 %in% ipatt) {
    out <- "attack" 
  # if IP not in vector, need to check connections
  }else if(length(wh1) > 0) {
   
    # set up connectivity data
    convector <- connect1$datetime
    conip <- connect1[, c("src", "dst")]
    conip$src <- as.character(conip$src)
    conip$dst <- as.character(conip$dst)

    # ip1 is IP in data
    # ipatt are victim IPs in attack data

    # what connections occurred in time window
    wh1 <- which((convector < max1) & (convector > min1))
    # which IPs talked to each other in window
    conip1 <- conip[wh1, ]

    #victim / attacker pair
    datva <- data.frame(rep(ip1, length(ipatt)), ipatt)

    # start counter
    l1 <- 0
    i <- 1
    while(l1 == 0 && i <= nrow(datva)) {
      # match ips between connectivity and attack
      m1 <- mapply(match, conip1, datva[i, ])
      
      # get different orders of match
      test <- data.frame(c(1, 2), c(2, 1))
    
      # Find those that match 1,2 or 2,1
      l0 <- apply(m1, 1, function(x) all(x == test[1, ]) | all(x == test[2, ]))
      l1 <- l1 + sum(1 * l0[complete.cases(l0)])
      i <- i + 1
    }

    if(l1 > 0) {
      browser()
      out <- "attack connectivity"
    } else {
      out <- "IP not attacked in window"
    }

    
  
  # If no attacks in time window, cannot be attack
  }else {
    out <- "no attacks in window"
  }

  out
}


