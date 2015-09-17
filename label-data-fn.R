


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

    app1 <- ifelse(i == 1, F, T)
    print(c(i, app1))
    write.table(dat1, sep = ",", file = "validation.csv", append = app1, col.names = !app1)
  }


  for(i in 1 : length(test1)) {
    
    dat1 <- getlabel(test1[i], type = "test" )

    app1 <- ifelse(i == 1, F, T)

    print(c(i, app1))
    write.table(dat1, sep = ",",file = "test.csv", append = app1, col.names = !app1)

  }
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
  ips <- dat$ip
  datvec <- dat$date


  # get 30 second window
  #dat <- mutate(dat, min1 = date - plusminus, max1 = date + plusminus)

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
  # find window for attack data
  attack <- mutate(attack, min1 = datetime - plusminus, max1 = datetime + plusminus)
  
  #ips <- attack$ip
  #attvector <- attack$datetime

  
  # get connection data
  connect1 <- get(sapply(strsplit(gsub("-", "_", filename), "\\."),
    function(x) x[1]))
  convector <- connect1$datetime
  conip <- connect1[, c("src", "dst")]
  conip$src <- as.character(conip$src)
  conip$dst <- as.character(conip$dst)


  # find which entries are attacks from attack data
  other1 <- list(dat = dat, connect1 = connect1)
  att1 <- mapply(windowfun, attack$min1, attack$max1, attack$ip, MoreArgs = other1)
 
  # Corresponds to at least one attack 
  att1 <- apply(att1, 1, function(x) any(x))

  # Add in labels
  dat <- mutate(dat, label = att1)

  return(dat)
}




#' \code{windowfun} Function to identify attacks in window
#'
#' This function takes the start and end of the window, and the IP
#' and outputs whether the time should be labelled as an attack
#'
#' @param min1 Start of window (datetime)
#' @param max1 End of window (datetime)
#' @param ip1 IP address in data
windowfun <- function(min1, max1, ip1, dat, connect1) {
  
  # Set up attack data
  datvec <- dat$date
  ips <- dat$ip
 
  # Number of potential attacks (in window of attack)
  wh1 <- which((datvec < max1) & (datvec > min1))
  
  # Set up output
  attout <- 0


  # The IP addresses involved in attacks in time window specified
  ipatt <- ips[wh1]

  # IP for attack on entire system
  ssipatt <- substr(ipatt, 1, 11)

  # Position of attacks with matching ips
  attacks <- wh1[which(ipatt == ip1 | ssipatt == ip1)]
  # Save these
  attout <- c(attout, attacks)

  # Position of attacks without matching ips
  notatt <- wh1[which(ipatt != ip1 & ssipatt != ip1)]
  
  # If IP not in vector, need to check connections
  if(length(notatt) > 0) {
   
    # set up connectivity data
    convector <- connect1$datetime
    conip <- connect1[, c("src", "dst")]
    conip$src <- as.character(conip$src)
    conip$dst <- as.character(conip$dst)

    # what connections occurred in time window of attack
    wh2 <- which((convector < max1) & (convector > min1))
    # which IPs talked to each other in window
    conip1 <- conip[wh2, ]

    #victim / attacker pair
    noip <- ips[notatt]
    datva <- data.frame(rep(ip1, length(noip)), noip)

    for(i in 1 : nrow(datva)) {
      # match ips between connectivity and attack
      m1 <- mapply(match, conip1, datva[i, ])
      
      # get different orders of match
      test <- data.frame(c(1, 2), c(2, 1))
    
      # Find those that match 1,2 or 2,1
      l0 <- try(apply(m1, 1, function(x) all(x == test[1, ]) | all(x == test[2, ])))
      if(class(l0) == "try-error") {browser()}

      l0 <- l0[complete.cases(l0)]
      l0 <- l0[l0]

      if(length(l0) > 0) {
        attout <- c(attout, notatt[i])  
      }
    }
  }
  attout <- attout[-1]
  # Save vector of attacks
  attacks <- rep(FALSE, length = length(datvec))
  attacks[attout] <- TRUE

  attacks
}


