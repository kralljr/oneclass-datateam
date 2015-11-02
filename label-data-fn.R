


#' Functions to apply attack labels
#'
#' \code{labelall} Label all datasets
#' 
#' This function labels all datasets, and appends them togther
#' 
#' @param valid1 list of filenames for validation datasets
#' @param test1 list of filenames for test datasets
labelall <- function(valid1, test1, mns1, sds1, rms = NULL) {
  for(i in 1 : length(valid1)) {
        
    dat <- read.csv(valid1[i], header = F, stringsAsFactors = F)
    
    # remove date and ip
    datd <- dat[, c(1, 2)]
    dat <- dat[, -c(1, 2)]

    # rescale
    dat <- sweep(dat, 2, mns1, "-")
    dat[, sds1 != 0] <- sweep(dat[, sds1 != 0], 2, sds1[sds1 != 0], "/")


    if(!is.null(rms)) {
      dat <- dat[, -which(colnames(dat) %in% rms)]
    }

    dat <- data.frame(datd, dat)
    dat1 <- getlabel(dat, valid1[i])
    names1 <- dat1$names
    dat1 <- dat1$dat

    app1 <- ifelse(i == 1, F, T)
    print(c(i, app1))



    # Save names
    names1 <- data.frame(dat1[, 1], dat1[, 2], dat1$label, names1)    
    write.table(names1, sep = ",", file = "validation-names.csv", append = app1, col.names = !app1, row.names = F)


    # Save data
    dat1 <- dat1[, -c(1, 2)]
    #write.table(dat1, sep = ",", file = "validation.csv", append = app1, col.names = !app1, row.names = F)


  }


  for(i in 1 : length(test1)) {
    if(i != 2) { 
      dat <- read.csv(test1[i], header = F, stringsAsFactors = F)

      # remove date and ip
      datd <- dat[, c(1, 2)]
      dat <- dat[, -c(1, 2)]

      # rescale
      dat <- sweep(dat, 2, mns1, "-")
      dat[, sds1 != 0] <- sweep(dat[, sds1 != 0], 2, sds1[sds1 != 0], "/")

      if(!is.null(rms)) {
        dat <- dat[, -which(colnames(dat) %in% rms)]
      }

      dat <- data.frame(datd, dat)
      dat1 <- getlabel(dat, test1[i], type = "test" )

      names1 <- dat1$names
      dat1 <- dat1$dat
      
      app1 <- ifelse(i == 1, F, T)

      print(c(i, app1))

      if(class(names1) == "list") {
        names1 <- sapply(names1, function(x) paste(x, collapse = ":"))
      }

      # save names
      names1 <- data.frame(dat1[, 1], dat1[, 2], dat1$label, names1)    
      write.table(names1, sep = ",", file = "test-names.csv", append = app1, col.names = !app1, row.names = F)

      #save data
      dat1 <- dat1[, -c(1, 2)]
      #write.table(dat1, sep = ",",file = "test.csv", append = app1, col.names = !app1, row.names = F)
      }
  }
}


#' 
#' \code{getlabel} Read in data and apply labels
#'
#' This function takes the filename, and some other information
#' and outputs the data with applied attack (1) and normal (0) labels
#'
#' @param dat validation or test data
#' @param filename filename of data
#' @param type validation or test data, default "valid"
#' @param plusminus number of seconds in window (+/-)
getlabel <- function(dat, filename, type = "valid", plusminus = 30, test = F) {
  
  # label columns in data
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

  # limit attack data to current day
  days <- unique(day(dat$date)) 
  attack <- dplyr::filter(attack, day(datetime) %in% days)

  # find window for attack data
  attack <- mutate(attack, min1 = datetime - plusminus, max1 = datetime + plusminus)
  
  #ips <- attack$ip
  #attvector <- attack$datetime

  
  # get connection data
  connect1 <- get(sapply(strsplit(gsub("-", "_", filename), "\\."),
    function(x) x[1]))


  # find which entries are attacks from attack data
  other1 <- list(dat = dat, connect1 = connect1, type = "attack")
  att1 <- mapply(windowfun, attack$min1, attack$max1, attack$ip, attack$name,  MoreArgs = other1)


  other1 <- list(dat = dat, connect1 = connect1, type = "names")
  names1 <- mapply(windowfun, attack$min1, attack$max1, attack$ip, attack$name,  MoreArgs = other1)
  names1 <- apply(names1, 1, function(x) {
    whx <- which(!is.na(x))
    if(length(whx) > 0) {
      out <- unique(x[whx])
    }else{
      out <- NA
    }
    out
})


  # Corresponds to at least one attack 
  att1 <- apply(att1, 1, function(x) min(x))
  att1 <- att1 == 1

  # Add in labels
  dat <- mutate(dat, label = att1)

  list1 <- list(dat = dat, names = names1)
  return(list1)
}




#' \code{windowfun} Function to identify attacks in window
#'
#' This function takes the start and end of the window, and the IP
#' and outputs whether the time should be labelled as an attack
#'
#' @param min1 Start of window (datetime)
#' @param max1 End of window (datetime)
#' @param ip1 IP address in data
#' @param name attack type
#' @param type return names or attack data
windowfun <- function(min1, max1, ip1, name, dat, connect1, type = "attack", shift = 2) {
  # Set up attack data
  datvec <- dat$date
  ips <- dat$ip
 
  # Number of potential attacks (in window of attack)
  wh1 <- which((datvec < max1) & (datvec > min1))
  

  # The IP addresses involved in attacks in time window specified
  ipatt <- ips[wh1]
  

  # IP for attack on entire system
  ssipatt <- substr(ipatt, 1, 11)

  # Position of attacks with matching ips
  attout <- wh1[which(ipatt == ip1 | ssipatt == ip1)]


  # Save vector of attacks
  attacks <- rep(T, length = length(datvec))
  attacks[attout] <- F

  names1 <- rep(NA, length = length(datvec))
  names1[attout] <- name

  # Position of attacks without matching ips
  notatt <- wh1[which(ipatt != ip1 & ssipatt != ip1)]
  
  
  # If IP not in vector, need to check connections
  if(length(notatt) > 0) {
   
    # set up connectivity data
    convector <- connect1$datetime
    conip <- connect1
    conip$src <- as.character(conip$src)
    conip$dst <- as.character(conip$dst)

    # what connections occurred in time window of attack
    wh2 <- which((convector < max1) & (convector > min1))
    # which IPs talked to each other in window
    conip1 <- conip[wh2, ]

    #limit to attack IP
    conip1 <- conip1[apply(conip1[, -1], 1, function(x) ((ip1 %in% x) | ip1 %in% substr(x, 1, 11))), ]
    # find unique IPs that talked with attack IP

    #for each observation in notatt
    for(i in 1 : length(notatt)) {
      date1 <- datvec[notatt[i]] 
      min1 <- date1 - shift
      max1 <- date1 + shift
      con2 <- filter(conip1, datetime < max1 & datetime > min1)
      con2 <- unique(unlist(con2))
      con2 <- con2[con2 != ip1]

      # what are the IPs in window, not IP in attack 
      noip <- ips[notatt[i]]
      ssnoip <- substr(noip, 1, 11)
      # find those that talked with attack in time window
      #notatt2 <- notatt[which(noip %in% con2 | ssnoip %in% con2)]


      if(noip %in% con2 | ssnoip %in% con2) {

        attacks[notatt[i]] <- F
        names1[notatt[i]] <- name 
      }
    }
    
    
  }  

  data.frame(attacks, names1)
  if(type == "attack") {
    out <- attacks
  } else{
    out <- names1
  }
  out
}


