################ Market Risk Analysis Workshop ################

######## Portfolio Generation ########

port_gen <- function(germ, asset_numb, asset_price_inter, asset_obi) {
  
  set.seed(seed = germ)
  
  asset_name <- paste("asset", letters[1:asset_numb], sep = "_")
  
  asset_price_sec <- seq( # asset_numb + 1
    from = min(asset_price_inter),
    to = max(asset_price_inter),
    by = (max(asset_price_inter) - min(asset_price_inter)) / asset_numb
  )
  
  asset_price <- sample(x = asset_price_sec, size = asset_numb)
  
  asset_shock <- rnorm(n = asset_numb * asset_obi, mean = 0, sd = 0.01)
  asset_shock_mat <- matrix(data = asset_shock, ncol = asset_numb)
  asset <- rbind(asset_price, as.data.frame(x = asset_shock_mat))
  colnames(asset) <- asset_name
  
  return(asset)
  
}

port <- port_gen(germ = 75, asset_numb = 10, asset_price_inter = c(75, 125), asset_obi = 2500)
View(port)

######## Plot Probability (Density|Mass) Function ########

see <- function(port) {
  for (index in colnames(port)) {
    asset_shock <- port[-1, index]
    hist(x = asset_shock, breaks = length(asset_shock) / sqrt(length(asset_shock)), main = index, freq = FALSE)
    den <- density(x = asset_shock)
    lines(x = den, col = "red")
  }
}

see(port)

######## Portfolio Position ########

port_posit <- as.data.frame(x = mapply(FUN = "*", port[1, ], 1 + cumsum(port[-1, ])))
View(port_posit)

######## Portfolio Shock ########

port_shock <- port[-1, ]
View(port_shock)

######## Aesthetics ########

port_date <- sort(x = unlist(lapply(X = 0:dim(port_posit)[1], FUN = function(X) {return(as.character(x = Sys.Date() - X))})))

port <- cbind(Date = port_date, port)

port_posit <- cbind(Date = port_date[-1], port_posit, stringsAsFactors = FALSE)
port_shock <- cbind(Date = port_date[-1], port_shock, stringsAsFactors = FALSE)

######## VAR ########

asset_VAR_gen <- function(port, port_posit, port_shock, VAR_sig_level = 0.01, VAR_time_horizon = 1, VAR_time_window = 250 * 5, VAR_date) {
  
  VAR_date_right <- as.Date(x = VAR_date)
  VAR_date_left <- as.Date(x = VAR_date) - VAR_time_window + 1
  
  VAR_date_right <- as.character(x = VAR_date_right)
  VAR_date_left <- as.character(x = VAR_date_left)
  
  port_posit <- port_posit[port_posit$Date == VAR_date, ]
  port_shock <- port_shock[port_shock$Date >= VAR_date_left & port_shock$Date <= VAR_date_right, ]
  
  shock_lower <- rep(x = NA, times = length(colnames(port_shock)[colnames(port_shock) != "Date"]))
  names(shock_lower) <- colnames(port_shock)[colnames(port_shock) != "Date"]
  shock_upper <- rep(x = NA, times = length(colnames(port_shock)[colnames(port_shock) != "Date"]))
  names(shock_upper) <- colnames(port_shock)[colnames(port_shock) != "Date"]
  
  for (index in colnames(port_shock)[colnames(port_shock) != "Date"]) {
    shock <- port_shock[, index]
    quant_lower <- quantile(x = shock, probs = VAR_sig_level)
    shock_lower[[index]] <- max(shock[shock < quant_lower], na.rm = TRUE)
    quant_upper <- quantile(x = shock, probs = 1 - VAR_sig_level)
    shock_upper[[index]] <- min(shock[shock > quant_upper], na.rm = TRUE)
  }
  
  VAR <- rep(x = NA, times = length(colnames(port)[colnames(port) != "Date"]))
  names(VAR) <- colnames(port)[colnames(port) != "Date"]
  
  for (index in colnames(port)[colnames(port) != "Date"]) {
    posit <- port_posit[, index]
    VAR[[index]] <- min(shock_lower[[index]] * posit, 0, shock_upper[[index]] * posit, na.rm = TRUE)
  }
  
  return(VAR)
  
}

asset_VAR <- asset_VAR_gen(port = port, port_posit = port_posit, port_shock = port_shock, VAR_date = "2018-05-08")

######## ETL ########

asset_ETL_gen <- function(port, port_posit, port_shock, ETL_sig_level = 0.025, ETL_time_horizon = 1, ETL_time_window = 250 * 5, ETL_date) {
  
  ETL_date_right <- as.Date(x = ETL_date)
  ETL_date_left <- as.Date(x = ETL_date) - ETL_time_window + 1
  
  ETL_date_right <- as.character(x = ETL_date_right)
  ETL_date_left <- as.character(x = ETL_date_left)
  
  port_posit <- port_posit[port_posit$Date == ETL_date, ]
  port_shock <- port_shock[port_shock$Date >= ETL_date_left & port_shock$Date <= ETL_date_right, ]
  
  shock_lower <- rep(x = NA, times = length(colnames(port_shock)[colnames(port_shock) != "Date"]))
  names(shock_lower) <- colnames(port_shock)[colnames(port_shock) != "Date"]
  shock_upper <- rep(x = NA, times = length(colnames(port_shock)[colnames(port_shock) != "Date"]))
  names(shock_upper) <- colnames(port_shock)[colnames(port_shock) != "Date"]
  
  for (index in colnames(port_shock)[colnames(port_shock) != "Date"]) {
    shock <- port_shock[, index]
    quant_lower <- quantile(x = shock, probs = ETL_sig_level)
    shock_lower[[index]] <- mean(shock[shock < quant_lower], na.rm = TRUE)
    quant_upper <- quantile(x = shock, probs = 1 - ETL_sig_level)
    shock_upper[[index]] <- mean(shock[shock > quant_upper], na.rm = TRUE)
  }
  
  ETL <- rep(x = NA, times = length(colnames(port)[colnames(port) != "Date"]))
  names(ETL) <- colnames(port)[colnames(port) != "Date"]
  
  for (index in colnames(port)[colnames(port) != "Date"]) {
    posit <- port_posit[, index]
    ETL[[index]] <- min(shock_lower[[index]] * posit, 0, shock_upper[[index]] * posit, na.rm = TRUE)
  }
  
  return(ETL)
  
}

asset_ETL <- asset_ETL_gen(port = port, port_posit = port_posit, port_shock = port_shock, ETL_date = "2018-05-08")

######## Aggregation ########

agree <- function(port, port_posit, port_shock, asset_risk_metric) {
  
  print("Universe! It is time to think about you!")
  
}
