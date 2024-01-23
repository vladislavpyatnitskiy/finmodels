library("rvest") # Library

OIBDA <- function(x, full = F){ # function to get OIBDA ratios
  
  oibda <- NULL # List for OIBDA values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
    
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    p <- c("Operating Income", "Tax Provision", "Interest Expense",
           "Reconciled Depreciation")
    
    c <- NULL  
    
    for (m in 1:length(p)){ c <- rbind(c, y[grep(p[m], y) + 1][1]) }
    
    c <- as.numeric(gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)))
    
    if (isTRUE(full)){ w <- cbind(sum(c[seq(4)]), c[1], c[2], c[3], c[4])
      
      colnames(w) <- c("OIBDA", "Operating Income", "Tax Provision",
                       "Interest Expense", "Reconciled Depreciation") 
      
      oibda <- rbind(oibda, w) } else {
        
        oibda <- rbind(oibda, sum(as.numeric(c[seq(4)]))) } } # OIBDA
    
  rownames(oibda) <- x # Ticker names
  
  if (isFALSE(full)){ colnames(oibda) <- "OIBDA"}
  
  oibda # Display
}
OIBDA(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA"), full = T) # Test
