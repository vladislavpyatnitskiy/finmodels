library("rvest") # Library

operating.ratios <- function(x){ # Function to get data for operational metrics
  
  ors <- NULL # List for operational metrics values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
  
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    p <- c("Operating Income", "Tax Provision", "Interest Expense",
           "Reconciled Depreciation", "EBITDA", "EBIT")
    
    c <- NULL  
    
    for (m in 1:length(p)){ c <- rbind(c, y[grep(p[m], y) + 1][1]) }
    
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) 
    
    w <- cbind(sum(as.numeric(c[seq(4)])), as.numeric(c[5]), as.numeric(c[6]))
    
    colnames(w) <- c("OIBDA", "EBITDA", "EBIT") # Column names
    
    ors <- rbind.data.frame(ors, w) } # Join to data frame
    
  rownames(ors) <- x # Ticker names
  
  ors # Display
}
operating.ratios(c("AAPL", "AIG", "C")) # Test
