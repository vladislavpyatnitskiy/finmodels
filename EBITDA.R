library("rvest") # Library

EBITDA <- function(x, full = F){ # function to get EBITDA ratios
  
  ebitda <- NULL # List for EBITDA values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
  
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    p <- c("EBITDA", "Net Income Common Stockholders", "Tax Provision",
           "Interest Expense", "Reconciled Depreciation")
    
    c <- NULL  
    
    for (m in 1:length(p)){ c <- rbind(c, y[grep(p[m], y) + 1][1]) }
    
    c <- as.numeric(gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)))
    
    if (isTRUE(full)){ w <- cbind(c[1], c[2], c[3], c[4], c[5])
    
      colnames(w) <- c("EBITDA", "Operating Income", "Tax Provision",
                       "Interest Expense", "Reconciled Depreciation") 
      
      ebitda<-rbind(ebitda,w) } else { ebitda<-rbind(ebitda,c[1]) } } # EBITDA
      
    rownames(ebitda) <- x # Ticker names
  
  if (isFALSE(full)){ colnames(ebitda) <- "EBITDA"}
  
  ebitda # Display
}
EBITDA(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA"), full = T) # Test
