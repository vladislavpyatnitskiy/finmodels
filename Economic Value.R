lapply(c("quantmod","timeSeries","rvest"),require,character.only=T) # Libraries

Economic.Value <- function(x,  tr = "^TNX", i = "^GSPC",N=10){ # Economic Value
  
  ev <- NULL # List for values
  
  for (g in 1:length(x)){ a <- x[g] # Assign variable for each ticker
  
    bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s",a,a)
    cf <- sprintf("https://finance.yahoo.com/quote/%s/cash-flow?p=%s", a, a)
    
    page.bs <- read_html(bs) # Read HTML & extract necessary info
    page.is <- read_html(is) # Read HTML & extract necessary info
    page.cf <- read_html(cf) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
    price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    price.yahoo3 <- page.cf %>% html_nodes('div') %>% .[[1]] -> tab.cf
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    w <- tab.cf %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    c <- NULL
    h <- NULL
    j <- NULL
    
    o <- c("Free Cash Flow", "Capital Expenditure")
    
    d <- c("Total Debt", "Total Liabilities Net Minority Interest",
           "Total Equity Gross Minority Interest", "Invested Capital")
    
    r <- c("Interest Expense","Tax Provision","Net Income Common Stockholders",
           "Operating Income", "Reconciled Depreciation", "EBIT")
    
    for (m in 1:length(r)){ c <- rbind(c, u[grep(r[m], u) + 1][1]) }
    for (m in 1:length(d)){ h <- rbind(h, y[grep(d[m], y) + 1][1]) }
    for (m in 1:length(o)){ j <- rbind(j, w[grep(o[m], w) + 1][1]) }
    
    c<-as.numeric(gsub(",","",gsub("([a-zA-Z]),","\\1 ",c))) # Income Statement
    h<-as.numeric(gsub(",","",gsub("([a-zA-Z]),","\\1 ",h))) # Balance Sheet
    j <- as.numeric(gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", j))) # FCF
    
    Rd <- c[1] / h[1] # Interest Expense / Total Debt
    after.tax.ratio <- 1 - c[2] / c[3] # Tax Provision / Net Income 
    debt.part <- h[2] / (h[2] + h[3]) # Liabilities / Total Assets
    
    costofdebt <- debt.part * Rd * after.tax.ratio # Cost of Debt
    
    capital.part <- 1 - debt.part # Equity / Total Assets
    
    oi <- c[4] # Operating Income
    ic <- h[4] # Investing Capital
    
    roic <- after.tax.ratio * oi / ic
    
    rr <- (j[2] - c[5]) / (c[6] * after.tax.ratio)
    
    today <-as.Date(as.character(y[grep("Breakdown", y) + 1]), "%m/%d/%Y")
    
    yi <- c(a, tr, i) # Add 10 year Treasuries to list
    
    p <- NULL # Create a list for securities data
    
    for (A in yi){ p <- cbind(p, getSymbols(A, from = today - 365 * N, 
                                            to = today, src = "yahoo",
                                            auto.assign=F)[,4]) }
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- yi # Put the tickers in data set
    
    p <- as.timeSeries(p) # Make it time series
    
    rf <- apply(p[,tr], 2, function(col) mean(col) / 100) # Risk Free Return
    
    # Calculate  beta
    b <- apply(diff(log(p[,a]))[-1,], 2,
               function(col) (lm((col)~diff(log(p[,i]))[-1,]))$coefficients[2])
    
    ER <- rf + b * ((exp(sum(diff(log(p[,i]))[-1,])))^(1 / N) - 1 - rf) # CAPM
    
    wacc <- costofdebt + capital.part * ER 
    
    ev <- rbind(ev, (j[1] / (wacc - rr * roic)) * 1000) } # Display
    
  rownames(ev) <- x # Row names
  colnames(ev) <- "Value" # Column names
  
  ev # Return
}
Economic.Value(c("JEF", "VIRT"), i = "^GSPC") # Test
