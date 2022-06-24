# Libraries ----
library(readxl)
library(httr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(timetk)
library(tidyquant)
#install.packages("RMySQL")
library(RMySQL)
packageVersion("readxl")

populate_new_data <- function(){
# Connection to MYSQL ----
## MYSQL Connection ----
mydb = dbConnect(MySQL(), user='ruser',password='user123!' ,dbname='rschema', host='127.0.0.1')

## Set to enable ----
dbSendQuery(mydb, "SET GLOBAL local_infile = true;") 

# Tempp work to create ----
#dates  = '2022-05-09'
# url1 <- paste("https://dps.psx.com.pk/download/indhist/",dates,".xls",sep = "")
# #    x <- GET(url1, write_disk(tf <- tempfile(pattern = "Z-PSX",fileext = paste(dates,".xls"),tmpdir = "./")))
# curl_var <- GET(url1, write_disk(path = paste("Z-PSX",dates,".xls")))
# 
# kse_all_shares_df <- read_excel(paste("Z-PSX",dates,".xls"),sheet = "KSE-ALL-Shares")
# 
# 
# kse_all_shares_df <- kse_all_shares_df %>%
#     add_column(stockdate = ymd(dates))%>%
#     rename(id_wt_perc=`IDX WT %`,
#            FF_BASED_MCAP= `FF BASED MCAP`,
#            FF_BASED_SHARES=`FF BASED SHARES`,
#            ORD_SHARES=`ORD SHARES`,
#            ORD_SHARES_MCAP=`ORD SHARES MCAP`
#     )
# 

dbSendQuery(mydb, "SET GLOBAL local_infile = true;") 


# close db connection after function call exits
on.exit(dbDisconnect(mydb))

query<- "select max(stockdate) from psx_stocksinfo_table"

# send Query to btain result set
rs <- dbSendQuery(mydb, query)

# get elements from result sets and convert to dataframe
result <- fetch(rs, -1)



# return the dataframe


# Create Date Sequence ----
dfd<- tibble(dates=seq(as.Date(result$`max(stockdate)`)+1, today(), by="days"))


# Scrape data from PSX Site ----
url <-     map(dfd$dates,.f=function(.x){
        ddate <- .x
        #ddate <- "2021-12-18"
        url1 <- paste("https://dps.psx.com.pk/download/indhist/",ddate,".xls",sep = "")
        url1
     zz <-  GET(url1, write_disk(path = paste("Z-PSX",ddate,".xls")))
        
     if(zz$status_code!=404){
        kse_all_shares_df <- read_excel(paste("Z-PSX",ddate,".xls"),sheet = "KSE-ALL-Shares")
        
      if(!is_empty(kse_all_shares_df) ) {   
        kse_all_shares_df <- kse_all_shares_df %>%
            add_column(stockdate = ymd(ddate))%>%
            rename(id_wt_perc=`IDX WT %`,
                   FF_BASED_MCAP= `FF BASED MCAP`,
                   FF_BASED_SHARES=`FF BASED SHARES`,
                   ORD_SHARES=`ORD SHARES`,
                   ORD_SHARES_MCAP=`ORD SHARES MCAP`
            )
      
      dbWriteTable(mydb, name='psx_stocksinfo_table', value=kse_all_shares_df,append= TRUE, temporary= FALSE)
      }  
     }
     file.remove(paste("Z-PSX",ddate,".xls"))
   }
    

    )
}

url1 <- "https://dps.psx.com.pk/download/text/listed_cmp.lst.Z"
GET(url1, write_disk(path = paste("symbol.zip")))
unzip("symbol.zip")
d1 <- read.table(file = ".//listed_cmp.lst",
                 header = FALSE,
                 sep = "|",
                 na.strings = "",
                 comment.char = "",
                 quote = "\"",
                 fill = FALSE,
                 nrows = 200000)
      
d1
#dbWriteTable(mydb, name='psx_stocksinfo_table', value=kse_all_shares_df,append= TRUE, temporary= FALSE)

#dbSendStatement(mydb, 'delete from psx_stocksinfo_table')

dee='abc'
dee ='\'abc\''
dee

from = today()
to= today() -180


xd<- paste("select * from psx_stocksinfo_table where symbol = 'WTL' and stockdate between '",
          from,"' and '",to,"'",sep = "")

# MYSQL queries ----
rs = dbSendQuery(mydb, 'select * from psx_stocksinfo_table')
dbClearResult(rs)
rs = dbSendQuery(mydb, xd)
psx_data = fetch(rs, n=-1)

psx_data%>% tail()

psx_data %>%
    mutate(price= as.numeric(PRICE))%>%
    select(stockdate,SYMBOL,price)%>%
    mutate(symbol=SYMBOL,
           date = stockdate,
           mavg_short = rollmean(price,k = 5, na.pad = TRUE,align = "right"),
           mavg_long = rollmean(price,k = 20, na.pad = TRUE,align = "left") )%>%
    select(date,adjusted,mavg_short, mavg_long)


###
library(xml2)
library(rvest)
library(xml2)


URLD <- "https://dps.psx.com.pk/downloads"

pg<- read_html(URLD)

head(html_attr(html_nodes(pg, "a"), "href"))

doc <- htmlParse(URLD)
links <- xpathSApply(doc, "//a/@href")
free(doc)
###

library(httr)
library(rvest)
library(tidyverse)

pg <- read_html("https://dps.psx.com.pk/downloads")

fils <- html_nodes(pg, xpath=".//a[contains(@href, 'text/html')]")

data_frame(
    filename = html_text(fils),
    link = sprintf("https://dps.psx.com.pk/downloads/symbol_name%s", html_attr(fils, "href"))
) -> xdf

glimpse(xdf)

# close connection ----

dbDisconnect(mydb)
