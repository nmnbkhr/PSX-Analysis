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

# Connection to MYSQL ----
## MYSQL Connection ----
mydb = dbConnect(MySQL(), user='ruser',password='user123!' ,dbname='rschema', host='127.0.0.1')

# Set to enable ----
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


# Create Date Sequence ----
dfd<- tibble(dates=seq(as.Date("2021-12-13"), as.Date("2022-05-20"), by="days"))

dfd<- tibble(dates=seq(as.Date("2022-05-09"), as.Date("2022-05-10"), by="days"))


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

#dbWriteTable(mydb, name='psx_stocksinfo_table', value=kse_all_shares_df,append= TRUE, temporary= FALSE)

#dbSendStatement(mydb, 'delete from psx_stocksinfo_table')
# MYSQL queries ----
rs = dbSendQuery(mydb, 'select * from psx_stocksinfo_table')

psx_data = fetch(rs, n=-1)

psx_data%>% tail()

psx_data

# close connection ----

dbDisconnect(mydb)
