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

mydb = dbConnect(MySQL(), user='ruser',password='user123!' ,dbname='rschema', host='127.0.0.1')

dates  = '2022-05-19'
url1 <- paste("https://dps.psx.com.pk/download/indhist/",dates,".xls",sep = "")
#    x <- GET(url1, write_disk(tf <- tempfile(pattern = "Z-PSX",fileext = paste(dates,".xls"),tmpdir = "./")))
GET(url1, write_disk(path = paste("Z-PSX",dates,".xls")))

kse_all_shares_df <- read_excel(paste("Z-PSX",dates,".xls"),sheet = "KSE-ALL-Shares")

kse_all_shares_df <- kse_all_shares_df %>%
    add_column(stockdate = ymd(date))%>%
    rename(id_wt_perc=`IDX WT %`,
           FF_BASED_MCAP= `FF BASED MCAP`,
           FF_BASED_SHARES=`FF BASED SHARES`,
           ORD_SHARES=`ORD SHARES`,
           ORD_SHARES_MCAP=`ORD SHARES MCAP`
    )



dbWriteTable(mydb, name='psx_stock_table', value=data.frame.name)


date= "2022-05-18"

dfd<- tibble(dates=seq(as.Date("2021-12-13"), as.Date("2022-05-20"), by="days"))

dfd%>rename(dates= starts_with("`seq(as.Date"))



url <-     map(dfd$dates,.f=function(.x){
        ddate <- .x
        #ddate <- "2021-12-18"
        url1 <- paste("https://dps.psx.com.pk/download/indhist/",ddate,".xls",sep = "")
        url1
     zz <-  GET(url1, write_disk(path = paste("Z-PSX",ddate,".xls")))
        
     if(zz$status_code!=404){
        kse_all_shares_df <- read_excel(paste("Z-PSX",ddate,".xls"),sheet = "KSE-ALL-Shares")
        
        kse_all_shares_df <- kse_all_shares_df %>%
            add_column(stockdate = ymd(ddate))%>%
            rename(id_wt_perc=`IDX WT %`,
                   FF_BASED_MCAP= `FF BASED MCAP`,
                   FF_BASED_SHARES=`FF BASED SHARES`,
                   ORD_SHARES=`ORD SHARES`,
                   ORD_SHARES_MCAP=`ORD SHARES MCAP`
            )
        
        kse_all_shares_df%>%
            write_rds(paste("Z-PSX",".rds"))
        
        
     }
     file.remove(paste("Z-PSX",ddate,".xls"))
    }
)

write
url%>%select
    map_df(.f=function(.x){
    GET(.x, write_disk(path = paste("Z-PSX",dates,".xls")))    
    })
    
df <- read_excel(tf, 2L)

dfd["dates"][[1]]

?c()
kse_all_shares_df <- read_excel("./file1384102e39e9.xls",sheet = "KSE-ALL-Shares")

kse_all_shares_df <- kse_all_shares_df %>%
    add_column(stockdate = ymd(date))%>%
    rename(id_wt_perc=`IDX WT %`,
          FF_BASED_MCAP= `FF BASED MCAP`,
          FF_BASED_SHARES=`FF BASED SHARES`,
          ORD_SHARES=`ORD SHARES`,
          ORD_SHARES_MCAP=`ORD SHARES MCAP`
    )

kse_all_shares_df
    

dfd %>% rename_at(vars(starts_with("`seq(as.Date")), 
                     funs("dates"))

dfd[colm]
