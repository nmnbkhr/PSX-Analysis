# # 
# # 
# time_unit <- "day"
# # 
# # data <- get_stock_data("HBL")
# # 
# # data
# # 
# # ?pivot_wider
# # 
# datadf <- aggregate_time_series(data,time_unit = "day")
# plot_time_series()
# # 
# #    datadf1 <- datadf %>% generate_forecast()%>%
# #        pivot_wider(id_cols =  date, names_from = key,values_from = total_adjusted)%>%
# #        mutate(symbol="HBL",adjusted = NA, mavg_short=NA,mavg_long=NA, Prediction, date = as.character(date))%>%
# #        select(symbol,date,adjusted,mavg_short,mavg_long,Prediction)
# #    
# # 
# # data1 <-  
# # add_row(datadf1)
# # data%>%mutate(Prediction=as.numeric(NA))%>%add_row(as.data.frame(datadf1)%>%filter(is.na(Prediction)==FALSE))%>%plot_stock_data()
# # 
# # plot_stock_data()
# # 
# # plot_time_series(datadf)
# # 
# datapred <- generate_forecast(datadf)
# # 
# # 
# # 
# # 
# # 


get_stock_list <-
    function(stock_index = "PSX") {
         
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
        file.remove("symbol.zip")
        file.remove(".//listed_cmp.lst")
        
        df <- d1%>%
        select(V2,V3)%>%
        mutate(label = str_c(V2, V3, sep = ", "))%>%
        select(label)
        
        return(df)
        
    
        
}





get_symbol_from_user_input <-
    function(user_input) {
        user_input %>% str_split(pattern = ", ") %>% pluck(1, 1)
    }


get_stock_data <-
    function(stock_symbol, 
             from = today() - days(180), 
             to   = today(), 
             mavg_short = 20, mavg_long = 50) {
       
        # Connection to MYSQL ----
        ## MYSQL Connection ----
        
     
        # mavg_short = 5
        # mavg_long = 8
        
        
        xd<- paste("select * from psx_stocksinfo_table where symbol = '",stock_symbol,"' and stockdate between '",
                   from,"' and '",to,"'",sep = "")
            stock_symbol = sqlQuery(xd)
        
       # dbDisconnect(mydb)
        
        stock_symbol %>% 
            mutate(adjusted= as.numeric(PRICE))%>%
            select(stockdate,SYMBOL,adjusted)%>%
            mutate(symbol=SYMBOL,
                   date = stockdate)%>%
            mutate(mavg_short = rollmean(adjusted, k = mavg_short, na.pad = TRUE, align = "right")) %>%
            mutate(mavg_long  = rollmean(adjusted, k = mavg_long, na.pad = TRUE, align = "right"))%>%
            select(symbol,date,adjusted:mavg_long)
            
        
       
        
    }

generate_favorite_cards <-
    function(favorites,
             from = today() - days(180), to = today(),
             mavg_short = 20, mavg_long = 50) {
        # mavg_short = 20
        # mavg_long = 50
        
        favorites %>%
            # Step 1
            map(.f = function(x) {
                x %>%
                    get_stock_data(
                        from = from,
                        to   = to,
                        mavg_short = mavg_short,
                        mavg_long  = mavg_long
                    )
            }) %>%
            set_names(favorites) %>%
            
            # Step 2
            map(.f = function(data) {
                data %>%
                    get_stock_mavg_info()
            }) %>%
            
            # Step 3
            bind_rows(.id = "stock") %>%
            mutate(stock = as_factor(stock)) %>%
            split(.$stock) %>%
            
            # Step 4
            map(.f = function(data) {
                data %>%
                    generate_favorite_card()
            }) %>%
            
            # Step 5 
            tagList()
    }

generate_favorite_card <-
    function(data) {
        column(
            width = 3,
            info_card(
                title = as.character(data$stock), 
                value = str_glue("{data$n_short}-Day <small>vs {data$n_long}-Day</small>") %>% HTML(),
                sub_value      = data$pct_chg %>% scales::percent(),
                sub_text_color = ifelse(data$mavg_warning_flag, "danger", "success"),
                sub_icon       = ifelse(data$mavg_warning_flag, "arrow-down", "arrow-up")
            )
        )
    }


plot_stock_data <-
    function(data) {
        g <- data %>%
            gather(key = "legend", value = "value", adjusted:mavg_long, factor_key = TRUE) %>%
            
            ggplot(aes(as_date(date), value, color = legend, group = legend)) +
            geom_line(aes(linetype = legend)) +
            theme_tq() +
            scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10)) +
            scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                         date_labels = "%b\n%Y")+
            scale_color_tq() +
            labs(y = "Adjusted Share Price", x = "")
        
        ggplotly(g)
    }



info_card <- function(title, value, sub_value,
                      main_icon = "chart-line", sub_icon = "arrow-up",
                      bg_color = "default", text_color = "default", sub_text_color = "success") {
    
    div(
        class = "panel panel-default",
        style = "padding: 0px;",
        div(
            class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
            p(class = "pull-right", icon(class = "fa-4x", main_icon)),
            h4(title),
            h5(value),
            p(
                class = str_glue("text-{sub_text_color}"),
                icon(sub_icon),
                tags$small(sub_value)
            )
        )
    )
    
}

get_stock_mavg_info <-
    function(data) {
        
        n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
        n_long  <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
        
        data %>%
            tail(1) %>%
            mutate(mavg_warning_flag = mavg_short < mavg_long) %>%
            mutate(
                n_short = n_short,
                n_long  = n_long,
                pct_chg = (mavg_short - mavg_long) / mavg_long
            )
        
    }


generate_commentary <-
    function(data, user_input) {
        warning_signal <- data %>%
            tail(1) %>%
            mutate(mavg_warning_flag = mavg_short < mavg_long) %>%
            pull(mavg_warning_flag)
        
        n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
        n_long  <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
        
        if (warning_signal) {
            str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends")
        } else {
            str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends")
            
        }
    }


sqlQuery <- function (query) {
    
    # creating DB connection object with RMysql package
    mydb = dbConnect(MySQL(), user='ruser',password='user123!' ,dbname='rschema', host='127.0.0.1')
    
    
    ## Set to enable ----
    dbSendQuery(mydb, "SET GLOBAL local_infile = true;") 
    
   
    # close db connection after function call exits
    on.exit(dbDisconnect(mydb))
    
    # send Query to btain result set
    rs <- dbSendQuery(mydb, query)
    
    # get elements from result sets and convert to dataframe
    result <- fetch(rs, -1)
    
    # return the dataframe
    return(result)
}



populate_new_data <- function(){
    # Connection to MYSQL ----
    ## MYSQL Connection ----
    mydb = dbConnect(MySQL(), user='ruser',password='user123!' ,dbname='rschema', host='127.0.0.1')
    
    ## Set to enable ----
    dbSendQuery(mydb, "SET GLOBAL local_infile = true;") 
    
   
    
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
    dbDisconnect(mydb)
}


aggregate_time_series <-
    function(data, time_unit = "month") {
        
        output_tbl <- data %>%
            
            mutate(date = floor_date(as_date(date), unit = time_unit)) %>%
            
            group_by(date) %>%
            summarize(total_adjusted = sum(adjusted)) %>%
            ungroup() %>%
            
            mutate(label_text = str_glue("Date: {date}
                                 Price: {scales::dollar(total_adjusted)}"))
        
        return(output_tbl)
        
    }

plot_time_series <-
    function(data) {
        
        g <- data %>%
            
            ggplot(aes(date, total_adjusted)) +
            
            geom_line(color = "#2c3e50") +
            geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
            geom_smooth(method = "loess", span = 0.2) +
            
            theme_tq() +
            expand_limits(y = 0) +
            scale_y_continuous(labels = scales::dollar_format()) +
            labs(x = "", y = "")
        
        
        ggplotly(g, tooltip = "text")
        
    }
generate_forecast <-
    function(data, n_future = 12, seed = NULL) {
        
        train_tbl <- data %>% 
            tk_augment_timeseries_signature()
        
        future_data_tbl <- data %>%
            tk_index() %>%
            tk_make_future_timeseries(n_future = n_future, inspect_weekdays = TRUE, inspect_months = TRUE) %>%
            tk_get_timeseries_signature() 
        
        time_scale <- data %>%
            tk_index() %>%
            tk_get_timeseries_summary() %>%
            pull(scale)
        
        if (time_scale == "year") {
            
            model <- linear_reg(mode = "regression") %>%
                set_engine(engine = "lm") %>%
                fit.model_spec(total_adjusted ~ ., data = train_tbl %>% select(total_adjusted, index.num))
            
        } else {
            seed <- seed
            set.seed(seed)
            model <- boost_tree(
                mode = "regression",
                mtry = 20,
                trees = 500,
                min_n = 3,
                tree_depth = 8,
                learn_rate = 0.01,
                loss_reduction = 0.01) %>%
                set_engine(engine = "xgboost") %>%
                fit.model_spec(total_adjusted ~ ., data = train_tbl %>% select(-date, -label_text, -diff))
        }
        
        
        prediction_tbl <- predict(model, new_data = future_data_tbl) %>%
            bind_cols(future_data_tbl) %>%
            select(.pred, index) %>%
            rename(total_adjusted = .pred, 
                   date        = index) %>%
            mutate(label_text = str_glue("Date: {date}
                                 Price: {scales::dollar(total_adjusted)}")) %>%
            add_column(key = "Prediction")
        
        output_tbl <- data %>%
            add_column(key = "Actual") %>%
            bind_rows(prediction_tbl) 
        
        return(output_tbl)
    }

plot_forecast <-
    function(data) {
        
        # Yearly - LM Smoother
        time_scale <- data %>%
            tk_index() %>%
            tk_get_timeseries_summary() %>%
            pull(scale)
        
        # Only 1 Prediction - points
        n_predictions <- data %>%
            filter(key == "Prediction") %>%
            nrow()
        
        
        g <- data %>%
            ggplot(aes(date, total_adjusted, color = key)) +
            
            geom_line() +
            # geom_point(aes(text = label_text), size = 0.01) +
            # geom_smooth(method = "loess", span = 0.2) +
            
            theme_tq() +
            scale_color_tq() +
            scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10))+
            scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                         date_labels = "%b\n%Y") +
            # scale_y_continuous(labels = scales::dollar_format()) +
           # expand_limits(y = 0) +
            labs(x = "", y = "")
        
        # Yearly - LM Smoother
        if (time_scale == "year") {
            g <- g +
                geom_smooth(method = "lm")
        } else {
            g <- g + geom_smooth(method = "loess", span = 0.2)
        }
        
        # Only 1 Prediction
        if (n_predictions == 1) {
            g <- g + geom_point(aes(text = label_text), size = 1)
        } else {
            g <- g + geom_point(aes(text = label_text), size = 0.01)
        }
        
        ggplotly(g, tooltip = "text")
    }



reload_history <- function(){
    
   
        # Connection to MYSQL ----
        ## MYSQL Connection ----
        mydb = dbConnect(MySQL(), user='ruser',password='user123!' ,dbname='rschema', host='127.0.0.1')
        
        ## Set to enable ----
        dbSendQuery(mydb, "SET GLOBAL local_infile = true;") 
        
        
        query<- "select min(stockdate) from psx_stocksinfo_table"
        
        # close db connection after function call exits
        on.exit(dbDisconnect(mydb))
        
        
        # send Query to btain result set
        rs <- dbSendQuery(mydb, query)
        
        # get elements from result sets and convert to dataframe
        result <- fetch(rs, -1)
        
        
        
         if(is.na(result$`min(stockdate)`)==TRUE){
             dfd<- tibble(dates=seq(today()-360, today()-1, by="days"))
         }else{
            dfd<- tibble(dates=seq(as.Date(result$`min(stockdate)`)-180, as.Date(result$`min(stockdate)`)-1, by="days"))
         }
        
        dfd
        
        # Scrape data from PSX Site ----
        url <-     map(dfd$dates,.f=function(.x){
            ddate <- .x
          
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
    


