library(tidyverse)
library(janitor)
library(lubridate)
library(DT)
library(reactable)
library(htmltools)
library(fontawesome)
library(scales)

rm(list = ls())
graphics.off()

ggplot2::theme_set(new = cowplot::theme_cowplot())
# 1_IMPORT ---------------------------------------------------------------------

df_raw <- read_csv("data/data.csv")

# 2_CLEAN ----------------------------------------------------------------------

df <- df_raw %>% 
        clean_names() %>% 
        mutate(date = mdy(date))

# 3_PLOTS -----------------------------------------------------------------------

plot_inventory <- df %>% 
                    ggplot(aes(x = date,
                               y = ending_inventory)) +
                    geom_line() +
                    scale_x_date(date_breaks = "7 days", date_labels = "%d-%b") +
                    labs(x = "", 
                         y = "Inventory", 
                         title = "Gas Inventory (in dth)", 
                         subtitle = "June 2022") +
  theme(plot.title = element_text(size = 20,hjust = .5),
        plot.subtitle = element_text(size = 16, hjust = .5))
  

plot_wacog <- df %>% 
                ggplot(aes(x = date,
                           y = wacog)) +
                geom_line() +
                scale_y_continuous(breaks = seq(0.5, 9)) +
                scale_x_date(date_breaks = "7 days", date_labels = "%d-%b") +
                labs(x = "", 
                     y = "Inventory", 
                     title = "WACOG", 
                     subtitle = "June 2022") +
                theme(plot.title = element_text(size = 20,hjust = .5),
                      plot.subtitle = element_text(size = 16, hjust = .5))

# 4_DATA_TABLE -----------------------------------------------------------------

# Step 1: Prepare
  dt <- df %>%
          select(-daily_inject_withdraw_in_dth) %>% 
          rename('Date' = date,
                 'Market Price' = market_price,
                 'Beginning Inventory' = beginning_inventory,
                 'Injection' = injection,
                 'Withdawal' = withdrawal,
                 'Ending Inventory' = ending_inventory,             
                 'WACOG' = wacog,
                 'Beginning Inventory Cost' = beginning_inventory_cost,     
                 'Injections at MP' = injections_at_mp,          
                 'Withdraws at WACOG' = withdraws_at_wacog,          
                 'Ending Inventory Cost' = ending_inventory_cost,      
                 'Ending Inventory Market Value' = ending_inventory_market_value) %>% 
          mutate(Date = format(x = Date, "%d-%b-%Y")) %>%
          mutate_at(.vars = c('Market Price',
                              'WACOG',
                              'Beginning Inventory Cost',
                              'Injections at MP',
                              'Withdraws at WACOG',
                              'Ending Inventory Cost',
                              'Ending Inventory Market Value'), scales::dollar)

# Step 2: Data Table
  reactable <- 
    
  # Download Button
    htmltools::browsable(
      tagList(tags$button(
        tagList(fontawesome::fa("download"), "Download as CSV"),
        onclick = "Reactable.downloadDataCSV('data-download-table', '06-2022_wacog.csv')"),
        
  # Reactable
    reactable::reactable(dt,
                         defaultPageSize = 15,
                         striped = T,
                         bordered = T,
                         compact = T,
                         highlight = T,
                         elementId = 'data-download-table',
                         theme = reactableTheme(
                           borderColor = "#dfe2e5",
                           stripedColor = "#f6f8fa",
                           highlightColor = "#f0f5f9",
                           cellPadding = "8px 12px",
                           style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
                         ),
                         columns = list(Date = colDef(footer = "Total"),
                                        'Injection' = colDef(footer = scales::number(sum(df$injection))),
                                        'Withdawal' = colDef(footer = scales::number(sum(df$withdrawal))),
                                        'Ending Inventory' = colDef(footer = scales::number(sum(df$injection) + sum(df$withdrawal))),
                                        'Injections at MP' = colDef(footer = scales::dollar(sum(df$injections_at_mp))),
                                        'Withdraws at WACOG' = colDef(footer = scales::dollar(sum(df$withdraws_at_wacog), accuracy = 1)),
                                        'Ending Inventory Cost' = colDef(footer = scales::dollar(last(df$ending_inventory_cost))),
                                        'Ending Inventory Market Value' = colDef(footer = scales::dollar(last(df$ending_inventory_market_value)))),
                         defaultColDef = colDef(footerStyle = list(fontWeight = 'bold')),
                         columnGroups = list(colGroup("In dth", columns = c("Beginning Inventory",
                                                                            "Injection",
                                                                            "Withdawal",
                                                                            "Ending Inventory"), sticky = "left"),
                                             colGroup("In $", columns = c("WACOG",
                                                                          "Beginning Inventory Cost",
                                                                          "Injections at MP",
                                                                          "Withdraws at WACOG",
                                                                          "Ending Inventory Cost",
                                                                          "Ending Inventory Market Value"), sticky = "left")))))
                                        

save(list = ls(), file = "data/data.Rdata")










