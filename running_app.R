# Getting most recent data file to use 



# ---- Neccessary Functions ----
data_load <- function(file)
  return(read_csv(file))

# function converts character time to usable numeric values 
chr2time <- function(x){
 x <- sapply(str_split(x, ":"), as.numeric)
 x[2,] <- x[2,] / 60
 return(colSums(x))
}
cum_avg <- function(x,N){cumsum(x)/(1:N)}

data_prep <- function(x,omit.col=c()){
  
  # Dealing with time formatting 
  time_split <- strsplit(as.character(x$Date)," ")
  n <- nrow(x)
  
  date <- c()
  time <- c()
  
  for (i in 1:length(time_split)){
    time <- c(time, time_split[[i]][2])
    date <- c(date, time_split[[i]][1])
  }
  time <- format(strptime(time,format = '%H:%M'), "%H:%M")
  
  # Introducing a classifier for times
  time_classify <- function(x){
    if(x < 600){return("Early Morning")}
    if(x < 1100){return("Morning")}
    if(x < 1700){return("Afternoon")}
    if(x < 2200){return("Evening")}
    if(x < 1159){return("Night")}
    if(x < 330){return("Night")}
  }
  

  out <- x %>% 
    select(-c(Date, Delete, `Athlete Settings`,
              `Avg Watts (w)`, `Power Stress Score`, 
              `Running Stress Score`, `Moving Time`, 
              `Avg HR (bpm)`, `Avg Active Cadence (rpm or spm)`,
              Hrss)
    ) %>%
    # Renaming
    rename(Dist = `Distance (km)` , Elv.gain = `Elevation Gain (m)`,
           Avg.sp = `Avg Speed (kph)` , Avg.pace=`Avg Pace (/km)`) %>%
    # Adding new info we created
    mutate('Date' = date,  
           "Time" = time, 
           "ToD" =  sapply(as.numeric(gsub(":", "", time)), time_classify),
           "Year" = format(as.POSIXct(Date), "%y"),
           "Month" = format(as.POSIXct(Date), "%m"),
           "DoM" = format(as.POSIXct(Date), "%d"),
           "Avg.pace" = format(as.POSIXct(Avg.pace), "%H:%M"),
           "10day.dist" = c(cum_avg(Dist[1:9],9),sapply(10:n, function(i) sum(Dist[(i-9):i]) / 10)),
           "10day.pace" =  c(cum_avg(chr2time(Avg.pace[1:9]),9),
                             sapply(10:n, function(i) sum(chr2time(Avg.pace[(i-9):i])) / 10))
    ) %>% 
    arrange(Date) %>%
    # Resorting table for ease of use
    select(Year, Month, DoM, Time, Date, everything(), -c(Name,Type,all_of(omit.col)))
  
  return(out)
}

# ---- Plotting ----
plot_pace <- function(dataset){
  n <- nrow(dataset)
  
  return(
    ggplot(runs, aes(x= 1:n)) +
      geom_line(aes(y= chr2time(Avg.pace)), col = "#fc4c02") +
      # geom_line(aes(y= cum_avg( chr2time(Avg.pace),n) ), col = 'black',
      #           lty = 2) +
      geom_vline(xintercept = 21, lty = 2, alpha = 0.4) +
      geom_line(aes(y = `10day.pace`), col = 'black', lty = 2) +
      labs(x = "Run #", y = "Average Pace (/km)") +
      theme_minimal()
  )

    
}
plot_dist <- function(dataset){
  n <- nrow(dataset)
  return(
    ggplot(dataset, aes(x= 1:n)) +
      geom_col(aes(y = Dist), col = "#fc4c02", fill = "#fc4c02"  ,alpha = 0.1) +
      geom_vline(xintercept = 21, lty = 2, alpha = 0.4) +
      # geom_line(aes(y= cum_avg(Dist,n)), col = 'black', lty =2 ) +
      geom_line(aes(y = `10day.dist`), col = 'black', lty = 2) +
      labs(x = "Run #", y = "Distance (km)") +
      theme_minimal()
    )
  }
plot_monthly_dist <- function(dataset){
  month_data <- dataset %>% group_by(Month, Year) %>% 
    summarise(tot.dist = sum(Dist), no.runs = n(), avg.dist = round(mean(Dist),2))
  n <- nrow(month_data)
  
  n_years <- length(unique(month_data$Year))
  return(
    ggplot(month_data) +
      geom_col(aes(x = 1:n, y =  tot.dist), 
               fill = "#fc4c02", colour = "#fc4c02", alpha = 0.1) +
      labs(x = "Month", y = "Distance (km)") +
      # Annotating with distance for month and (number of runs)
      annotate("text",
               x = 1:n, 
               y = month_data$tot.dist + 4,
               label = paste(month_data$tot.dist, "km (",
                             month_data$no.runs, ")", sep = "")) +
      scale_x_continuous(breaks = 1:n, labels = rep(month.abb, n_years)[1:n]) +
      theme_minimal()
  )
}


# ---- Shiny App ----

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      strong("Sidebar"),
      "Usually inputs go here, width = 4 by default",
      fileInput("file", NULL, accept = c(".csv")),
    ),
    mainPanel(
      tableOutput("summaryTable"),
      fluidRow(
        column(plotOutput("plot", width = "100%"), width = 6),
        column(plotOutput("plot1", width = '100%'), width = 6)
      ),
      plotOutput("plot2", width = '100%')
    )
  )
)
server <- function(input, output, session) {
  # Create a reactive expression
  
  data <- reactive({
    req(input$file)
    df <- read_csv(input$file$datapath)
    
    return(data_prep(df))
  })
  
  output$summaryTable <- renderTable({
    summaryTable <- head(data())
  })
  
  output$plot2 <- renderPlot({plot_monthly_dist(data())})
  output$plot1 <- renderPlot({plot_pace(data())})
  output$plot <- renderPlot({plot_dist(data())})
}

shinyApp(ui, server)

