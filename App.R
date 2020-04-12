rm(list = ls())

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyMatrix)
library(DT)
library(reshape2)
library(ggplot2)
library(shinydashboardPlus)
library(plotly)
library(sqldf)


dr <- getwd()


dat_func <- function(){
  try(load(paste0(dr,"/data_raw/all_data.RData")))
}
dat_func()

# dat_func <- function(){
#   # all_data <- read.csv("data/covid_19_data.csv")
#   all_data[ ,"Date"] <- as.Date(all_data[ ,"ObservationDate"],"%m/%d/%Y")
#   colnames(all_data) <- c("SNo","ObservationDate","ProvinceState","CountryRegion",
#                          "LastUpdate","Confirmed","Deaths","Recovered","Date")
#   # all_dat[ ,"CountryRegion"] <- gsub("\\."," ",all_dat[ ,"CountryRegion"])
#   all_data <<- all_data
# }
# dat_func()

# dr <- "C:/Nirosha/COVID19"
# try(load(paste0(dr,"/data_raw/all_data.RData")))

f_cnty <- function(){
  try(load(paste0(dr,"/data_raw/all_data.RData")))
  # all_data <- read.csv("data/covid_19_data.csv")
  countries <- unique(all_data$CountryRegion)
  countries <<- countries[order(countries)]
}
f_cnty()

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = span("COVID-19 Dashboard", style="color: white; font-size:35px"), titleWidth = "450px"),
                    dashboardSidebar(width = "100px"
                                     # sidebarMenu(
                                     #   menuItem("Plots")
                                     # )
                                     # dataTableOutput("dfAll"), width = "450px"   
                    ),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('
                                                .skin-blue .main-sidebar {
                                                background-color: #1a1e24;
                                                }
                                                .skin-blue .main-header .navbar {
                                                background-color: #1a1e24;
                                                }
                                                .skin-blue .main-header .logo {
                                                background-color: #1a1e24;
                                                }
                                                .content-wrapper, .right-side {
                                                background-color: #9aa7ba;
                                                }
                                                /* tabBox backgroud */
                                                 .nav-tabs-custom>.nav-tabs {
                                                background-color: #9aa7ba;
                                                }
                                                .nav-tabs-custom > .nav-tabs > li.header {
                                                font-size: 60px;
                                                color: white; 
                                                }
                                                '))
                      ),
                      fluidRow(
                        tabBox(width = 12,id="tabset1", height="1800px",
                               # tabsetPanel(
                                 tabPanel("Plots",width = 4,
                                          # selectInput("Country","Select country", choices = countries, selected = "US", selectize = FALSE),
                                          
                                          box(width = 12,background = "black",
                                              valueBoxOutput(width=3,"InitialBox"),
                                              valueBoxOutput(width=4,"currentCases"),
                                              valueBoxOutput(width=5,"prate")
                                          ),
                                          
                                          # box(width = 12,background = "black",solidHeader = TRUE, collapsible = TRUE,title = "Global Statistics",
                                          #     valueBoxOutput(width = 2,"lUpdate"),
                                          #     valueBoxOutput(width = 2,"cnt1"),
                                          #     valueBoxOutput(width = 2,"cnt2"),
                                          #     valueBoxOutput(width = 2,"cnt3")
                                          # ),
                                          
                                          box(width = 12,background = "black",title="Statistics By Country",
                                              fluidRow(
                                                # column(width=4, valueBoxOutput(width=10,"By_cntry")),
                                                
                                                column(width=6,checkboxGroupInput("svar"," Select/Unselect here:", 
                                                                                  c("Confirmed","Deaths","Recovered"),
                                                                                  selected =c("Recovered","Deaths"),inline = TRUE)),
                                                column(width=2,
                                                       selectInput("Country","Select country", choices = countries, selected = "US", selectize = FALSE))
                                              )
                                          ),
                                          box(width = 12,
                                              plotOutput("pAll1", height = 500)
                                          ),
                                         
                                          box(width = 12,background = "black",solidHeader = TRUE, collapsible = TRUE,
                                              valueBoxOutput(width = 6,"By_cntry"),
                                              # valueBoxOutput(width = 2,"lUpdate"),
                                              valueBoxOutput(width = 2,"cnt1"),
                                              valueBoxOutput(width = 2,"cnt2"),
                                              valueBoxOutput(width = 2,"cnt3")
                                          ),
                                          box(width = 12,
                                              plotOutput("plog", height = 500)
                                          )
                                 ),
                               
                               tabPanel("Tables", 
                                        # box(width=12,background = "black",
                                        #   valueBoxOutput(width = 4,"C_name")
                                        # ),
                                        
                                        box(width = 12,background = "navy",solidHeader = TRUE, collapsible = TRUE,
                                            fluidRow(
                                              column(width=5,dataTableOutput("freq", width=400)),
                                              column(width=6,dataTableOutput("Tctry",width=600))
                                            ),
                                            fluidRow(
                                              column(width=6,dataTableOutput("dfAll",width=600))
                                            )
                                        )
                               )
                        )
                      ),
                      
                      
                      div(
                        h5("Data sources: based on ",tags$a(href="https://coronavirus.jhu.edu/","John Hopkins")," and ",
                           tags$a("kaggle", href="https://www.kaggle.com/"), "which include global data extracted from",
                           tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports","WHO"),",",
                           tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/index.html","CDC"),",",
                           tags$a(href="https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases","ECDC"),",",
                           tags$a(href="https://www.worldometers.info/coronavirus/","worldometer"),".")
                        # div(class = "alert alert-warning", role = "alert",
                        #     "Warning: warning msg goes here!"
                        # )
                      )
                      ,
                      tags$footer("The dashboard will be daily updated after 6:00PM(CT) US time."), br(),
                      tags$footer("By Nirosha Rathnayake, April 9th, 2020.")
                      
                      )
                    
                      )



server <- function(input, output) {
  
  
  output$cnt1 <- renderValueBox({
    dat <- sqldf::sqldf(paste0("select Date, sum(Confirmed) as Confirmed,
                               sum(Deaths) as Deaths,sum(Recovered) as Recovered from all_data 
                               group by Date"))
    
    valueBox(tags$p(paste0("Total Cases "),style="font-size:50%; color:white; text-align:center;"),
             tags$p(paste0(dat$Confirmed[nrow(dat)]),style="font-size:150%; color:white; text-align:center;"),
             color = "blue")})
  
  output$cnt2 <- renderValueBox({
    dat <- sqldf::sqldf(paste0("select Date, sum(Confirmed) as Confirmed,
                               sum(Deaths) as Deaths,sum(Recovered) as Recovered from all_data 
                               group by Date"))
    valueBox(tags$p(paste0("Total Deaths "),style="font-size:50%; color:white; text-align:center;"),
             tags$p(paste0(dat$Deaths[nrow(dat)]),style="font-size:150%; color:white; text-align:center;"),
             color = "red")}) 
  output$cnt3 <- renderValueBox({
    dat <- sqldf::sqldf(paste0("select Date, sum(Confirmed) as Confirmed,
                               sum(Deaths) as Deaths,sum(Recovered) as Recovered from all_data 
                               group by Date"))
    valueBox(tags$p(paste0("Total Recovered "),style="font-size:50%; color:white; text-align:center;"),
             tags$p(paste0(dat$Recovered[nrow(dat)]),style="font-size:150%; color:white; text-align:center;"),
             color = "teal")}) 
  
   output$By_cntry <- renderValueBox({
     lst_date <- max(as.Date(all_data$Date))  #ObservationDate,format="%m/%d/%Y"))
     valueBox(tags$p(paste0("Global Statistics As of "),style="font-size:60%; color:white; text-align:center;"),
              tags$p(paste0(lst_date),style="font-size:150%; color:white; text-align:center;"))
   })
   
   # output$By_cntry <- renderValueBox({
   #   valueBox(tags$p(paste0("Global Statistics"),style="font-size:70%; color:red; text-align:center;"),
   #            tags$p(paste0("-------"),style="text-align:center;"),color = "teal")
   # })
   
  output$C_name <- renderValueBox({
    valueBox(tags$p(paste0(input$Country),style="font-size:90%; color:white; text-align:center;"),
             tags$p(paste0(" ")),color = "black")
  })
  
  output$dfAll <- renderDataTable({
    dat <- sqldf::sqldf(paste0("select Date, sum(Confirmed) as Confirmed,
                               sum(Deaths) as Deaths,sum(Recovered) as Recovered from all_data 
                               group by Date"))
    dat <- dat[order(-dat$Confirmed), ]
    datatable(dat, options = list(pageLength=20,autoWidth=TRUE,
                                  initComplete = JS(
                                    "function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': 'Navy', 'color': 'white'});",
                                    "}")),rownames = FALSE,
              caption = htmltools::tags$caption(paste0("Global Counts"), 
                                                style="font-size:150%; font-weight:bold; color:white; text-align:center;"))  %>% 
      formatStyle('Date',backgroundColor = 'gray') %>%
      formatStyle('Confirmed',backgroundColor = 'lightblue') %>% 
      formatStyle('Recovered',backgroundColor = 'teal') %>%
      formatStyle('Deaths',backgroundColor = 'red')
  })
  
  
  output$freq <- renderDataTable({
    dat_sub <- sqldf::sqldf(paste0("select Date,sum(Confirmed) as Confirmed,sum(Deaths) as Deaths,
                                  sum(Recovered) as Recovered from all_data
                                   where CountryRegion='", input$Country, "' group by Date"))
    # dat_sub <- data.frame(dat_sub)
    # dat_sub0 <- reshape2::dcast(data=dat_sub,Date~., value.var="Confirmed", fun.aggregate = sum,na.rm=TRUE)
    # colnames(dat_sub0) <- c("Date","Count")
    # dat_sub0 <- dat_sub0[!is.na(dat_sub0$Date), ]
    dat_sub0 <- dat_sub[order(-dat_sub$Confirmed), ]
    datatable(dat_sub0, options = list(pageLength=20,
                                       initComplete = JS(
                                         "function(settings, json) {",
                                         "$(this.api().table().header()).css({'background-color':'Navy','color':'white'});",
                                         "}")),rownames = FALSE,
              caption = htmltools::tags$caption(paste0("Counts of ",input$Country), 
                                                style="font-size:150%; color:white; font-weight:bold; text-align:center;")) %>%    #,dom = 'tip'
      formatStyle('Date',backgroundColor = 'gray') %>%
      formatStyle('Confirmed',backgroundColor = 'lightblue') %>%
      formatStyle('Recovered',backgroundColor = 'teal') %>%
      formatStyle('Deaths',backgroundColor = 'red')
  })
  
  output$Tctry <- renderDataTable({
    last_date <- max(all_data[ ,"Date"])
    dat_max <- all_data %>% select(Date,CountryRegion,Confirmed,Deaths,Recovered) %>% 
      filter(Date==last_date)
    dat_tot <- sqldf::sqldf(paste0("select CountryRegion,sum(Confirmed) as Confirmed,sum(Deaths) as Deaths,
                                  sum(Recovered) as Recovered from dat_max
                                  group by CountryRegion"))
    dat_tot <- dat_tot[order(-dat_tot$Confirmed), ]
    # dat_tot <- sqldf::sqldf(paste0("select CountryRegion,sum(Confirmed) as Confirmed,sum(Deaths) as Deaths,
    #                               sum(Recovered) as Recovered from all_data
    #                                where Date='", 2020-04-11 , "' group by CountryRegion"))
    # dat_tot <- dat_tot[ ,-1]
    datatable(dat_tot, options = list(columnDefs = list(list(className = 'dt-center')),
                                      pageLength=20,
                                      # columnDefs = list(list(className = 'dt-center', targets = 1:4)),
                                       initComplete = JS(
                                         "function(settings, json) {",
                                         "$(this.api().table().header()).css({'background-color':'Navy','color':'white'});",
                                         "}")),rownames = FALSE,
              caption = htmltools::tags$caption(paste0("Cumulative Counts As Of ",last_date), 
                                                style="font-size:150%; color:white; font-weight:bold; text-align:center;")) %>%    #,dom = 'tip'
      formatStyle('CountryRegion',backgroundColor = 'grey') %>%
      formatStyle('Confirmed',backgroundColor = 'lightblue') %>%
      formatStyle('Recovered',backgroundColor = 'teal') %>%
      formatStyle('Deaths',backgroundColor = 'red')
  })
  
  
  
  output$prate <- renderValueBox({
    cases0 <- sqldf::sqldf(paste0("select Date,sum(Confirmed) as Confirmed,sum(Deaths) as Deaths,
                                  sum(Recovered) as Recovered from all_data
                                  where CountryRegion='", input$Country, "' group by Date"))
    cases0 <- cases0[complete.cases(cases0), ]
    cases1 <- cases0[nrow(cases0), ]
    dRate <- round(cases1[,'Deaths']/cases1[,'Confirmed']*100,2)
    rRate <- round(cases1[,'Recovered']/cases1[,'Confirmed']*100,2)
    valueBox(
      tags$p(paste0("Total Deaths: ",cases1$Deaths," (Rate: ",dRate,"%)"),style="font-size:60%; color:white; text-align:center;"),
      tags$p(paste0("Total Recovery: ",cases1$Recovered," (Rate: ",rRate,"%)"), style="font-size:140%; color:white; text-align:center;"),
      color = "red"
    )
  })
  
  
  output$InitialBox <- renderValueBox({
    all_data <- data.frame(all_data)
    dat_sub <- sqldf(paste0("select Date, CountryRegion, Confirmed from all_data
                            where CountryRegion='", input$Country, "' "))
    dat_sub <- data.frame(dat_sub)
    dat_sub0 <- reshape2::dcast(data=dat_sub,Date~., value.var="Confirmed", fun.aggregate = sum,na.rm=TRUE)
    colnames(dat_sub0) <- c("Date","cnt")
    dat_sub0 <- dat_sub0[!is.na(dat_sub0$Date), ]
    dat_sub0$cnt <- as.numeric(dat_sub0$cnt)
    dat_sub0 <- data.frame(dat_sub0[dat_sub0$cnt>0, ])
    dat_sub0$Date <- as.Date(dat_sub0$Date)
    dat_out <- dat_sub0[1, ]
    valueBox(
      tags$p(paste0(dat_out$cnt),style="font-size:80%; color:white; text-align:center;"),
      tags$p(paste0("case/s confirmed on ",dat_out$Date),style="font-size: 140%; text-align:center;"), icon = icon("list"),
      color = "teal"
    )
  })
  
  output$currentCases <- renderValueBox({
    all_data <- data.frame(all_data)
    dat_sub <- sqldf(paste0("select Date, CountryRegion, Confirmed from all_data
                            where CountryRegion='", input$Country, "' "))
    dat_sub <- data.frame(dat_sub)
    dat_sub0 <- reshape2::dcast(data=dat_sub,Date~., value.var="Confirmed", fun.aggregate = sum,na.rm=TRUE)
    colnames(dat_sub0) <- c("Date","cnt")
    
    dat_sub0 <- dat_sub0[!is.na(dat_sub0$Date), ]
    dat_sub0$cnt <- as.numeric(dat_sub0$cnt)
    dat_sub0 <- data.frame(dat_sub0)
    dat_sub0$Date <- as.Date(dat_sub0$Date)
    dat_out <- dat_sub0[nrow(dat_sub0), ]
    
    valueBox(
      tags$p(paste0(dat_out$cnt),style="font-size:80%; color:white; text-align:center;"),
      tags$p(paste0("Confirmed cases as of ",dat_out$Date),style="font-size: 140%; text-align:center;"), icon = icon("list"),
      color = "blue"
    )
  })
   
  

  output$pAll1 <- renderPlot({
    cases0 <- sqldf::sqldf(paste0("select Date, sum(Confirmed) as Confirmed, 
                                  sum(Deaths) as Deaths,sum(Recovered) as Recovered from all_data
                                  where CountryRegion='", input$Country, "' group by Date"))
    dat_all <- reshape2::melt(cases0, id.vars="Date")
    
    dat_all <- dat_all[dat_all$variable==input$svar, ]
    pCountry1 <- ggplot(dat_all, aes(x=Date,y=value, group=variable, colour=variable))+geom_line(size=1) +
      xlab("Date") + scale_x_date(date_breaks = "4 days")+  scale_colour_hue(l=40, c=450) +
      theme(axis.text.x = element_text(size=14, angle=90,color="white"),
            axis.text.y = element_text(size=14, color="white"),
            axis.title = element_blank(),
            legend.position = c(0.15,0.85),
            plot.title = element_text(hjust = 0.5,size=20,face="bold",color="blue"),
            plot.background = element_rect(fill = "azure4",color="black"),
            panel.background = element_rect(fill="black"),
            panel.grid = element_line(color="darkslategrey"))+
      ggtitle(paste0("Cumulative Counts - ",  input$Country))
    
    
    
    cases01 <- sqldf::sqldf(paste0("select Date, log(sum(Confirmed)) as Confirmed
                                 from all_data
                                  where CountryRegion='", input$Country, "' group by Date"))
    dat_all01 <- reshape2::melt(cases01, id.vars="Date")
    
    # dat_all01 <- dat_all01[dat_all01$variable==input$svar, ]
    pCountry2 <- ggplot(dat_all01, aes(x=Date,y=value, group=variable, colour=variable))+geom_line(size=1) +
      xlab("Date") + scale_x_date(date_breaks = "4 days")+  scale_colour_hue(l=40, c=450) +
      theme(axis.text.x = element_text(size=14, angle=90,color="white"),
            axis.text.y = element_text(size=14, color="white"),
            axis.title = element_blank(),
            legend.position = c(0.15,0.85),
            plot.title = element_text(hjust = 0.5,size=20,face="bold",color="blue"),
            plot.background = element_rect(fill = "azure4",color="black"),
            panel.background = element_rect(fill="black"),
            panel.grid = element_line(color="darkslategrey"))+
      ggtitle(paste0("Cumulative Counts (log scale) - ",  input$Country))
    
    pOut <- gridExtra::grid.arrange(pCountry1,pCountry2,ncol=2) 
    return(pOut)
    
  })
  
  output$plog <- renderPlot({
    
    dat1 <- sqldf::sqldf(paste0("select Date, sum(Confirmed) as Confirmed,
                               sum(Deaths) as Deaths,sum(Recovered) as Recovered from all_data 
                               group by Date"))
    dat_all1 <- reshape2::melt(dat1, id.vars="Date")
    
    plCountry1 <- ggplot(dat_all1, aes(x=Date,y=value, group=variable, colour=variable))+geom_line(size=1) +
      xlab("Date") + scale_x_date(date_breaks = "4 days")+  scale_colour_hue(l =40, c=450) +
      ggtitle("Global Cumulative Counts (Confirmed, Deaths, Recovered)") +
      theme(axis.text.x = element_text(size=14, angle=90,color="white"), 
            axis.text.y = element_text(size=14,color="white"),
            axis.title = element_blank(),
            legend.position = c(0.15,0.85),
            plot.title = element_text(hjust = 0.5,size=20,face="bold",color="blue"),
            plot.background = element_rect(fill = "azure4",color="black"),
            panel.background = element_rect(fill="black"),
            panel.grid = element_line(color="darkslategrey"))
   
    
    dat2 <- sqldf::sqldf(paste0("select Date, log(sum(Confirmed)) as Confirmed,
                               log(sum(Deaths)) as Deaths,log(sum(Recovered)) as Recovered from all_data 
                               group by Date"))
    dat_all2 <- reshape2::melt(dat2, id.vars="Date")
    plCountry2 <- ggplot(dat_all2, aes(x=Date,y=value, group=variable, colour=variable))+geom_line(size=1) +
      xlab("Date") + scale_x_date(date_breaks = "4 days")+  scale_colour_hue(l =40, c=450) +
      ggtitle("Global Cumulative Counts (Confirmed, Deaths, Recovered)") +
      theme(axis.text.x = element_text(size=14, angle=90,color="white"), 
            axis.text.y = element_text(size=14,color="white"),
            axis.title = element_blank(),
            legend.position = c(0.15,0.85),
            plot.title = element_text(hjust = 0.5,size=20,face="bold",color="blue"),
            plot.background = element_rect(fill = "azure4",color="black"),
            panel.background = element_rect(fill="black"),
            panel.grid = element_line(color="darkslategrey"))
    plCountry <- gridExtra::grid.arrange(plCountry1,plCountry2,ncol=2) 
    return(plCountry)
  })
 
  
}

shinyApp(ui, server)