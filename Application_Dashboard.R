## app.R ##
##########################################################################################################################
#Dashboard Project for DATA Science at Excellus Blue Cross BlueShield
#Influenza statistics and HCP (Health Care Personel) vaccination Rates and Comparisons
#Author(s): Chris VanKerkhove
##########################################################################################################################

library(shiny)
library(shinydashboard)
library(choroplethr)
library(choroplethrMaps)
library(leaflet)
library(geojsonio)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(DT)
library(broom)
library(readxl)

#DATA
##########################################################################################################################
#DataSet with HCP vaccine rate (Dataset 1)#####################

Influenza_Vaccination_Rates <- read.csv("./Influenza_Vaccination_Rates_for_Health_Care_Personnel.csv")

#Removing all "NA" in data and non-useful data 
data <- filter(Influenza_Vaccination_Rates, Influenza_Vaccination_Rates$Total > 49)
data2 <- filter(data, !is.na(data$Percent.Unvaccinated), !is.na(data$Percent.Vaccinated))

#Group by HCP type, facility type, and County
HCP_Fac_county <- data2 %>%
  group_by(County = data2$County, HCP = data2$Healthcare.Personnel..HCP..Type, Facility = data2$Facility.Type) %>%
  summarise(HCP_Percent_Vaccinated = 100 * mean(Vaccinated) / mean(Total),
            HCP_Percent_Unvaccinated = 100 * mean(Unvaccinated) / mean(Total)
  ) 

#Group by HCP type and County
HCP_county <- data2 %>%
  group_by(County = data2$County, HCP = data2$Healthcare.Personnel..HCP..Type) %>%
  summarise(HCP_Percent_Vaccinated = 100 * mean(Vaccinated) / mean(Total),
            HCP_Percent_Unvaccinated = 100 * mean(Unvaccinated) / mean(Total)
            ) 

#Group by County (Percent is target variable)
county_data <- data2 %>% 
  group_by(County = data2$County) %>% 
  summarise(Percent_Vaccinated = 100 * mean(Vaccinated) / mean(Total), 
            Percent_Unvaccinated = 100 * mean(Unvaccinated) / mean(Total)
  )

#Group by County and Year (Percent is target variable)
county_pct_year <- data2 %>% 
  group_by(County = data2$County, Year = data2$Year) %>% 
  summarise(Percent_vaccinated = 100 * mean(Vaccinated) / mean(Total), 
            Percent_Unvaccinated = 100 * mean(Unvaccinated) / mean(Total)
  )

#Group by HCP Type (Percent is target variable)
HCP <- data2 %>% 
  group_by(HCP_Type = data2$Healthcare.Personnel..HCP..Type, Year = data2$Year) %>% 
  summarise(Percent_vaccinated = mean(Vaccinated) / mean(Total), 
            Percent_Unvaccinated = mean(Unvaccinated) / mean(Total)
  )
HCP <- filter(HCP, Year != "2012-2013")

#Group by Facility Type (Percent is target variable)
Facility <- data2 %>% group_by(Facility_Type = data2$Facility.Type, Year = data2$Year) %>%
  summarise(
    Percent_vaccinated = mean(Vaccinated) / mean(Total), Percent_Unvaccinated = mean(Unvaccinated) / mean(Total)
  )
Facility <- filter(Facility, Year != "2012-2013")
#################################################################


#DataSet with flu county count (Dataset 2)#######################

Flu_by_county <- read.csv("./Influenza_Confirmed_Cases.csv")
Flu_by_county$County <- as.character(Flu_by_county$County)
Flu_by_county <- mutate_at(Flu_by_county, vars(County), str_to_title)
#Group By County measuring total cases
county_tot <- Flu_by_county %>% 
  group_by(County = Flu_by_county$County) %>% 
  summarise(
    Total_Count = sum(Count)
  )
#Group By County and Year
county_tot_year <- Flu_by_county %>% 
  group_by(County = Flu_by_county$County, Year = Flu_by_county$Season) %>%
  summarise(
    Total_Count = sum(Count)
  )

#Group By County and Year and type of Flu measuring total cases
county_year_type <- Flu_by_county %>% 
  group_by(County = Flu_by_county$County, Year = Flu_by_county$Season, Flue_Type = Flu_by_county$Disease) %>%
  summarise(
    Total_Count = sum(Count)
  )
##################################################################

#Joined table
Joined_Table <- inner_join(county_year_type, county_pct_year, by = c("Year" = "Year", "County" = "County")) %>%
  as.data.frame(Joined_Table)


#Creat counties SP object
counties <- geojsonio::geojson_read("./US_counties.json", what = "sp")
#Isolate for NYS counties
NYcounties <- counties[counties$STATE == 36, ]
#Data frame with county poplations
pop <- read_excel("./NYcountiespop.xlsx")
pop$Population <- as.integer(pop$Population)
#Joining the data for the county totals and percentages to the SP object
NYcounties@data <- NYcounties@data %>%
  left_join(., pop, c("NAME" = "County")) 


#SHINY USER INTERFACE SPECIFICS
##########################################################################################################################


#Specifics for the Header
header <- dashboardHeader(title = "NY State Inlfuenza",
                          titleWidth = 320)

#Specifics for the dashboard
sidebar <- dashboardSidebar(
  width = "300px",
  HTML("<br><center>"),
  img(src = "White-Excellus-Logo.png", width = "300px", align = "middle"),
  HTML("</center><br>"),
  HTML("<br>"),
  sidebarMenu(
    #sidebarMenuOutput("menu"),
    id = "sidebarMenu",
    menuItem("About",
             tabName = "about",
             icon = icon("info-circle"),
             selected = TRUE,
             menuSubItem("Abstract",
                         tabName = "abstract",
                         icon = icon("file-alt"),
                         selected = TRUE),
             menuSubItem("Data",
                         tabName = "datatables",
                         icon = icon("table"))
    ),
    menuItem("Influenza Correlation", 
             tabName = "trends", 
             icon = icon("chart-line"), 
             selected = FALSE,
             menuSubItem("ScatterPlot", 
                         tabName = "trends", 
                         icon = icon("chart-line")
                         ),
             menuSubItem("Data",
                         tabName = "dta",
                         icon = icon("table"))
    ),
    menuItem("Targeting Cause Variables",
             tabName = "graphs",
             icon = icon("chart-area"),
             selected = FALSE,
             menuSubItem("Vaccination Rate Trends",
                         tabName = "trends2",
                         icon = icon("chart-bar")
                         ),
             menuSubItem("HCP Type Distributions",
                         tabName = "HCPdist",
                         icon = icon("address-card")),
             menuSubItem("Facilty Type Distribution",
                         tabName = "facilitydist",
                         icon = icon("hospital-alt"))
    ),
    menuItem("New York State Counties Map", 
             tabName = "map", 
             icon = icon("map-marked"), 
             selected = FALSE,
             menuSubItem("HCP Percent by County", 
                         tabName = "HCPmap", 
                         icon = icon("map"), 
                         selected = FALSE
             ),
             menuSubItem("Map Comparisons", 
                         tabName = "maps", 
                         icon = icon("map-signs"), 
                         selected = FALSE
             )
    )
  
  ),
  conditionalPanel(
    condition = "input.sidebarMenu == 'maps'",
    class = "shiny-input-container",
    selectInput("pctInput", 
                "Rate Measure:", 
                c("Vaccinated", "Unvaccinated")
                ),
    selectInput("yearinput", 
                "Season:", 
                c("2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "ALL"), 
                selected = "2012-2013"
    )
  ),
  conditionalPanel(
    condition = "input.sidebarMenu == 'HCPmap'",
    class = "shiny-input-container",
    selectInput("HCPmapinput",
                "HCP Type:",
                c("Total Healthcare Personnel", "Contractors", "Employees", "Licensed Independent Practitioners",
                  "Students, Trainees and Volunteers"),
                selected = "Total Healthcare Personnel")
  ),
  conditionalPanel(
    condition = "input.sidebarMenu == 'HCPmap'",
    class = "shiny-input-container",
    selectInput("rateHCPmap",
                "Rate Measure:",
                c("Vaccinated", "Unvaccinated"))
  ),
  conditionalPanel(
    condition = "input.sidebarMenu == 'HCPmap'",
    class = "shiny-input-container",
    selectInput("facilitymapinput1",
                "Facility Type:",
                c("ALL", "Adult Day Health Care Program", "Diagnostic and Treatment Center", "Home Health Agency",
                  "Hospice", "Hospital", "Licensed Home Care Service Agency", "Nursing Home"),
                selected = "ALL")
  ),
  
  conditionalPanel(
    condition = "input.sidebarMenu == 'trends'",
    class = "shiny-input-container",
    selectInput("grphinput",
                "Rate Measure:",
                c("Vaccinated", "Unvaccinated")),
    selectInput("yrinput", 
                "Season:", 
                c("2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "ALL"), 
                selected = "2017-2018"
                ),
    checkboxGroupInput("flutype", 
                       label = "Influenza Type:", 
                       choices = list("INFLUENZA_A", "INFLUENZA_B", "INFLUENZA_UNSPECIFIED"), 
                       selected = list("INFLUENZA_A", "INFLUENZA_B")
                       ),
    sliderInput("rangeinput", 
                "Percent Vaccinated Range:", 
                min  = 20, 
                max = 100, 
                value = c(20, 100)
                ),
    sliderInput("rangeinput2", 
                "Percent Unvaccinated Range:", 
                min = 0, 
                max = 50, 
                value = c(0, 50)
                )
  ),
  conditionalPanel(
    condition = "input.sidebarMenu == 'trends2'",
    class = "shiny-input-container",
    selectInput("rtinput",
                "Rate Measure:",
                c("Vaccinated", "Unvaccinated")
                ),
    checkboxGroupInput("HCPtype",
                       label = "Health Care Professional Type:",
                       choices = list("Contractors", "Employees", "Licensed Independent Practitioners", 
                                      "Students, Trainees and Volunteers", "Total Healthcare Personnel"),
                       selected = list("Contractors", "Employees", "Licensed Independent Practitioners", 
                                       "Students, Trainees and Volunteers", "Total Healthcare Personnel")
                       ),
    checkboxGroupInput("facilitytype",
                       label = "Facility Type:",
                       choices = list("Adult Day Health Care Program", "Diagnostic and Treatment Center", 
                                      "Home Health Agency", "Hospice", "Hospital",
                                      "Licensed Home Care Service Agency", "Nursing Home"),
                       selected = list("Adult Day Health Care Program", "Diagnostic and Treatment Center", 
                                       "Home Health Agency", "Hospice", "Hospital",
                                       "Licensed Home Care Service Agency", "Nursing Home"))
  ),
  conditionalPanel(condition = "input.sidebarMenu == 'HCPdist'",
                   class = "shiny-input-container",
                   selectInput("rate2input",
                               "Rate Measure:",
                               c("Vaccinated", "Unvaccinated")),
                   selectInput("flterinput",
                                "Target Variable:",
                                c("None", "Adult Day Health Care Program", "Diagnostic and Treatment Center", 
                                  "Home Health Agency", "Hospice", "Hospital", 
                                  "Licensed Home Care Service Agency", "Nursing Home")),
                   checkboxGroupInput("ttestfactors",
                                      label = "T-Test Comparison Variables (Select Two):",
                                      choices = list("Contractors", "Employees", "Licensed Independent Practitioners",
                                      "Students, Trainees and Volunteers", "Total Healthcare Personnel"),
                                      selected = c("Contractors", "Employees"))
  ),
  conditionalPanel(condition = "input.sidebarMenu == 'facilitydist'",
                  class = "shiny-input-container",
                  selectInput("rate3input",
                              "Rate Measure:",
                              c("Vaccinated", "Unvaccinated")),
                  selectInput("flter2input",
                              "Target Variable:",
                              c("None", "Contractors", "Employees", "Licensed Independent Practitioners",
                                "Students, Trainees and Volunteers", "Total Healthcare Personnel")),
                  checkboxGroupInput("ttestfactors2",
                              label = "T-Test Comparison Variables (Select Two):",
                              choices = list("Adult Day Health Care Program", "Diagnostic and Treatment Center",
                                             "Home Health Agency", "Hospice", "Hospital",
                                             "Licensed Home Care Service Agency", "Nursing Home"),
                              selected = c("Adult Day Health Care Program", "Diagnostic and Treatment Center"))
                  )
)


#Specifics for the body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "abstract",
            box(title = "Abstract",
                solidHeader = TRUE,
                status = "success",
                htmlOutput("rmkd"),
                width = 200)
            ),
    tabItem(tabName = "datatables",
            tabBox(title = "Data Tables",
                   id = "datatbl1",
                   tabPanel(title = "Influenza Vaccination Rates by Health Care Personnel", 
                            h2("Influenza Vaccination Rates by Health Care Personnel starting in 2012-2013"),
                            DT::dataTableOutput("exceltable1")),
                   tabPanel(title = "Influenza Confirmed Cases", 
                            h2("Influenza Confirmed Cases starting in 2009-2010"),
                            DT::dataTableOutput("exceltable2")),
                   height = ,
                   width = 13)
            ),
    #Second Tab Content (NY Maps)
    tabItem(tabName = "maps",
            h1("County Map Comparisons"),
            column(6,
                   box(title = "Rate of Health Care Personnel Vaccinations", 
                       solidHeader = TRUE, 
                       status = "primary", 
                       leafletOutput("map1", 
                                     height = 700), 
                       width = 100)
                  ),
            column(6,
                   box(title = "Total Reported Cases of Influenza by County", 
                       solidHeader = TRUE, 
                       status = "warning", 
                       leafletOutput("map2", 
                                     height = 700), 
                       width = 100)
                  )
            ), 
    tabItem(tabName = "HCPmap",
            fluidPage(
              box(title = "Rate of Health Care Personnel Vaccinations (By HCP Type)",
                  solidHeader = TRUE,
                  status = "success",
                  leafletOutput("map3",
                                height = 820),
                  width = 200)
            )
          ),
    
    tabItem(tabName = "trends",
                   box(title = "Vaccination Rates of HCP vs. Total Reported Influenza Cases", 
                       solidHeader = TRUE, 
                       status = "success", 
                       plotlyOutput("plot2", 
                                   height = 560), 
                       width = 400),
                   tabBox(title = "Test for Correlation",
                       id = "tabbox1",
                       tabPanel(title = "Pearson Test", 
                                value = "Tab1",
                                tableOutput("cortest1")),
                       tabPanel(title = "Spearman Test", 
                                value = "Tab2",
                                tableOutput("cortest2")),
                       height = 250,
                       width = 10)
            ),
    tabItem(tabName = "dta",
            h2("Data Table"),
            DT::dataTableOutput("mytable")
    ),
    
    tabItem(tabName = "trends2",
            column(6,
                   box(title = "Trends for HCP Type over Seasons",
                       solidHeader = TRUE,
                       status = "success",
                       plotlyOutput("plot3",
                                    height = 700),
                       width = 100)
                   ),
            column(6, 
                   box(title = "Trends for Facility Type over Seasons",
                       solidHeader = TRUE,
                       status = "success",
                       plotlyOutput("plot4",
                                    height = 700),
                       width = 100)
                   )
            ),
    tabItem(tabName = "HCPdist", 
            column(6,
                   box(title = "Boxplot Spread of HCP Type",
                       solidHeader = TRUE,
                       status = "success",
                       plotlyOutput("plot5",
                                    height = 610),
                       width = 100)
                   ),
            column(6,
                   box(title = "Violinplot Spread of HCP Type",
                       solidHeader = TRUE,
                       status = "success",
                       plotlyOutput("plot6",
                                    height = 610),
                       width = 100)
                   ),
            column(8,
                   box(title = "T-test for difference",
                       solidHeader = TRUE,
                       status = "success",
                       tableOutput("ttest1"),
                       width = 100
                       )
                   ),
            
            column(4, 
                   box(title = "Description",
                       solidHeader = TRUE,
                       status = "success",
                       textOutput("text1"),
                       textOutput("text11"),
                       textOutput("text111"),
                       width = 100)
                  )
            ),
    tabItem(tabName = "facilitydist",
            column(6,
                   box(title = "Boxplot Spread of HCP Type",
                       solidHeader = TRUE,
                       status = "success",
                       plotlyOutput("plot7",
                                    height = 610),
                       width = 100)
            ),
            column(6,
                   box(title = "Violinplot Spread of HCP Type",
                       solidHeader = TRUE,
                       status = "success",
                       plotlyOutput("plot8",
                                    height = 610),
                       width = 100)
            ),
            column(8,
                   box(title = "T-test for difference",
                       solidHeader = TRUE,
                       status = "success",
                       tableOutput("ttest2"),
                       width = 100
                   )
            ),
            column(4, box(title = "Description",
                          solidHeader = TRUE,
                          status = "success",
                          textOutput("text2"),
                          textOutput("text22"),
                          textOutput("text222"),
                           
                           width = 100)
            )
          )
   )
)



ui <- dashboardPage(header, sidebar, body, skin = "green")


#SHINY SERVER SPECIFICS
##########################################################################################################################

server <- function(input, output) {
#############################################################
#Outputs for Information Tab
#R-Markdown Abstract#############################
  
  output$rmkd <- renderUI({
    includeHTML("./Project_Abstract.html"
    )
  })
  
  output$exceltable1 <- DT::renderDataTable({
    data2
  })
  
  output$exceltable2 <- DT::renderDataTable({
    Flu_by_county
  })
#############################################################
#Outputs for TAB 1
#Trend Plot Vaccinated###########################
  
  output$plot2 <- renderPlotly({
    if (input$grphinput == "Vaccinated") {
      if (length(input$flutype) == 3) {
        if (input$yrinput == "ALL") {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1])
          ggplotly(   
          ggplot(t_vac, 
                 aes(t_vac$Percent_vaccinated, 
                     t_vac$Total_Count, 
                     color = t_vac$Flue_Type)) +
            geom_point() +
            geom_smooth(se = FALSE, method = lm) +
            xlab("Percent of HCP Vaccinated") +
            ylab("Total Cases of Influenza") +
            theme(legend.title = element_blank())
        )
        }
        else {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1], 
                          Joined_Table$Year == input$yrinput)
          ggplotly(
          ggplot(t_vac, 
                 aes(t_vac$Percent_vaccinated, 
                     t_vac$Total_Count, 
                     color = t_vac$Flue_Type)) +
            geom_point() +
            geom_smooth(se = FALSE, method = lm) +
            xlab("Percent of HCP Vaccinated") +
            ylab("Total Cases of Influenza") +
            theme(legend.title = element_blank())
          )
        }
      }
      else if (length(input$flutype) == 2) {
        if (input$yrinput == "ALL") {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1],
                          Joined_Table$Flue_Type %in% input$flutype)
          ggplotly(
          ggplot(t_vac, 
                 aes(t_vac$Percent_vaccinated, 
                     t_vac$Total_Count, 
                     color = t_vac$Flue_Type)) +
            geom_point() +
            geom_smooth(se = FALSE, method = lm) +
            xlab("Percent of HCP Vaccinated") +
            ylab("Total Cases of Influenza") +
            theme(legend.title = element_blank())
          )
        }
        else {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1], 
                          Joined_Table$Year == input$yrinput,
                          Joined_Table$Flue_Type %in% input$flutype)
          ggplotly(
          ggplot(t_vac, 
                 aes(t_vac$Percent_vaccinated, 
                     t_vac$Total_Count, 
                     color = t_vac$Flue_Type)) +
            geom_point() +
            geom_smooth(se = FALSE, method = lm) +
            xlab("Percent of HCP Vaccinated") +
            ylab("Total Cases of Influenza") +
            theme(legend.title = element_blank())
          )
        }
      }
      else if (length(input$flutype) == 1) {
        if (input$yrinput == "ALL") {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1],
                          Joined_Table$Flue_Type == input$flutype)
          ggplotly(
          ggplot(t_vac, 
                 aes(t_vac$Percent_vaccinated, 
                     t_vac$Total_Count, 
                     color = t_vac$Flue_Type)) +
            geom_point() +
            geom_smooth(se = FALSE, method = lm) +
            xlab("Percent of HCP Vaccinated") +
            ylab("Total Cases of Influenza") +
            theme(legend.title = element_blank())
          )
        }
        else {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1], 
                          Joined_Table$Year == input$yrinput,
                          Joined_Table$Flue_Type == input$flutype)
          ggplotly(
          ggplot(t_vac, 
                 aes(t_vac$Percent_vaccinated, 
                     t_vac$Total_Count, 
                     color = t_vac$Flue_Type)) +
            geom_point() +
            geom_smooth(se = FALSE, method = lm) +
            xlab("Percent of HCP Vaccinated") +
            ylab("Total Cases of Influenza") +
            theme(legend.title = element_blank())
          )
        }
      }
    } 
  else {
    if (length(input$flutype) == 3) {
      if (input$yrinput == "ALL") {
      t_unvac <- filter(Joined_Table,
                        Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                        Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1])
    ggplotly(
    ggplot(t_unvac, 
           aes(t_unvac$Percent_Unvaccinated, 
               t_unvac$Total_Count, 
               color = t_unvac$Flue_Type)) +
      geom_point() +
      geom_smooth(se = FALSE, method = lm) +
      xlab("Percent of HCP Unvaccinated") +
      ylab("Total cases of Influenza") +
      theme(legend.title = element_blank())
    )
      }
      else {
        t_unvac <- filter(Joined_Table,
                          Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                          Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                          Joined_Table$Year == input$yrinput)
        ggplotly(
        ggplot(t_unvac, 
               aes(t_unvac$Percent_Unvaccinated, 
                   t_unvac$Total_Count, 
                   color = t_unvac$Flue_Type)) +
          geom_point() +
          geom_smooth(se = FALSE, method = lm) +
          xlab("Percent of HCP Unvaccinated") +
          ylab("Total cases of Influenza") +
          theme(legend.title = element_blank())
        )
      }
    }
    else if (length(input$flutype) == 2) {
      if (input$yrinput == "ALL") {
        t_unvac <- filter(Joined_Table,
                          Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                          Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                          Joined_Table$Flue_Type %in% input$flutype)
        ggplotly(
        ggplot(t_unvac, 
               aes(t_unvac$Percent_Unvaccinated, 
                   t_unvac$Total_Count, 
                   color = t_unvac$Flue_Type)) +
          geom_point() +
          geom_smooth(se = FALSE, method = lm) +
          xlab("Percent of HCP Unvaccinated") +
          ylab("Total cases of Influenza") +
          theme(legend.title = element_blank())
        )
      }
      else {
        t_unvac <- filter(Joined_Table,
                          Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                          Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                          Joined_Table$Year == input$yrinput,
                          Joined_Table$Flue_Type %in% input$flutype)
        ggplotly(
        ggplot(t_unvac, 
               aes(t_unvac$Percent_Unvaccinated, 
                   t_unvac$Total_Count, 
                   color = t_unvac$Flue_Type)) +
          geom_point() +
          geom_smooth(se = FALSE, method = lm) +
          xlab("Percent of HCP Unvaccinated") +
          ylab("Total cases of Influenza") +
          theme(legend.title = element_blank())
        )
      }
    }
    else if (length(input$flutype) == 1) {
      if (input$yrinput == "ALL") {
        t_unvac <- filter(Joined_Table,
                          Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                          Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                          Joined_Table$Flue_Type == input$flutype)
        ggplotly(
        ggplot(t_unvac, 
               aes(t_unvac$Percent_Unvaccinated, 
                   t_unvac$Total_Count, 
                   color = t_unvac$Flue_Type)) +
          geom_point() +
          geom_smooth(se = FALSE, method = lm) +
          xlab("Percent of HCP Unvaccinated") +
          ylab("Total cases of Influenza") +
          theme(legend.title = element_blank())
        )
      }
      else {
        t_unvac <- filter(Joined_Table,
                          Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                          Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                          Joined_Table$Year == input$yrinput,
                          Joined_Table$Flue_Type == input$flutype)
        ggplotly(
        ggplot(t_unvac, 
               aes(t_unvac$Percent_Unvaccinated, 
                   t_unvac$Total_Count, 
                   color = t_unvac$Flue_Type)) +
          geom_point() +
          geom_smooth(se = FALSE, method = lm) +
          xlab("Percent of HCP Unvaccinated") +
          ylab("Total cases of Influenza") +
          theme(legend.title = element_blank())
        )
      }
    }
  }  
})

###################################
#Output for Tab 1 BOX 2
###################################
  output$cortest1 <- renderTable({
    if (input$grphinput == "Vaccinated") {
      if (length(input$flutype) == 3) {
        if (input$yrinput == "ALL") {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1])
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "pearson") %>%
            tidy(.)

        }
        else {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1], 
                          Joined_Table$Year == input$yrinput)
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "pearson") %>%
            tidy(.)
        }
      }
      else if (length(input$flutype) == 2) {
        if (input$yrinput == "ALL") {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1],
                          Joined_Table$Flue_Type %in% input$flutype)
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "pearson") %>%
            tidy(.)
        }
        else {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1], 
                          Joined_Table$Year == input$yrinput,
                          Joined_Table$Flue_Type %in% input$flutype)
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "pearson") %>%
            tidy(.)
        }
      }
      else if (length(input$flutype) == 1) {
        if (input$yrinput == "ALL") {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1],
                          Joined_Table$Flue_Type == input$flutype)
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "pearson") %>%
            tidy(.)
        }
        else {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1], 
                          Joined_Table$Year == input$yrinput,
                          Joined_Table$Flue_Type == input$flutype)
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "pearson") %>%
            tidy(.)
        }
      }
    } 
    else {
      if (length(input$flutype) == 3) {
        if (input$yrinput == "ALL") {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1])
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "pearson") %>%
            tidy(.)
          
        }
        else {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                            Joined_Table$Year == input$yrinput)
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "pearson") %>%
            tidy(.)
          
        }
      }
      else if (length(input$flutype) == 2) {
        if (input$yrinput == "ALL") {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                            Joined_Table$Flue_Type %in% input$flutype)
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "pearson") %>%
            tidy(.)
          
        }
        else {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                            Joined_Table$Year == input$yrinput,
                            Joined_Table$Flue_Type %in% input$flutype)
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "pearson") %>%
            tidy(.)
          
        }
      }
      else if (length(input$flutype) == 1) {
        if (input$yrinput == "ALL") {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                            Joined_Table$Flue_Type == input$flutype)
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "pearson") %>%
            tidy(.)

        }
        else {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                            Joined_Table$Year == input$yrinput,
                            Joined_Table$Flue_Type == input$flutype)
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "pearson") %>%
            tidy(.)
        }
      }
    }
  })
  
  output$cortest2 <- renderTable({
    if (input$grphinput == "Vaccinated") {
      if (length(input$flutype) == 3) {
        if (input$yrinput == "ALL") {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1])
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "spearman") %>%
            tidy(.)
          
        }
        else {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1], 
                          Joined_Table$Year == input$yrinput)
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "spearman") %>%
            tidy(.)
        }
      }
      else if (length(input$flutype) == 2) {
        if (input$yrinput == "ALL") {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1],
                          Joined_Table$Flue_Type %in% input$flutype)
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "spearman") %>%
            tidy(.)
        }
        else {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1], 
                          Joined_Table$Year == input$yrinput,
                          Joined_Table$Flue_Type %in% input$flutype)
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "spearman") %>%
            tidy(.)
        }
      }
      else if (length(input$flutype) == 1) {
        if (input$yrinput == "ALL") {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1],
                          Joined_Table$Flue_Type == input$flutype)
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "spearman") %>%
            tidy(.)
        }
        else {
          t_vac <- filter(Joined_Table, 
                          Joined_Table$Percent_vaccinated <= input$rangeinput[2],
                          Joined_Table$Percent_vaccinated >= input$rangeinput[1], 
                          Joined_Table$Year == input$yrinput,
                          Joined_Table$Flue_Type == input$flutype)
          cor.test(t_vac$Percent_vaccinated, t_vac$Total_Count, method = "spearman") %>%
            tidy(.)
        }
      }
    } 
    else {
      if (length(input$flutype) == 3) {
        if (input$yrinput == "ALL") {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1])
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "spearman") %>%
            tidy(.)
          
        }
        else {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                            Joined_Table$Year == input$yrinput)
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "spearman") %>%
            tidy(.)
          
        }
      }
      else if (length(input$flutype) == 2) {
        if (input$yrinput == "ALL") {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                            Joined_Table$Flue_Type %in% input$flutype)
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "spearman") %>%
            tidy(.)
          
        }
        else {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                            Joined_Table$Year == input$yrinput,
                            Joined_Table$Flue_Type %in% input$flutype)
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "spearman") %>%
            tidy(.)
          
        }
      }
      else if (length(input$flutype) == 1) {
        if (input$yrinput == "ALL") {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                            Joined_Table$Flue_Type == input$flutype)
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "spearman") %>%
            tidy(.)
          
        }
        else {
          t_unvac <- filter(Joined_Table,
                            Joined_Table$Percent_Unvaccinated <= input$rangeinput2[2],
                            Joined_Table$Percent_Unvaccinated >= input$rangeinput2[1],
                            Joined_Table$Year == input$yrinput,
                            Joined_Table$Flue_Type == input$flutype)
          cor.test(t_unvac$Percent_Unvaccinated, t_unvac$Total_Count, method = "spearman") %>%
            tidy(.)
        }
      }
    }
  })

  
###################################
#Output for Tab 1.2
###################################
  output$mytable <- DT::renderDataTable({
    Joined_Table
  })

  

#############################################################
#Outputs for TAB 2
#PLOTLYOUTPUT FOR BOX 1
#################################
  output$plot3 <- renderPlotly({
    
    if (input$rtinput == "Vaccinated") {
      HCP2 <- filter(HCP, 
                     HCP_Type %in% input$HCPtype)
      
      ggplotly(ggplot(HCP2, 
                      aes(fill = HCP2$HCP_Type, 
                          y = HCP2$Percent_vaccinated, 
                          x = HCP2$Year)) +
                 geom_bar(position = "dodge", 
                          stat = "identity") +
                 xlab("Season") +
                 ylab("Percent Vaccinated") +
                 theme(legend.title = element_blank())
      )
    }
    else {
      HCP2 <- filter(HCP, HCP_Type %in% input$HCPtype)
      
      ggplotly(ggplot(HCP2, 
                      aes(fill = HCP2$HCP_Type, 
                          y = HCP2$Percent_Unvaccinated, 
                          x = HCP2$Year)) +
                 geom_bar(position = "dodge", 
                          stat = "identity") +
                 xlab("Season") +
                 ylab("Percent Unvaccinated") +
                 theme(legend.title = element_blank())
      )
    }
  })
#################################
#PLOTLYOUTPUT FOR BOX 2
#################################
  output$plot4 <- renderPlotly({
    
    if (input$rtinput == "Vaccinated") {
      Facility2 <- filter(Facility, Facility_Type %in% input$facilitytype)
      
      ggplotly(ggplot(Facility2, 
                      aes(fill = Facility2$Facility_Type, 
                          y = Facility2$Percent_vaccinated, 
                          x = Facility2$Year)) +
                 geom_bar(position = "dodge", 
                          stat = "identity") +
                 xlab("Season") +
                 ylab("Percent Vaccinated") +
                 theme(legend.title = element_blank())
      )
    }
    else {
      Facility2 <- filter(Facility, Facility_Type %in% input$facilitytype)
      
      ggplotly(ggplot(Facility2, 
                      aes(fill = Facility2$Facility_Type, 
                          y = Facility2$Percent_Unvaccinated, 
                          x = Facility2$Year)) +
                 geom_bar(position = "dodge", 
                          stat = "identity") +
                 xlab("Season") +
                 ylab("Percent Unvaccinated") +
                 theme(legend.title = element_blank())
      )
    }
  })
  
#############################################################
#Output for TAB 2.2
#PLOTLY Distribution Plot for HCP (boxplot)
################################################
  output$text1 <- renderText({
    "DESCRIPTION:"
    })
  
  output$text11 <- renderText({
  "SELECT BAR: The select-bar allows selection of certain target variables. In this instance,
  there is the option to isolate different Facility types in order to see the distribution of 
     information (Percent of HCP Vaccinated or Unvaccinated) in just that specified facility."
    })
  
  output$text111 <- renderText({
  "T-TEST: The check boxes allow for selection of two specific types of HCP which will 
  result in the return of a 2 group T-Test (comparing means of Percent Vaccinated or Unvaccinated). estimate "
    })
  
  output$plot5 <- renderPlotly({
    if (input$rate2input == "Vaccinated") {
      if (input$flterinput == "None") {
        ggplotly(ggplot(data2, 
                        aes(Healthcare.Personnel..HCP..Type, Percent.Vaccinated, 
                            color = Healthcare.Personnel..HCP..Type)) + 
                   geom_boxplot() +
                   xlab("HCP Type") +
                   ylab("Percent Vaccinated") +
                   theme(legend.text = element_blank(), 
                         axis.text.x = element_blank())
        )
      }
      else {
        data3 <- filter(data2, Facility.Type == input$flterinput)
        ggplotly(ggplot(data3, 
                        aes(Healthcare.Personnel..HCP..Type, Percent.Vaccinated, 
                            color = Healthcare.Personnel..HCP..Type)) + 
                   geom_boxplot() +
                   xlab("HCP Type") +
                   ylab("Percent Vaccinated") +
                   theme(legend.text = element_blank(),
                         axis.text.x = element_blank())
        )
      }
    }
    else {
      if (input$flterinput == "None") {
        ggplotly(ggplot(data2, 
                        aes(Healthcare.Personnel..HCP..Type, Percent.Unvaccinated, 
                            color = Healthcare.Personnel..HCP..Type)) + 
                   geom_boxplot() +
                   xlab("HCP Type") +
                   ylab("Percent Unvaccinated") +
                   theme(legend.text = element_blank(), 
                         axis.text.x = element_blank())
        )
      }
      else {
        data3 <- filter(data2, Facility.Type == input$flterinput)
        ggplotly(ggplot(data3, 
                        aes(Healthcare.Personnel..HCP..Type, Percent.Unvaccinated, 
                            color = Healthcare.Personnel..HCP..Type)) + 
                   geom_boxplot() +
                   xlab("HCP Type") +
                   ylab("Percent Unvaccinated") +
                   theme(legend.text = element_blank(),
                         axis.text.x = element_blank())
        )
      }
    }
  })
#######################################
#PLOTLY Distribution (Violin plot) for Box 2
#######################################
  output$plot6 <- renderPlotly({
    if (input$rate2input == "Vaccinated") {
      if (input$flterinput == "None") {
        ggplotly(ggplot(data2, 
                        aes(Healthcare.Personnel..HCP..Type, Percent.Vaccinated, 
                            color = Healthcare.Personnel..HCP..Type)) + 
                   geom_violin() +
                   xlab("Facility Type") +
                   ylab("Percent Vaccinated") +
                   theme(legend.text = element_blank(), 
                         axis.text.x = element_blank())
        )
      }
      else {
        data3 <- filter(data2, Facility.Type == input$flterinput)
        ggplotly(ggplot(data3, 
                        aes(Healthcare.Personnel..HCP..Type, Percent.Vaccinated, 
                            color = Healthcare.Personnel..HCP..Type)) + 
                   geom_violin() +
                   xlab("Facility Type") +
                   ylab("Percent Vaccinated") +
                   theme(legend.text = element_blank(),
                         axis.text.x = element_blank())
        )
      }
    }
    else {
      if (input$flterinput == "None") {
        ggplotly(ggplot(data2, 
                        aes(Healthcare.Personnel..HCP..Type, Percent.Unvaccinated, 
                            color = Healthcare.Personnel..HCP..Type)) + 
                   geom_violin() +
                   xlab("Facility Type") +
                   ylab("Percent Unvaccinated") +
                   theme(legend.text = element_blank(), 
                         axis.text.x = element_blank())
        )
      }
      else {
        data3 <- filter(data2, Facility.Type == input$flterinput)
        ggplotly(ggplot(data3, 
                        aes(Healthcare.Personnel..HCP..Type, Percent.Unvaccinated, 
                            color = Healthcare.Personnel..HCP..Type)) + 
                   geom_violin() +
                   xlab("Facility Type") +
                   ylab("Percent Unvaccinated") +
                   theme(legend.text = element_blank(),
                         axis.text.x = element_blank())
        )
      }
    }
  })
  
############################################
#T-TEST for HCP (BOX 3)
############################################
  output$ttest1 <- renderTable({
    if (length(input$ttestfactors) == 2) {
      if (input$rate2input == "Vaccinated") {
        if (input$flterinput == "None") {
          x1 <- filter(data2, Healthcare.Personnel..HCP..Type == input$ttestfactors[1]) %>%
            pull(., Percent.Vaccinated)
          x2 <- filter(data2, Healthcare.Personnel..HCP..Type == input$ttestfactors[2]) %>%
            pull(., Percent.Vaccinated)
          t.test(x1, x2) %>%
            tidy(.)
        }
        else {
          data4 <- filter(data2, Facility.Type == input$flterinput)
          x1 <- filter(data4, Healthcare.Personnel..HCP..Type == input$ttestfactors[1]) %>%
            pull(., Percent.Vaccinated)
          x2 <- filter(data4, Healthcare.Personnel..HCP..Type == input$ttestfactors[2]) %>%
            pull(., Percent.Vaccinated)
          t.test(x1, x2) %>%
            tidy(.)
        }
      }
      else {
        if (input$flterinput == "None") {
          x1 <- filter(data2, Healthcare.Personnel..HCP..Type == input$ttestfactors[1]) %>%
            pull(., Percent.Unvaccinated)
          x2 <- filter(data2, Healthcare.Personnel..HCP..Type == input$ttestfactors[2]) %>%
            pull(., Percent.Unvaccinated)
          t.test(x1, x2) %>%
            tidy(.)
        }
        else {
          data4 <- filter(data2, Facility.Type == input$flterinput)
          x1 <- filter(data4, Healthcare.Personnel..HCP..Type == input$ttestfactors[1]) %>%
            pull(., Percent.Unvaccinated)
          x2 <- filter(data4, Healthcare.Personnel..HCP..Type == input$ttestfactors[2]) %>%
            pull(., Percent.Unvaccinated)
          t.test(x1, x2) %>%
            tidy(.)
        }
      }
    }
  })
  
  
  
############################################################
#Outputs for TAB 2.3
#############################################
#TEXT Output for the Description
#############################################
  output$text2 <- renderText({
    "DESCRIPTION:"
  })
  
  output$text22 <- renderText({
    "SELECT BAR: The select-bar allows selection of certain target variables. In this instance,
  there is the option to isolate different HCP (Health Care Personnel) types in order to see the distribution of 
     information (Percent of people Vaccinated or Unvaccinated by facility) for just that specific HCP type."
  })
  
  output$text222 <- renderText({
    "T-TEST: The check boxes allow for selection of two specific Facility types which will 
  result in the return of a 2 group T-Test (comparing means of Percent Vaccinated or Unvaccinated). estimate 1 represents
    the mean of the first selected type, while estimate 2 represents the mean of the second and estimate represents 
    the difference in means"
  })

   
  
#############################################  
#PLOTLY Output for Facility (boxplots)
#############################################
  output$plot7 <- renderPlotly({
    if (input$rate3input == "Vaccinated") {
      if (input$flter2input == "None") {
        ggplotly(ggplot(data2, 
                        aes(Facility.Type, Percent.Vaccinated, 
                            color = Facility.Type)) + 
                   geom_boxplot() +
                   xlab("Facility Type") +
                   ylab("Percent Vaccinated") +
                   theme(legend.text = element_blank(), 
                         axis.text.x = element_blank())
        )
      }
      else {
        data3 <- filter(data2, Healthcare.Personnel..HCP..Type == input$flter2input)
        ggplotly(ggplot(data3, 
                        aes(Facility.Type, Percent.Vaccinated, 
                            color = Facility.Type)) + 
                   geom_boxplot() +
                   xlab("Facility Type") +
                   ylab("Percent Vaccinated") +
                   theme(legend.text = element_blank(),
                         axis.text.x = element_blank())
        )
      }
    }
    else {
      if (input$flter2input == "None") {
        ggplotly(ggplot(data2, 
                        aes(Facility.Type, Percent.Unvaccinated, 
                            color = Facility.Type)) + 
                   geom_boxplot() +
                   xlab("Facility Type") +
                   ylab("Percent Unvaccinated") +
                   theme(legend.text = element_blank(), 
                         axis.text.x = element_blank())
        )
      }
      else {
        data3 <- filter(data2, Healthcare.Personnel..HCP..Type == input$flter2input)
        ggplotly(ggplot(data3, 
                        aes(Facility.Type, Percent.Unvaccinated, 
                            color = Facility.Type)) + 
                   geom_boxplot() +
                   xlab("Facility Type") +
                   ylab("Percent Unvaccinated") +
                   theme(legend.text = element_blank(),
                         axis.text.x = element_blank())
        )
      }
    }
  })
 
################################################
#PLOTLY Output for Facility (violin plots)
################################################
  output$plot8 <- renderPlotly({
    if (input$rate3input == "Vaccinated") {
      if (input$flter2input == "None") {
        ggplotly(ggplot(data2, 
                        aes(Facility.Type, Percent.Vaccinated, 
                            color = Facility.Type)) + 
                   geom_violin() +
                   xlab("Facility Type") +
                   ylab("Percent Vaccinated") +
                   theme(legend.text = element_blank(), 
                         axis.text.x = element_blank())
        )
      }
      else {
        data3 <- filter(data2, Healthcare.Personnel..HCP..Type == input$flter2input)
        ggplotly(ggplot(data3, 
                        aes(Facility.Type, Percent.Vaccinated, 
                            color = Facility.Type)) + 
                   geom_violin() +
                   xlab("Facility Type") +
                   ylab("Percent Vaccinated") +
                   theme(legend.text = element_blank(),
                         axis.text.x = element_blank())
        )
      }
    }
    else {
      if (input$flter2input == "None") {
        ggplotly(ggplot(data2, 
                        aes(Facility.Type, Percent.Unvaccinated, 
                            color = Facility.Type)) + 
                   geom_violin() +
                   xlab("Facility Type") +
                   ylab("Percent Unvaccinated") +
                   theme(legend.text = element_blank(), 
                         axis.text.x = element_blank())
        )
      }
      else {
        data3 <- filter(data2, Healthcare.Personnel..HCP..Type == input$flter2input)
        ggplotly(ggplot(data3, 
                        aes(Facility.Type, Percent.Unvaccinated, 
                            color = Facility.Type)) + 
                   geom_violin() +
                   xlab("Facility Type") +
                   ylab("Percent Unvaccinated") +
                   theme(legend.text = element_blank(),
                         axis.text.x = element_blank())
        )
      }
    }
  })
  
############################################
#T-TEST for HCP (BOX 3)
############################################
  output$ttest2 <- renderTable({
    if (length(input$ttestfactors2) == 2) {
      if (input$rate3input == "Vaccinated") {
        if (input$flter2input == "None") {
          y1 <- filter(data2, Facility.Type == input$ttestfactors2[1]) %>%
            pull(., Percent.Vaccinated)
          y2 <- filter(data2, Facility.Type == input$ttestfactors2[2]) %>%
            pull(., Percent.Vaccinated)
          t.test(y1, y2) %>%
            tidy(.)
        }
        else {
          data4 <- filter(data2, Healthcare.Personnel..HCP..Type == input$flter2input)
          y1 <- filter(data4, Facility.Type == input$ttestfactors2[1]) %>%
            pull(., Percent.Vaccinated)
          y2 <- filter(data4, Facility.Type == input$ttestfactors2[2]) %>%
            pull(., Percent.Vaccinated)
          t.test(y1, y2) %>%
            tidy(.)
        }
      }
      else {
        if (input$flter2input == "None") {
          y1 <- filter(data2, Facility.Type == input$ttestfactors2[1]) %>%
            pull(., Percent.Unvaccinated)
          y2 <- filter(data2, Facility.Type == input$ttestfactors2[2]) %>%
            pull(., Percent.Unvaccinated)
          t.test(y1, y2) %>%
            tidy(.)
        }
        else {
          data4 <- filter(data2, Healthcare.Personnel..HCP..Type == input$flter2input)
          y1 <- filter(data4, Facility.Type == input$ttestfactors2[1]) %>%
            pull(., Percent.Unvaccinated)
          y2 <- filter(data4, Facility.Type == input$ttestfactors2[2]) %>%
            pull(., Percent.Unvaccinated)
          t.test(y1, y2) %>%
            tidy(.)
        }
      }
    }
  })

#############################################################
#Outputs for TAB 3
#LEAFLETOUTPUT FOR MAP 1 (TAB 3.3)
################################################
  output$map1 <- renderLeaflet({
    
    if ((input$pctInput) == "Unvaccinated") {
      if ((input$yearinput) == "ALL") {
    #Create Map ALL
        NYcounties@data <- NYcounties@data %>%
          left_join(., county_data, c("NAME" = "County"))
        
          bins <- c(0, 2, 4, 6, 9, 15, 20, 30)
          pal <- colorBin("YlOrRd", domain = NYcounties$Percent_Unvaccinated, bins = bins)
          labels <- sprintf(
            "<strong>%s</strong><br/>%g percent unvaccinated",
            NYcounties$NAME, NYcounties$Percent_Unvaccinated
          ) %>% lapply(htmltools::HTML)
          
          m <- leaflet(NYcounties) %>%
            setView(lng = -76, lat = 43, zoom = 7) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~pal(NYcounties$Percent_Unvaccinated),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels
      )
    m %>% addLegend(pal = pal, values = ~density, opacity = 0.7, title = "Percent of HCP Unvaccinated",
                    position = "topright")
      }
      else  {
        #Create Map for Specified Years
        mapdata <- filter(county_pct_year, Year == input$yearinput)
        
        NYcounties@data <- NYcounties@data %>%
          left_join(., mapdata, c("NAME" = "County"))
        
        bins <- c(0, 2, 4, 6, 9, 15, 20, 30)
        pal <- colorBin("YlOrRd", domain = NYcounties$Percent_Unvaccinated, bins = bins)
        labels <- sprintf(
          "<strong>%s</strong><br/>%g percent unvaccinated",
          NYcounties$NAME, NYcounties$Percent_Unvaccinated
        ) %>% lapply(htmltools::HTML)
        
        m <- leaflet(NYcounties) %>%
          setView(lng = -76, lat = 43, zoom = 7) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(NYcounties$Percent_Unvaccinated),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = labels
          )
        m %>% addLegend(pal = pal, values = ~density, opacity = 0.7, title = "Percent of HCP Unvaccinated",
                        position = "topright")
      }
      
    }
    
    else {
      if ((input$yearinput) == "ALL") {
      #Create Map
        NYcounties@data <- NYcounties@data %>%
          left_join(., county_data, c("NAME" = "County"))
        
      bins <- c(0, 40, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
      pal <- colorBin("Blues", domain = NYcounties$Percent_Vaccinated, bins = bins)
      labels <- sprintf(
        "<strong>%s</strong><br/>%g percent vaccinated",
        NYcounties$NAME, NYcounties$Percent_Vaccinated
      ) %>% lapply(htmltools::HTML)
      
      m <- leaflet(NYcounties) %>%
        setView(lng = -76, lat = 43, zoom = 7) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(NYcounties$Percent_Vaccinated),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels
        )
      m %>% addLegend(pal = pal, values = ~density, opacity = 0.7, title = "Percent of HCP Vaccinated",
                      position = "topright")
      }
      else  {
        #Create Map for Specified Years
        mapdata <- filter(county_pct_year, Year == input$yearinput)
        
        NYcounties@data <- NYcounties@data %>%
          left_join(., mapdata, c("NAME" = "County"))
        
        bins <- c(0, 40, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
        pal <- colorBin("Blues", domain = NYcounties$Percent_vaccinated, bins = bins)
        labels <- sprintf(
          "<strong>%s</strong><br/>%g percent vaccinated",
          NYcounties$NAME, NYcounties$Percent_vaccinated
        ) %>% lapply(htmltools::HTML)
        
        m <- leaflet(NYcounties) %>%
          setView(lng = -76, lat = 43, zoom = 7) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(NYcounties$Percent_vaccinated),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = labels
          )
        m %>% addLegend(pal = pal, values = ~density, opacity = 0.7, title = "Percent of HCP Vaccinated",
                        position = "topright")
      }
      
    }
  })
  
#LEAFLET OUTPUT FOR MAP 2 (TAB 3.3)
######################################################  
  output$map2 <- renderLeaflet({
    if(FALSE) {
    #Create Map All years
      NYcounties@data <- NYcounties@data %>%
        left_join(., county_tot, c("NAME" = "County"))
      
    bins <- c(0, .01, .02, .03, .04, .05, .06, .07, .2)
    pal <- colorBin("YlOrRd", domain = (NYcounties$Total_Count / NYcounties$Population), bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/>%g reported cases per capita",
      NYcounties$NAME, (NYcounties$Total_Count / NYcounties$Population)
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet(NYcounties) %>%
      setView(lng = -76, lat = 43, zoom = 7) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal((NYcounties$Total_Count / NYcounties$Population)),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels
      )
    m %>% addLegend(pal = pal, 
                    values = ~density, 
                    opacity = 0.7, 
                    title = "Number of Reported Cases of Influenza per capita",
                    position = "topright")
    }
    
    else {
      #Create Map for Specified Years
      mapdata2 <- filter(county_tot_year, Year == "2012-2013")
      
      NYcounties@data <- NYcounties@data %>%
        left_join(., mapdata2, c("NAME" = "County"))
      
      bins <- c(0, .001, .002, .003, .004, .005, .006, .007, .02)
      pal <- colorBin("YlOrRd", domain = NYcounties$Total_Count / NYcounties$Population, bins = bins)
      labels <- sprintf(
        "<strong>%s</strong><br/>%g reported cases per capita",
        NYcounties$NAME, NYcounties$Total_Count / NYcounties$Population
      ) %>% lapply(htmltools::HTML)
      
      m <- leaflet(NYcounties) %>%
        setView(lng = -76, lat = 43, zoom = 7) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(NYcounties$Total_Count / NYcounties$Population),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels
        )
      m %>% addLegend(pal = pal, 
                      values = ~density, 
                      opacity = 0.7, 
                      title = "Number of Reported Cases of Influenza per capita",
                      position = "topright")
    }
    
  })
#LEAFLET OUTPUT FOR MAP 3 (Tab 3.1)
######################################################
  output$map3 <- renderLeaflet({
    
    if(input$rateHCPmap == "Vaccinated") {
      if(input$facilitymapinput1 == "ALL") {
        mapdata1 <- filter(HCP_county, HCP == input$HCPmapinput)
        NYcounties@data <- NYcounties@data %>%
          left_join(., mapdata1, c("NAME" = "County"))
        
        bins <- c(0, 50, 60, 70, 75, 80, 85, 90, 95, 100)
        pal <- colorBin("Blues", domain = NYcounties$HCP_Percent_Vaccinated, bins = bins)
        labels <- sprintf(
          paste("<strong>%s</strong><br/>%g percent of", input$HCPmapinput, "vaccianted"),
          NYcounties$NAME, NYcounties$HCP_Percent_Vaccinated
        ) %>% lapply(htmltools::HTML)
        
        m <- leaflet(NYcounties) %>%
          setView(lng = -76, lat = 43, zoom = 7.3) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(NYcounties$HCP_Percent_Vaccinated),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = labels
          )
        m %>% addLegend(pal = pal, 
                        values = ~density, 
                        opacity = 0.7, 
                        title = paste("Percent of", input$HCPmapinput, "Vaccinated"),
                        position = "topright")
      }
      else {
        mapdata1 <- filter(HCP_Fac_county, HCP == input$HCPmapinput, Facility == input$facilitymapinput1)
        NYcounties@data <- NYcounties@data %>%
          left_join(., mapdata1, c("NAME" = "County"))
        
        bins <- c(0, 50, 60, 70, 75, 80, 85, 90, 95, 100)
        pal <- colorBin("Blues", domain = NYcounties$HCP_Percent_Vaccinated, bins = bins)
        labels <- sprintf(
          paste("<strong>%s</strong><br/>%g percent of", input$HCPmapinput, "vaccianted at", input$facilitymapinput1),
          NYcounties$NAME, NYcounties$HCP_Percent_Vaccinated
        ) %>% lapply(htmltools::HTML)
        
        m <- leaflet(NYcounties) %>%
          setView(lng = -76, lat = 43, zoom = 7.3) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(NYcounties$HCP_Percent_Vaccinated),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = labels
          )
        m %>% addLegend(pal = pal, 
                        values = ~density, 
                        opacity = 0.7, 
                        title = paste("Percent of", input$HCPmapinput, "Vaccinated"),
                        position = "topright")
      }
    }
    
    else {
      if(input$facilitymapinput1 == "ALL") {
        mapdata1 <- filter(HCP_county, HCP == input$HCPmapinput)
        NYcounties@data <- NYcounties@data %>%
          left_join(., mapdata1, c("NAME" = "County"))
        
        bins <- c(0, 2, 4, 6, 9, 15, 20, 30)
        pal <- colorBin("YlOrRd", domain = NYcounties$HCP_Percent_Unvaccinated, bins = bins)
        labels <- sprintf(
          paste("<strong>%s</strong><br/>%g percent of", input$HCPmapinput, "unvaccianted"),
          NYcounties$NAME, NYcounties$HCP_Percent_Unvaccinated
        ) %>% lapply(htmltools::HTML)
        
        m <- leaflet(NYcounties) %>%
          setView(lng = -76, lat = 43, zoom = 7.3) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(NYcounties$HCP_Percent_Unvaccinated),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = labels
          )
        m %>% addLegend(pal = pal, 
                        values = ~density, 
                        opacity = 0.7, 
                        title = paste("Percent of", input$HCPmapinput, "Unvaccinated at", input$facilitymapinput1),
                        position = "topright")
      }
      else {
        mapdata1 <- filter(HCP_Fac_county, HCP == input$HCPmapinput, Facility == input$facilitymapinput1)
        NYcounties@data <- NYcounties@data %>%
          left_join(., mapdata1, c("NAME" = "County"))
        
        bins <- c(0, 2, 4, 6, 9, 15, 20, 30)
        pal <- colorBin("YlOrRd", domain = NYcounties$HCP_Percent_Unvaccinated, bins = bins)
        labels <- sprintf(
          paste("<strong>%s</strong><br/>%g percent of", input$HCPmapinput, "unvaccianted"),
          NYcounties$NAME, NYcounties$HCP_Percent_Unvaccinated
        ) %>% lapply(htmltools::HTML)
        
        m <- leaflet(NYcounties) %>%
          setView(lng = -76, lat = 43, zoom = 7.3) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(NYcounties$HCP_Percent_Unvaccinated),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = labels
          )
        m %>% addLegend(pal = pal, 
                        values = ~density, 
                        opacity = 0.7, 
                        title = paste("Percent of", input$HCPmapinput, "Unvaccinated"),
                        position = "topright")
      }
    }
  })
  
}
##########################################################################################################################
shinyApp(ui, server)