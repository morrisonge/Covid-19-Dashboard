library(sf)
library(leaflet)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(shinythemes)
library(classInt)
library(RColorBrewer)





time_data <- read.csv("times_series.csv")
time_data$Date <- as.Date(time_data$Date)
gdf <- st_read("demographic.shp")
gdf <- st_transform(gdf,crs = 4326)
names(gdf) <- c("STATEFP",
                "COUNTYF",
                "COUNTYN",
                "AFFGEOI",
                "GEOID",
                "NAME",
                "population",
                "percent_male",
                "percent_female",
                "percent_black",
                "percent_asian",
                "percent_hispanic",
                "percent_public_transportation",
                "percent_hs",
                "percent_bachelors",
                "household_income",
                "per_capita_income",
                "percent_health_workers",
                "percent_over65",
                "county",
                "population_density",
                "geometry")


covid_key <- c("Confirmed Cases",
               "Active Cases",
               "Deaths",
               "Confirmed Cases per 10000 People",
               "Active Cases per 10000 People",
               "Deaths per 10000 People")

covid_val <- c("Confirmed",
               "Active",
               "Deaths",
               "confirmed_rate_10000",
               "active_rate_10000",
               "death_rate_10000")

demographic_key <- c("Percent Male",
                     "Percent Female",
                     "Percent Black",
                     "Percent Asian",
                     "Percent Hispanic",
                     "Percent Public Transportation",
                     "Percent with HS Diploma",
                     "Percent with Bachelor Degree",
                     "Percent Health Care Workers",
                     "Percent Over the Age of 65",
                     "Median Household Income",
                     "Per Capita Income",
                     "Population Density")

demographic_val <- c("percent_male",
                     "percent_female",
                     "percent_black",
                     "percent_asian",
                     "percent_hispanic",
                     "percent_public_transportation",
                     "percent_hs",
                     "percent_bachelors",
                     "percent_health_workers",
                     "percent_over65",
                     "household_income",
                     "per_capita_income",
                     "population_density")




ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "US COVID-19 and Demographic Data", id="nav",
             
             tabPanel("Mapping Application",
                      div(class="outer",
                          column(
                            width = 8, 
                            leafletOutput("covid_map"),
                            leafletOutput("demographic_map"),
                            plotlyOutput("scatterplot")
                          ),
                          column(
                            width = 4,
                            plotlyOutput("covid_histogram"),
                            plotlyOutput("demographic_histogram"),
                            box(
                              background = "black",
                              br(),
                              br(),
                              h4("Regression Statistics"),
                              h5(textOutput("variables")),
                              h5(textOutput("slope")),
                              h5(textOutput("intercept")),
                              h5(textOutput("correlation"))
                            )
                          ),
                          absolutePanel(id = "controls", class = "panel",
                                        top = 75, left = 65, width = 250,
                                        draggable = TRUE, height = "auto",
                                        h3(textOutput("case_count")),
                                        span(h3(textOutput("death_count")),style = "color:#FF0000"),
                                        plotOutput("confirmed_ts", height="130px", width="100%"),
                                        plotOutput("death_ts", height="130px", width="100%"),
                                        dateInput(inputId = "date_selector",
                                                  label = "Pick a date",
                                                  min = "2020-03-01",
                                                  max = "2020-05-25",
                                                  value = "2020-05-25"),
                                        selectInput(inputId = "covid_type",
                                                    label = "Choose a COVID-19 variable",
                                                    choices = c("Confirmed Cases",
                                                                "Active Cases",
                                                                "Deaths",
                                                                "Confirmed Cases per 10000 People",
                                                                "Active Cases per 10000 People",
                                                                "Deaths per 10000 People"),
                                                    selected = "Confirmed Cases per 10000 People"
                                        ),
                                        h6("COVID-19 Data from CSSE at John Hopkins University"),
                                        selectInput(inputId = "demographic_type",
                                                    label = "Choose a demographic variable",
                                                    choices = c("Percent Male",
                                                                "Percent Female",
                                                                "Percent Black",
                                                                "Percent Asian",
                                                                "Percent Hispanic",
                                                                "Percent Public Transportation",
                                                                "Percent with HS Diploma",
                                                                "Percent with Bachelor Degree",
                                                                "Percent Health Care Workers",
                                                                "Percent Over the Age of 65",
                                                                "Median Household Income",
                                                                "Per Capita Income",
                                                                "Population Density"),
                                                    selected = "Percent Male"
                                        ),
                                        h6("5 year averages from the ACS 2014-2018")
                                        
                          )
                      )
             ),
             
             tabPanel("Overview",
                      column(
                        width = 8,
                        h2("Introduction"),
                        p("There are wide disparities in public health access and infectious disease risk among
                          minority and disadvantaged groups. These groups suffer higher risk in infection and
                          mortality in both annual influenza and pandemic influenza(Hutchins 2009). This increased 
                          risk comes from a variety of factors including crowding, lower socioeconomic status, 
                          lower health literacy, occupational differences and more. While these groups are at 
                          higher risk of infection and mortality in a pandemic, they also suffer more from the
                          economic effects of pandemic policy(Blumenshine 2008). Policy makers must account for these 
                          disparities in order make an equitable plan. Strategic planning must consider differing 
                          life circumstances, cultural values, and perspectives on risk guide behavior in a 
                          pandemic(Vaughan and Tinker 2009). "),
                        br(),
                        p("The relationship between socioeconomic status and infectious disease has not been explored
                          in many papers. A large number of papers control for it as a variable within their models,
                          but few actually examine the relationship between the two. In the papers that do examine 
                          the relationship between socioeconomic status and infectious disease, the relationship is 
                          not entirely clear or defined for all pathogens, but depends on the type of disease. In a 
                          study done in the Netherlands, it was found that for some diseases, higher education levels
                          and higher socioeconomic status had higher levels of immunity to some diseases and lower for 
                          others. For measles and rubella, higher education levels had higher immunity to the disease.
                          For pneumoccus, MenC, and CMV they had lower immunity. They concluded that socioeconomic status
                          is associated with antibody level, but on a pathogen dependent basis(Hoes 2018). "),
                        br(),
                        p("With the Covid-19 and Demographic Data Dashboard, the user will be able to explore simple linear 
                          relationships and spatial patterns of demographic variables and COVID-19 case rates. This can yield
                          insight as to which groups have the highest risk with COVID-19.  There are many exploratory data and 
                          mapping dashboards for COVID-19. Most of the dashboards focus on where COVID-19 is most concentrated 
                          and provide some kind of exploratory time series plots and histograms. Others employ different 
                          exploratory statistical methods. For instance the Center for Spatial Data Science adds the LISA 
                          clustering option to explore hot and cold spots. This dashboard is different in that it will 
                          be exploring relationships between COVID-19 cases and an assortment of demographic and socioeconomic
                          variables. The application does not prove relationships between COVID-19 and other variables, but 
                          provides exploratory insight through visual analysis and other exploratory techniques. "),
                        br(),
                        h2("Methodology"),
                        p("The final product is a shiny application that does exploratory mapping of different types of
                          COVID-19 cases, demographic variables, and socioeconomic indicators. Both of the maps are
                          the United States at the county level. One map focuses on COVID-19 case types and population 
                          based rates. The other map visualizes demographic and socioeconomic variables. The maps are
                          placed adjacently to allow for visual comparison of spatial trends. Below the two maps are
                          some exploratory plots of both selected variables. One is a scatterplot with the COVID-19 variable 
                          on the y-axis and the demographic or socioeconomic variable on the x axis. Both of these variables
                          have a histogram adjacent to their map. I added some additional summary statistics to accompany
                          these plots too. The bulk of the work for the application was in creating processes to aggregate
                          and visualize the COVID-19 data for the selected date and case type. The demographic and socioeconomic 
                          data was simpler to process, but required significant work in gathering the desired variables 
                          from the American Community Survey and putting them together into one cohesive dataset for the application. 
                          The socioeconomic and demographic variables include income, age distributions, race, demographics, 
                          and gender."),
                        br(),
                        h2("Workflow and Schedule for Completion"),
                        p("April 8th to April 15th Project Proposal"),
                        p("April 15th to April 27th Background and Research"),
                        p("April 27th to May 11th Data and Methodology "),
                        p("May 11th to May 27th Prototype of the application"),
                        p("May 27th to June 3rd  Update Application and Refine Final Report")
                        ),
                      column(
                        width = 4,
                        h2("References"),
                        p("Elliott P, Wartenberg D (2004) Spatial epidemiology: current approaches and future challenges.
                          Environ Health Perspect 112(9):998"),
                        p("Beale L, Abellan JJ, Hodgson S, Jarup L. Methodologic issues and approaches to spatial epidemiology.
                          Environ Health Perspect. 2008;116:1105–10."),
                        p("Pappas L., Whitman L. (2011) Riding the Technology Wave: Effective Dashboard Data Visualization. 
                          In: Smith M.J., Salvendy G. (eds) Human Interface and the Management of Information. Interacting with 
                          Information. Human Interface 2011. Lecture Notes in Computer Science, vol 6771. Springer, Berlin, Heidelberg"),
                        p("Hoes J, Boef AGC, Knol MJ, de Melker HE, Mollema L, van der Klis FRM, Rots NY and van Baarle D (2018) 
                          Socioeconomic Status Is Associated With Antibody Levels Against Vaccine Preventable Diseases in the Netherlands. 
                          Front. Public Health 6:209. doi: 10.3389/fpubh.2018.00209"),
                        p("Blumenshine P, Reingold A, Egerter S, Mockenhaupt R, Braveman P, Marks J. Pandemic influenza planning in the 
                          United States from as health disparities perspective. Emerg Infect Dis. 2008;14(5):709–15."),
                        p("Hutchins S, Fiscella K, Levine R, Ompad D, McDonald M. Protection of racial/ethnic minority populations
                          during an influenza pandemic. Am J Public Health. 2009;99(S2):S261–S270."),
                        p("Vaughan E, Tinker T. Effective health risk communication about pandemic influenza for vulnerable populations.
                          Am J Public Health. 2009;99(S2):S324–S332."),
                        p("Chowell G, Viboud C. Pandemic influenza and socioeconomic disparities: Lessons from 1918 Chicago.
                          Proc Natl Acad Sci U S A. 2016;113(48):13557–13559. doi:10.1073/pnas.1616537113")
                        )
                      
                      
                      
                        ),
             
             tabPanel("Background",
                      column(
                        width = 8,
                        h2("Background"),
                        br(),
                        p("Disparities in public health and infectious disease risk are widespread among minority
                          and disadvantaged groups. These groups suffer from higher risk in infection and mortality 
                          in both annual influenza and pandemic influenza(Hutchins 2009). This increased risk stems from a
                          variety of different factors. Some of these include crowding, occupation, and disparities in 
                          health care access. Crowding increases the overall likelihood of pathogen transmission. 
                          Occupational differences lead to different levels of exposure to infectious diseases(Blumenshine 2008).
                          Disparities in access to health resources cause higher mortality rates. Although not intentional, 
                          these disparities arise due to significant cultural, socioeconomic, and communication barriers.
                          These groups are also more susceptible to the economic consequences of the pandemic response 
                          strategy. Without observing and effectively planning for these differences, there will be unequal
                          protection for vulnerable groups in the response(Hutchins 2009). Measuring the disparities in risk and 
                          effectively accounting for them within the response strategy can be very difficult. Some of the 
                          approaches assess risk on an individual level by collecting patient demographic and socioeconomic 
                          data, while others aggregate this information to a geographic area. "),
                        br(),
                        p("The reduction of socioeconomic and racial disparities in health and healthcare has been a goal of 
                          both federal and state policy a two decades. However, these disparities remain widespread within the
                          current system. With the current limitations in the public health infrastructure, and the disparities 
                          in healthcare, a pandemic is likely to disproportionately affect disadvantaged groups(Blumenshine 2008). 
                          Strategic planning must consider differing life circumstances, cultural values, and perspectives on risk
                          guide behavior in a pandemic(Vaughan and Tinker 2009). Minority groups have lower capacity to participate in
                          pandemic interventions and to endure a pandemic because of disparities caused by socioeconomic disadvantages,
                          cultural, educational, linguistic barriers, and lack of access to health care(Hutchins 2009). Widespread 
                          participation in pandemic procedures is necessary to mitigate the risk to the overall population. 
                          In order to facilitate this participation, proper communication and implementation of pandemic procedures
                          must assess health disparities and effects on disadvantaged groups. While minority and disadvantaged groups 
                          suffer from higher risk of infection and mortality rates in pandemics, they also suffer more from the economic
                          implications of the policy than other groups(Blumenshine 2008). To create an effective and equitable pandemic
                          response, these disparities need detailed attention in each facet of the plan. For instance, a plan that 
                          includes strict social distancing measures will have stronger negative economic effects on disadvantaged groups.
                          For this type of approach to be equitable, this economic disparity would need to be addressed in some way."),
                        br(),
                        p("The role of socioeconomic status and disparities in health access can be assessed with many different methods.
                          Each provides their own perspective and a unique lens to examine the effects on disadvantaged groups with respect
                          to risk of infection and mortality. Many empirical studies account for socioeconomic status in their model, but
                          do not model specific effects of it on health risk. The studies that do explore the relationship between 
                          socioeconomic status and health risk are limited in the approaches taken. Most examine patients as cases and
                          gather socioeconomic data about the subjects in order to make statistical inferences about the relationship 
                          between the two. In a study in the Netherlands, the relationship between socioeconomic status(SES) is compared 
                          with immunity levels against various pathogens. They find that the direction of the relationship depends on the
                          pathogen. For example pneumoccus has a negative relationship with SES, while measles has a positive one.(Hoes 2018). The 
                          study examines the effects of SES on immunity, so the relationships found could be due to increased/decreased exposure
                          based on SES. The common theme throughout most the literature is that the effects of SES on infectious disease are 
                          ambiguous. However, it is clear that disadvantaged groups have higher mortality and infection rates in certain kinds 
                          of diseases, such as influenza. COVID-19 is most comparable with viral infections like influenza, but there are 
                          important differences that can muddle the comparison. An empirical study on spatial patterns with the influenza
                          pandemic of 1918 found relationships between socioeconomic variables and mortality, most notably in literacy 
                          rates(Chowell and Viboud 2016). This empirical study is the most relevant to the project data dashboard in topic,
                          variables, and spatial methodology."),
                        br(),
                        p("The methodological focus will be on spatial approaches in assessing and exploring the role of demographics 
                          and socioeconomic status in COVID-19. With area level aggregation, the effects of demographic and socioeconomic 
                          indicators can be taken into account. The spatial component of data can play a crucial role in explaining the 
                          variability of risk because socioeconomic status, demographic compositions, and other relevant characteristics
                          vary across space(Beale 2008). This approach allows the researcher to rapidly communicate patterns and information 
                          through maps, while assessing relationships through simple and sophisticated statistical methods. Disease mapping 
                          is common throughout epidemiology, and can lead to a variety of different patterns and information depending on the 
                          spatial resolution and methods of aggregation. This approach has limitations with regard to interpretation because 
                          choices in aggregation and spatial resolution can drastically affect results. Patterns and be lost or found depending 
                          the on the approach taken(Elliot and Wartenburg 2004). "),
                        br(),
                        p("A data dashboard can useful for the exploration and analysis of infectious diseases. In the case of COVID-19, there
                          many data dashboards. Some focus on time series plots and daily increases in confirmed cases and deaths, while other
                          focus on a specific aspect like location. One of the most widely known spatial dashboards is from John Hopkins. It
                          depicts the spatial distribution of COVID-19 cases with a variety of other visualizations. With dashboards it is 
                          important to build the application with the target audience in mind. The overall message should be easily 
                          interpretable, and not hidden in a myriad plots and unnecessary information (Pappas 2011). While disease mapping contains
                          a wide variety of sophisticated statistical techniques, the overall interpretation would not be intuitive to most
                          people. Exploratory data dashboards can include advance methods and statistics, if they are comprehendible to the 
                          target audience. With the COVID-19 dashboard, I want to keep the exploration simple and easy to understand, so I will
                          avoid advance statistical methodology. "),
                        br(),
                        p("Many of the COVID-19 dashboards overload information and do not make effective use of space. The John Hopkins 
                          dashboard may be the most popular and widely referenced spatial COVID-19 dashboard, but overall is not a great
                          dashboard. There is too much information displayed on the screen, and much of it is not useful to the user. The 
                          German COVID-19 dashboard is similar, but better in that it provides more relevant and unique information like 
                          gender and age cohort data. The GeoDa center atlas takes a unique approach to visualizing the spatial distribution 
                          of COVID-19 cases with the inclusion of population based rates and local moran clustering. This allows the user to 
                          see hot and cold spots of COVID-19, while controlling for population size with rates. This application’s layout is 
                          concise, while providing a unique perspective on the COVID-19 data. "),
                        br(),
                        p("This project differs from existing applications in a few key aspects. The main difference
                          is the cross comparison of COVID-19 case variables and other relevant socioeconomic and demographic variables. 
                          The COVID-19 variables include the county based counts, population based rates, and relative risk rates. The
                          demographic variables include percent composition of different minorities groups, education, transportation, occupation,
                          age, and gender. The socioeconomic variables include per capita
                          income and median household income. The application contains two maps, one for COVID-19 variables, and the other for 
                          socioeconomic and demographic variables. The two selected variables output a scatterplot and associated
                          histograms for each variable. Both maps are interactive, and provide variable information on the county hovered 
                          over by the mouse.")
                        
                        ),
                      column(
                        width = 4,
                        h2("References"),
                        p("Elliott P, Wartenberg D (2004) Spatial epidemiology: current approaches and future challenges.
                          Environ Health Perspect 112(9):998"),
                        p("Beale L, Abellan JJ, Hodgson S, Jarup L. Methodologic issues and approaches to spatial epidemiology.
                          Environ Health Perspect. 2008;116:1105–10."),
                        p("Pappas L., Whitman L. (2011) Riding the Technology Wave: Effective Dashboard Data Visualization. 
                          In: Smith M.J., Salvendy G. (eds) Human Interface and the Management of Information. Interacting with 
                          Information. Human Interface 2011. Lecture Notes in Computer Science, vol 6771. Springer, Berlin, Heidelberg"),
                        p("Hoes J, Boef AGC, Knol MJ, de Melker HE, Mollema L, van der Klis FRM, Rots NY and van Baarle D (2018) 
                          Socioeconomic Status Is Associated With Antibody Levels Against Vaccine Preventable Diseases in the Netherlands. 
                          Front. Public Health 6:209. doi: 10.3389/fpubh.2018.00209"),
                        p("Blumenshine P, Reingold A, Egerter S, Mockenhaupt R, Braveman P, Marks J. Pandemic influenza planning in the 
                          United States from as health disparities perspective. Emerg Infect Dis. 2008;14(5):709–15."),
                        p("Hutchins S, Fiscella K, Levine R, Ompad D, McDonald M. Protection of racial/ethnic minority populations
                          during an influenza pandemic. Am J Public Health. 2009;99(S2):S261–S270."),
                        p("Vaughan E, Tinker T. Effective health risk communication about pandemic influenza for vulnerable populations.
                          Am J Public Health. 2009;99(S2):S324–S332."),
                        p("Chowell G, Viboud C. Pandemic influenza and socioeconomic disparities: Lessons from 1918 Chicago.
                          Proc Natl Acad Sci U S A. 2016;113(48):13557–13559. doi:10.1073/pnas.1616537113")
                        )
                        ),
             
             tabPanel("Data and Methodology",
                      column(
                        width = 8,  
                        h2("Data and Methodology"),
                        br(),
                        p("The data for this application focuses on COVID-19 data and a variety of demographic 
                          and socioeconomic variables. The COVID-19 data comes from CSSE at John Hopkins University. 
                          It includes confirmed cases, active cases, and deaths at the county level in the United 
                          States. The data is organized by date into separate csv files. This means the application 
                          contains a folder of COVID-19 data by date. The dates range from the beginning of March 
                          to the current date. The demographic and socioeconomic data comes from the NHGIS website.
                          These variables are all 5 year averages for 2014-2018 from the American Community Survey.
                          The spatial resolution of all of these variables is at the county level to allow comparison 
                          with the COVID-19 data. These variables contain a wide range of demographic and socioeconomic
                          variables for the user to explore. They include categories such as age, population, race,
                          sex, income, education, occupation, and transportation. For a comprehensive list of each 
                          variable, the source, and a description see figure 2."),
                        br(),
                        p("Most of the data workflow was in creating the final shapefile with all of the demographic
                          and socioeconomic variables. The raw dataset contained many unnecessary variables. I 
                          selected the relevant ones, and converted them from factors to numeric values. These 
                          variables were all renamed from their encoded data cookbook name to something more
                          descriptive. When downloaded, most of the demographic variables were
                          in counts. These variables were divided by the total population and multiplied 
                          by 100 to get percents. All of these variables were then joined to the same US county 
                          shapefile by a geographic identifier. The final dataset is a shapefile that contains 
                          descriptive names of each variable, and values for each US county. "),
                        br(),
                        p("The COVID-19 data did not need any editing for the application. The user will select a 
                          date with the date selection widget, then a csv file with COVID-19 data on the corresponding 
                          date will be loaded into the environment. This data will be joined to the main shapefile, 
                          which contains all of the demographic and socioeconomic data. Next, the population based rates
                          of each COVID-19 variable are computed. The user can then select a COVID-19 variable to map
                          and visualize. This is the basic data flow for the COVID-19 data. The demographic and
                          socioeconomic variables do need any adjustments after they are loaded into the environment.
                          For a comprehensive visualization of the data flow with the date selection and variable 
                          selection widgets see figure 1."),
                        br(),
                        p("The application has a simple layout with both maps and statistical visualizations.
                          The user can select the date of COVID-19 cases that they would like to examine with the 
                          date selection widget. Next the user selects a COVID-19 variable with a selection widget. 
                          This will output a choropleth map and a histogram of the selected COVID-19 variable.
                          Once the user selects a socioeconomic or demographic variable, a scatterplot of both 
                          selected variables will be outputted along with the corresponding choropleth maps and
                          histograms. This layout will allow the user to visually compare variables and conduct 
                          exploratory analysis through the maps, scatterplots, and histograms of a variety of
                          different variables. "),
                        br(),
                        p("The application is made with leaflet and plotly visualizations in shiny. The
                          plotly package contains a wide range of interactive visualizations. These include maps, 
                          tables, and statistical visualizations. Each of the visualizations has great visual
                          aesthetics and some degree of interactivity. The leaflet package has great mapping 
                          capabilities with interactive maps and strong visual aesthetics. Both of the maps in
                          the application have hover interactivity, displaying the name of the county and 
                          values of the selected variable for the county hovered over with the mouse. The
                          scatterplot also has this type of interactivity, but it displays the selected
                          x and y variable values of a hovered over observation. The visualizations included are 
                          two choropleth maps, two histograms, and one scatterplot. These are packaged
                          together into a web application with the R shiny package. This package gives a variety
                          of widgets and user interface design options. It also integrates well with the plotly 
                          and leaflet packages to make interactive web applications. With this layout and design,
                          the user can explore relationships between COVID-19 data and a range of demographic 
                          and socioeconomic variables."),
                        br(),
                        h3("Figure 1"),
                        img(src="workflow.png"),
                        h3("Figure 2"),
                        img(src="variables.png")
                        )
                      
                      
                      
                        ),
             tabPanel("Data Sources",
                      h2("Data Sources"),
                      a("COVID-19 Data", href="https://github.com/CSSEGISandData/COVID-19"),
                      br(),
                      a("US County Shapefile",
                        href = ": https://catalog.data.gov/dataset/tiger-line-shapefile-2017-nation-u-s-current-county-and-equivalent-national-shapefile"),
                      br(),
                      a("US COVID Time Series Data",
                        href = "https://ourworldindata.org/covid-cases?country=~USA"),
                      br(),
                      a("Demographic and Socioeconomic Data",
                        href = "https://www.nhgis.org ")
                      
                      
             ),
             tabPanel("Other COVID-19 Dashboards",
                      h2("Links"),
                      a("Center for Spatial Data Science Atlas", href="https://geodacenter.github.io/covid/#"),
                      br(),
                      a("CSSE John Hopkins", href = "https://coronavirus.jhu.edu/us-map")
             )
             
             
                        )
  
  
                        )



##################################################################
#server
##################################################################

server = function(input, output,session){
  
  showModal(modalDialog(
    title = "Brief Introduction(Click anywhere to explore the app)",
    "This is a COVID-19 Dashboard meant to explore the relationships between demographic variables and
    COVID-19 variables. This application accomplishes this through visual comparison of interactive maps
    and exploratory plots and statistics",
    easyClose = TRUE,
    footer = NULL
  ))
  
  
  output$confirmed_ts <- renderPlot({
    ggplot(data = time_data, aes(x = Date, y = confirmed)) +
      geom_line(color = "orange") +
      geom_point(color = "orange")
  })
  
  output$death_ts <- renderPlot({
    ggplot(data = time_data, aes(x = Date, y = deaths)) +
      geom_line(color = "red") +
      geom_point(color = "red")
  })
  
  full_data <- reactive({
    date <- input$date_selector
    date_type <- as.Date(date)
    date_str <- format(date_type, "%m-%d-%Y")
    file_path <- paste0("csse_covid_19_daily_reports/", date_str, ".csv")
    covid_data <- read.csv(file_path)
    covid_data <- covid_data %>% filter(Country_Region == "US")
    covid_data <- covid_data %>% filter(Lat != "NA")
    covid_points <- st_as_sf(covid_data, coords = c("Long_","Lat"), crs = 4326)
    full_data <- st_join(gdf,covid_points)
    population <- full_data$population
    full_data$confirmed_rate_10000 <- full_data$Confirmed / population * 10000
    full_data$death_rate_10000 <- full_data$Deaths / population * 10000
    full_data$active_rate_10000 <- full_data$Active / population * 10000
    full_data
  })
  
  lmfit_reactive <- reactive({
    type.col1 <- covid_val[which(covid_key==input$covid_type)]
    type.col2 <- demographic_val[which(demographic_key==input$demographic_type)]
    
    covid_variable <- full_data()[[type.col1]]
    demographic_variable <- full_data()[[type.col2]]
    
    df <- data.frame(x=demographic_variable,y=covid_variable)
    df <- df %>% dplyr::filter(!is.na(x)) %>% 
      dplyr::filter(!is.na(y)) %>% 
      dplyr::filter(y != 0) %>%
      dplyr::filter(x != 0)
    
    df$log_x <- log(df$x)
    df$log_y <- log(df$y)
    
    lmfit <- lm(data = df, log_y~log_x)
    lmfit
  })
  
  
  output$covid_map <- renderLeaflet({
    
    type.col <- covid_val[which(covid_key==input$covid_type)]
    full_data <- full_data()[which(!is.na(full_data()[[type.col]])),]
    full_data <- full_data[which(full_data[[type.col]] != 0),]
    
    
    breaks_qt <- classIntervals(full_data[[type.col]], n = 4, style = "quantile")
    pal_fun <- colorQuantile("RdBu", NULL, n = 4, reverse = TRUE)
    
    enter_str <- paste0("<strong>%s</strong><br/>", input$covid_type, ": %g")
    labels <- sprintf(
      enter_str,
      full_data[["NAME"]], full_data[[type.col]]
    ) %>% lapply(htmltools::HTML)
    
    tag.map.title <- tags$style(HTML("
                                     .leaflet-control.map-title { 
                                     transform: translate(-50%,20%);
                                     position: fixed !important;
                                     left: 50%;
                                     text-align: center;
                                     padding-left: 10px; 
                                     padding-right: 10px; 
                                     background: rgba(255,255,255,0.75);
                                     font-weight: bold;
                                     font-size: 80px;
                                     }
                                     "))
    
    title <- tags$div(
      tag.map.title, HTML("Covid-19 Map")
    )  
    
    leaflet(full_data) %>%
      setView(-96, 37.8, 3) %>%
      addPolygons(
        stroke = TRUE,
        color = "black",
        weight = .5,
        fillColor = ~pal_fun(full_data[[type.col]]), # set fill color with function from above and value
        fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
        label = labels) %>%
      addLegend("bottomright", 
                colors = rev(brewer.pal(4, "RdBu")), 
                labels = paste0(format(breaks_qt$brks[-5], digits = 6)," to ", format(breaks_qt$brks[-1], digits = 6)),
                title =  input$covid_type) %>%
      addTiles() %>%
      addControl(title, position = "topright")
    
  })
  
  output$demographic_map <- renderLeaflet({
    
    type.col <- demographic_val[which(demographic_key==input$demographic_type)]
    
    breaks_qt <- classIntervals(gdf[[type.col]], n = 5, style = "quantile")
    pal_fun <- colorQuantile("Blues", NULL, n = 5)
    breaks_qt$brks[1] <- 0
    
    enter_str <- paste0("<strong>%s</strong><br/>", input$demographic_type, ": %g")
    labels <- sprintf(
      enter_str,
      gdf[["NAME"]], gdf[[type.col]]
    ) %>% lapply(htmltools::HTML)
    
    tag.map.title <- tags$style(HTML("
                                     .leaflet-control.map-title { 
                                     transform: translate(-50%,20%);
                                     position: fixed !important;
                                     left: 50%;
                                     text-align: center;
                                     padding-left: 10px; 
                                     padding-right: 10px; 
                                     background: rgba(255,255,255,0.75);
                                     font-weight: bold;
                                     font-size: 80px;
                                     }
                                     "))
    
    title <- tags$div(
      tag.map.title, HTML("Demographic Map")
    )  
    
    leaflet(gdf) %>%
      setView(-96, 37.8, 3) %>%
      addPolygons(
        stroke = TRUE,
        color = "black",
        weight = .5,
        fillColor = ~pal_fun(gdf[[type.col]]), # set fill color with function from above and value
        fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
        label = labels) %>%
      addLegend("bottomright", 
                colors = brewer.pal(5, "Blues"), 
                labels = paste0(format(breaks_qt$brks[-6], digits = 4), " to ", format(breaks_qt$brks[-1], digits = 4)),
                title =  input$demographic_type) %>%
      addTiles() %>%
      addControl(title, position = "topright")
  })
  
  output$covid_histogram <- renderPlotly({
    
    
    type.col <- covid_val[which(covid_key==input$covid_type)]
    x <- full_data[[type.col]]
    plot_ly(x = log(x),type = "histogram")
  }
  )
  
  
  
  output$scatterplot <- renderPlotly({
    
    full_data <- full_data()
    
    type.col1 <- covid_val[which(covid_key==input$covid_type)]
    type.col2 <- demographic_val[which(demographic_key==input$demographic_type)]
    
    
    covid_variable <- full_data[[type.col1]]
    demographic_variable <- full_data[[type.col2]]
    
    
    df <- data.frame(x=demographic_variable,y=covid_variable)
    
    df <- df %>% dplyr::filter(!is.na(x)) %>% 
      dplyr::filter(!is.na(y)) %>% 
      dplyr::filter(y != 0) %>%
      dplyr::filter(x != 0)
    
    df$log_x <- log(df$x)
    df$log_y <- log(df$y)
    
    lmfit <- lm(data = df, log_y~log_x)
    df$fv <- fitted.values(lmfit)
    
    xlab <- list(
      title = paste0("Logarithm of ", input$demographic_type)
    )
    
    ylab = list(
      title = paste0("Logarithm of ", input$covid_type)
    )
    
    plot_ly(data = df,x=~log_x,y=~log_y, mode = "markers") %>%
      add_markers(color = I("red"), stroke = I("black"), span = I(1)) %>%
      add_trace(x = ~log_x, y = ~fv, mode = "lines") %>%
      layout(showlegend = F, xaxis = xlab, yaxis = ylab)
  })
  
  output$covid_histogram <-  renderPlotly({
    
    full_data <- full_data()
    type.col <- covid_val[which(covid_key==input$covid_type)]
    
    
    covid_variable <- full_data[[type.col]]
    
    xlab = list(
      title = paste0("Logarithm of ", input$covid_type)
    )
    plot_ly(x = log(covid_variable),type = "histogram",
            marker = list(color = "red",
                          line = list(color = "black",
                                      width = .5))) %>%
      layout(xaxis = xlab)
    
  })
  
  
  
  output$demographic_histogram <-  renderPlotly({
    
    full_data <- full_data()
    type.col <- demographic_val[which(demographic_key==input$demographic_type)]
    
    demographic_variable <- full_data[[type.col]]
    
    xlab <- list(
      title = paste0("Logarithm of ", input$demographic_type)
    )
    
    plot_ly(x = log(demographic_variable),
            type = "histogram",
            marker = list(color = "blue",
                          line = list(color = "black",
                                      width = .5))) %>%
      layout(xaxis = xlab)
    
  })
  
  
  
  
  output$case_count <- renderText({
    paste0(prettyNum((sum(full_data()$Confirmed,na.rm = TRUE)), big.mark = ","), " Cases")
  })
  
  
  output$death_count <- renderText({
    paste0(prettyNum((sum(full_data()$Deaths, na.rm = TRUE)), big.mark = ","), " Deaths")
  })
  
  output$slope <- renderText({
    slope <- format(lmfit_reactive()$coefficients[[2]], digits = 4)
    paste0("Slope: ", slope)
  })
  
  output$intercept <- renderText({
    intercept <- format(lmfit_reactive()$coefficients[[1]], digits = 4)
    paste0("Intercept: ", intercept)
  })
  
  
  output$correlation <- renderText({
    rsquared <- format(summary(lmfit_reactive())$r.squared, digits = 4)
    paste0("RSquared: ", rsquared)
  })
  
  output$variables <- renderText({
    paste0(input$covid_type, " vs ", input$demographic_type)
  })
  
  
  
  }

shinyApp(ui = ui, server=server)


