# Title:              MPG Application (Updated Version)
# Programmer:         Adam Ng
# Original Date:      4/25/2022
# Update Date:        5/28/2022
# Purpose:            Create an application that gathers MPG data for vehicles 
#                     for the current year based on vehicle information input 
#                     by the user. 
#                     Webscrape gas prices online. Determines price of gasoline
#                     for a vehicle by state.
#                     Only works for vehicles made from 2000 or later

# Establish two-digit year
yr <- 22

# Convert two-digit year to character and append "20" to the beginning
yr4 <- as.numeric(paste("20", as.character(yr), sep=""))

# Load in following libraries. Purposes listed for each

# Standard data manipulation libraries for working with/cleaning up data
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(tibble) # has add_column function
library(naniar) # has replace_with_na function

# Webscraping libraries
library(httr) # Also good for loading Excel files from internet (this is 
# technically not webscraping)
library(rvest)
library(robotstxt) # Use this to call function to determine if we're allowed to
# scrape the selected URL

# Used when loading Excel files from website
library(readxl)

# Libraries for the Shiny application
library(shiny)
library(shinythemes)
library(DT)

# Libraries for graphs used in Shiny application
library(plotly)
library(ggplot2)

#### Part 1: Load and clean Excel Fuel Economy Data

# Load xlsx and xls files from www.fueleconomy.gov
# This is faster outside of the loop. However, since it works and the goal
# of this project involves expanding R skills, I will leave this in until
# performing the next update
fueleco <- function(year){
  count <- 0
  iterate <- year
  year4 <- paste("20", as.character(year), sep = "")
  year4 <- as.numeric(year4)
  # Loop on xlsx files first (2011-Present are xlsx)
  while (iterate > 10){
    year2 <- as.character(iterate)
    url <- 
      paste("https://www.fueleconomy.gov/feg/EPAGreenGuide/xls/all_alpha_",
            year2, ".xlsx", sep = "")
    GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
    df <- read_excel(tf) %>%
      add_column(Year = year4, .before = 1)
    if ("Comb CO2" %in% colnames(df)){
      df <- df %>%
        within(rm("Comb CO2"))
    }
    if ("Stnd Description" %in% colnames(df)){
      df <- df %>%
        within(rm("Stnd Description"))
    }
    if (iterate == year){
      out_frame <- df
    } else{
      names(df) <- names(out_frame)
      out_frame <- rbind(out_frame, df)
    }
    iterate <- iterate - 1
    year4 <- year4 - 1
  }
  # Loop on xls files which go back to 2000
  while (iterate > -1){
    year2 <- as.character(iterate)
    if (iterate > 9){
      url <- 
        paste("https://www.fueleconomy.gov/feg/EPAGreenGuide/xls/all_alpha_",
              year2, ".xls", sep = "")
    } else {
      url <- 
        paste("https://www.fueleconomy.gov/feg/EPAGreenGuide/xls/all_alpha_",
              "0", year2, ".xls", sep = "")
    }
    GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
    df <- read_excel(tf) %>%
      add_column(Year = year4, .before = 1)
    if ("Comb CO2" %in% colnames(df)){
      df <- df %>%
        within(rm("Comb CO2"))
    }
    if ("Stnd Description" %in% colnames(df)){
      df <- df %>%
        within(rm("Stnd Description"))
    }
    if("Unadj Cmb MPG" %in% colnames(df)){
      df <- df %>%
        within(rm("Unadj Cmb MPG"))
    }
    if ("FE Calc Appr" %in% colnames(df)){
      df <- df %>%
        within(rm("FE Calc Appr"))
    }
    names(df) <- names(out_frame)
    out_frame <- rbind(out_frame, df)
    iterate <- iterate - 1
    year4 <- year4 - 1
  }
  return(out_frame)
}

# Load in Excel files from fueleconomy.gov using function and assign it to a 
# dataframe called df (named as such for ease of reference)
df <- fueleco(yr)

# We need to clean up the data
# We won't convert to MPG to numeric just yet because of the electric cars
# with weirdly written MPG values.
# NOTE: EVERY SINGLE FUNCTION CALLED HERE COMES FROM DPLYR
df <- df %>%
  # Rename columns with spaces in the names (happens often with Excel)
  rename(Cert_Region = `Cert Region`,
         Air_Pollution_Score = `Air Pollution Score`,
         City_MPG = `City MPG`,
         Hwy_MPG = `Hwy MPG`,
         Cmb_MPG = `Cmb MPG`,
         Greenhouse_Gas_Score = `Greenhouse Gas Score`,
         Underhood_ID = `Underhood ID`,
         Veh_Class = `Veh Class`) %>%
  # We don't need all the columns here; select the most relevant and ones that
  # allow for the most distinction between vehicles
  select(Year, Model, Displ, Cyl, Trans, Drive, Fuel, Veh_Class,
         City_MPG, Hwy_MPG, Cmb_MPG, SmartWay) %>%
  # Remove duplicate rows by first listing all relevant rows to check
  # followed by slicing
  
  # NOTE: Initially included smartway but stopped because some rows were 
  # exactly the same but had different SmartWay values
  group_by(Year, Model, Displ, Cyl, Trans, Drive, Fuel, Veh_Class,
           City_MPG, Hwy_MPG, Cmb_MPG) %>%
  slice(1) %>%
  # Reorder the data
  arrange(desc(Year))

# Now we wish to perform further data wrangling by changing the data itself.
# We will replace all specialized N/A and N/A* values to R's built in NA
# for ease of manipulation using the naniar package. 
# From there, we will covert character the necessary columns into numeric 
# format. We will also transform values for consistency
# After this, we will split up make and model into separate columns
# Finally, we will replace all character NAs to N/A, for use when creating the
# Shiny app (R built in NA won't show up in Shiny dropdown)

# List different ways NA is stored in the dataframe
na_strings <- c("N/A", "N/A*")

df <- df %>%
  # Use naniar package for this
  replace_with_na(replace = list(Displ = na_strings,
                                 Cyl = na_strings,
                                 Trans = na_strings,
                                 Drive = na_strings,
                                 Veh_Class = na_strings,
                                 City_MPG = na_strings,
                                 Hwy_MPG = na_strings,
                                 Cmb_MPG = na_strings,
                                 SmartWay = na_strings)) %>%
  # Remove cases with missing MPG values
  filter(!is.na(Cmb_MPG) & !(is.na(Hwy_MPG)) | !(is.na(City_MPG))) %>%
  # Transform select columns from character to numeric and transform
  # select values for consistency
  transform(
    Cyl = as.numeric(
      ifelse(substr(Cyl, 1, 1) == "(", 
             trimws(substr(Cyl, 2, 3), which = c("right")), 
             Cyl)),
    Displ = round(as.numeric(Displ), digits = 1)) %>%
  # Update SmartWay for consistency. Must do this in different transform
  # statements otheriwse an error will occur
  transform(SmartWay = ifelse(SmartWay == "yes", "Yes", SmartWay)) %>%
  transform(SmartWay = ifelse(SmartWay == "no", "No", SmartWay)) %>%
  transform(SmartWay = ifelse(SmartWay == "Elite", "Electric", SmartWay)) %>%
  # If MPG values for certain PHEV have gas and electric combined
  # values, go with the higher value. Do this in separate transform
  # statements because we call the same variable several times
  transform(Hwy_MPG = as.numeric(ifelse(substr(Hwy_MPG,3,3) == "/",
                                        substr(Hwy_MPG,4,5),
                                        Hwy_MPG))) %>%
  transform(Cmb_MPG = ifelse(substr(Cmb_MPG,3,3) == "/",
                             substr(Cmb_MPG,4,5),
                             Cmb_MPG)) %>%
  transform(Cmb_MPG = ifelse(substr(Cmb_MPG,2,2) == "/",
                             substr(Cmb_MPG,3,4),
                             Cmb_MPG)) %>%
  transform(City_MPG = ifelse(substr(City_MPG,3,3) == "/",
                              substr(City_MPG,4,5),
                              City_MPG)) %>%
  transform(City_MPG = ifelse(substr(City_MPG,2,2) == "/",
                              substr(City_MPG,3,4),
                              City_MPG)) %>%
  transform(Cmb_MPG = as.numeric(Cmb_MPG)) %>%
  transform(City_MPG = as.numeric(City_MPG)) %>%
  # Split up cars into Make and Model
  separate(Model, c("Make", "Model"), sep = " ", 
           extra = "merge", fill = "right") %>%
  # For ease of reference, make all Make and Model names capital
  mutate(Make = toupper(Make),
         Model = toupper(Model),
         # Change NA into another value so that they show up in Shiny dropdown
         Displ = replace_na(Displ, 0),
         Cyl = replace_na(Cyl, 0),
         Trans = replace_na(Trans, "NA"),
         Drive = replace_na(Drive, "NA")) %>%
  # Finally, remove leading and trailing blanks for all character values
  # in the table
  mutate_if(is.character, str_trim)

# Finally, we need to update some of the car names that didn't come out
# properly. We do it by creating three separate dataframes from the df
# for each case

df_hold1 <- df %>%
  # Read in the Vehicle Names that need updating
  filter(Make %in% c("ALFA", "ASTON", "LAND", "MOBILITY")) %>%
  separate(Model, c("hold", "Model"), sep = " ",
           extra = "merge", fill = "right") %>%
  unite("Make", Make:hold, sep = " ", remove = TRUE)

# Vehicle Production Group Needs additional updates
df_hold2 <- df %>%
  filter(Make == "VEHICLE") %>%
  separate(Model, c("hold", "Model"), sep = " ",
           extra = "merge", fill = "right") %>%
  unite("Make", Make:hold, sep = " ", remove = TRUE) %>%
  separate(Model, c("hold", "Model"), sep = " ",
           extra = "merge", fill = "right") %>%
  unite("Make", Make:hold, sep = " ", remove = TRUE)

# Grab everything that we didn't need to update
df_hold3 <- df %>% 
  filter(!(Make %in% c("ALFA", "ASTON", "LAND", "MOBILITY", "VEHICLE")))

# Merge the three dataframes back together
df <- df_hold1 %>%
  full_join(df_hold2) %>%
  full_join(df_hold3) %>%
  arrange(desc(Year), Make)

# Don't need these dataframes taking up extra space
rm(df_hold1, df_hold2, df_hold3)


#### Part 2: Webscrape AAA Average Gas Prices by State

# We'll webscrape the gas prices across the United States

# First make sure we are allowed to scrape 
paths_allowed(paths = c(
  "https://gasprices.aaa.com/state-gas-price-averages/"
))
# This returns the value "TRUE", so we can continue

# Normally, this would be simple, but read_html() wouldn't work without all
# those extra lines of code. Something to look into further
# This gets the AAA table with average gas prices around the US
# We'll scrape and wrangle the data in one step
# We add a hashtag in front of sortable because this is an HTML ID
webpage <- "https://gasprices.aaa.com/state-gas-price-averages/"
gas_prices <- webpage %>%
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>%
  read_html() %>%
  html_element("#sortable") %>%
  html_table() %>%
  rename(Mid = `Mid-Grade`) %>%
  mutate(Regular = Regular %>% str_remove_all("\\$") %>% as.numeric(),
         Mid = Mid %>% str_remove_all("\\$") %>% as.numeric(),
         Premium = Premium %>% str_remove_all("\\$") %>% as.numeric(),
         Diesel = Diesel %>% str_remove_all("\\$") %>% as.numeric())


#### Part 3: Webscrape Average Miles Driven By State and Number of Drivers
#            in each State

# Get number of miles driven by state

# Let's make sure we're allowed to scrape from this website
paths_allowed(paths = c(
  "https://www.fhwa.dot.gov/policyinformation/statistics/2019/vm2.cfm"
))
# This returns TRUE so we can continue

webpage <- "https://www.fhwa.dot.gov/policyinformation/statistics/2019/vm2.cfm"

# Combine webscraping and data manipulation into one step
state_miles <- webpage %>%
  read_html() %>%
  html_table() %>% .[[1]] %>%
  select(STATE, TOTAL) %>%
  filter(!(STATE == "STATE")) %>%
  # Plotly masked rename in dplyr, need to explicitly call it
  dplyr::rename(State = STATE, Miles = TOTAL) %>%
  mutate(Miles = Miles %>% str_remove_all("\\,") %>% as.numeric(),
         State = str_replace(State, "\\(2\\)", ""),
         State = str_replace(State, "\\(3\\)", ""),
         State = str_replace(State, "\\(4\\)", ""),
         State = str_replace(State, "Dist. of Col.", 
                             "District of Columbia")) %>%
  mutate_if(is.character, str_trim)


# Then let's gather the total number of drivers in each state, listed by age
# Again, check if we're allowed to scrape the data
paths_allowed(paths = c(
  "https://www.fhwa.dot.gov/policyinformation/statistics/2019/dl22.cfm"
))

# It's TRUE so we can continue
webpage <- "https://www.fhwa.dot.gov/policyinformation/statistics/2019/dl22.cfm"

# Use this to clean up the data values for certain states
remove <- cat(c("(2)", "(3)", "(4)"), "\n")

# Scrape and manipulate the data all in one step
driver_totals <- webpage %>%
  read_html() %>%
  html_table %>% .[[5]] %>%
  select(STATE, TOTAL) %>%
  rename(State = STATE, Drivers = TOTAL) %>%
  mutate(State = str_replace(State, "\\(2\\)", ""),
         State = str_replace(State, "\\(3\\)", ""),
         State = str_replace(State, "\\(4\\)", ""),
         State = str_replace(State, "Dist. of Col.", 
                             "District of Columbia"),
         State = str_replace(State, "Total", "U.S. Total"),
         Drivers = Drivers %>% str_remove_all("\\,") %>% as.numeric()) %>%
  mutate_if(is.character, str_trim)

# Merge these tables together by state to get mile averages. 
# This means we will drop the Puerto Rico and Grand Total rows from the 
# state_miles data frame
avg_miles_state <- left_join(driver_totals, state_miles,
                             by=c("State" = "State")) %>%
  rename(Annual_Miles = Miles) %>%
  mutate(ann_avg_miles = 
           round((Annual_Miles/Drivers) * 1000000, digits = 0),
         monthly_avg_miles = 
           (round(((Annual_Miles/Drivers) * 1000000)/12, digits = 0)))


#### Part 4: Create list with fuel costs for each vehicle by state

# Create a vector of states
state_list <- gas_prices[['State']]

# Iterate through this vector to create a list of 51 different elements of 
# data for each state. We will also use this to calculate the cost of gas per
# vehicle based on 

# Should be able to do this outside of a for loop. Will come back to this in
# future updates
i <- 1
j <- 1
state_cars <- list()
for (i in state_list){
  
  # Iterate through variables that gather the gas price for each state
  # as well as average monthly and annual mileage
  reg <- gas_prices$Regular[j]
  mid <- gas_prices$Mid[j]
  premium <- gas_prices$Premium[j]
  diesel <- gas_prices$Diesel[j]
  
  monthly_avg <- avg_miles_state$monthly_avg_miles[j]
  annual_avg <- avg_miles_state$ann_avg_miles[j]
  
  # Increase iteration
  j <- j + 1
  
  # Create list of cars with gas prices for each state
  # Gas cost per month equation: 
  # Cost of Gas per Month = (Avg Monthly Miles/MPG) * Cost of Gas
  # Cost of Gas per Year = (Avg Yearly Miles/MPG) * Cost of Gas
  # Keep in mind different vehicles take different fuel
  state_cars[[i]] <- df %>%
    mutate(State = i,
           Monthly_City_Cost_Reg =  
             ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                  "CNG", "diesel")), 
                    (monthly_avg/City_MPG) * reg, NA),
           Monthly_City_Cost_Mid = 
             ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                  "CNG", "diesel")),
                    (monthly_avg/City_MPG) * mid, NA),
           Monthly_City_Cost_Premium = 
             ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                  "CNG", "diesel")),
                    (monthly_avg/City_MPG) * premium, NA),
           Monthly_City_Cost_Diesel = 
             ifelse(Fuel %in% c("Diesel", "diesel"),
                    (monthly_avg/City_MPG) * diesel, NA),
           Monthly_Hwy_Cost_Reg = 
             ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                  "CNG", "diesel")),
                    (monthly_avg/Hwy_MPG) * reg, NA),
           Monthly_Hwy_Cost_Mid = 
             ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                  "CNG", "diesel")),
                    (monthly_avg/Hwy_MPG) * mid, NA),
           Monthly_Hwy_Cost_Premium = 
             ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                  "CNG", "diesel")),
                    (monthly_avg/Hwy_MPG) * premium, NA),
           Monthly_Hwy_Cost_Diesel = 
             ifelse(Fuel %in% c("Diesel", "diesel"),
                    (monthly_avg/Hwy_MPG) * diesel, NA),
           Monthly_Cmb_Cost_Reg =  
             ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                  "CNG", "diesel")), 
                    (monthly_avg/Cmb_MPG) * reg, NA),
           Monthly_Cmb_Cost_Mid = 
             ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                  "CNG", "diesel")),
                    (monthly_avg/Cmb_MPG) * mid, NA),
           Monthly_Cmb_Cost_Premium = 
             ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                  "CNG", "diesel")),
                    (monthly_avg/Cmb_MPG) * premium, NA),
           Monthly_Cmb_Cost_Diesel = 
             ifelse(Fuel %in% c("Diesel", "diesel"),
                    (monthly_avg/Cmb_MPG) * diesel, NA),
           # New inclusion for the second page of the Shiny App - 6.13.22
           reg_avg = ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                          "CNG", "diesel")),
                            reg, NA),
           mid_avg = ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                          "CNG", "diesel")),
                            mid, NA),
           premium_avg = ifelse(!(Fuel %in% c("Electricity", "Diesel", "Hydrogen",
                                              "CNG", "diesel")),
                                premium, NA),
           diesel_avg = ifelse(Fuel %in% c("Diesel", "diesel"),
                               reg, NA)) %>%
    relocate(State, .after = Year) %>%
    # Get rid of Electric, CNG, and Hydrogen cars
    filter(!(Fuel %in% c("Electricity", "Hydrogen", "CNG")))
  
}


#### Part 5: Create Shiny Application

# The UI establishes the dropdown selection options from the state_list vector
# and the state_cars list of dataframes when linked to it through the server
# component.
# The UI also customizes the overall look of the Shiny web application
# NOTE: The UI cannot be saved to the global environment. Must run the 
# program via the "Run App" button
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  # Formats the sidebarPanel text size
  tags$style(type='text/css', 
             ".selectize-input { font-size: 12px; line-height: 12px;} 
  .selectize-dropdown { font-size: 10px; line-height: 12px; }
  label { font-size: 14px; line-height: 14px}"),
  navbarPage("Fuel Price Application: ",
             tabPanel("General Information",
                      sidebarPanel(width = 3,
                                   # HTML fxn w/ <h3>controls size of sidebarPanel title
                                   HTML("<h3> Select Parameters <h3>"),
                                   selectizeInput("State", label = "State: ", 
                                                  choices = state_list,
                                                  selected = 1),
                                   selectizeInput("Year", label = "Year: ", 
                                                  choices = NULL, 
                                                  selected = ""),
                                   
                                   selectizeInput("Make", label = "Manufacturer: ", 
                                                  choices = NULL,
                                                  selected = ""),
                                   
                                   selectizeInput("Model", label = "Model: ", 
                                                  choices = NULL,
                                                  selected = ""),
                                   
                                   selectizeInput("Displ", 
                                                  label = "Engine Displacement: ", 
                                                  choices = NULL, selected = ""),
                                   
                                   selectizeInput("Cyl", 
                                                  label = 
                                                    "Number of Enigine Cylinders: ", 
                                                  choices = NULL, selected = ""),
                                   
                                   selectizeInput("Trans", label = 
                                                    "Transmission Type: ", 
                                                  choices = NULL, selected = ""),
                                   
                                   selectizeInput("Drive", label = "Drivetrain: ", 
                                                  choices = NULL,
                                                  selected = "")#,
                                   #actionButton("submitbutton",
                                   #"Submit",
                                   #class = "btn btn-primary")
                                   
                      ),
                      mainPanel(tags$label(h3("Monthly Fuel Cost (Based on Avg Miles per Driver)")),
                                div(DT::dataTableOutput("states_filter")),
                                # Creates space between datatable and graph
                                tags$label(h3("")),
                                plotlyOutput('mpg_over_time'))
             ),
             # Create second panel for a more specific calc
             tabPanel("Calculator", 
                      sidebarPanel(width = 3,
                                   # HTML fxn w/ <h3>controls size of sidebarPanel title
                                   HTML("<h3> Select Parameters <h3>"),
                                   selectizeInput("State2", label = "State: ", 
                                                  choices = state_list,
                                                  selected = 1),
                                   selectizeInput("Year2", label = "Year: ", 
                                                  choices = NULL, 
                                                  selected = ""),
                                   
                                   selectizeInput("Make2", label = "Manufacturer: ", 
                                                  choices = NULL,
                                                  selected = ""),
                                   
                                   selectizeInput("Model2", label = "Model: ", 
                                                  choices = NULL,
                                                  selected = ""),
                                   
                                   selectizeInput("Displ2", 
                                                  label = "Engine Displacement: ", 
                                                  choices = NULL, selected = ""),
                                   
                                   selectizeInput("Cyl2", 
                                                  label = 
                                                    "Number of Enigine Cylinders: ", 
                                                  choices = NULL, selected = ""),
                                   
                                   selectizeInput("Trans2", label = 
                                                    "Transmission Type: ", 
                                                  choices = NULL, selected = ""),
                                   
                                   selectizeInput("Drive2", label = "Drivetrain: ", 
                                                  choices = NULL,
                                                  selected = "")),
                      mainPanel(tags$label(h3("User-Specific Monthly Gas Cost Estimator")),
                                # HAVING THIS IN THE MAIN PANEL IS CAUSING ISSUES
                                # WHEN CHANGING STATES/CARS. PUTTING IT IN THE
                                # SIDEBARPANEL WORKS, BUT IT LOOKS MORE UGLY
                                # MUST FIND A WAY TO FIX THIS
                                numericInput("miles", "Miles Driven Per Month: ",
                                             500, 1, 1000000),
                                div(DT::dataTableOutput("user_prices")))
             ),
             # Create a third panel with markdown document describing purpose
             # of the application and the origin of the data presented
             tabPanel("About",
                      div(includeMarkdown("about.md"), align = "justify"))
  ))

# Establishes server component for gas app
server <- function(input, output, session) {
  
  values <- reactiveValues(state_cars = NULL)
  
  df_states <- reactive({
    
    data <- state_cars[[input$State]]
    
    return(data)
    
  })
  
  observeEvent(
    input$State,
    updateSelectizeInput(session, "Year", "Year: ",
                         choices = unique(df_states()$Year
                                          [df_states()$State==input$State]))
  )
  
  observeEvent(
    input$Year,
    updateSelectizeInput(session, "Make", "Manufacturer: ",
                         choices = 
                           unique(df_states()$Make[
                             df_states()$Year==input$Year & 
                               df_states()$State==input$State]),
                         server = TRUE)
  )
  observeEvent(
    input$Make,
    updateSelectizeInput(session, "Model", "Model: ",
                         choices = 
                           unique(df_states()$Model[df_states()$Make==input$Make &
                                                      df_states()$Year==input$Year & 
                                                      df_states()$State==input$State]),
                         server = TRUE)
  )
  
  observeEvent(
    input$Model,
    updateSelectizeInput(session, "Displ", 
                         label = "Engine Displacement: ",
                         choices = 
                           unique(
                             df_states()$Displ[df_states()$Model==input$Model &
                                                 df_states()$Make==input$Make &
                                                 df_states()$Year==input$Year &
                                                 df_states()$State==input$State]),
                         server = TRUE)
  )
  
  observeEvent(
    input$Displ,
    updateSelectizeInput(session, "Cyl", 
                         label = "Number of Enigine Cylinders: ",
                         choices = 
                           unique(
                             df_states()$Cyl[df_states()$Displ==input$Displ &
                                               df_states()$Model==input$Model & 
                                               df_states()$Make==input$Make & 
                                               df_states()$Year==input$Year &
                                               df_states()$State==input$State]),
                         server = TRUE)
  )
  
  observeEvent(
    input$Cyl,
    updateSelectizeInput(session, "Trans", 
                         label = "Transmission Type: ",
                         choices = 
                           unique(
                             df_states()$Trans[df_states()$Cyl==input$Cyl &
                                                 df_states()$Displ==input$Displ & 
                                                 df_states()$Model==input$Model & 
                                                 df_states()$Make==input$Make & 
                                                 df_states()$Year==input$Year &
                                                 df_states()$State==input$State]),
                         server = TRUE)
  )
  
  observeEvent(
    input$Trans,
    updateSelectizeInput(session, "Drive", 
                         label = "Drivetrain: ",
                         choices = 
                           unique(
                             df_states()$Drive[df_states()$Trans==input$Trans &
                                                 df_states()$Cyl==input$Cyl & 
                                                 df_states()$Displ==input$Displ & 
                                                 df_states()$Model==input$Model & 
                                                 df_states()$Make==input$Make & 
                                                 df_states()$Year==input$Year & 
                                                 df_states()$State==input$State]),
                         server = TRUE)
  )
  
  # Creates reactive dataframe to filter on using user input
  dat <- reactive({
    row1 <- row.names(df_states()[df_states()$State==input$State &
                                    df_states()$Year==input$Year &
                                    df_states()$Make==input$Make & 
                                    df_states()$Model==input$Model &
                                    df_states()$Displ==input$Displ &
                                    df_states()$Cyl==input$Cyl &
                                    df_states()$Trans==input$Trans &
                                    df_states()$Drive==input$Drive, ])
    
    # This prevents error from appearing when changing inputs in table
    if (length(row1) == 0){
      return(NULL)
    }
    
    # Since this references a specific row number as a character, we need to
    # convert it to numeric
    row1 <- as.numeric(row1)
    
    # Could just reference the row and be done with it, but won't do that
    # check <- df_states[row1,]
    
    # Monthly City Cost
    out1 <- round(df_states()$Monthly_City_Cost_Reg[row1], digits = 2)
    out2 <- round(df_states()$Monthly_City_Cost_Mid[row1], digits = 2)
    out3 <- round(df_states()$Monthly_City_Cost_Premium[row1], digits = 2)
    out4 <- round(df_states()$Monthly_City_Cost_Diesel[row1], digits = 2)
    
    # Monthly Highway Cost
    out5 <- round(df_states()$Monthly_Hwy_Cost_Reg[row1], digits = 2)
    out6 <- round(df_states()$Monthly_Hwy_Cost_Mid[row1], digits = 2)
    out7 <- round(df_states()$Monthly_Hwy_Cost_Premium[row1], digits = 2)
    out8 <- round(df_states()$Monthly_Hwy_Cost_Diesel[row1], digits = 2)
    
    # Monthly Combined Cost
    out9 <- round(df_states()$Monthly_Cmb_Cost_Reg[row1], digits = 2)
    out10 <- round(df_states()$Monthly_Cmb_Cost_Mid[row1], digits = 2)
    out11 <- round(df_states()$Monthly_Cmb_Cost_Premium[row1], digits = 2)
    out12 <- round(df_states()$Monthly_Cmb_Cost_Diesel[row1], digits = 2)
    
    # We will keep spaces in the column names since we want it to 
    # look presentable and won't reference the table later on
    val <- data.frame("Fuel Type" = c("Regular", "Midgrade", 
                                      "Premium", "Diesel"), 
                      "Monthly City Cost" = c(out1, out2, out3, out4),
                      "Monthly Highway Cost" = c(out5, out6, out7, out8),
                      "Monthly Combined Cost" = c(out9, out10, out11,
                                                  out12),
                      check.names = FALSE)
    
    #val <- data.frame(check)
    
    # Necessary to return reactive dataframe
    return(val)
  })
  
  output$states_filter <- DT::renderDataTable(dat(),
                                              options = 
                                                list(lengthChange = FALSE,
                                                     # Remove search filter
                                                     bFilter = 0,
                                                     # Remove count of 
                                                     # records
                                                     bInfo = 0,
                                                     # Remove sorting
                                                     ordering = F,
                                                     # Get rid of previous
                                                     # and next options
                                                     # This actually also
                                                     # removes count of 
                                                     # records, search filter
                                                     # etc. but it doesn't
                                                     # get rid of sorting
                                                     dom = "t"))
  
  # Create reactive dataframe for graph
  graph_df <- reactive({
    
    data <- df_states()[df_states()$State==input$State &
                          df_states()$Make==input$Make & 
                          df_states()$Model==input$Model &
                          df_states()$Displ==input$Displ &
                          df_states()$Cyl==input$Cyl &
                          df_states()$Trans==input$Trans &
                          df_states()$Drive==input$Drive, ]
    
    return(data)
    
  })
  
  output$mpg_over_time <- renderPlotly({
    
    data <- graph_df()
    plot_ly(data,
            x = ~as.numeric(unlist(data$Year)), 
            y = ~as.numeric(unlist(data$Cmb_MPG)), 
            type = 'scatter', mode = 'lines+markers', 
            line = list(color = '#FF5733',width = 2),
            marker = list(color = '#FF5733',width = 3)) %>%
      layout(title = "Change in Combined City and Highway MPG Over Time",
             xaxis = list(title = "Year",
                          tickmode = "linear"),
             yaxis = list(title = "Combined MPG"))
    
  })
  
  ### Begin output for the second, more specific page
  df_states2 <- reactive({
    
    data <- state_cars[[input$State2]]
    
    return(data)
    
  })
  
  observeEvent(
    input$State2,
    updateSelectizeInput(session, "Year2", "Year: ",
                         choices = unique(df_states2()$Year
                                          [df_states2()$State==input$State2]))
  )
  
  observeEvent(
    input$Year2,
    updateSelectizeInput(session, "Make2", "Manufacturer: ",
                         choices = 
                           unique(df_states2()$Make[
                             df_states2()$Year==input$Year2 & 
                               df_states2()$State==input$State2]),
                         server = TRUE)
  )
  observeEvent(
    input$Make2,
    updateSelectizeInput(session, "Model2", "Model: ",
                         choices = 
                           unique(df_states2()$Model[df_states2()$Make==input$Make2 &
                                                       df_states2()$Year==input$Year2 & 
                                                       df_states2()$State==input$State2]),
                         server = TRUE)
  )
  
  observeEvent(
    input$Model2,
    updateSelectizeInput(session, "Displ2", 
                         label = "Engine Displacement: ",
                         choices = 
                           unique(
                             df_states2()$Displ[df_states2()$Model==input$Model2 &
                                                  df_states2()$Make==input$Make2 &
                                                  df_states2()$Year==input$Year2 &
                                                  df_states2()$State==input$State2]),
                         server = TRUE)
  )
  
  observeEvent(
    input$Displ2,
    updateSelectizeInput(session, "Cyl2", 
                         label = "Number of Enigine Cylinders: ",
                         choices = 
                           unique(
                             df_states2()$Cyl[df_states2()$Displ==input$Displ2 &
                                                df_states2()$Model==input$Model2 & 
                                                df_states2()$Make==input$Make2 & 
                                                df_states2()$Year==input$Year2 &
                                                df_states2()$State==input$State2]),
                         server = TRUE)
  )
  
  observeEvent(
    input$Cyl2,
    updateSelectizeInput(session, "Trans2", 
                         label = "Transmission Type: ",
                         choices = 
                           unique(
                             df_states2()$Trans[df_states2()$Cyl==input$Cyl2 &
                                                  df_states2()$Displ==input$Displ2 & 
                                                  df_states2()$Model==input$Model2 & 
                                                  df_states2()$Make==input$Make2 & 
                                                  df_states2()$Year==input$Year2 &
                                                  df_states2()$State==input$State2]),
                         server = TRUE)
  )
  
  observeEvent(
    input$Trans2,
    updateSelectizeInput(session, "Drive2", 
                         label = "Drivetrain: ",
                         choices = 
                           unique(
                             df_states2()$Drive[df_states2()$Trans==input$Trans2 &
                                                  df_states2()$Cyl==input$Cyl2 & 
                                                  df_states2()$Displ==input$Displ2 & 
                                                  df_states2()$Model==input$Model2 & 
                                                  df_states2()$Make==input$Make2 & 
                                                  df_states2()$Year==input$Year2 & 
                                                  df_states2()$State==input$State2]),
                         server = TRUE)
  )
  
  # Creates datatable with personalized monthly fuel price calculated
  dat2 <- reactive({
    
    row2 <- row.names(df_states2()[df_states2()$State==input$State2 &
                                     df_states2()$Year==input$Year2 &
                                     df_states2()$Make==input$Make2 & 
                                     df_states2()$Model==input$Model2 &
                                     df_states2()$Displ==input$Displ2 &
                                     df_states2()$Cyl==input$Cyl2 &
                                     df_states2()$Trans==input$Trans2 &
                                     df_states2()$Drive==input$Drive2, ])
    
    row2 <- as.numeric(row2)
    
    # This prevents error from appearing when changing inputs in table
    if (length(row2) == 0){
      return(NULL)
    }
    
    # Calculate gas price for each type of gasoline
    
    # Monthly City Cost
    out1 <- round((input$miles / df_states2()$City_MPG[row2]) * 
                    (df_states2()$reg_avg[row2]), digits = 2)
    out2 <- round((input$miles / df_states2()$City_MPG[row2]) * 
                    df_states2()$mid_avg[row2], digits = 2)
    out3 <- round((input$miles / df_states2()$City_MPG[row2]) * 
                    df_states2()$premium_avg[row2], digits = 2)
    out4 <- round((input$miles / df_states2()$City_MPG[row2]) * 
                    df_states2()$diesel_avg[row2], digits = 2)
    
    # Monthly Highway Cost
    out5 <- round((input$miles / df_states2()$Hwy_MPG[row2]) * 
                    df_states2()$reg_avg[row2], digits = 2)
    out6 <- round((input$miles / df_states2()$Hwy_MPG[row2]) * 
                    df_states2()$mid_avg[row2], digits = 2)
    out7 <- round((input$miles / df_states2()$Hwy_MPG[row2]) * 
                    df_states2()$premium_avg[row2], digits = 2)
    out8 <- round((input$miles / df_states2()$Hwy_MPG[row2]) * 
                    df_states2()$diesel_avg[row2], digits = 2)
    
    # Monthly Combined Cost
    out9 <- round((input$miles / df_states2()$Cmb_MPG[row2]) * 
                    df_states2()$reg_avg[row2], digits = 2)
    out10 <- round((input$miles / df_states2()$Cmb_MPG[row2]) * 
                     df_states2()$mid_avg[row2], digits = 2)
    out11 <- round((input$miles / df_states2()$Cmb_MPG[row2]) * 
                     df_states2()$premium_avg[row2], digits = 2)
    out12 <- round((input$miles / df_states2()$Cmb_MPG[row2]) * 
                     df_states2()$diesel_avg[row2], digits = 2)
    
    # Create user custom data table
    val2 <- data.frame("Fuel Type" = c("Regular", "Midgrade", 
                                       "Premium", "Diesel"), 
                       "Monthly City Cost" = c(out1, out2, out3, out4),
                       "Monthly Highway Cost" = c(out5, out6, out7, out8),
                       "Monthly Combined Cost" = c(out9, out10, out11,
                                                   out12),
                       check.names = FALSE)
    
    return(val2)
    
  })
  
  output$user_prices <- DT::renderDataTable(dat2(),
                                            options = 
                                              list(# Remove sorting
                                                ordering = F,
                                                # Get rid of previous
                                                # and next options
                                                # This actually also
                                                # removes count of 
                                                # records, search filter
                                                # etc. but it doesn't
                                                # get rid of sorting
                                                dom = "t"))
  
}

# Run this to call the Shiny web application referencing the UI and server
# THE SECOND PAGE DOES NOT WORK WHEN USING DIFFERENT STATES
# COME BACK TO THIS
shinyApp(ui = ui, server = server)