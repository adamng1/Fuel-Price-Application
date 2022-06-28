## About

### Purpose:

The motivation for this application emerged from rising fuel prices in the United States. As budgeting proves more crucial in an era with rising inflation, this program seeks to help people manage their finances by providing an estimate as to how much it costs an individual to fill up a vehicle per month. This program creates a comprehensive R Shiny application to calculate this cost.

### Data:

The application contains a selection of vehicles from the years 2000 through 2022, including data on the fuel economy of these vehicles. The MPG information for all vehicles comes from [fueleconomy.gov](https://www.fueleconomy.gov/feg/download.shtml). 

Note that since this application focuses specifically on vehciles that use some sort of fuel (gasoline, diesel, etc.), it does not include electric vehicles even though their information exists in the fueleconomy.gov Excel spreadsheets. Furthermore, because gasoline powered vehicles do not use diesel, gasoline powered vehicles have the "Diesel" row left as blank. Similarly, because diesel powered vehciles do not use gasoline, the diesel powered vehicles have the "Regular", "Mid-Grade", and "Premium" rows left as blank.

In addition to this information, the application also uses webscraped information to perform calculations. Firstly, gathers the average gas prices for all U.S. states and the District of Columbia from [AAA's website](https://gasprices.aaa.com/state-gas-price-averages/). Secondly, it also scrapes average annual miles driven by state from [U.S. Department of Transportation Federal Highway Administration](https://www.fhwa.dot.gov/policyinformation/statistics/2019/).

### Functionality:

The application uses this information in two ways - 

* Calculate how much it would cost on average to fill up a vehicle in a particular state based on the average number of miles driven in that state per month

* Calculate how much it would cost a specific user to fill up a vehicle based on a user entered number of monthly miles

In other words, the application provides a generic cost to fill up a vehicle in a month as well as a more user-specific cost.

### Calculations:

Determining the cost of to fill up the vehicle requires using a basic formula

> (Miles Driven / Miles Per Gallon) * Fuel Price

The first tab of the application performs this calculation by using average monthly miles as the value for "Miles Driven".

The second tab of the application performs this calculation by using a user input number of miles for "Miles Driven".

### Additional Features:

As an additional feature, the application contains a year by year comparison of a vehicle's Combined MPG. The line graph is generated via the Plotly package.
