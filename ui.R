library(shiny)
library(leaflet)
library(plotly)
library(Hmisc)
library(reshape2)



## Loading dataframe from Script file (database variable defined as "df")
# I. Read file ________________________________________________________________________________
# Data should be downloaded at the following website: 
# https://www.irena.org/Statistics/Download-Data
# The Country latitude and longitude data can be found here: 
# https://developers.google.com/public-data/docs/canonical/countries_csv
fileName = "REProd.csv"
latlongName = "latlong.csv"

headers = read.csv(fileName, skip=7, header=F, nrows=1, stringsAsFactors=F)
df = read.csv(fileName, skip=8, header=F, stringsAsFactors=F)
colnames(df) = headers
df = df[1:1743, 2:21]

latlong = read.csv(latlongName, stringsAsFactors=F)


# II. Process dataset ________________________________________________________________________________
# II.A. Rename Continent ___________________________
df[df$`Country/area` == "C America + Carib",]$`Country/area` = "Central America"
df[df$`Country/area` == "N America",]$`Country/area` = "North America"
df[df$`Country/area` == "S America",]$`Country/area` = "South America"

# II.B. Add Latitude and Longitude ___________________________
df$latitude = NA
df$longitude = NA
for (i in which(df$`Country/area` %in% latlong$name)) {
    df[i, "latitude"] = latlong[df[i, "Country/area"] == latlong$name, "latitude"]
    df[i, "longitude"] = latlong[df[i, "Country/area"] == latlong$name, "longitude"]
}

# II.C. Treat unused latitude ad longitude ___________________________
world = "World"
continent = c("Africa", "Asia", "Central America", "Eurasia", "Europe", 
              "European Union", "Middle East", "North America", "Oceania", "South America")
lat = c(35, 11, 48, 17, 43, 48, 48, 29.5, 50, -24, -11)
long = c(-38, 23, 97, -84, 44, 14, 7, 48, -104, 135, -58)

doNotExist = c("South Sudan", "BES Islands", "Curacao", "St Barth", "St Maarten", "St Martin")
latDNE = c(4.859319, 12.1784, 12.1696, 17.9000, 18.0425, 18.0708)
longDNE = c(31.58062, -68.2385, -68.9900, -62.8333, -63.0548, -63.0501)

newLatlong = data.frame(name = c(world, continent, doNotExist),
                        latitude = c(lat, latDNE), longitude = c(long, longDNE))

for (i in which(df$`Country/area` %in% newLatlong$name)) {
    df[i, "latitude"] = newLatlong[df[i, "Country/area"] == newLatlong$name, "latitude"]
    df[i, "longitude"] = newLatlong[df[i, "Country/area"] == newLatlong$name, "longitude"]
}


unused = setdiff(setdiff(unique(df$`Country/area`), latlong$name), c(world, continent, doNotExist,""))
# Painful but needs to be done manually
realNames = c("Cape Verde", "Central African Republic", "Congo [DRC]", "Congo [Republic]", 
              "Cote d'Ivoire", "Equatorial Guinea", "Swaziland", "Guinea-Bissau", 
              "Sao Tome and Principe",
              "Western Sahara", "Brunei", "Hong Kong", "Macau", "Taiwan", "North Korea",
              "South Korea", "Laos", "Myanmar [Burma]", "Timor-Leste", "Vietnam", "Antigua and Barbuda",
              "British Virgin Islands", "Cayman Islands", "Dominican Republic", "Saint Kitts and Nevis",
              "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago",
              "Turks and Caicos Islands", "U.S. Virgin Islands", "Russia", "Bosnia and Herzegovina",
              "Czech Republic", "Vatican City", "Kosovo", "Moldova", "Macedonia [FYROM]",
              "United Kingdom", "Iran", "Palestinian Territories", "Syria", "United Arab Emirates",
              "Saint Pierre and Miquelon", "United States", "American Samoa", "Christmas Island",
              "Cocos [Keeling] Islands", "Cook Islands", "French Polynesia", "Marshall Islands",
              "New Caledonia", "Norfolk Island", "Northern Mariana Islands", "Papua New Guinea",
              "Pitcairn Islands", "Solomon Islands", "Wallis and Futuna", 
              "Falkland Islands [Islas Malvinas]", "French Guiana")
newLatlong = as.data.frame(cbind(as.character(unused), as.character(realNames)))
colnames(newLatlong) = c("unused", "realNames")
for (i in 1:nrow(newLatlong)) {
    newLatlong[i, "latitude"] = latlong[newLatlong[i, "realNames"] == latlong$name, "latitude"]
    newLatlong[i, "longitude"] = latlong[newLatlong[i, "realNames"] == latlong$name, "longitude"]
}

for (i in which(df$`Country/area` %in% newLatlong$unused)) {
    df[i, "latitude"] = newLatlong[df[i, "Country/area"] == newLatlong$unused, "latitude"]
    df[i, "longitude"] = newLatlong[df[i, "Country/area"] == newLatlong$unused, "longitude"]
}


# II. D. Repeat last entry value function ___________________________
# It repeats the last non NA value, keeps leading NA.
# First, it get positions of nonmissing values.
# If it begins with a missing, add the first position to the indices.
# Repeat the values at these indices
# Diffing the indices against length yields how often they need to be repeated
repeat.before = function(x) {    
    ind = which(x != "")       
    if(is.na(x[1]))              
        ind = c(1,ind)  
    rep(x[ind], times = diff(    
        c(ind, length(x) + 1) )) 
}  


for (i in c(1,21,22)) {
    df[,i] = repeat.before(df[,i])
}


# II.E. Create "High-level" column ___________________________
index = which(df$`Country/area` %in% c(world, continent))
df$highLevel = rep("None", nrow(df))
df[index, "highLevel"] = df[index, "Country/area"]


# II.F. Add "Low-Level" Column ___________________________
n = 0; index = vector(); diff = vector()
for (i in continent) {
    n = which(unique(df$`Country/area`) == i)
    index = c(index, n)
}

for (i in 1:(length(index)-1)) {
    n = index[i + 1] - index[i] - 1
    diff = c(diff, n)
}
diff = c(diff, length(unique(df$`Country/area`)) - index[length(index)])

infoDf = data.frame(country = setdiff(df$`Country/area`, c(world, continent)))
infoDf$lowLevel = rep(continent, diff)

add = data.frame(country = c(world, continent), lowLevel = rep("None", length(c(world, continent))))

infoDf = rbind(infoDf, add)


for (i in 1:nrow(df)) {
    df[i, "lowLevel"] = infoDf[which(infoDf == df[i, "Country/area"])[1], "lowLevel"]
}


# II.G. Reshape dataframe, transposing year columns into one ___________________________
df = melt(df, 
          id = c("Country/area", "Technology", "latitude", "longitude", "lowLevel", "highLevel"),
          measure.vars = colnames(df[, 3:20]),
          variable.name="year", 
          value.name="energyGeneration_(GWh)")


# II.H. Transform into numeric and Treat NA as 0 ___________________________
df$`energyGeneration_(GWh)` = gsub(" ", "", df$`energyGeneration_(GWh)`)
df$`energyGeneration_(GWh)` = as.numeric(df$`energyGeneration_(GWh)`)
df[is.na(df)] = 0


# II.I. Drop "European Union" to only focus on "Europe" ___________________________
df = df[df$`Country/area` != "European Union",]



# Drop Down for Leaflet and Datatable
energyDropDown = unique(df$Technology)
regionDropDown = c("Overview" = "None", unique(df[df$highLevel != "None" & 
                                                      df$highLevel != "World", ]$highLevel))
yearDropDown = unique(df$year)


# Drop Down for Plot
plotDropDown = c("Energy Type" = "Technology", "Region" = "belong")
regionDropDownPlot = unique(df[df$highLevel != "None",]$highLevel)
# Country Drop Down
    africaDropDown = unique(
        df[ (df$highLevel == "Africa") | (df$lowLevel == "Africa"), ]$`Country/area`)
    asiaDropDown = unique(
        df[ (df$highLevel == "Asia")  | (df$lowLevel == "Asia"), ]$`Country/area`)
    camericaDropDown = unique(
        df[ (df$highLevel == "Central America")  | (df$lowLevel == "Central America"), ]$`Country/area`)
    eurasiaDropDown = unique(
        df[ (df$highLevel == "Eurasia")  | (df$lowLevel == "Eurasia"), ]$`Country/area`)
    europeDropDown = unique(
        df[ (df$highLevel == "Europe")  | (df$lowLevel == "Europe"), ]$`Country/area`)
    middleeastDropDown = unique(
        df[ (df$highLevel == "Middle East")  | (df$lowLevel == "Middle East"), ]$`Country/area`)
    namericaDropDown = unique(
        df[ (df$highLevel == "North America")  | (df$lowLevel == "North America"), ]$`Country/area`)
    oceaniaDropDown = unique(
        df[ (df$highLevel == "Oceania")  | (df$lowLevel == "Oceania"), ]$`Country/area`)
    samericaDropDown = unique(
        df[ (df$highLevel == "South America")  | (df$lowLevel == "South America"), ]$`Country/area`)

    

shinyUI(fluidPage(
    
# Creating a Top level tab navigation Panel
    navbarPage("Renewable Energy Production",

# First Tab navigation panel: Map
        tabPanel(
            "Interactive Map", 
            
            sidebarPanel(id = "check", class = "panel panel-default", width=3,
                h3("Select Input"),
                selectInput("energyType", label = "Energy Type", choices=energyDropDown),
                selectInput("regionType", label = "Region to display", choices=regionDropDown),
                selectInput("yearType", label = "Year", choices=yearDropDown),
                textOutput("yearOutput")
            ),
            
            mainPanel(leafletOutput("map", width=1100, height=650))
        ),

               
# Second Tab navigation panel: Plot
        tabPanel(
            "Graph",
            selectInput("regionTypePlot", label = "Region", choices=regionDropDownPlot),
            conditionalPanel("input.regionTypePlot == 'Africa'", 
                            selectInput("africa", label = "Country", choices=africaDropDown)),
            conditionalPanel("input.regionTypePlot == 'Asia'", 
                             selectInput("asia", label = "Country", choices=asiaDropDown)),
            conditionalPanel("input.regionTypePlot == 'Central America'", 
                             selectInput("camerica", label = "Country", choices=camericaDropDown)),
            conditionalPanel("input.regionTypePlot == 'Eurasia'", 
                             selectInput("eurasia", label = "Country", choices=eurasiaDropDown)),
            conditionalPanel("input.regionTypePlot == 'Europe'", 
                             selectInput("europe", label = "Country", choices=europeDropDown)),
            conditionalPanel("input.regionTypePlot == 'Middle East'", 
                             selectInput("middleeast", label = "Country", choices=middleeastDropDown)),
            conditionalPanel("input.regionTypePlot == 'North America'", 
                             selectInput("namerica", label = "Country", choices=namericaDropDown)),
            conditionalPanel("input.regionTypePlot == 'Oceania'", 
                             selectInput("oceania", label = "Country", choices=oceaniaDropDown)),
            conditionalPanel("input.regionTypePlot == 'South America'", 
                             selectInput("samerica", label = "Country", choices=samericaDropDown)),
            
            plotlyOutput("regionPlot"),

            fluidRow(
                column(2, 
                       selectInput("energyTypePlot", label = "Energy Type", choices=energyDropDown)),
                column(2, 
                       selectInput("energyTypePerCountryPlot", label = "Continent", choices=regionDropDown))
                ),
            plotlyOutput("energyPlot")
        ),


# Third Tab navigation panel: Database
        tabPanel(
            "Database",
            fluidRow(
                column(3,
                       selectInput("energyTypeDB", label = "Energy Type", choices=energyDropDown, multiple=T)),
                column(3,
                       selectInput("regionTypeDB", label = "Region to display", choices=regionDropDown, multiple=T)),
                column(3,
                       selectInput("yearTypeDB", label = "Year", choices=yearDropDown, multiple=T)),
            ),
            DT::dataTableOutput("RenewableEnergyTable")
        ),

# Fourth Tab navigation panel: Extrapolation
        tabPanel(
            "Forecast",
            fluidRow(
                column(2,
                       sliderInput("sliderYear", "Until what year should we extrapolate?", 2018, 2050, value = 2020)),
                column(2,
                       selectInput("energyTypeDT", label = "Energy Type", choices=energyDropDown))
            ),
            
            selectInput("regionTypeDT", label = "Region", choices=regionDropDownPlot),
            conditionalPanel("input.regionTypeDT == 'Africa'", 
                                        selectInput("africaDT", label = "Country", choices=africaDropDown)),
            conditionalPanel("input.regionTypeDT == 'Asia'", 
                                        selectInput("asiaDT", label = "Country", choices=asiaDropDown)),
            conditionalPanel("input.regionTypeDT == 'Central America'", 
                                        selectInput("camericaDT", label = "Country", choices=camericaDropDown)),
            conditionalPanel("input.regionTypeDT == 'Eurasia'", 
                                        selectInput("eurasiaDT", label = "Country", choices=eurasiaDropDown)),
            conditionalPanel("input.regionTypeDT == 'Europe'", 
                                        selectInput("europeDT", label = "Country", choices=europeDropDown)),
            conditionalPanel("input.regionTypeDT == 'Middle East'", 
                                        selectInput("middleeastDT", label = "Country", choices=middleeastDropDown)),
            conditionalPanel("input.regionTypeDT == 'North America'", 
                                        selectInput("namericaDT", label = "Country", choices=namericaDropDown)),
            conditionalPanel("input.regionTypeDT == 'Oceania'", 
                                        selectInput("oceaniaDT", label = "Country", choices=oceaniaDropDown)),
            conditionalPanel("input.regionTypeDT == 'South America'", 
                                        selectInput("samericaDT", label = "Country", choices=samericaDropDown)),
            
            plotlyOutput("extrapolationPlot"),
            h3("Energy Forecast"),
            tableOutput("forecastTable")
        )
    )
))
