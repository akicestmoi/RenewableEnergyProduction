library(shiny)
library(leaflet)
library(plotly)
library(Hmisc)
library(reshape2)


shinyServer(function(input, output) {

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

    
    
## Output Leaflet
    output$map = renderLeaflet({
        energy = reactive({input$energyType})
        region = reactive({input$regionType})
        year = reactive({input$yearType})

        plotDf = df[ (df$Technology == energy()) & 
                         (df$lowLevel == region()) &
                         (df$year == year()),]
        
        myPopup = paste("Selected Area:", plotDf$`Country/area`, "<br>",
                        format(plotDf$`energyGeneration_(GWh)`, big.mark=","), "GWh")
        
        plotDf %>%
            leaflet() %>% 
            addTiles() %>%
            addCircleMarkers(weight = 3, radius = sqrt(plotDf$`energyGeneration_(GWh)`/100),
                             color = "red", popup=myPopup)
    })
    
    
## Output Graph
    ## Plot Energy By Region (per Energy Type) 
    output$regionPlot <- renderPlotly({
        region = reactive({input$regionTypePlot})
        country = reactiveVal()
        if (region() == "Africa") {
            country = reactive({input$africa})
        } else if (region() == "Asia") {
            country = reactive({input$asia})
        } else if (region() == "Central America") {
            country = reactive({input$camerica})
        } else if (region() == "Eurasia") {
            country = reactive({input$eurasia})
        } else if (region() == "Europe") {
            country = reactive({input$europe})
        } else if (region() == "Middle East") {
            country = reactive({input$middleeast})
        } else if (region() == "North America") {
            country = reactive({input$namerica})
        } else if (region() == "Ocenaia") {
            country = reactive({input$oceania})
        } else if (region() == "South America") {
            country = reactive({input$samerica})
        }
        
        if (region() == "World") {
            plotDf = df[(df$highLevel == region()) &
                            (df$Technology != "Total renewable energy"),]
        } else {
            plotDf = df[(df$`Country/area` == country()) &
                            (df$Technology != "Total renewable energy"),]
        }

        rP = plot_ly(plotDf) %>% 
            add_trace(x = ~year, y = ~`energyGeneration_(GWh)`, 
                      type = 'scatter', mode="lines", color=~Technology) %>%
            layout(title="Energy Generation in GW per hour", xaxis = list(title="Year"),
                   yaxis = list(title="Energy (GWh, logarithmic scale)", type="log"))
        rP
    })
    
    
    ## Plot Energy By Energy Type (per Region)
    output$energyPlot <- renderPlotly({
        energy = reactive({input$energyTypePlot})
        energyRegion = reactive({input$energyTypePerCountryPlot})
        if (energyRegion() == "None") {
            plotDf = df[(df$Technology == energy()) &
                        (df$highLevel != "None") &
                        (df$highLevel != "World"),]
            
            eP = plot_ly(plotDf) %>% 
                add_trace(x = ~year, y = ~`energyGeneration_(GWh)`, 
                          type = 'scatter', mode="lines", color=~highLevel)
        } else {
            plotDf = df[(df$Technology == energy()) &
                            (df$lowLevel == energyRegion()),]
            
            eP = plot_ly(plotDf) %>% 
                add_trace(x = ~year, y = ~`energyGeneration_(GWh)`, 
                          type = 'scatter', mode="lines", color=~`Country/area`)
        } 
        
        eP %>% 
            layout(title="Energy Generation in GW per hour", xaxis = list(title="Year"),
                   yaxis = list(title="Energy (GWh, logarithmic scale)", type="log"))
    })
    
    
## Output Table
    output$RenewableEnergyTable = DT::renderDataTable({
        
        plotDf = subset(df, select = -highLevel)
        plotDf$`energyGeneration_(GWh)` = format(plotDf$`energyGeneration_(GWh)`, big.mark=",")

        colnames(plotDf) = c("Area", "Technology", "Latitude", "Longitude", "Continent", 
                             "Year", "Energy Generation (GW/h)")
        
        plotDf = plotDf %>%
            filter(
                is.null(input$energyTypeDB) | Technology %in% input$energyTypeDB,
                is.null(input$regionTypeDB) | Continent %in% input$regionTypeDB,
                is.null(input$yearTypeDB) | Year %in% input$yearTypeDB
            )
        
        plotDf
    })
    
    
## Output Extrapolation Graph
    output$extrapolationPlot = renderPlotly({
    #Linear Extrapolation
        energyDb = reactive({input$energyTypeDT})
        regionDb = reactive({input$regionTypeDT})
        
        countryDb = reactiveVal(value="World")
        if (regionDb() == "Africa") {
            countryDb = reactive({input$africaDT})
        } else if (regionDb() == "Asia") {
            countryDb = reactive({input$asiaDT})
        } else if (regionDb() == "Central America") {
            countryDb = reactive({input$camericaDT})
        } else if (regionDb() == "Eurasia") {
            countryDb = reactive({input$eurasiaDT})
        } else if (regionDb() == "Europe") {
            countryDb = reactive({input$europeDT})
        } else if (regionDb() == "Middle East") {
            countryDb = reactive({input$middleeastDT})
        } else if (regionDb() == "North America") {
            countryDb = reactive({input$namericaDT})
        } else if (regionDb() == "Ocenaia") {
            countryDb = reactive({input$oceaniaDT})
        } else if (regionDb() == "South America") {
            countryDb = reactive({input$samericaDT})
        }
        
        xmax = input$sliderYear
        
        
        x = as.numeric(as.character(unique(df$year)))
        y = df[df$Technology == energyDb() & df$`Country/area` == countryDb(),]$`energyGeneration_(GWh)`
        
        newy = approxExtrap(x, y, (max(x)+1):xmax)$y
        newy[newy<0] = 0
        
        tmp = data.frame(Year=c(x, max(x):xmax), Value=c(y, y[length(y)], newy), 
                         Origin=c(rep("Data", length(x)), c(rep("Extrapolated", length(newy)+1))) )
        
        pal = c("black", "lightblue")
        
        plot_ly(tmp) %>% 
            add_trace(x = ~Year, y = ~Value, type = 'scatter', mode="lines", color = ~Origin, colors=pal, 
                      showlegend = FALSE) %>%
            add_trace(x = ~Year, y = ~Value, type = 'scatter', mode="marker", color = ~Origin, colors=pal) %>%
            layout(title="Energy Generation in GW per hour", xaxis = list(title="Year"),
                   yaxis = list(title="Energy (in GW per hour)"))
    })

    
    output$forecastTable = renderTable({
        
        energyDb = reactive({input$energyTypeDT})
        regionDb = reactive({input$regionTypeDT})
        
        countryDb = reactiveVal(value="World")
        if (regionDb() == "Africa") {
            countryDb = reactive({input$africaDT})
        } else if (regionDb() == "Asia") {
            countryDb = reactive({input$asiaDT})
        } else if (regionDb() == "Central America") {
            countryDb = reactive({input$camericaDT})
        } else if (regionDb() == "Eurasia") {
            countryDb = reactive({input$eurasiaDT})
        } else if (regionDb() == "Europe") {
            countryDb = reactive({input$europeDT})
        } else if (regionDb() == "Middle East") {
            countryDb = reactive({input$middleeastDT})
        } else if (regionDb() == "North America") {
            countryDb = reactive({input$namericaDT})
        } else if (regionDb() == "Ocenaia") {
            countryDb = reactive({input$oceaniaDT})
        } else if (regionDb() == "South America") {
            countryDb = reactive({input$samericaDT})
        }
        
        
        xmax = input$sliderYear
        
        
        x = as.numeric(as.character(unique(df$year)))
        y = df[df$Technology == energyDb() & df$`Country/area` == countryDb(),]$`energyGeneration_(GWh)`
        
        newy = approxExtrap(x, y, (max(x)+1):xmax)$y
        newy[newy<0] = 0
        
        tmp = data.frame(Year=c(x, max(x):xmax), Value=c(y, y[length(y)], newy), 
                         Origin=c(rep("Data", length(x)), c(rep("Extrapolated", length(newy)+1))) )
        
        yearCol = (max(x)+1):xmax
        energyCol = newy
        tmp = rbind(as.integer(yearCol), format(energyCol, big.mark=","))
        rownames(tmp) = c("Year", "Energy Production (in GW/h")
        tmp
    },         
    colnames=FALSE, rownames=TRUE, align="c")
})
