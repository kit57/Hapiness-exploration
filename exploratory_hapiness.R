
# Get libraries
library(tidyverse)    # metapackage of all tidyverse packages
library(magrittr)
library(corrplot)     # Nice correlation matrix
library(ggplot2)      # Data visualization
library(plotly)       # Dynamic data visualization
library(dplyr)        # Data manipulation
library(countrycode)  # Gets country code
library(maps)         # World Map

# sessionInfo()

Happy_2018<-read.csv("data/2018.csv")
Happy_2019<-read.csv("data/2019.csv")

str(Happy_2019)
str(Happy_2018)

# Rename column names
names(Happy_2019) <- c("Happiness_Rank","Country","Happiness_Score","GDP_per_Capita","Social_Support",
                       "Healthy_Life_Expectancy","Freedom_life_choices","Generosity","Perception_Corruption")

# Add column year
Happy_2019$Year <- 2019

Happy_2019$Country[Happy_2019$Country == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
Happy_2019$Country[Happy_2019$Country == "Congo (Brazzaville)"] <- "Republic of Congo"

# Rename column names
names(Happy_2018) <- c("Happiness_Rank","Country","Happiness_Score","GDP_per_Capita","Social_Support",
                       "Healthy_Life_Expectancy","Freedom_life_choices","Generosity","Perception_Corruption")

# Add column year
Happy_2018$Year <- 2018

Happy_2018$Country[Happy_2018$Country == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
Happy_2018$Country[Happy_2018$Country == "Congo (Brazzaville)"] <- "Republic of Congo"


# Print the correlation of the variables (except for Happiness_Rank, Country and Year variables)
corrplot(Happy_2019 %>% select(-Happiness_Rank, -Country, -Year) %>% cor(method = "pearson"), 
         title = "2019 correlation plot", method = "square")

Happy_2018$Happiness_Rank <- as.integer(Happy_2018$Happiness_Rank)
Happy_2018$Year <- as.numeric(Happy_2018$Year)
Happy_2018$Perception_Corruption <- as.numeric(Happy_2018$Perception_Corruption)

sapply(Happy_2019, class) 
sapply(Happy_2018, class) 

# Print the correlation of the variables /except for Happiness_Rank, Country and Year variables)
corrplot(Happy_2018 %>% select(-Happiness_Rank, -Country, -Year) %>% cor(method = "pearson"), 
         title = "2018 correlation plot", method = "square")
#print(Happy_2018)


Happy_2019$Continent <- NA
Happy_2019$Continent[which(Happy_2019$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                             "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                             "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                                             "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                             "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                             "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                             "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"
Happy_2019$Continent[which(Happy_2019$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                             "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                             "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                             "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                             "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                                             "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                                             "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                             "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                             "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
Happy_2019$Continent[which(Happy_2019$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                                             "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                             "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                             "Haiti"))] <- "North America"
Happy_2019$Continent[which(Happy_2019$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                             "Colombia", "Ecuador", "Bolivia", "Peru",
                                                             "Paraguay", "Venezuela"))] <- "South America"
Happy_2019$Continent[which(Happy_2019$Country %in% c("New Zealand", "Australia"))] <- "Australia"


Happy_2019$Continent[which(is.na(Happy_2019$Continent))] <- "Africa"

### Plot world map with happiness_Score
Happy_2019$code <- countrycode(Happy_2019$Country, 'country.name', 'iso3c')
Happy_2019[Happy_2019$Country == "Kosovo", "code"] <- "XKX"

# Remove null  &amp; NA values
2
Happy_2019[!(is.na(Happy_2019$Country) | Happy_2019$Country=="" | is.na(Happy_2019$code) | 
               Happy_2019$code==""),] 


# light grey boundaries
l <- list(color = "grey", width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

plot_geo(Happy_2019) %>%
  add_trace(
    z = ~round(Happiness_Score, 2), color = ~Happiness_Score, colors = 'Blues',
    # Hover text:
    text = ~with(data=Happy_2019, paste("<b>Country:</b> ", Country,
                                        "<br><b>Continent:</b> ", Continent,
                                        "<br><b>Happiness Score: </b>", Happiness_Score)),
    locations = ~code, marker = list(line = l)) %>%
  colorbar(title = 'Happiness Score') %>%
  layout(
    title = 'World Happiness Map: Year 2019',
    geo = g
  )
