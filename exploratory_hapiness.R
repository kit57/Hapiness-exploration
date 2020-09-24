
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


### Plot world map with happiness_Score
Happy_2019$code <- countrycode(Happy_2019$Country, 'country.name', 'iso3c')
Happy_2019[Happy_2019$Country=="Kosovo", "code"] <- "XKX"

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

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
                                        "<br><b>Region:</b> ", Region,
                                        "<br><b>Happiness Score: </b>", Happiness_Score)),
    locations = ~code, marker = list(line = l)) %>%
  colorbar(title = 'Happiness Score') %>%
  layout(
    title = 'World Happiness Map: Year 2019',
    geo = g
  )
