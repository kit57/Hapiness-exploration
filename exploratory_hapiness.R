
# Get libraries
library(corrplot)     # Nice correlation matrix
library(ggplot2)      # Data visualization


Happy_2018<-read.csv("data/2018.csv")
Happy_2019<-read.csv("data/2019.csv")

# Rename column names
names(Happy_2019) <- c("Happiness_Rank","Country","Happiness_Score","GDP_per_Capita","Social_Support",
                  "Healthy_Life_Expectancy","Freedom_life_choices","Generosity","Perception_Corruption")

# Add column year
Happy_2019$Year <- 2019

Happy_2019$Country[Happy_2019$Country == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
Happy_2019$Country[Happy_2019$Country == "Congo (Brazzaville)"]<- "Republic of Congo"

# Rename column names
names(Happy_2018) <- c("Happiness_Rank","Country","Happiness_Score","GDP_per_Capita","Social_Support",
                       "Healthy_Life_Expectancy","Freedom_life_choices","Generosity","Perception_Corruption")

# Add column year
Happy_2018$Year <- 2018

Happy_2018$Country[Happy_2018$Country == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
Happy_2018$Country[Happy_2018$Country == "Congo (Brazzaville)"]<- "Republic of Congo"


# Print the correlation of the variables (Happiness_Rank -Country -Year)
corrplot(Happy_2019 %>% select(-Happiness_Rank, -Country, -Year) %>% cor(method = "pearson"), 
         title = "2019 correlation plot", method = "square")

Happy_2018$Happiness_Rank <- as.integer(Happy_2018$Happiness_Rank)
Happy_2018$Year <- as.numeric(Happy_2018$Year)

sapply(Happy_2019, class) 
sapply(Happy_2018, class) 

# Print the correlation of the variables (Happiness_Rank -Country -Year)
corrplot(Happy_2018 %>% select(-Happiness_Rank, -Country, -Year) %>% cor(method = "pearson"), 
         title = "2018 correlation plot", method = "square")
#print(Happy_2018)





