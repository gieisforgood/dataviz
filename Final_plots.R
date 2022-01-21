library(ggplot2)
library(ggforce)
library(data.table)
library(magrittr) # Needed for %>% operator
library(tidyr)
library(GGally)
library(pheatmap)
library(RColorBrewer)
library(plyr)
library(stringr) #useful for wrapping chart titles
library(scales)

average_rent_2015 <- fread("2015_lloguer_preu_trim.csv", encoding="UTF-8")
average_rent_2016 <- fread("2016_lloguer_preu_trim.csv", encoding="UTF-8")
average_rent_2017 <- fread("2017_lloguer_preu_trim.csv", encoding="UTF-8")

unemployment <- fread("unemployment.csv")
unemployment[, Month := as.factor(Month)]
colnames(unemployment) <- gsub(" ", "_", colnames(unemployment))

average_rent <- rbindlist(list(average_rent_2015, average_rent_2016, average_rent_2017))

translate_columns <- function(dt){
  translate_column <- function(col){
    if (col == "Nom_Barri"){
      translated_col <- "Neighborhood_Name"
    }
    else if (col == "Preu"){
      translated_col <- "Price"
    }
    else if (col == "Any"){
      translated_col <- "Year"
    }
    else if (col == "Codi_Barri"){
      translated_col <- "Neighborhood_Code"
    }
    else if (col == "Trimestre"){
      translated_col <- "Quarter"
    }
    else if (col == "Codi_Districte"){
      translated_col <- "District_Code"
    }
    else if (col == "Nom_Districte"){
      translated_col <- "District_Name"
    }
    else if (col == "Edat_quinquennal"){
      translated_col <- "Age"
    }
    else if (col == "Codi_Barri"){
      translated_col <- "Neighborhood_Code"
    }
    else if (col == "Any"){
      translated_col <- "Year"
    }
    else if (col == "Nombre"){
      translated_col <- "Immigrants"
    }
    else{
      translated_col <- col
    }
  }
  translated_dt <- copy(dt)
  names(translated_dt) <- sapply(names(translated_dt), translate_column)
  return(translated_dt)
}

av_quarter <- function(dt, col){
  first_quarter <- dt[Month %in% c("January", "February", "March"), .(Number = mean(Number)), by=c(col, "Year")]
  first_quarter[, Quarter := 1]
  second_quarter <- dt[Month %in% c("April", "May", "June"), .(Number = mean(Number)), by=c(col, "Year")]
  second_quarter[, Quarter := 2]
  third_quarter <- dt[Month %in% c("July", "August", "September"), .(Number = mean(Number)), by=c(col, "Year")]
  third_quarter[, Quarter := 3]
  fourth_quarter <- dt[Month %in% c("October", "November", "December"), .(Number = mean(Number)), by=c(col, "Year")]
  fourth_quarter[, Quarter := 4]
  
  averaged_dt <- rbindlist(list(first_quarter, second_quarter, third_quarter, fourth_quarter))
}

average_rent[, Lloguer_mitja := as.factor(Lloguer_mitja)]
average_rent <- translate_columns(average_rent)

# only consider price per m^2
average_rent_m2 <- average_rent[Lloguer_mitja == "Lloguer mitjà per superfície (Euros/m2 mes)"]

quarter_unemployment_distr <- av_quarter(unemployment, "District_Code")
slim_average_rent_distr <- average_rent_m2[, c("District_Code", "Price", "Year", "Quarter")]

# we only have rent data for 2015, 2016, 2017
rent_unemployment_distr <- merge(quarter_unemployment_distr[Year > 2014], slim_average_rent_distr, by=c("District_Code", "Year", "Quarter"))
rent_unemployment_distr[, District_Code := as.factor(District_Code)]
rent_unemployment_distr[, Year := as.factor(Year)]
rent_unemployment_distr <- na.omit(rent_unemployment_distr)

ggplot(rent_unemployment_distr, aes(Number, Price, color=Year)) + geom_jitter()

ggplot(rent_unemployment_distr, aes(sample=Number)) + geom_qq() + geom_qq_line()
`D`

### NEW CODE AND PLOTS ####


# population data set

## showing population of barcelona's districts

population <- fread("population.csv",encoding="UTF-8")
colnames(population) <- c("Year","District_Code","District_Name","Neighborhood_Code",
                             "Neighborhood_Name","Gender","Age","Population")

aggregated_population <- aggregate(Population ~ Year + District_Code + District_Name + Neighborhood_Code, population, FUN = sum)
aggregated_population <- as.data.table(aggregated_population)
aggregated_population <- aggregated_population[Year != 2013 & Year != 2014]
total_population_district <- aggregated_population[Year == 2017, .(Total_Population = sum(Population)/1000), by = "District_Name"]
total_population_district$District_Name <- factor(total_population_district$District_Name, levels = 
                                      total_population_district$District_Name[order(total_population_district$Total_Population, decreasing = F)])


ggplot(total_population_district, aes(District_Name,Total_Population)) + 
  geom_col(fill = "DarkBlue", show.legend = T, orientation = "x") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  geom_text(size = 3, aes(label = Total_Population), vjust = -0.5) + 
  labs(title = "Populations of Barcelona's 10 districts ranging from c. 82,000 to 270,000", x = "District", y = "Inhabitants [in thousands]") + 
  theme_minimal()



# Rent & Unemployment

## Preparing data tables --> adding district name

district_name_dt <- unemployment[District_Code !=99, unique(District_Name), by = "District_Code"]
colnames(district_name_dt) <- c("District_Code","District_Name")
district_name_dt$District_Code <- as.factor(district_name_dt$District_Code)
merged_rent_unemployment <- merge(district_name_dt,rent_unemployment_distr, by = "District_Code")

# Boxplots

ggplot(merged_rent_unemployment,aes(District_Name, Price))+geom_boxplot() + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  labs(title = "Median rent price per sqm between EUR 9.15 and EUR 14.59", x = "District", y = "Average rent price per sqm [in EUR]") + 
  theme_minimal()

merged_rent_unemployment[,median(Price, na.rm = T),by=District_Name] #providing concrete numbers for title
  
ggplot(merged_rent_unemployment,aes(District_Name, Number))+geom_boxplot() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  labs(title = "Unequal distribution of unemployed among different districts", x = "District", y = "Average number of unemployed per quarter") + theme_minimal()

# Histograms

ggplot(merged_rent_unemployment, aes(Price)) + geom_histogram(fill = "DarkBlue") +
  labs(title = "Average rent price per sqm follows a normal distribution", x = "Average rent price per sqm [in EUR]" ) +theme_minimal()

ggplot(merged_rent_unemployment, aes(Number)) + geom_histogram(fill = "DarkBlue") +
  labs(title = "Number of unemployed with bi-modal distribution", x = "Average number of unemployed per quarter" ) +theme_minimal()
  


