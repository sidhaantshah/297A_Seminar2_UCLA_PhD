#---- Install and Load Required Libraries ----
install.packages("prism")
install.packages("tidyr")
install.packages("stringr")
install.packages("fixest")
install.packages("dplyr")
install.packages("collapse")

library(prism)
library(terra)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(collapse)
library(stringr)
library(fixest)

#---- Importing Prism Data ----
options(prism.path = "/Users/sidshah/Library/CloudStorage/GoogleDrive-sidhaant@g.ucla.edu/My Drive/Graduate/Fall Classes/297 A (Seminar 2)/Assignment Folders/Module 5")

get_prism_monthlys(
  type = "tmean",
  years = 2010:2015,
  mon = 1:12,  # All months
  keepZip = FALSE  # Set to TRUE if you want to keep the zip files
)

prism_files <- ls_prism_data()
print(prism_files)
prism_files <- prism_archive_ls()
prism_tmean_stack <- pd_stack(prism_files)

#---- Creating a Weather dataset that has 2010-2015 tmean by State ----
weather <- rast(prism_tmean_stack)
weather <- rast(prism_tmean_stack, type = "xyz", crs = "EPSG:4326")

yearly_weather <- tapp(
  weather,
  index = rep(1:6, each = 12),  # 6 years, each with 12 months
  fun = mean,
  na.rm = TRUE
)

shapefile_path <- "/Users/sidshah/Library/CloudStorage/GoogleDrive-sidhaant@g.ucla.edu/My Drive/Graduate/Fall Classes/297 A (Seminar 2)/Assignment Folders/Module 6/geo1_us1960_2020/geo1_us1960_2020.shp"
borders <- vect(shapefile_path)

sum_borders <- zonal(weather, borders, fun=mean, na.rm=TRUE, as.polygons=TRUE)
print(sum_borders)
sum_table <- as.data.frame(sum_borders)

names(yearly_weather) <- as.character(2010:2015)

sum_borders3 <- zonal(yearly_weather, borders, fun=mean, na.rm=TRUE, as.polygons=TRUE)
print(sum_borders3)
sum_table3 <- as.data.frame(sum_borders3)

head(sum_table3)

collapsed_weather <- sum_table3 %>%
  group_by(ADMIN_NAME) %>%
  summarize(
    Mean_2010 = mean(`2010`, na.rm = TRUE),
    Mean_2011 = mean(`2011`, na.rm = TRUE),
    Mean_2012 = mean(`2012`, na.rm = TRUE),
    Mean_2013 = mean(`2013`, na.rm = TRUE),
    Mean_2014 = mean(`2014`, na.rm = TRUE),
    Mean_2015 = mean(`2015`, na.rm = TRUE)
  )

#---- Pivoting the Weather dataset so that the tmean data is long form ----
pivoted_weather <- collapsed_weather%>%
  pivot_longer(
    cols = starts_with("Mean_"), # Select columns starting with 'Mean_'
    names_to = "Year",           # Name of the new 'Year' column
    values_to = "Mean_Temperature" # Name of the values column
  )

pivoted_weather$Year <- gsub("Mean_", "", pivoted_weather$Year)
pivoted_weather <- pivoted_weather %>%
  rename(State = ADMIN_NAME)

pivoted_weather <- pivoted_weather %>%
  rename(YEAR = Year)

pivoted_weather <- na.omit(pivoted_weather)
pivoted_weather$YEAR <- as.numeric(pivoted_weather$YEAR)

#---- Importing the Y Data (in my case it is the Cost of Annual Household Electricity from 2010-2015 ACS) ----
bigcensus <- read_csv("/Users/sidshah/Library/CloudStorage/GoogleDrive-sidhaant@g.ucla.edu/My Drive/Graduate/Fall Classes/297 A (Seminar 2)/Assignment Folders/Module 3/Module 3 Dataset.csv")

statefip_to_state <- c(
  "1" = "Alabama", "2" = "Alaska", "4" = "Arizona", "5" = "Arkansas",
  "6" = "California", "8" = "Colorado", "9" = "Connecticut", "10" = "Delaware",
  "11" = "District of Columbia", "12" = "Florida", "13" = "Georgia", "15" = "Hawaii",
  "16" = "Idaho", "17" = "Illinois", "18" = "Indiana", "19" = "Iowa",
  "20" = "Kansas", "21" = "Kentucky", "22" = "Louisiana", "23" = "Maine",
  "24" = "Maryland", "25" = "Massachusetts", "26" = "Michigan", "27" = "Minnesota",
  "28" = "Mississippi", "29" = "Missouri", "30" = "Montana", "31" = "Nebraska",
  "32" = "Nevada", "33" = "New Hampshire", "34" = "New Jersey", "35" = "New Mexico",
  "36" = "New York", "37" = "North Carolina", "38" = "North Dakota", "39" = "Ohio",
  "40" = "Oklahoma", "41" = "Oregon", "42" = "Pennsylvania", "44" = "Rhode Island",
  "45" = "South Carolina", "46" = "South Dakota", "47" = "Tennessee", "48" = "Texas",
  "49" = "Utah", "50" = "Vermont", "51" = "Virginia", "53" = "Washington",
  "54" = "West Virginia", "55" = "Wisconsin", "56" = "Wyoming"
)
bigcensus$State <- statefip_to_state[as.character(bigcensus$STATEFIP)]
bigcensus <- bigcensus[!bigcensus$COSTELEC %in% c(9997, 9993), ]
bigcensus <- collap(bigcensus, COSTELEC ~ State, FUN = list(y_mean = mean, count = length))

#---- Collapsing my Census Data Variable by Region and Year ----
collap_ipums_geog_year <- collap(bigcensus, COSTELEC+DENSITY ~ State + YEAR)

collap_ipums_geog_year$State <- str_trim(collap_ipums_geog_year$State)
collap_ipums_geog_year$YEAR <- as.integer(collap_ipums_geog_year$YEAR)
collap_ipums_geog_year$COSTELEC <- as.numeric(collap_ipums_geog_year$COSTELEC)
collap_ipums_geog_year$DENSITY <- as.numeric(collap_ipums_geog_year$DENSITY)

#---- Collapsing COSTELEC by Region and tmean by Region ----
collap_costelec <- collap(collap_ipums_geog_year, COSTELEC ~ State)
collap_pivoted_weather <- collap(pivoted_weather, Mean_Temperature ~ State)

#---- Creating 2 conjoined datasets  -> 1. For COSTELEC+tmean by State ONLY and 2. For COSTELEC+tmean by State and Year ----
joined_data <- full_join(collap_pivoted_weather, collap_costelec)
joined_data_withyear <- full_join(pivoted_weather, collap_ipums_geog_year)
joined_data_withyear <- left_join(joined_data_withyear, bigcensus %>% select(State, count.COSTELEC), by = "State")


#---- Phase 1 of Final Project - Figures ----
# Graphing mean temperature by year
weather_by_yrs <- collap(pivoted_weather, Mean_Temperature ~ YEAR, FUN = fmean)

ggplot(weather_by_yrs, aes(y = Mean_Temperature, x = YEAR)) + 
  geom_point(size = 3) +
  geom_line() +
  theme_minimal() +
  labs(
    y = "Mean Temperature (°C)", 
    x = "Year"
  ) +
  theme(
    axis.line = element_line(),        # Ensures axes lines are visible
    axis.ticks = element_line(),       # Ensures ticks are visible
    axis.title = element_text(size = 12)  # Adds clear axis titles
  )
ggsave("mean_temperature_by_year.png")

# Graphing average annual cost of household electricity by year
electriccost_by_yrs <- collap(collap_ipums_geog_year, COSTELEC ~ YEAR, FUN = fmean)

ggplot(electriccost_by_yrs, aes(y = COSTELEC, x = YEAR)) + 
  geom_point(size = 3) +
  geom_line() +
  theme_minimal() +
  labs(
    y = "Average Annual Cost of Household Electricity ($)", 
    x = "Year"
  ) +
  theme(
    axis.line = element_line(),        # Ensures axes lines are visible
    axis.ticks = element_line(),       # Ensures ticks are visible
    axis.title = element_text(size = 12)  # Adds clear axis titles
  )
ggsave("electric_cost_by_year.png")

# Graphing cost vs temperature
ggplot(joined_data, aes(y = COSTELEC, x = Mean_Temperature)) + 
  geom_point(size = 3) +
  geom_line() +
  theme_minimal() +
  labs(
    y = "Average Annual Cost of Household Electricity ($)", 
    x = "Mean Temperature (°C)"
  ) +
  theme(
    axis.line = element_line(),        # Ensures axes lines are visible
    axis.ticks = element_line(),       # Ensures ticks are visible
    axis.title = element_text(size = 12)  # Adds clear axis titles
  )
ggsave("cost_vs_temperature.png")

#---- Phase 2 of Final Project - Models ----
# Model 1
model_1 <- feols(COSTELEC ~ Mean_Temperature, data = joined_data_withyear, weights = ~count.COSTELEC, cluster = ~State)

# Model 2
model_2 <- feols(COSTELEC ~ Mean_Temperature | State + YEAR, data = joined_data_withyear, weights = ~count.COSTELEC, cluster = ~State)

# Model 3
model_3 <- feols(COSTELEC ~ Mean_Temperature + DENSITY | State + YEAR, data = joined_data_withyear, weights = ~count.COSTELEC, cluster = ~State)

# Creating Dummy Variable based on Density
median_density <- median(joined_data_withyear$DENSITY, na.rm = TRUE)
joined_data_withyear <- mutate(joined_data_withyear, dummy_density = ifelse(DENSITY > median_density, 1, 0))
joined_data_withdummy <- collap(joined_data_withyear, COSTELEC + Mean_Temperature ~ State + YEAR + dummy_density+count.COSTELEC)

# Model 4
subset_dummy0 <- joined_data_withdummy %>% filter(dummy_density == 0)
model_4 <- feols(COSTELEC ~ Mean_Temperature | State + YEAR, data = subset_dummy0, weights = ~count.COSTELEC, cluster = ~State)

# Model 5
subset_dummy1 <- joined_data_withdummy %>% filter(dummy_density == 1)
model_5 <- feols(COSTELEC ~ Mean_Temperature | State + YEAR, data = subset_dummy1, weights = ~count.COSTELEC, cluster = ~State)

# Extract Model Results
model_results_revised <- tibble(
  Variable = c(
    "Mean Temperature Coefficient", 
    "Mean Temperature Standard Error", 
    "DENSITY Coefficient", 
    "DENSITY Standard Error", 
    "State Fixed Effects", 
    "Year Fixed Effects", 
    "R-Squared"
  ),
  Model_1 = c(
    round(coef(model_1)["Mean_Temperature"], 4),
    round(se(model_1)["Mean_Temperature"], 4),
    "NA", 
    "NA", 
    "N", 
    "N", 
    round(as.numeric(fitstat(model_1, "r2")), 4)
  ),
  Model_2 = c(
    round(coef(model_2)["Mean_Temperature"], 4),
    round(se(model_2)["Mean_Temperature"], 4),
    "NA", 
    "NA", 
    "Y", 
    "Y", 
    round(as.numeric(fitstat(model_2, "r2")), 4)
  ),
  Model_3 = c(
    round(coef(model_3)["Mean_Temperature"], 4),
    round(se(model_3)["Mean_Temperature"], 4),
    round(coef(model_3)["DENSITY"], 4),
    round(se(model_3)["DENSITY"], 4),
    "Y", 
    "Y", 
    round(as.numeric(fitstat(model_3, "r2")), 4)
  ),
  Model_4 = c(
    round(coef(model_4)["Mean_Temperature"], 4),
    round(se(model_4)["Mean_Temperature"], 4),
    "NA", 
    "NA", 
    "Y", 
    "Y", 
    round(as.numeric(fitstat(model_4, "r2")), 4)
  ),
  Model_5 = c(
    round(coef(model_5)["Mean_Temperature"], 4),
    round(se(model_5)["Mean_Temperature"], 4),
    "NA", 
    "NA", 
    "Y", 
    "Y", 
    round(as.numeric(fitstat(model_5, "r2")), 4)
  )
)

# Transpose the Table to Have Variables as Rows and Models as Columns
model_results_transposed <- model_results_revised %>%
  pivot_longer(
    cols = starts_with("Model_"), 
    names_to = "Model", 
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Model, 
    values_from = Value
  )

# Print the Final Transposed Table
print(model_results_transposed)