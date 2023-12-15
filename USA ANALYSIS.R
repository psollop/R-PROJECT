# Install required packages

# install.packages("ggplot2")
# install.packages("readr")
# install.packages("scales")
# install.packages("dplyr")

# Load libraries
library(ggplot2) 
library(readr)
library(dplyr)
library(scales)

# Read CSV document.
vehicles <- read_csv(
  "vehicles.csv",
  col_types = cols(
    year = col_character(),
    odometer = col_character(),
    posting_date = col_datetime(format = "%Y/%m/%d %H:%M:%S")
  )
)

# Clean NA values.
vehicles <- vehicles %>%
  filter(!is.na(manufacturer))
vehicles <- vehicles %>%
  filter(!is.na(fuel))
vehicles <- vehicles %>%
  filter(!is.na(transmission))

# Delete unnecessary columns 
vehicles$lat <- NULL
vehicles$long <- NULL
vehicles$county <- NULL
vehicles$url <- NULL
vehicles$region_url <- NULL 
vehicles$description <- NULL
vehicles$VIN <- NULL
vehicles$drive <- NULL
vehicles$image_url <- NULL

# COUNT OF VEHICLES BY YEAR
vehicles %>%
  filter(year > 2000 & price < 1000000) %>%
  ggplot(aes(x = as.factor(year))) +
  geom_line(stat = "count", aes(group = 1), color = "blue") +
  labs(title = "Count of Vehicles by Year", x = "Year", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) 

# COUNT OF VEHICLE BY MANUFACTURER 
lut <- c("alfa" = "alfa-romeo", "aston" = "aston-martin", "chev" = "chevrolet", "chevy" = "chevrolet",
         "harley" = "harley-davidson", "infinity" = "infiniti", "land rover" = "landrover",
         "mercedes" = "mercedes-benz", "mercedesbenz" = "mercedes-benz", "vw" = "volkswagen")

vehicles$manufacturer <- ifelse(vehicles$manufacturer %in% c("alfa", "aston", "chev", "chevy", "harley",
                                                             "infinity", "land rover", "mercedes", "mercedesbenz", "vw"),
                                lut[vehicles$manufacturer], vehicles$manufacturer)
vehicles %>%
  group_by(manufacturer) %>%
  summarise(Count = n()) %>%
  ggplot(aes(reorder(manufacturer, Count), Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(x = "Vehicle Manufacturer", y = "Count")

# COUNT OF VEHICLES BY TRANSMISSION
vehicles %>%
  filter(!(transmission == "other")) %>%
  group_by(transmission) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = reorder(transmission, Count), y = Count)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Transmission", y = "Count")

# AVERAGE PRICE BY MANUFACTURER AND FUEL TYPE
# Filter data for specific manufacturers
selected_manufacturer <- c("ford", "chevrolet", "toyota",  "volkswagen", "audi", "bmw")
filtered_data1 <- vehicles %>%
  filter(price <= 500000, 
         manufacturer %in% selected_manufacturer,
         !(fuel == "other"))

# Calculate the average price by manufacturer and fuel type
mean_prices_by_manufacturer <- filtered_data1 %>%
  group_by(manufacturer, fuel) %>%
  summarise(mean_price = mean(price), .groups = "drop")

# Create the plot for selected manufacturers
ggplot(mean_prices_by_manufacturer, aes(x = manufacturer, y = mean_price, fill = fuel)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Average Price by Manufacturer and Fuel Type",
       x = "Manufacturer",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = guide_legend(title = "Fuel Type"))

# BOX PLOT
vehicles <- vehicles %>%
  filter(!is.na(fuel))

# Filter the data with an average price less than or equal to 500,000.
byfuel_filtered <- vehicles[vehicles$price <= 500000, ]

# Calculate price average by fuel type.
mean_prices <- aggregate(price ~ fuel, data = byfuel_filtered, FUN = mean)

ggplot(byfuel_filtered, aes(x = fuel, y = price, fill = fuel)) +
  geom_boxplot() +
  labs(title = "Boxplot of Mean Price by Fuel Type",
       x = "Fuel type",
       y = "Average Price") +
  scale_fill_brewer(palette = "Set3") +  # Use a random color range
  theme_minimal()



