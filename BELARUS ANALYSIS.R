# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)
library(scales)

# Read CSV document.
cars <- read_csv("cars.csv", col_types = cols(priceUSD = col_number(), year = col_number(), `mileage(kilometers)` = col_number(), `volume(cm3)` = col_number()), na = "empty")
#View(cars)

# COUNT OF VEHICLES BY YEAR
cars %>%
  filter(year > 2000 & priceUSD < 10000000) %>%
  ggplot(aes(x = as.factor(year))) +
  geom_line(stat = "count", aes(group = 1), color = "blue") +
  labs(title = "Count of Vehicles by Year", x = "Year", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))  


# COUNT OF VEHICLE BY MANUFACTURER 
lut <- c("alfa" = "alfa-romeo", "aston" = "aston-martin", "chev" = "chevrolet", "chevy" = "chevrolet",
         "harley" = "harley-davidson", "infinity" = "infiniti", "land rover" = "landrover",
         "mercedes" = "mercedes-benz", "mercedesbenz" = "mercedes-benz", "vw" = "volkswagen")

cars$make <- ifelse(cars$make %in% c("alfa", "aston", "chev", "chevy", "harley",
                                     "infinity", "land rover", "mercedes", "mercedesbenz", "vw"),
                    lut[cars$make], cars$make)
cars %>%
  group_by(make) %>%
  summarise(Count = n()) %>%
  ggplot(aes(reorder(make, Count), Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  theme_minimal() +  # Use theme_minimal instead of theme_hc as I don't have access to your theme_hc function
  labs(x = "Vehicle Make", y = "Count")


# COUNT OF VEHICLES BY TRANSMISSION
cars %>%
  group_by(transmission) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = reorder(transmission, Count), y = Count)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), fill = "skyblue") +  # Adjust width based on your preferences
  coord_flip() +
  theme_minimal() +
  labs(x = "Transmission", y = "Count")


# AVERAGE PRICE BY MANUFACTURER AND FUEL TYPE
# Filter data for specific manufacturers
selected_make <- c("ford", "chevrolet", "jeep", "volkswagen", "audi", "bmw")
filtered_data <- cars %>%
  filter(priceUSD <= 500000, make %in% selected_make)

# Calculate the average price by manufacturer and fuel type
mean_prices_by_make <- filtered_data %>%
  group_by(make, fuel_type) %>%
  summarise(mean_price = mean(priceUSD), .groups = "drop")

# Create the plot for selected manufacturers
ggplot(mean_prices_by_make, aes(x = make, y = mean_price, fill = fuel_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Average Price by Manufacturer and Fuel Type",
       x = "Manufacturer",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = guide_legend(title = "Fuel"))


# BOX PLOT
cars <- cars %>%
  filter(!is.na(fuel_type))

# Filter the data with an average price less than or equal to 500,000.
byfuel_filtered <- cars[cars$priceUSD <= 500000, ]

# Calculate price average by fuel type.
mean_prices <- aggregate(priceUSD ~ fuel_type, data = byfuel_filtered, FUN = mean)

ggplot(byfuel_filtered, aes(x = fuel_type, y = priceUSD, fill = fuel_type)) +
  geom_boxplot() +
  labs(title = "Boxplot of Mean Price by Fuel Type",
       x = "Fuel type",
       y = "Average Price") +
  scale_fill_brewer(palette = "Set3") +  # Use a random color range
  theme_minimal()






