library(sf)
library(stars)
library(terra)
library(sp)
library(RColorBrewer)
# install tibble package
# Read original csv and extract necessary columns for alameda tracts
all_us_data <- read.csv("~/Documents/census-project-data-2024/ct10_acs2015_2019.csv")
selected_columns <- all_us_data[, c(grep("^RACE", names(all_us_data), value = TRUE), "INC_MEDHH", "GEOID10")]
selected_columns$GEOID <- sprintf("%011.0f", selected_columns$GEOID10)
alameda_csv <-selected_columns[grepl("^06001", selected_columns$GEOID), ]

# reach the shape file and extract the lat/long for alameda tracts
setwd("~/Documents/census-project-data-2024/tl_2019_06_tract")
census_shp <- read_sf("~/Documents/census-project-data-2024/tl_2019_06_tract/tl_2019_06_tract.shp")
alameda_shp <- census_shp[grepl("^06001", census_shp$GEOID), ]
alameda_centroids <- st_centroid(alameda_shp)
alameda_coord <- st_coordinates(alameda_centroids)
alameda_shp$latitude <- alameda_coord[, "Y"]
alameda_shp$longitude <- alameda_coord[, "X"]

# merge the shape file and csv
alamedaMerged <- merge(alameda_shp, alameda_csv, by = "GEOID")
alamedaMerged <- alamedaMerged[rowSums(is.na(alamedaMerged)) == 0, ]

# Alameda tract variable list of different combinations

# create a predominant race column RACE_PREDOM
alamedaMerged$RACE_PREDOM <- apply(alamedaMerged[, c("RACE_BLACKNH", "RACE_HISP", "RACE_WHITENH", "RACE_ASIANNH","RACE_OTHERNH", "RACE_MULTIPLENH")], 1, function(row) {
  above60_columns <- names(row[row > 0.6])
  above60_columns_no_na <- na.omit(above60_columns)
  if (length(above60_columns_no_na) > 0) {
    above60_columns_no_na
  } else {
    "RACE_MIXED"
  }
})
# find median of INC_MEDHH
median_INC_MEDHH <- median(alamedaMerged$INC_MEDHH, na.rm = TRUE)
alamedaMerged$IS_INCOME_ABOVE_MEDIAN <- ifelse(alamedaMerged$INC_MEDHH > median_INC_MEDHH, 1, 0)

# assign val
alamedaMerged$VAL <- apply(alamedaMerged, 1, function(row) {
  switch(   
    row$RACE_PREDOM,   
    "RACE_WHITENH"= ifelse(row$IS_INCOME_ABOVE_MEDIAN, 1, 2),   
    "RACE_BLACKNH"= 3,
    "RACE_HISP"= 4,
    "RACE_ASIANNH"= ifelse(row$IS_INCOME_ABOVE_MEDIAN, 5, 6),
    "RACE_MIXED"= ifelse(row$IS_INCOME_ABOVE_MEDIAN, 7, 8),
  )
})

# plot the maps
colors <- brewer.pal(length(unique(alamedaMerged$VAL)), "Set1")
color_mapping <- setNames(colors, unique(alamedaMerged$VAL))
print(color_mapping[4])
alamedaMerged$SectionColors <- color_mapping[alamedaMerged$VAL]
plot(alamedaMerged$geometry, col = alamedaMerged$SectionColors)

legend_labels <- levels(as.factor(alamedaMerged$VAL))
legend_labels[legend_labels == "1"] <- "1 (white, income > 97309)"
legend_labels[legend_labels == "2"] <- "2 (white, income < 97309)"
legend_labels[legend_labels == "3"] <- "3 (black, income < 97309)"
legend_labels[legend_labels == "4"] <- "4 (hispanic, income < 97309)"
legend_labels[legend_labels == "5"] <- "5 (asian, income > 97309)"
legend_labels[legend_labels == "6"] <- "6 (asian, income < 97309)"
legend_labels[legend_labels == "7"] <- "7 (mixed, income > 97309)"
legend_labels[legend_labels == "8"] <- "8 (mixed, income < 97309)"

legend("topright", legend = legend_labels, fill = colors, title = "Value")

write.csv(alamedaMerged, "~/Documents/census-project-data-2024/final-data.csv", row.names=FALSE)

