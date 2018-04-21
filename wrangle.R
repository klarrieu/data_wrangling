library(tidyverse)

#read in data
gaz_raw <- read_delim("C:/Users/pc/Desktop/gaz_data/CA_Features_20180401.txt", "|")

#select columns to include
gaz <- gaz_raw %>% select(FEATURE_ID,
                          FEATURE_NAME,
                          FEATURE_CLASS,
                          STATE_ALPHA,
                          COUNTY_NAME,
                          PRIM_LAT_DEC,
                          PRIM_LONG_DEC,
                          SOURCE_LAT_DEC,
                          SOURCE_LONG_DEC,
                          ELEV_IN_M,
                          MAP_NAME,
                          DATE_CREATED,
                          DATE_EDITED)

#set columns to correct type (everything auto-parsed correctly but dates)
gaz <- gaz %>%
  mutate(DATE_CREATED = parse_date(DATE_CREATED, format = "%M/%d/%Y"),
          DATE_EDITED = parse_date(DATE_EDITED, format = "%M/%d/%Y"))

#filter out missing lat/long, only include placse in CA
gaz <- gaz %>%
  filter(!is.na(PRIM_LAT_DEC) & !is.na(PRIM_LONG_DEC)) %>%
  filter(!PRIM_LAT_DEC == 0 & !PRIM_LONG_DEC == 0 ) %>%
  filter(STATE_ALPHA == "CA")

#write out filtered data to csv
write_delim(gaz, "C:/Users/pc/Desktop/gaz_data/gaz.txt", delim = "|")

#most common feature name
top_feature = gaz %>%
  group_by(FEATURE_NAME) %>%
  summarize(count = n()) %>%
  filter(count == max(count))

#least common feature class(es)
bottomm_class = gaz %>%
  group_by(FEATURE_CLASS) %>%
  summarize(count = n()) %>%
  filter(count == min(count))

#center of bounding box for each county
county_centers = gaz %>%
  filter(!is.na(COUNTY_NAME)) %>%
  #remove linear features (features with a source lat/long), only want point features
  filter(is.na(SOURCE_LAT_DEC) | is.na(SOURCE_LONG_DEC)) %>%
  group_by(COUNTY_NAME) %>%
  summarize(min_lat = min(PRIM_LAT_DEC), max_lat = max(PRIM_LAT_DEC),
            min_long = min(PRIM_LONG_DEC), max_long = max(PRIM_LONG_DEC)) %>%
  mutate(center_lat = (min_lat+max_lat)/2, center_long = (min_long+max_long)/2) %>%
  select(COUNTY_NAME, center_lat, center_long)

#determine which features are manmade and natural
class_types = gaz %>%
  group_by(FEATURE_CLASS) %>%
  summarize() %>%
  mutate(type = ifelse(FEATURE_CLASS %in% c("Airport",
                                            "Bridge",
                                            "Building",
                                            "Canal",
                                            "Cemetery",
                                            "Census",
                                            "Church",
                                            "Civil",
                                            "Dam",
                                            "Harbor",
                                            "Hospital",
                                            "Locale",
                                            "Military",
                                            "Mine",
                                            "Oilfield",
                                            "Populated Place",
                                            "Post Office",
                                            "Reservoir",
                                            "School",
                                            "Tower",
                                            "Well"),
                       "manmade", "natural"))

gaz = gaz %>%
  left_join(class_types, by = "FEATURE_CLASS")

#count number of manmade and natural features
types_counts = gaz %>%
  group_by(type) %>%
  summarize(count = n())

total = types_counts %>% summarize(sum(count))
manmade = types_counts %>% filter(type == "manmade") %>% select(count)
natural = types_counts %>% filter(type == "natural") %>% select(count)

#fraction of features that are natural and manmade
natural_fraction = natural/total
manmade_fraction = manmade/total
