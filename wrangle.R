library(tidyverse)

gaz_raw <- read_delim("C:/Users/pc/Desktop/gaz_data/CA_Features_20180401.txt", "|")

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

gaz <- gaz %>%
  mutate(DATE_CREATED = parse_date(DATE_CREATED, format = "%M/%d/%Y"),
          DATE_EDITED = parse_date(DATE_EDITED, format = "%M/%d/%Y"))

gaz <- gaz %>%
  filter(!is.na(PRIM_LAT_DEC) & !is.na(PRIM_LONG_DEC)) %>%
  filter(!PRIM_LAT_DEC == 0 & !PRIM_LONG_DEC == 0 ) %>%
  filter(STATE_ALPHA == "CA")

write_delim(gaz, "C:/Users/pc/Desktop/gaz_data/gaz.txt", delim = "|")

top_feature = gaz %>%
  group_by(FEATURE_NAME) %>%
  summarize(count = n()) %>%
  filter(count == max(count))

bottomm_class = gaz %>%
  group_by(FEATURE_CLASS) %>%
  summarize(count = n()) %>%
  filter(count == min(count))

counties = gaz %>%
  filter(!is.na(COUNTY_NAME)) %>%
  group_by(COUNTY_NAME) %>%
  summarize(min_lat = min(PRIM_LAT_DEC), max_lat = max(PRIM_LAT_DEC),
            min_long = min(PRIM_LONG_DEC), max_long = max(PRIM_LONG_DEC))


