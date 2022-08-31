# Code to produce geography lookup and ethnicity lookup used in tool
# Cancer, outpatient and drug sections will need to follow this approach to work better with the modules
# CHanges to outpatient process easier, for the other two some changes in data prep and shiny side would be needed

###############################################.
## Packages ----
###############################################.
library(dplyr)

###############################################.
## Creating geo lookup ----
###############################################.

# Taking out old codes and keeping only newest configuration
# Excluding national facilities at the moment as well (used on outpatient though)
codes_to_exclude <- c(paste0("S0800000", 1:9), paste0("S0800001", 0:4), "S08000018",
                      "S08000021", "S08000023", "S08000027", "S37000014", "S37000015",
                      "S37000021", "S37000023", paste0("S0810000", 2:8),  paste0("S0820000", 1:8))

# Reading lookup from phsmethods base lookup
geo_lookup <- phsmethods::area_lookup %>% filter(substr(geo_code,1,3) %in% c("S37", "S08")) %>% 
  rename(code = geo_code, areaname = area_name) %>% 
  mutate(areatype= case_when(substr(code, 1, 3) == "S37" ~ "HSC partnership",
                              substr(code, 1, 3) == "S08" ~ "Health board"),
         areaname = case_when(areatype=="Health board" ~ (paste0("NHS ",gsub(" and ", " & ", areaname))), 
                               TRUE ~ areaname)) %>% 
  filter(!(code %in% codes_to_exclude))

# Outpatients lookup has a few differences
op_lookup <- readRDS("shiny_app/data/outpats.rds") %>%
  select(areaname = area_name, areatype = area_type) %>%
  distinct() %>% arrange(areatype) %>% 
  filter(areatype != "Scotland")

# Adding extra area types
geo_lookup <- bind_rows(
  tibble(code = "S00000001", areaname = "Scotland", areatype = "Scotland"), 
  geo_lookup,
  op_lookup,
  tibble(code = paste0("S9900000", 1:3), areaname = c("NCA", "SCAN", "WOSCAN"), areatype = "Cancer network"),
  tibble(code = paste0("S9800000", 1:31), areatype = "Alcohol and drug partnership",
         areaname = c("Aberdeen City ADP", "Aberdeenshire ADP", "Angus ADP", "Argyll & Bute ADP", 
                  "City of Edinburgh ADP", "City of Glasgow ADP", "Clackmannanshire ADP", 
                  "Dumfries & Galloway ADP", "Dundee City ADP", "East Ayrshire ADP", 
                  "East Dunbartonshire ADP", "East Renfrewshire ADP", "Falkirk ADP", 
                  "Fife ADP", "Highland ADP", "Inverclyde ADP", "Mid and East Lothian ADP", 
                  "Moray ADP", "North Ayrshire ADP", "North Lanarkshire ADP", "Orkney Islands ADP", 
                  "Perth & Kinross ADP", "Renfrewshire ADP", "Scottish Borders ADP", 
                  "Shetland Islands ADP", "South Ayrshire ADP", "South Lanarkshire ADP", 
                  "Stirling ADP", "West Dunbartonshire ADP", "West Lothian ADP", 
                  "Western Isles ADP"))) %>% 
  mutate(areaname = case_when(areaname == "NHS Golden Jubilee Hospital" ~ "NHS Golden Jubilee",
                    TRUE ~ areaname)) %>% 
  arrange(areatype, code, areaname)

saveRDS(geo_lookup, "shiny_app/data/geo_lookup.rds")

###############################################.
## Creating ethnicity lookup ----
###############################################.

# Create a lookup based on the 2011 census categories and groupings as advised by
# PHS team reviewing ethnicity data (Sandra Robb, Catriona Haddow). 

eth_lookup <- data.frame("ethnic_code" = c("1A Scottish", "1B Other British", "1C Irish", "1K Gypsy/ Traveller", "1L Polish", 
                                                "1Z Other white ethnic group", "2A Any mixed or multiple ethnic groups", 
                                                "3F Pakistani, Pakistani Scottish or Pakistani British", "3G Indian, Indian Scottish or Indian British", 
                                                "3H Bangladeshi, Bangladeshi Scottish or Bangladeshi British", 
                                                "3J Chinese, Chinese Scottish or Chinese British", "3Z Other Asian, Asian Scottish or Asian British", 
                                                "4D African, African Scottish or African British", "4Y Other African", 
                                                "5C Caribbean, Caribbean Scottish or Caribbean British", "5D Black, Black Scottish or Black British", 
                                                "5Y Other Caribbean or Black", "6A Arab, Arab Scottish or Arab British", 
                                                "6Z Other ethnic group", "98 Refused/Not provided by patient", 
                                                "99 Not Known"), 
                              "Grouping" = c("White Scottish", "White Other British", 
                                             "White Irish", "White Other", "White Polish", "White Other", 
                                             "Mixed", "Pakistani", "Indian", "Other Asian", "Chinese", "Other Asian", 
                                             "African", "African", "Caribbean or Black", "Caribbean or Black", 
                                             "Caribbean or Black", "Other ethnic group", "Other ethnic group", 
                                             "Missing", "Missing"))

eth_lookup <- eth_lookup %>% rename("Ethnic Group (2011 census categories)" = ethnic_code)

saveRDS(eth_lookup, "shiny_app/data/eth_lookup.rds")

# END