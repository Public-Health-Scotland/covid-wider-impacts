# Code to produce geography lookup used in tool
# Cancer, outpatient and drug sections will need to follow this approach to work better with the modules
# CHanges to outpatient process easier, for the other two some changes in data prep and shiny side would be needed

###############################################.
## Packages ----
###############################################.
library(dplyr)

###############################################.
## Creating lookup ----
###############################################.

# Taking out old codes and keeping only newest configuration
# Excluding national facilities at the moment as well (used on outpatient though)
codes_to_exclude <- c(paste0("S0800000", 1:9), paste0("S0800001", 0:4), "S08000018",
                      "S08000021", "S08000023", "S08000027", "S37000014", "S37000015",
                      "S37000021", "S37000023", paste0("S0810000", 1:8),  paste0("S0820000", 1:8))

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
  filter(areatype == "Scotland")

# Adding extra area types
geo_lookup <- rbind(
  tibble(code = "S00000001", areaname = "Scotland", areatype = "Scotland"), 
  geo_lookup,
  op_lookup,
  tibble(code = paste0("S9900000", 1:3), areaname = c("NCA", "SCAN", "WOSCAN"), areatype = "Cancer network")
)

saveRDS(geo_lookup, "shiny_app/data/geo_lookup.rds")
