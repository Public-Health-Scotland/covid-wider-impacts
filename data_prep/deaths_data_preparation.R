##### Weekly all-cause deaths data preparation for Wider Impacts app

###############################################.
## Functions/Packages/filepaths ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

library(odbc)
# SMR login details
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
## Lookups ----
###############################################.
# Bringing  LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2020_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2011, hscp2019)

# SIMD quintile to datazone lookup
dep_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/deprivation_geography.rds") %>%
  rename(datazone2011 = datazone) %>%
  select(datazone2011, year, sc_quin) %>% 
  filter(year>2014)

dep_lookup20 <- dep_lookup %>%  filter(year == 2019) %>% mutate(year = 2020)
dep_lookup21 <- dep_lookup %>%  filter(year == 2019) %>% mutate(year = 2021)

dep_lookup <- rbind(dep_lookup, dep_lookup20, dep_lookup21)

geo_lookup <- left_join(dep_lookup, postcode_lookup)

###############################################.
## Extract deaths data from SMRA ----
###############################################.
data_deaths <- as_tibble(dbGetQuery(channel, statement=
"SELECT date_of_registration, age, sex, UNDERLYING_CAUSE_OF_DEATH diag, 
        HBRES_CURRENTDATE hb, POSTCODE pc7
    FROM ANALYSIS.GRO_DEATHS_C
    WHERE date_of_registration >= '29 December 2014'
  UNION ALL
    SELECT date_of_registration, age, sex, UNDERLYING_CAUSE_OF_DEATH diag, 
        HB9 hb, POSTCODE pc7
    FROM ANALYSIS.GRO_DEATHS_WEEKLY_C")) %>%
  setNames(tolower(names(.))) %>% 
# Formatting variables
  mutate(week_ending = ceiling_date(as.Date(date_of_registration), "week", change_on_boundary = F)) %>% 
  mutate(sex = recode(sex, "1" = "Male", "2" = "Female", "0" = NA_character_, "9" = NA_character_),
         age = case_when(between(age, 0,64) ~ "Under 65", T ~ "65 and over"),
         year = year(week_ending)) #to allow merging

### check that last week is a complete week. complete weeks have 6 or 7 days of data in them. 
# There could be more recent data available in SMR than what we want
as.integer(data_deaths %>% 
    filter( week_ending==max(week_ending)) %>%
    group_by(date_of_registration) %>%
    summarise() %>% ungroup() %>% count())

#Merging with deprivation and geography lookup
data_deaths <- left_join(data_deaths, geo_lookup) %>% select(-datazone2011) 

#Pivoting so one row per area
data_deaths %<>% 
  mutate(scot = "Scotland") %>% 
  pivot_longer(cols = c(hb, hscp2019, scot)) %>% 
  #filtering out NA duplicates (which are counted in Scotland totals, but not elsewhere)
  filter(!is.na(value)) %>% 
  # More formatting
  mutate(area_name = case_when(value == "Scotland" ~ "Scotland", 
                               T ~ match_area(value)),
         dep = recode(sc_quin, 
                      "1" = "1 - most deprived", "2" = "2",  "3" = "3", 
                      "4" = "4", "5" = "5 - least deprived"),
         area_type = recode(name, "hb" = "Health board", 
                            "hscp2019" = "HSC partnership", "scot" = "Scotland")) %>% 
  mutate(area_name = case_when(area_type=="Health board" ~ (paste0("NHS ",gsub(" and ", " & ", area_name))), 
                               TRUE~area_name)) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
  summarise(count = n())  %>% ungroup()

# Create aggregations for each split
deaths_all <- data_deaths %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
deaths_sex <- data_deaths %>% agg_cut(grouper="sex") %>% rename(category = sex)
deaths_dep <- data_deaths %>% agg_cut(grouper="dep") %>% rename(category = dep)
deaths_age <- data_deaths %>% agg_cut(grouper="age") %>% rename(category = age)

data_deaths <- rbind(deaths_all, deaths_age, deaths_sex, deaths_dep) %>% 
  filter(!(area_type != "Scotland" & type == "dep")) %>% #SIMD only at Scotland level
  mutate(area_id = paste(area_type, "-", area_name)) # this helps with the next step

# This step is to make sure we have rows for all weeks for all areas/category
# even those with zeroes. It's a bit convoluted but it works
data_deaths %<>%
  pivot_wider(id_cols = c(area_type, category, type, week_ending), 
              names_from = area_name, values_from = count, values_fill = list(count = 0)) %>% 
  pivot_longer(c(`Aberdeen City`:`Western Isles`), values_to = "count", names_to = "area_name") %>% 
  # This is to get rid of combinations that don't exist (e.g. Scotland - Fife)
  mutate(area_id = paste(area_type, "-", area_name)) %>% 
  filter(area_id %in% unique(data_deaths$area_id)) %>% 
  select(-area_id) %>% 
  filter(area_name != "NHS Unknown residency")

# Running final functions
prepare_final_data(dataset = data_deaths, filename = "deaths", 
                   last_week = "2021-05-23", aver = 5)

# Dealing with variation to replicate previous output. 
# This might not be needed in future if we set a standard way of dealing with this.
final_deaths <- final_data %>% 
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1),
        # Dealing with infinite values from historic average = 0
        variation =  case_when(count_average == 0 & count == 0 ~ 0, T ~ variation),
        variation =  ifelse(is.infinite(variation), NA_integer_, variation)) 

saveRDS(final_deaths, paste0("shiny_app/data/deaths.rds"))
saveRDS(final_deaths, paste0(data_folder,"final_app_files/deaths_", 
                          format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(final_deaths, paste0(open_data, "deaths_data.rds"))

### END