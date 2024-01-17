# Daycent results at county level mapped out
# January 15 2023


library(tigris)
# options(tigris_use_cache = TRUE)
library(ggplot2)
library(sf)
library(dplyr)
library(rnassqs)
# NASSQS_TOKEN="25DCA0AC-9720-345F-8989-49876C2D6C30"
# nassqs_auth(key = NASSQS_TOKEN)


# Get county crop acreage
params <- list(
  commodity_desc = c("CORN","WHEAT", "SOYBEANS"),
  year = "2017",
  agg_level_desc = "COUNTY"
)
crop_area<-nassqs_acres(params)
head(crop_area)

unique(crop_area$statisticcat_desc)

results_path<-'/home/ap/Daycent_results'

# quickstats_data<-'/home/ap/Documents/GitHub/national_gwp/Data/County_start/county_crop_totals_nass_quickstats.csv'

# lower_48<-c('AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'ID', 
#              'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 
#              'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 
#              'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 
#              'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')
# 
# 
# lower_48<-c(1,4,5,6,8,9,10,11,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
#             30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56)
# ("%05d",
lower_48<-sprintf("%02d" , c(1,4,5,6,8,9,10,11,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
            30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56))
# 
# georgia<-c(13)



# Get in Daycent results

# Get 2017 SOC, Corn, Wheat, Soybean Yield into 1 table

daycent_results<-function(state=NULL, Year=NULL, Results_path, crop_area=quickstats_data){
  # state: Full name capitalized, eg. Georgia
  # Crop options: CORN, WHEAT, SOYBEANS
  csv_list<-list.files(Results_path, full.names=T)
  files<-list.files(Results_path) # NOT FULL NAMES

  if (!is.null(state)){ # get state only results
    gets<-grep(state, csv_list)
    csv_list<-csv_list[gets]
    files<-files[gets]
    print("Getting state only results")
    print(state)
  }
  
  # read csvs in list and place in df
  df<-data.frame()
  for (i in 1:length(csv_list)){
    # print(paste0("Reading ", csv_list[i]))
    
    # Split file name to get geoid
    geoid<-as.integer(strsplit(files[i], "_")[[1]][3])

    r_county<-read.csv(csv_list[i])
    if (!is.null(Year)){
      r_county<-filter(r_county, year==Year)
    }
    # Add county GEOID
    r_county$GEOID<-geoid

    df<-rbind(df, r_county)
  }
  # # Add crop totals
  # 
  # if(!is.null(Crop)){
  #   crop_area2<-filter(crop_area_df, commodity_desc==Crop)
  # }

  df<-select(df, MaizeYld_Mgha:SOC_Mgha, GEOID, year)%>%
    arrange(GEOID)%>%
    as_tibble()
  return(df)
  
}




result<-daycent_results(state='Georgia', Year=2017, Results_path=results_path)
head(result)
nrow(result)



############ GET CROP ACREAGE

the_state<-counties() %>%
  filter(STATEFP %in% c("08"))%>%
  mutate(GEOID2=as.integer(GEOID))

crop_area2<-crop_area%>%
  filter(state_fips_code=="08")%>%
  filter(statisticcat_desc=="AREA PLANTED", commodity_desc == "CORN")%>%
  mutate(GEOID=as.integer(paste0(state_ansi, county_ansi)))%>%
  filter(GEOID > 1000)%>% # this removes the "overall" row
  select(statisticcat_desc, commodity_desc, GEOID, state_name, county_name, state_ansi, county_ansi, source_desc, Value, unit_desc)%>%
  arrange(GEOID)%>%
  as_tibble()

# also read crop area data

# State2<-toupper(gsub("_", " ", state)) # this should get all the states
# crop_area_df<-crop_area2%>%
#   filter(state_name==State2)

counties_df<-st_drop_geometry(the_state)%>%as_tibble()%>%mutate(GEOID2=as.integer(GEOID))%>%
  select(STATEFP, COUNTYFP, GEOID, GEOID2, NAME)

# THIS GETS ALL THE DATA
all_data<-left_join(crop_area2, result, by="GEOID")%>%
  mutate(TONS_ACRE=MaizeYld_Mgha*0.446089561)%>%
  select(year, MaizeYld_Mgha:SOC_Mgha, state_name, county_name, GEOID, commodity_desc, Value, unit_desc, TONS_ACRE)

all_data


# NOW DO SPATIAL JOIN

all_data_sp<-left_join(all_data, counties, by=c("GEOID"="GEOID2"))


state_plot<-ggplot() +
  geom_sf(data = counties, aes(geometry = geometry)) +
  geom_sf(data = all_data_sp, aes(geometry = geometry, fill = TONS_ACRE)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  theme_bw()








############ GET CROP ACREAGE FOR WHOLE US

result<-daycent_results(state=NULL, Year=2017, Results_path=results_path)
head(result)
nrow(result)

counties<-counties() %>%
  filter(STATEFP %in% lower_48)%>%
  mutate(GEOID2=as.integer(GEOID))

crop_area2<-crop_area%>%
  # filter(state_name=="GEORGIA")%>%
  filter(statisticcat_desc=="AREA PLANTED", commodity_desc == "CORN")%>%
  mutate(GEOID=as.integer(paste0(state_ansi, county_ansi)))%>%
  filter(GEOID > 1000)%>% # this removes the "overall" row
  select(statisticcat_desc, commodity_desc, GEOID, state_name, county_name, state_ansi, county_ansi, source_desc, Value, unit_desc)%>%
  arrange(GEOID)%>%
  as_tibble()

# also read crop area data

# State2<-toupper(gsub("_", " ", state)) # this should get all the states
# crop_area_df<-crop_area2%>%
#   filter(state_name==State2)

counties_df<-st_drop_geometry(counties)%>%as_tibble()%>%mutate(GEOID2=as.integer(GEOID))%>%
  select(STATEFP, COUNTYFP, GEOID, GEOID2, NAME)

# THIS GETS ALL THE DATA
all_data<-left_join(crop_area2, result, by="GEOID")%>%
  mutate(TONS_ACRE=MaizeYld_Mgha*0.446089561)%>%
  select(year, MaizeYld_Mgha:SOC_Mgha, state_name, county_name, GEOID, commodity_desc, Value, unit_desc, TONS_ACRE)

all_data


# NOW DO SPATIAL JOIN

all_data_sp<-left_join(all_data, counties, by=c("GEOID"="GEOID2"))


us<-ggplot() +
  geom_sf(data = counties, aes(geometry = geometry)) +
  geom_sf(data = all_data_sp, aes(geometry = geometry, fill = TONS_ACRE)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  theme_bw()

us

ggsave('/home/ap/Documents/GitHub/scratch_figs/us_corn.png',plot=us, dpi=300)




NASS_data<-left_join(counties, crop_area2, by =c('GEOID2'='GEOID'))

us<-ggplot() +
  geom_sf(data = counties, aes(geometry = geometry)) +
  geom_sf(data = NASS_data, aes(geometry = geometry, fill = Value)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  theme_bw()

us


nass_corn<-read.csv('/home/ap/Documents/GitHub/national_gwp/Data/County_start/NASS/corn_2017.csv')%>%as_tibble()%>%
  mutate(GEOID=paste0(sprintf("%02d", State.ANSI), sprintf("%03d", County.ANSI)), Acres = as.integer(Value))%>%
  select(Year, GEOID, Data.Item, Domain, Acres)

nass_corn2<-left_join(nass_corn, counties, by =c('GEOID'='GEOID'))

corn<-ggplot() +
  # geom_sf(data = counties, aes(geometry = geometry)) +
  geom_sf(data = nass_corn2, aes(geometry = geometry, fill = Acres)) +
  # scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  theme_bw()

corn




#
























# 
# 
# 
# 
# 
# 
# 
# 
# # get map for state or lower 48
# 
# get_Daycent_map<-function(State=NULL, Crop_area=crop_area, Crop=NULL){
#   
#   ############ GET CROP ACREAGE
#   
#   if(!is.null(State)){
#     crop_area2<-Crop_area%>%
#       filter(state_name==State)
#     
#   if(!is.null(Crop)){
#     crop_area2<-filter(statisticcat_desc=="AREA PLANTED", commodity_desc == "CORN")%>%
#       mutate(GEOID=as.integer(paste0(state_ansi, county_ansi)))%>%
#       filter(GEOID > 1000)%>% # this removes the "overall" row
#       select(statisticcat_desc, commodity_desc, GEOID, state_name, county_name, state_ansi, county_ansi, source_desc, Value, unit_desc)%>%
#       arrange(GEOID)%>%
#       as_tibble()
#   
# 
#   
#   # also read crop area data
#   
#   # State2<-toupper(gsub("_", " ", state)) # this should get all the states
#   # crop_area_df<-crop_area2%>%
#   #   filter(state_name==State2)
#   
#   counties_df<-st_drop_geometry(counties)%>%as_tibble()%>%mutate(GEOID2=as.integer(GEOID))%>%
#     select(STATEFP, COUNTYFP, GEOID, GEOID2, NAME)
#   
#   # THIS GETS ALL THE DATA
#   all_data<-left_join(crop_area2, result, by="GEOID")%>%
#     mutate(TONS_ACRE=MaizeYld_Mgha*0.446089561)%>%
#     select(year, MaizeYld_Mgha:SOC_Mgha, state_name, county_name, GEOID, commodity_desc, Value, unit_desc, TONS_ACRE)
#   
#   all_data
#   
#   
#   # NOW DO SPATIAL JOIN
#   
#   all_data_sp<-left_join(all_data, counties, by=c("GEOID"="GEOID2"))
#   
#   
#   ggplot() +
#     geom_sf(data = counties, aes(geometry = geometry)) +
#     geom_sf(data = all_data_sp, aes(geometry = geometry, fill = TONS_ACRE)) +
#     scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#     theme_bw()
# }
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
