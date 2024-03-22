#*************************************************
# Function: 3_Create_management_input_files-LDNDC.R
# Author: Ellen Maas
# Date: Sept 23, 2022
# Output: It creates files in the appropriate folder for each model.
# Description: "This procedure generates management data - including crop,
# fertilizer, irrigation, fertilizer, harvest, etc. - for every model in the 
# format needed by each. For APSIM, fertilizer is collected as kg N/ha of the N 
# in the fertilizer (not kg/ha of the whole fertilizer)."
#*************************************************
# Audit Log
# 9/30/2022: Modified to include T3 data for scenario 3.
# 10/3/2022: Changed hairy vetch crop type to white clover
#*************************************************

print(paste0("Starting 3_Create_management_input_files-LDNDC_County.R"))

mgmt_filename <- paste0(dndc_path,"mana_",mgmt_scenario_num,".xml")


# Remove existing file ----------------------------------------------------
unlink(mgmt_filename)


# experimental ------------------------------------------------------------

## start with cleaning experimental data

exp_df_raw <- unique(full_ops_ext_adj[full_ops_ext_adj$treatment==treatment,
                                  c("year","treatment","observation_type","date","crop",
                                    "obs_code","n_rate_kg_ha","till_depth")])

## remove phosphate and lime - not an option in LDNDC
exp_df <- exp_df_raw %>%
  filter(obs_code != "RockP" & obs_code != "CalciteCA")

## convert tillage depth to m
exp_df$till_depth_m <- exp_df$till_depth/1000

# start full-scenario data frame
full_df <- exp_df


  DNDC_3yr <- exp_df[exp_df$year %in% (experiment_end_year-2):experiment_end_year,]
  
  # Add conventional tillage back in for future scenario
  if(mgmt_scenario_grp!=2) {
    tillage_ops <- data.frame(year=c(2020,2021,2021),
                              treatment=c(treatment,treatment,treatment),
                              observation_type=c("Soil Preparation","Soil Preparation","Soil Preparation"),
                              date=c(as.Date("2020/04/10"),as.Date("2021/04/15"),as.Date("2021/04/20")),
                              crop=NA,
                              obs_code=c("plow","plow","disk"),
                              n_rate_kg_ha=NA,
                              till_depth=NA,
                              till_depth_m=c(.110,.110,.030)
    )
    DNDC_ops_3yr <- rbind(DNDC_3yr,tillage_ops) %>%
      arrange(date)
  } else {
    DNDC_ops_3yr <- DNDC_3yr
  }
  
  if(mgmt_scenario_grp==4) {
    # reduce fertilizer
    DNDC_ops_3yr$n_rate_kg_ha <- DNDC_ops_3yr$n_rate_kg_ha*fert_adjust
  } 
  
  # Now take the 3-year management actions and repeat them out to 2100
  repeat_times <- ceiling((max_fut_period_year-experiment_end_year)/3)
  
  for (i in 1:repeat_times) {
    DNDC_ops_3yr$year <- as.character(as.integer(DNDC_ops_3yr[,"year"]) + 3)
    DNDC_ops_3yr$date <- DNDC_ops_3yr$date + years(3)
    full_df <- rbind(full_df, DNDC_ops_3yr)
  }
  


# tailor to LDNDC --------------------------------------------------


full_df$dndc_event <- if_else(full_df$observation_type=="Harvest","harvest",
                 if_else(full_df$observation_type=="Planting","plant",
                 if_else(full_df$observation_type=="Fertilizer application", "fertilize",
                 if_else(full_df$obs_code %in% c("plow","disk","cultivate",
                                            "cultipack","finish","hoe"),
                 "till", 'NA'))))  # AD REPLACED NULL WITH 'NA' string because NULL was throwing error

full_df$dndc_plant_type <- if_else(full_df$crop=="Maize","FOCO",
                                   
                      if_else(full_df$crop=="Soybean","SOYB",
                      if_else(full_df$crop=="Wheat","WIWH",
                      'NA')))

# ?
# df$initialbiomass <- if_else(df$crop=="Maize",
#                              as.numeric(ObsBiomass[ObsBiomass$year==min(ObsBiomass[ObsBiomass$Species=="Glycine max L. (*)","year"]) &
#                                                      ObsBiomass$Fraction=="WHOLE", "biomass_gm2"])*10,
#                              if_else(df$crop=="Soybean",
#                                      as.numeric(ObsBiomass[ObsBiomass$year==min(ObsBiomass[ObsBiomass$Species=="Glycine max L. (*)","year"]) &
#                                                              ObsBiomass$Fraction=="WHOLE", "biomass_gm2"])*10,
#                                      if_else(df$crop=="Wheat",
#                                              as.numeric(ObsBiomass[ObsBiomass$year==min(ObsBiomass[ObsBiomass$Species=="Glycine max L. (*)","year"]) &
#                                                                      ObsBiomass$Fraction=="WHOLE", "biomass_gm2"])*10,
#                                              NULL)))

# ***** NEED TO CHECK THESE VALUES *****
full_df$initialbiomass <- if_else(full_df$crop=="Maize",120,
                     if_else(full_df$crop=="Soybean",100,
                     if_else(full_df$crop=="Wheat",80,
                     -9999))) # had to set to integer to avoid error


# create XML --------------------------------------------------------------


# this is the top level - "event" encapsulates the whole doc
doc <- newXMLDoc()
root = newXMLNode("event", doc=doc)


# this is the second level - also "event" tags, with attributes and sub-child nodes
for(i in 1:nrow(full_df)) {
  event_node <- newXMLNode("event", parent=root)
  addAttributes(event_node, type = full_df[i,"dndc_event"],
                time = as.character(full_df[i,"date"])
  # addAttributes(event_node, type = paste0("\"",df[i,"dndc_event"],"\""),
  #               time = paste0("\"",df[i,"date"],"\"")
  )
  sub_node <- newXMLNode(full_df[i,"dndc_event"],parent=event_node)
  switch(full_df[i,"dndc_event"],
         "plant" = {addAttributes(sub_node, type = full_df[i,"dndc_plant_type"])
           sub2_node <- newXMLNode("crop",parent=sub_node)
           addAttributes(sub2_node, initialbiomass = full_df[i,"initialbiomass"])
           },
         "till" = addAttributes(sub_node, depth = full_df[i,"till_depth_m"]),
         "fertilize" = addAttributes(sub_node, amount = full_df[i,"n_rate_kg_ha"],
                                     type = "no3"),
         "harvest" = addAttributes(sub_node, remains = "0.25",
                                   name = full_df[i,"dndc_plant_type"])
  )
}



# Write file --------------------------------------------------------------


# future
saveXML(doc,file=mgmt_filename)


# Clean up ----------------------------------------------------------------

rm(exp_df_raw, exp_df, full_df, DNDC_3yr, DNDC_ops_3yr, tillage_ops,
   doc, root, event_node, sub_node)

