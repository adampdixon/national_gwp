#######################################
# Function: 3_Create_management_input_files-LDNDC.R
# Author: Ellen Maas
# Date: Sept 23, 2022
# Output: It creates files in the appropriate folder for each model.
# Description: "This procedure generates management data - including crop,
# fertilizer, irrigation, fertilizer, harvest, etc. - for every model in the 
# format needed by each. For APSIM, fertilizer is collected as kg N/ha of the N 
# in the fertilizer (not kg/ha of the whole fertilizer)."
#######################################
# Audit Log
# 9/30/2022: Modified to include T3 data for scenario 3.
# 10/3/2022: Changed hairy vetch crop type to white clover
#######################################

print("Starting 3_Create_management_input_files-LDNDC.R")

## combine experimental with future

exp_df <- unique(full_ops_ext_adj[full_ops_ext_adj$treatment==treatment,
                       c("year","treatment","observation_type","date","crop",
                         "obs_code","n_rate_kg_ha","till_depth")])

DNDC_3yr <- exp_df[exp_df$year %in% experiment_end_year-2:experiment_end_year,]

for (i in 1:27) {
  DNDC_3yr$year <- as.character(as.integer(DNDC_3yr[,"year"]) + 3)
  DNDC_3yr$date <- DNDC_3yr$date + years(3)
  df <- rbind(exp_df, DNDC_3yr)
}



df$dndc_event <- if_else(df$observation_type=="Harvest","harvest",
                 if_else(df$observation_type=="Planting","plant",
                 if_else(df$observation_type=="Fertilizer application", "fertilize",
                 if_else(df$obs_code %in% c("plow","disk","cultivate",
                                            "cultipack","finish","hoe"),
                 "till",NULL))))

df$dndc_plant_type <- if_else(df$crop=="Maize","foco",
                      if_else(df$crop=="Soybean","SOYB",
                      if_else(df$crop=="Wheat","WIWH",
                      NULL)))

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
df$initialbiomass <- if_else(df$crop=="Maize",120,
                     if_else(df$crop=="Soybean",100,
                     if_else(df$crop=="Wheat",80,
                     NULL)))



# this is the top level - "event" encapsulates the whole doc
doc <- newXMLDoc()
root = newXMLNode("event", doc=doc)


# this is the second level - also "event" tags, with attributes and sub-child nodes
for(i in 1:nrow(df)) {
  event_node <- newXMLNode("event", parent=root)
  addAttributes(event_node, type = df[i,"dndc_event"],
                time = as.character(df[i,"date"])
  # addAttributes(event_node, type = paste0("\"",df[i,"dndc_event"],"\""),
  #               time = paste0("\"",df[i,"date"],"\"")
  )
  sub_node <- newXMLNode(df[i,"dndc_event"],parent=event_node)
  switch(df[i,"dndc_event"],
         # "plant" = addAttributes(sub_node, type = paste0("\"",df[i,"dndc_plant_type"],"\"")),
         # "till" = addAttributes(sub_node, depth = paste0("\"",df[i,"till_depth"],"\"")),
         # "fertilize" = addAttributes(sub_node, amount = paste0("\"",df[i,"n_rate_kg_ha"],"\""),
         #                             type = "no3"),
         # "harvest" = addAttributes(sub_node, remains = "\"0.25\"",
         #                           name = paste0("\"",df[i,"dndc_plant_type"],"\""))
         "plant" = {addAttributes(sub_node, type = df[i,"dndc_plant_type"])
           sub2_node <- newXMLNode("crop",parent=sub_node)
           addAttributes(sub2_node, initialbiomass = df[i,"initialbiomass"])
           },
         "till" = addAttributes(sub_node, depth = df[i,"till_depth"]),
         "fertilize" = addAttributes(sub_node, amount = df[i,"n_rate_kg_ha"],
                                     type = "no3"),
         "harvest" = addAttributes(sub_node, remains = "0.25",
                                   name = df[i,"dndc_plant_type"])
  )
}



# Write file --------------------------------------------------------------


# future
saveXML(doc,file=paste0(dndc_path,"mana_",mgmt_scenario_num,".xml"))


