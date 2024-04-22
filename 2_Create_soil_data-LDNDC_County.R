#######################################
# Function: "p_Create_soil_data"
# Author: "Ellen Maas"
# Date: "July 11, 2022"
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates files in the appropriate folder for each model.
# Description: "This procedure generates soil data for the site."
#######################################

###########################
# LDNDC
###########################

library(xml2)

soil_filename <- paste0(dndc_path, site_name, "_site.xml")


# Remove old file ---------------------------------------------------------

# unlink(soil_filename)



# Generate new file -------------------------------------------------------

## this is the top level
doc <- xml_new_root("site",.version="1.0", .encoding="UTF-8")
xml_set_attr(doc, "id", site_id)

## these are the second level
xml_add_child(doc, "description") 
xml_add_child(doc, "soil")

## these are the third level
description_nodes <- xml_find_all(doc, "//description")
xml_add_child(description_nodes, "author", "Adam Dixon")
xml_add_child(description_nodes, "date", as.character(Sys.Date()))
xml_add_child(description_nodes, "dataset", site_name)

soil_nodes <- xml_find_all(doc, "//soil")
xml_add_child(soil_nodes, "general")
general_node <- xml_find_all(doc, "//general")
xml_set_attrs(general_node,c("usehistory" = "arable",
                             "humus" = "HUMUS",
                             "soil" = soil_type_code))
xml_add_child(soil_nodes, "layers")

layer_nodes <- xml_find_all(doc, "//layers")

## flip order of soil layers (bug in xml2?)
flipped_soil_df <- soil_df_L[order(nrow(soil_df_L):1),]

## LDNDC requires non-zero Corg values, so add a tiny amount in lower depths
flipped_soil_df[flipped_soil_df$orgC_fraction==0,"orgC_fraction"] <- 0.001

## add parameters, per feedback in log file, setting these for testing
## (to see if the messages go away)
flipped_soil_df$stonefraction <- as.numeric(c(0,0,0,0,0,0,0,0,0,0,0,0,0))
flipped_soil_df$iron <- as.numeric(c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))
#flipped_soil_df$macropores <- as.numeric(c(31,31,31,31,69.9,69.9,86,86.9,90.4,90.4,90.4,90.4,90.4))
flipped_soil_df$vangenuchten_n <- as.numeric(c(1.56,1.56,1.56,1.56,1.56,1.56,1.48,1.89,2.28,2.28,2.28,2.68,2.68))
flipped_soil_df$vangenuchten_alpha <- as.numeric(c(3.6,3.6,3.6,3.6,3.6,3.6,1.48,7.5,12.4,12.4,12.4,14.5,14.5))
flipped_soil_df$wfps_max <- as.numeric(c(0.83871,0.83871,0.83871,0.83871,1,1,0.88396,0.863309,0.821639,0.821639,0.821639,0.817398,0.817398))
flipped_soil_df$wfps_min <- as.numeric(c(0.217839,0.217839,0.217839,0.217839,0.0964858,0.0964858,0.0160529,0.0796081,0.0250582,0.0250582,0.0250582,0.0221216,0.0221216))

xml_add_child((layer_nodes), paste("layer ",
                                   #paste0("wcmin=\"",flipped_soil_df$LL15,"\""),
                                   #paste0("wcmax=\"",flipped_soil_df$DUL,"\""),
                                   paste0("wcmin=\"",flipped_soil_df$LL15_dm3m3,"\""),
                                   paste0("wcmax=\"",flipped_soil_df$DUL_dm3m3,"\""),
                                   paste0("ph=\"",flipped_soil_df$phaq_value_avg,"\""),
                                   paste0("corg=\"",flipped_soil_df$orgC_fraction,"\""),
                                   paste0("clay=\"",flipped_soil_df$clay_frac,"\""), #fraction
                                   paste0("sand=\"",flipped_soil_df$sand_frac,"\""), #fraction
                                   paste0("bd=\"",flipped_soil_df$bdfiod_value_avg,"\""), # BD
                                   paste0("depth=\"",flipped_soil_df$Thickness,"\""), # AD added reasonable numbers
                                   paste0("sks=\"",flipped_soil_df$KS_cmmin,"\"")#,
                                   # paste0("stonefraction=\"",flipped_soil_df$stonefraction,"\""),
                                   # paste0("iron=\"",flipped_soil_df$iron,"\""),
                                   # #paste0("macropores=\"",flipped_soil_df$macropores,"\""),
                                   # paste0("vangenuchten_n=\"",flipped_soil_df$vangenuchten_n,"\""),
                                   # paste0("vangenuchten_alpha=\"",flipped_soil_df$vangenuchten_alpha,"\""),
                                   # paste0("wfps_max=\"",flipped_soil_df$wfps_max,"\""),
                                   # paste0("wfps_min=\"",flipped_soil_df$wfps_min,"\"")
                                   )
              )

#doc
write_xml(doc,file=soil_filename,
          append=F)

# For looking at the data
# library(flextable)
# klaus<-select(flipped_soil_df, LL15_dm3m3, DUL_dm3m3, phaq_value_avg, orgC_fraction, clay_frac, sand_frac, bdfiod_value_avg, Thickness, KS_cmmin) #, upper_depth_cm, lower_depth_cm, DUL_dm3m3, LL15_dm3m3)
# 
# names(klaus)<-c('wcmin', 'wcmax', 'ph', 'corg', 'clay', 'sand', 'bd', 'depth', 'sks')
# flextable(klaus)%>%
#   colformat_double(digits = 3)


# Clean up ----------------------------------------------------------------

rm(doc, description_nodes, soil_nodes, general_node, layer_nodes,
   flipped_soil_df)

