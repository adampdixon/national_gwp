#######################################
# File: "GWP_national_maps.R"
# Author: "Adam Dixon"
# Date: "July 2024"
# Description: Model results at county level mapped out for the whole US.
#
#######################################

if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    source('/home/ap/Documents/GitHub/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  } else {
    source('/glade/derecho/scratch/apdixon/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  }
}


library(tigris)
options(tigris_use_cache = TRUE)
library(ggplot2)
library(sf)
library(dplyr)
library(scales)
library(gridExtra)
library(patchwork)
# library(grid)
# library(png)
# library(stringr)
# library(rnassqs)
# NASSQS_TOKEN="25DCA0AC-9720-345F-8989-49876C2D6C30"
# nassqs_auth(key = NASSQS_TOKEN)

# this script has get model table functions
source(file.path(master_path, 'data_explore', 'get_model_tables.R'), local = TRUE)



# home_folder<-'/home/ap/Documents/GitHub/national_gwp/Data/County_start'
# cdl_count<-'/home/ap/CDL/main_county_crops.csv'

print(date)

############ GET MAPS FOR WHOLE US AND ALL SCENARIOS




national_map_all_scenarios<-function(Year_, Crop_, Output, map_type, clim_scen){
  
  type = map_type
  
  

  # Function from get_model_tables.R
  county_df<-get_all_models_national_df(crop_to_get = Crop_)
  
  # subset to just year 2050
  county_df<-county_df[year == 2050]

  # Get a rotation yield column if it's rotation
  if(identical(Crop_, 'Rotation') & identical("Yld", map_type)){
    county_df<-county_df%>%
      mutate(RotationYld_Mgha = ifelse(is.na(MaizeYld_Mgha), SoybeanYld_Mgha, MaizeYld_Mgha))
      # select(year, model, climate_scenario, RotationYld_Mgha)
  }
  
  # remove millennial if it didn't output variable
  if (identical("Yld", map_type) | identical(map_type, 'N2O') | identical(map_type, 'CH4')){
    county_df<-county_df%>%
      filter(model != 'Millennial')
  }
  
    # result<-list.files(results_folder, pattern = "annual", recursive = T, full.names = TRUE)%>%
    #   lapply(read.csv)%>%
    #   bind_rows()%>%
    #   as_tibble()%>%
    #   filter(crop == Crop_, year == Year_)%>%
    #   select(GEOID, year, crop, crop_yld_Mgha, crop_ha, SOC_Mgha, mgmt_scenario_num, climate_scenario_num, scenario_name)
    

  lower_48<-sprintf("%02d" , c(1,4,5,6,8,9,10,11,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
                                 30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56))
    
    # lower_48<-18
    
  counties<-counties() %>% #tigris function
    filter(STATEFP %in% lower_48)%>%
    st_transform(5070)%>% # EQUAL AREA PROJECTION
    mutate(GEOID2=as.integer(GEOID))%>%
    st_simplify(dTolerance = 2000) # take time now to simplify polygons so drawing several maps later is faster
    # simplified by 2000 meters
  
  # NOW DO SPATIAL JOIN
  
  map_in_df<-left_join(county_df, counties, by=c("GEOID"="GEOID2"))
  
  if(map_type=='Yld'){
    breaks_df<-select(df, contains(paste0(Crop_, "Yld")))
    breaks<-unlist(breaks_df[,1])
    breaks_<-round(c(min(breaks), mean(breaks), max(breaks)),1)
    map_var<-paste0(Crop_, type, '_Mgha') # ALSO while we're here, get variable name for map
    }
  
  if(map_type=='SOC'){
    breaks_df<-select(df, contains("SOC"))
    breaks<-unlist(breaks_df[,1])
    breaks_<-round(c(min(breaks), mean(breaks), max(breaks)),1)
    map_var<-'SOC_Mghayr'
  }
  
  if(map_type=='CO2'){
    breaks_df<-select(df, contains("CO2"))
    breaks<-unlist(breaks_df[,1])
    breaks_<-round(c(min(breaks), mean(breaks), max(breaks)),1)
    map_var<-'CO2resp_ghayr'
  }
  
  if(map_type=='N2O'){
    breaks_df<-select(df, contains("N2O"))
    breaks<-unlist(breaks_df[,1])
    breaks_<-round(c(min(breaks), mean(breaks), max(breaks)),1)
    map_var<-'N2OEmissions_ghayr'
  }
  
  if(map_type=='CH4'){
    breaks_df<-select(df, contains("CH4"))
    breaks<-unlist(breaks_df[,1])
    breaks_<-round(c(min(breaks), mean(breaks), max(breaks)),1)
    map_var<-'CH4Emissions_ghayr'
  }
 
  ##############################################################################
  map_out<-function(df, type, breaks, prac_scen, clim_scen, mod){
    
    df2<-filter(df, 
                mgmt_scenario_num == prac_scen, 
                climate_scenario == clim_scen,
                model == mod)
    
    print('********************************************************************')
    print('Summary stats for model/mgmt/climate')
    print(paste0('Crop is ', Crop_, 'YldMgha'))
    print(paste('mgmt scenario is', prac_scen))
    print(paste('climate scenario is', clim_scen))
    print(paste('model is', mod))
    print(paste('map type is', type))
    
    df3<-select(df2, contains(type))
    print(paste('min:', round(min(as.numeric(unlist(df3[,1])), na.rm = T), 2)))
    print(paste('max:', round(max(as.numeric(unlist(df3[,1])), na.rm = T), 2)))
    print(paste('mean:', round(mean(as.numeric(unlist(df3[,1])), na.rm = T), 2)))
    print(paste('std dev:', round(sd(as.numeric(unlist(df3[,1])), na.rm = T), 2)))
    
    print('********************************************************************')
      
    text<-ifelse(df2$mgmt_scenario_num[1]==1, "Monocropping", 
                 ifelse(df2$mgmt_scenario_num[1]==2, "No-till",
                        ifelse(df2$mgmt_scenario_num[1]==3, "Cover crop species mix",
                               ifelse(df2$mgmt_scenario_num[1]==4, "Cover crop rye",
                                      ifelse(df2$mgmt_scenario_num[1]==5, "Cover crop legume",
                                             "No-till and cover crop mix"
                                      )))))
    
    
    # This is for title at top of grobs. if practice scenario is 1, then title goes at the top of plot and it only prints once, otherwise skip plot title
    clim_title<-ifelse(clim_scen=="low", " low clim.", " high clim.")
    
    if(prac_scen<2 & type=='Yld'){
      yield_title<-paste0(mod, ' ', Crop_, " yield (Mg ha yr) - ", Year_, clim_title)
      print(yield_title)
      
    }
    
    if(prac_scen<2 & type=='SOC'){
      yield_title<-paste0(mod, ' ', Crop_, " SOC (Mg ha yr) - ", clim_title)
      print(yield_title)
      
    }
    
    if(prac_scen<2 & type=='CH4'){
      yield_title<-paste0(mod, ' ', Crop_, " CH4 (g ha yr) - ", clim_title)
      print(yield_title)

    }
    
    if(prac_scen<2 & type=='N2O'){
      yield_title<-paste0(mod, ' ', Crop_, " ", 'N2O (g ha yr) - ', clim_title)
      print(yield_title)
      
    }
    
    if(prac_scen<2 & type=='CO2'){
      yield_title<-paste0(mod, ' ', Crop_, " ", " CO2 (g ha yr) - ", clim_title)
      print(yield_title)
      
    }
    
    if(prac_scen>1){
      yield_title<-''
    } 
    ##############################################################################
    # colo<-c('white', 'blue')
    
    
 
    
    map<-ggplot() +
      geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
      geom_sf(data = df2, aes(geometry = geometry, fill = get(map_var)), linewidth = 0) +
      # scale_color_manual(values= colo, breaks = breaks, name = "Mg ha", labels=breaks) +
      # scale_color_continuous(breaks = breaks, type = "viridis", name = "Mg ha", 
      #                        labels=breaks, limits=c(breaks[1], breaks[4])) +
      scale_fill_gradient(low = "white",high = "blue",
                          breaks = breaks, labels=breaks,
                          limits=c(breaks[1], breaks[3]),
                          name = "Mg ha") +
      # scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
      labs(title = yield_title,
           subtitle = paste0(text, " scenario")) +
      theme_bw()
    return(map)
  } # end of map function
  ##############################################################################
  
  # df = map_in_df; type=map_type; breaks = breaks_; prac_scen = 2; clim_scen = clim_scen; mod = 'LDNDC'
  
  scen_d_1<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 1, clim_scen = clim_scen, mod = 'Daycent')
  scen_d_2<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 2, clim_scen = clim_scen, mod = 'Daycent')
  scen_d_3<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 3, clim_scen = clim_scen, mod = 'Daycent')
  scen_d_4<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 4, clim_scen = clim_scen, mod = 'Daycent')
  scen_d_5<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 5, clim_scen = clim_scen, mod = 'Daycent')
  scen_d_6<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 6, clim_scen = clim_scen, mod = 'Daycent')

  scen_l_1<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 1, clim_scen = clim_scen, mod = 'LDNDC')
  scen_l_2<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 2, clim_scen = clim_scen, mod = 'LDNDC')
  scen_l_3<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 3, clim_scen = clim_scen, mod = 'LDNDC')
  scen_l_4<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 4, clim_scen = clim_scen, mod = 'LDNDC')
  scen_l_5<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 5, clim_scen = clim_scen, mod = 'LDNDC')
  scen_l_6<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 6, clim_scen = clim_scen, mod = 'LDNDC')
  
  crop_out<-file.path(Output, paste0('Maps_', Crop_, "_" ,map_type,"_maps.png"))
  
  # Outputs without Millennial
  if (identical("Yld", map_type) | identical(map_type, 'N2O') | identical(map_type, 'CH4')){
    out<-arrangeGrob(scen_d_1, scen_l_1, 
                     scen_d_2, scen_l_2, 
                     scen_d_3, scen_l_3, 
                     scen_d_4, scen_l_4,
                     scen_d_5, scen_l_5,
                     scen_d_6, scen_l_6,
                     ncol = 2, nrow = 6)
    ggsave(file = crop_out, plot=out, dpi=300, width = 10, height = 16)
    
    print(paste0('map plots saved to ', crop_out))

  } else { #Outputs with Millennial
    
    scen_m_1<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 1, clim_scen = clim_scen, mod = 'Millennial')
    scen_m_2<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 2, clim_scen = clim_scen, mod = 'Millennial')
    scen_m_3<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 3, clim_scen = clim_scen, mod = 'Millennial')
    scen_m_4<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 4, clim_scen = clim_scen, mod = 'Millennial')
    scen_m_5<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 5, clim_scen = clim_scen, mod = 'Millennial')
    scen_m_6<-map_out(df = map_in_df, type=map_type, breaks = breaks_, prac_scen = 6, clim_scen = clim_scen, mod = 'Millennial')
    
      out<-arrangeGrob(scen_d_1, scen_l_1, scen_m_1,
                       scen_d_2, scen_l_2, scen_m_2,
                       scen_d_3, scen_l_3, scen_m_3,
                       scen_d_4, scen_l_4, scen_m_4,
                       scen_d_5, scen_l_5, scen_m_5,
                       scen_d_6, scen_l_6, scen_m_6,
                       ncol = 3, nrow = 6)
      ggsave(file = crop_out, plot=out, dpi=300, width = 14, height = 16)
      
      print(paste0('map plots saved to ', crop_out))
      
    }
  }# end of function
  

# Year_ = 2050; Crop_ = 'Rotation'; Output = output_figs

# national_map_all_scenarios(Year_ = 2050, Crop_ = 'Maize', Output = results_folder, map_type = 'SOC', clim_scen = 'low')
