#######################################
# File: "GWP_national_boxplots.R"
# Author: "Adam Dixon"
# Date: "July 2024"
# Description: Model result boxplots.
#
#######################################

if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    source('/home/ap/Documents/GitHub/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  } else {
    source('/glade/derecho/scratch/apdixon/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  }
}

library(ggplot2)
library(dplyr)
library(scales) # needed?
library(gridExtra)
library(patchwork) # needed?

# this script has get model table functions
source(file.path(master_path, 'data_explore', 'get_model_tables.R'), local = TRUE)



# home_folder<-'/home/ap/Documents/GitHub/national_gwp/Data/County_start'
# cdl_count<-'/home/ap/CDL/main_county_crops.csv'

print(date)

############ GET MAPS FOR WHOLE US AND ALL SCENARIOS




national_boxplots_all_scenarios<-function(Crop_){
  
  print(paste('starting boxplots for', Crop_))
  
  # Function from get_model_tables.R
  county_df<-get_all_models_national_df(crop_to_get = Crop_)
  
  text<-function(x){
    ifelse(x==1, "Monocropping", 
           ifelse(x==2, "No-till",
                  ifelse(x==3, "Cover crop species mix",
                         ifelse(x==4, "Cover crop rye",
                                ifelse(x==5, "Cover crop legume",
                                       "No-till and cover crop mix"
                                )))))
  }
  
  county_df<-county_df%>%
    mutate(epoch = ifelse(year<2022, '1850-2021', '2022-2050'),
           epoch_levels = ifelse(year<2022, 1, 2),
           prac_scen = text(mgmt_scenario_num))
  
  
  ##############################################################################
  ##############################################################################
  boxplot_out<-function(df, result){
    
    # df2<-filter(df, mgmt_scenario_num == prac_scen, 
    #             climate_scenario == climate_scenario,
    #             model == model)
    
    # remove millennial if it didn't output variable
    if (grepl('Yld', result) | grepl('N2O', result)| grepl('CH4', result)){
      df<-df%>%
        filter(model != 'Millennial')
    }
    
    # Get a rotation yield column if it's rotation
    if(grepl('Rotation', result)){
      df<-df%>%
        mutate(RotationYld_Mgha = ifelse(is.na(MaizeYld_Mgha), SoybeanYld_Mgha, MaizeYld_Mgha))
      # select(year, model, climate_scenario, RotationYld_Mgha)
    }

  
    ##############################################################################
    
    if(grepl('SOC', result)){
      # grouped boxplot
      Boxplot<-ggplot(df, aes(x=reorder(factor(prac_scen), mgmt_scenario_num), y=get(result), 
                              color = model, fill = reorder(epoch, epoch_levels))) + 
        geom_boxplot() + 
        scale_fill_manual(values=c("blue","lightblue", "purple")) +
        scale_color_manual(values=c("black","gray50", "gray")) +
        theme_classic() +
        xlab('') +
        ylab(result) +
        ggtitle(Crop_) +
        theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=.5, size = 7)) +
        theme(legend.position="top") +
        guides(fill=guide_legend(title="epoch")) +
        facet_wrap(~factor(climate_scenario, levels = c('low', 'high'))) #INCLUDE FACET LABELS
      return(Boxplot)
    } else {
        # grouped boxplot NO FACET LABELS
        Boxplot<-ggplot(df, aes(x=reorder(factor(prac_scen), mgmt_scenario_num), y=get(result), 
                                color = model, fill = reorder(epoch, epoch_levels))) + 
          geom_boxplot() + 
          scale_fill_manual(values=c("blue","lightblue", "purple")) +
          scale_color_manual(values=c("black","gray50", "gray90")) +
          theme_classic() +
          xlab('') +
          ylab(result) +
          theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=.5, size = 7)) +
          theme(legend.position = "none") +
          facet_wrap(~factor(climate_scenario, levels = c('low', 'high'))) +
          theme( # NO FACET LABELS OR LEGEND
            strip.background = element_blank(),
            strip.text.x = element_blank()
          )
        return(Boxplot)
      }

    

  ##############################################################################
  
  }
  SOC<-boxplot_out(df = county_df, result = 'SOC_Mghayr')
  CO2<-boxplot_out(df = county_df, result = 'CO2resp_ghayr')  
  yield<-boxplot_out(df = county_df, result = paste0(Crop_, 'Yld_Mgha'))
  CH4<-boxplot_out(df = county_df, result = 'CH4Emissions_ghayr')
  N20<-boxplot_out(df = county_df, result = 'N2OEmissions_ghayr')

  
  out<-arrangeGrob(SOC, CO2, yield, CH4, N20, ncol = 1, nrow = 5)
  
  box_out<-file.path(national_figs, paste0('Boxplots_National_model_results_', Crop_,  "_", date, ".png"))
  
  ggsave(file = box_out, plot=out, dpi=300, width = 16, height = 16)
  
  print(paste0('saving Boxplots to ', box_out))
  
  
}# end of function

# national_boxplots_all_scenarios(Crop_='Maize')
