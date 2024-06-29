# Create management xml LDNDC file

library(XML)


# this is the top level - "event" encapsulates the whole doc
doc <- newXMLDoc()
root = newXMLNode("event", doc=doc)

full_df<-county_mana%>%
  filter(Crop_Name==crop)%>%
  filter(Scenario==0 | Scenario==m) # 0 is the time before scenarios start, m is mgmt scenario from controller

mgmt_hist<-unique(full_df$time_period)

# create the management cycle for every year

# Create loop for all crops besides rotation and wheat
if(!identical(crop, "Rotation")){
  
  for(h in mgmt_hist){
    
    # get first and last year of management period
    first_year<-as.integer(substr(h, 1, 4))
    last_year<-as.integer(substr(h, 6, 9))
    
    management_period_df<-full_df%>%
      filter(time_period==h) # gets only the scenario and drops historic data coded 0
    
    print(paste0("Creating events for LDNDC from ", first_year, " to ", last_year))
    
    # go through each year and repeat the management cycle
    for(y in first_year:last_year){
      
      # this is the second level - also "event" tags, with attributes and sub-child nodes
      for(i in 1:nrow(management_period_df)) {
        event_node <- newXMLNode("event", parent=root)
        addAttributes(event_node, type = management_period_df[i,"dndc_event"],
                      # time = as.character(management_period_df[i,"date"])
                      time = paste0(y, "-", as.character(management_period_df[i,"date2"]))
                      # addAttributes(event_node, type = paste0("\"",df[i,"dndc_event"],"\""),
                      #               time = paste0("\"",df[i,"date"],"\"")
        )
        sub_node <- newXMLNode(management_period_df[i,"dndc_event"],parent=event_node)
        switch(management_period_df[i,"dndc_event"],
               "plant" = {addAttributes(sub_node, type = management_period_df[i,"dndc_plant_type"],
                                        name =  management_period_df[i,"dndc_plant_type"])
                 sub2_node <- newXMLNode("crop",parent=sub_node)
                 addAttributes(sub2_node, initialbiomass = management_period_df[i,"initialbiomass"],
                               covercrop = management_period_df[i,"covercrop"])
               },
               "till" = addAttributes(sub_node, depth = management_period_df[i,"till_depth_m"]),
               "fertilize" = addAttributes(sub_node, amount = management_period_df[i,"n_rate_kg_ha"],
                                           type = "no3"),
               "harvest" = addAttributes(sub_node, remains = "0.25",
                                         name = management_period_df[i,"dndc_plant_type"])
        )
      }
    } # end of year sequence
  }
} # end of if statement

# Create loop for rotation 
if(identical(crop, "Rotation")){
  
  for(h in mgmt_hist){
    print(h)
    
    # get first and last year of management period
    first_year<-as.integer(substr(h, 1, 4))
    last_year<-as.integer(substr(h, 6, 9))
    
    # Filter for time period
    management_period_df<-full_df%>%
      filter(time_period==h)
    
    # print(management_period_df)
    #################################################################
    paste0("Creating events for Rotation for LDNDC from ", first_year, " to ", last_year)
    #################################################################
    if (management_period_df$Scenario[1] == 0){ # If the first scenario is 0, then it is the historic data
      # go through each year and repeat the management cycle
      for(y in first_year:last_year){
        #################################################################
        # this is the second level - also "event" tags, with attributes and sub-child nodes
        #################################################################
        for(i in 1:nrow(management_period_df)) {
          event_node <- newXMLNode("event", parent=root)
          addAttributes(event_node, type = management_period_df[i,"dndc_event"],
                        # time = as.character(management_period_df[i,"date"])
                        time = paste0(y, "-", as.character(management_period_df[i,"date2"]))
                        # addAttributes(event_node, type = paste0("\"",df[i,"dndc_event"],"\""),
                        #               time = paste0("\"",df[i,"date"],"\"")
          )
          sub_node <- newXMLNode(management_period_df[i,"dndc_event"],parent=event_node)
          switch(management_period_df[i,"dndc_event"],
               "plant" = {addAttributes(sub_node, type = management_period_df[i,"dndc_plant_type"], 
                                        name =  management_period_df[i,"dndc_plant_type"])
                 sub2_node <- newXMLNode("crop",parent=sub_node)
                 addAttributes(sub2_node, initialbiomass = management_period_df[i,"initialbiomass"],
                               covercrop = management_period_df[i,"covercrop"])
                 },
                 "till" = addAttributes(sub_node, depth = management_period_df[i,"till_depth_m"]),
                 "fertilize" = addAttributes(sub_node, amount = management_period_df[i,"n_rate_kg_ha"],
                                             type = "no3"),
                 "harvest" = addAttributes(sub_node, remains = "0.25",
                                           name = management_period_df[i,"dndc_plant_type"])
          )
        }
      } # end of year sequence
    }
    
    if (management_period_df$Scenario[1] != 0){ # now move into scenario years
      #################################################################
      # only run for one year if it's a single year management cycle
      #################################################################
      if(length(first_year:last_year) == 1){
        print("one year mgmt cycle")
        # go through each year and repeat the management cycle
        # for(y in seq(first_year, last_year, by = 2)){
        #   print(y)
        #   for (p in 0:1){ # run the loop twice for each year
        
        p=0 # p =0 because no years added (this was repurposed from p loop below)
        y=first_year
            
            print(paste0("sequence year written: ", y+p)) ## THIS IS THE DIFFERENCE, ADDING A YEAR TO EACH SEQUENCE YEAR
            management_period_df2<-filter(management_period_df, start==y)
            # this is the second level - also "event" tags, with attributes and sub-child nodes
            for(i in 1:nrow(management_period_df2)) {
              event_node <- newXMLNode("event", parent=root)
              addAttributes(event_node, type = management_period_df2[i,"dndc_event"],
                            # time = as.character(management_period_df2[i,"date"])
                            time = paste0(y+p, "-", as.character(management_period_df2[i,"date2"]))  # add 1 during each second year
                            # addAttributes(event_node, type = paste0("\"",df[i,"dndc_event"],"\""),
                            #               time = paste0("\"",df[i,"date"],"\"")
              )
              sub_node <- newXMLNode(management_period_df2[i,"dndc_event"],parent=event_node)
              switch(management_period_df2[i,"dndc_event"],
                     "plant" = {addAttributes(sub_node, type = management_period_df2[i,"dndc_plant_type"])
                       sub2_node <- newXMLNode("crop",parent=sub_node)
                       addAttributes(sub2_node, initialbiomass = management_period_df2[i,"initialbiomass"])
                     },
                     "till" = addAttributes(sub_node, depth = management_period_df2[i,"till_depth_m"]),
                     "fertilize" = addAttributes(sub_node, amount = management_period_df2[i,"n_rate_kg_ha"],
                                                 type = "no3"),
                     "harvest" = addAttributes(sub_node, remains = "0.25",
                                               name = management_period_df2[i,"dndc_plant_type"])
              )
              
          #   } # end of i loop
          # } # end of p loop
        } # end of year sequence
      } # end of if == 1 year sequence statement
      #################################################################
      # Or -- run for multiple years
      #################################################################
      if(length(seq(first_year, last_year, by = 2)) > 1){
        print("now in 2 year mgmt cycle")
        
        # go through each year and repeat the management cycle
        for(y in seq(first_year, last_year, by = 2)){
          print(y)
          for (p in 0:1){ # run the loop twice, once for each year
            
            
            print(paste0("sequence year written: ", y+p)) ## THIS IS THE DIFFERENCE, ADDING A YEAR TO EACH SEQUENCE YEAR
            management_period_df2<-filter(management_period_df, sequence_years==p)
            # this is the second level - also "event" tags, with attributes and sub-child nodes
            for(i in 1:nrow(management_period_df2)) {
              event_node <- newXMLNode("event", parent=root)
              addAttributes(event_node, type = management_period_df2[i,"dndc_event"],
                            # time = as.character(management_period_df2[i,"date"])
                            time = paste0(y+p, "-", as.character(management_period_df2[i,"date2"]))  # add 1 during each second year
                            # addAttributes(event_node, type = paste0("\"",df[i,"dndc_event"],"\""),
                            #               time = paste0("\"",df[i,"date"],"\"")
              )
              sub_node <- newXMLNode(management_period_df2[i,"dndc_event"],parent=event_node)
              switch(management_period_df2[i,"dndc_event"],
                     "plant" = {addAttributes(sub_node, type = management_period_df2[i,"dndc_plant_type"])
                       sub2_node <- newXMLNode("crop",parent=sub_node)
                       addAttributes(sub2_node, initialbiomass = management_period_df2[i,"initialbiomass"])
                     },
                     "till" = addAttributes(sub_node, depth = management_period_df2[i,"till_depth_m"]),
                     "fertilize" = addAttributes(sub_node, amount = management_period_df2[i,"n_rate_kg_ha"],
                                                 type = "no3"),
                     "harvest" = addAttributes(sub_node, remains = "0.25",
                                               name = management_period_df2[i,"dndc_plant_type"])
              )
              
            } # end of i loop
          } # end of p loop
        } # end of year sequence
      } # end of if > 1 year sequence statement

    } # end of if statement
  } # end of mgmt periods loop
} # end of if identical to Rotation


saveXML(doc,file=paste0(dndc_path,"mana_",scenario_name2,".xml"))

print(paste0("Saved LDNDC event file from ", experiment_start_date, " to ", last_year))


