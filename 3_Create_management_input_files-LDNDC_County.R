# Create management xml LDNDC file


# this is the top level - "event" encapsulates the whole doc
doc <- newXMLDoc()
root = newXMLNode("event", doc=doc)

full_df<-read.csv('/home/ap/Downloads/test3.csv')
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
doc


saveXML(doc,file=paste0(dndc_path,"mana_",mgmt_scenario_num,".xml"))