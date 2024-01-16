# Daycent results at county level mapped out
# January 15 2023


library(tigris)
# options(tigris_use_cache = TRUE)

lower_48<-c('AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'ID', 
             'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 
             'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 
             'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 
             'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')

counties<-counties() %>% 
