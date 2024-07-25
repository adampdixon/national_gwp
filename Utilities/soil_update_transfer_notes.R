to_get<-c(1043,
  1045,
  1047,
  1049,
  1051,
  9003,
  25001,
  29093,
  29179,
  48007,
  51059,
  51087,
  51089,
  51153,
  51161,
  51600,
  51685,
  51690,
  51760,
  51775,
  53029,
  55011,
  55091,
  56031,
  56033,
  56035,
  56037,
  56039,
  56041,
  56043,
  56045)

soils_data<-list.files('/home/ap/soils_all', pattern = 'csv', full.names = T)

search<-paste0(to_get, collapse = '_|_')

search2<-paste0('_', search, '_')

file.copy(soils_data[grepl(search2, soils_data)], '/home/ap/soils_to_transfer')

