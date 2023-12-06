
# copying files over from glade to github so they can be used in prototyping

nclim_dir<-'/glade/work/apdixon/Output_nClimGrid'
cmip6_dir<-'/glade/work/apdixon/Output_climate'

years<-c('1951','1952','1953','1954')

pr<-list.files(nclim_dir, pattern = 'prcp', full.names = T)

pr2<-pr[str_detect(pr, '1951') | str_detect(pr, '1952') | str_detect(pr, '1953') | str_detect(pr, '1954')]
file.copy(pr2, '/glade/u/home/apdixon/Documents/national_gwp/climate_nclim/', overwrite = T)


cm<-list.files(cmip6_dir, pattern = 'ssp126_gfdl-esm4', full.names = T)
cm2<-cm[str_detect(cm, 'Y2022') | str_detect(cm, 'Y2023') | str_detect(cm, 'Y2024') | str_detect(cm, 'Y2025')]

cm3<-cm2[str_detect(cm2, 'pr_') | str_detect(cm2, 'tasmax') | str_detect(cm2, 'tasmin')]

file.copy(cm3, '/glade/u/home/apdixon/Documents/national_gwp/cmip6_climate')




