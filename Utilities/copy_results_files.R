# Copy files from results folder to folder for transfer via Globus

library(R.utils)

# # File name
# r2<-list.files('/glade/derecho/scratch/apdixon/national_gwp_results', recursive=T, pattern='Annual_results', full.names=F)
# # file name and full path
# r1<-list.files('/glade/derecho/scratch/apdixon/national_gwp_results', recursive=T, pattern='Annual_results', full.names=T)

# # Dir name
# r2<-dir('/glade/derecho/scratch/apdixon/national_gwp_results', recursive=T, pattern='Kansas', full.names=F)
# # Dor name and full path
# r1<-dir('/glade/derecho/scratch/apdixon/national_gwp_results', recursive=T, pattern='Kansas', full.names=T)
# 
# for (i in 1:length(r2)){
#   paste0('Copying ', gsub("/", "_", r2[i]))
#   copyFile(r1[i], paste0('/glade/derecho/scratch/apdixon/national_gwp_results_send/',gsub("/", "_", r2[i])), skip=TRUE) 
# }




# Dir name
r2<-dir('/glade/derecho/scratch/apdixon/national_gwp_results', recursive=F, pattern='Kansas', full.names=F)
# Dir name and full path
r1<-dir('/glade/derecho/scratch/apdixon/national_gwp_results', recursive=F, pattern='Kansas', full.names=T)

for (i in 1:length(r2)){
  files<-list.files(r1[i], full.names=T, recursive=T, pattern='Annual_results')
  for (f in files){
    print(paste0('Copying ', f))
    copyFile(f, file.path('/glade/derecho/scratch/apdixon/national_gwp_results_send', basename(dirname(f)), basename(f)), skip=TRUE) 
  }
}
