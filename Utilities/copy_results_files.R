# Copy files from results folder to folder for transfer via Globus

library(R.utils)

# File name
r2<-list.files('/glade/derecho/scratch/apdixon/national_gwp_results', recursive=T, pattern='Annual_results', full.names=F)
# file name and full path
r1<-list.files('/glade/derecho/scratch/apdixon/national_gwp_results', recursive=T, pattern='Annual_results', full.names=T)

for (i in 1:length(r2)){
  copyFile(r1[i], paste0('/glade/derecho/scratch/apdixon/national_gwp_results_send/',gsub("/", "_", r2[i]))) 
}
