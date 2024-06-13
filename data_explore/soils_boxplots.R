

library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)
library(scales)

soil_data<-list.files('/home/ap/soils', full.names = T, pattern = '.csv')

library(data.table)

tbl_fread <- 
  list.files('/home/ap/soils', full.names = T, pattern = '.csv') %>% 
  map_df(~fread(.))%>%
  mutate(order = rep(1:14, 9))


tbl_fread_corrected<-mutate(tbl_fread, pH = ifelse(pH > 7.5, NA, pH)) # remove pH values greater than 7.5, some are obviously in error

tbl_fread_corrected<-mutate(tbl_fread_corrected, pH = ifelse(BD < 1, NA, pH)) # remove pH less than 1, less than this throws an error

tbl_fread_corrected<-na.locf(tbl_fread_corrected) # This sets NA values to the last non-NA value


level_order<-tbl_fread[1:14,'Depth_cm'][['Depth_cm']]



# grouped boxplot
BD_plot<-ggplot(tbl_fread, aes(x=factor(Depth_cm, level = level_order), y=BD)) + 
  geom_boxplot() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=.5)) +
  xlab('depth')


pH_plot<-ggplot(tbl_fread, aes(x=factor(Depth_cm, level = level_order), y=pH)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=.5)) +
  xlab('depth')

pH_plot_corrected<-ggplot(tbl_fread_corrected, aes(x=factor(Depth_cm, level = level_order), y=pH)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=.5)) +
  xlab('depth') +
  ylab('pH corrected')

Clay_plot<-ggplot(tbl_fread, aes(x=factor(Depth_cm, level = level_order), y=Clay)) + 
  geom_boxplot() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=.5)) +
  xlab('depth')

Sand_plot<-ggplot(tbl_fread, aes(x=factor(Depth_cm, level = level_order), y=Sand)) + 
  geom_boxplot() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=.5)) +
  xlab('depth')

Silt_plot<-ggplot(tbl_fread, aes(x=factor(Depth_cm, level = level_order), y=Silt)) + 
  geom_boxplot() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=.5)) +
  xlab('depth')

SOC_plot<-ggplot(tbl_fread, aes(x=factor(Depth_cm, level = level_order), y=SOC)) + 
  geom_boxplot() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=.5)) +
  xlab('depth')

out<-arrangeGrob(BD_plot, pH_plot, pH_plot_corrected,
                 Clay_plot, Sand_plot, 
                 Silt_plot, SOC_plot, ncol = 1, nrow = 7)

ggsave(file = '/home/ap/soils/figs/soils.png', plot=out, dpi=300, width = 8, height = 10)

