library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
source("add_z.R")
player_game_data <- read_csv("https://raw.githubusercontent.com/dutta/NFL3D/master/data/ngs_passing_play_index_expanded_full.csv")
# 
# by_passer<- player_game_data %>% group_by(passer) %>% summarise(mean_launch_angle = mean(launch_angle), num_passes = n()) %>% 
#   arrange(desc(mean_launch_angle)) %>% filter(mean_launch_angle < 45) %>% filter(num_passes > 3)
# 
# grouped <- player_game_data %>% group_by(passer) %>% 
#   summarise(passes = n(), mla = mean(launch_angle), mlv = mean(v)*2.05) %>% 
#   mutate_if(is.numeric, round, digits = 2)
# grouped <- grouped %>% subset(passes > 5)
# 
# 
# mg <- player_game_data %>% group_by(passer) %>% 
#   summarise(passes = n(), mv = max(v)*2.05) %>% 
#   mutate_if(is.numeric, round, digits = 2)
# mg <- mg %>% subset(passes > 5)
# print(mg)
# p <- ggplot(data = mg, aes(x= reorder(passer, -mv), y=mv)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) + 
#   xlab("Passer") + ylab("Max Velocity (mph)")
#  
# print(grouped)
# 
# p <- ggplot(grouped, aes(x = mlv, y = mla)) + geom_point(aes(size = passes))  + geom_label_repel(aes(label=passer)) + xlab("Mean Launch Velocity (mph)") + ylab("Mean Launch Angle (degrees)")
#  




by_passer<- player_game_data %>% group_by(passer) %>% filter(launch_angle <= 45)
temp <- by_passer %>% subset(passer == "R.Wilson")
print(temp)
p <- ggplot(temp, aes(x=v)) + geom_histogram(aes(y = ..density..), binwidth=1) + geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(v, na.rm=T)), color="red", linetype="dashed", size=1) + ggtitle("Distribution of Launch Angle")


ggsave(filename = "plot.pdf", plot = p)
