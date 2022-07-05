library("readxl")
library("ggplot2")
library("dplyr")
library("ggpubr")

# import data
G = (read_excel("/Users/odedsabah/Desktop/Joni - master/28.7.2020_1.xlsx"))

# Shows all treatments including the seconds
vec_group = table(G$Group_name)

# change output_time to series of consecutive numbers 1:1800
len_group = length(table(G$Group_name))
G$outpot_time = rep(c(1:1801), time = len_group )
ir <- G %>%
group_by(Group_name)
group_split(ir)
group_keys(ir)

# box_plot

# create list with only valuse second half of treatment
G_new = filter(G, outpot_time >=900 & outpot_time<=1800)
by_spec = group_by(G_new, Group_name)
by_spec

# create box_plot for only In the second half of treatment

box_plot = ggplot(by_spec, aes(Group_name ,mm,group = Group_name)) + 
    geom_boxplot(fill = 'grey') + 
    labs(title = "", x = "Group", y = "mm") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,2) + geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(by_spec$mm, c(0.25, 0.75))) 

box_plot

i=1
Vec_group = c("1 uM", "2 uM", "3 uM", "ET 0.1%", "VPA", "WATER", "WATER TO PTZ", "water-> ptz")
for (T in Vec_group) { x = by_spec[by_spec$Group_name == T, ]
sd_x = sd(x$mm)
mean_x = mean(x$mm)
by_spec = by_spec[!(by_spec$Group_name== T & by_spec$mm > (mean_x + 2*sd_x )),]
by_spec = by_spec[!(by_spec$Group_name== T & by_spec$mm < (mean_x - 2*sd_x )),]
i = i+1
}

#k.t = kruskal.test(mm ~ Group_name, data = G_new)
#k.t
k.t  = kruskal.test(mm ~ Group_name, data = by_spec)
pairwise.wilcox.test(by_spec$mm, by_spec$Group_name,
                     p.adjust.method = "BH")

# linear_plot

#filter nunber 
G_new_linear = filter(G, outpot_time >=1 & outpot_time<=1800)
by_spec_2 = group_by(G_new_linear, Group_name)
by_spec_2

# group naming: if the treatment names are different, change the names in the vector - Vec_group
Vec_group = c("1 uM", "2 uM", "3 uM", "ET 0.1%", "VPA", "WATER", "WATER TO PTZ", "water-> ptz")
Vec_group_each_18 = rep(Vec_group, each=18)
Group_name_1 = Vec_group_each_18

# Replacing a range in a single number
outpot_time_100_sec = rep(seq(100,1800, by = 100),time = 8)

# mean of every 100 rows
#mean_mm = tapply(G$mm, rep(seq_along(G$mm), each = 100, length.out = length(G$mm)), mean)
#mean_mm 

mean_mm = tapply(by_spec_2$mm, rep(seq_along(by_spec_2$mm), each = 100, length.out = length(by_spec_2$mm)), mean)
mean_mm 

# creating the new dataframe (mean of every 100 rows)
GG = data.frame(Group_name_1, outpot_time_100_sec, mean_mm)


# create linear_plot for all treatment
ggplot(data = GG, aes(x = GG$outpot_time_100_sec, y = GG$mean_mm, color = GG$Group_name_1)) +
  geom_line()+ylim(0,2.1)+xlim(0,1800) +
  xlab("Time (sec)") +
  ylab("mm") 

