library(tidyverse)
library(viridis)

# reading in a single file for working with page start coordinates rather than quadrants
clusters_c <- read_csv("./data/part-00000-f986c8c6-0ffa-4eec-b2ae-778e956cd85f.csv") [ , c("cluster", "text", "date", "title", "placeOfPublication", "seq", "pageWidth", "pageHeight", "firstx", "firsty", "firstw", "firsth", "pageLeft", "pageUpper", "corpus")] %>%
  filter(corpus == "ca") %>%
  select(-corpus) %>%
  rename(pageNumber = seq) %>%
  mutate(title = gsub('(.*)\\.*\\(.*','\\1',title)) %>%
  mutate(decade = paste(substring(date, 1,3))) %>%
  mutate(decade = paste(decade, 0, sep="")) %>%
  filter(decade > 1820) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) # %>%
# mutate(pageX = firstx / pageWidth) %>%
# mutate(pageY = firsty / pageHeight)

ggplot(clusters_c %>% filter(pageNumber < 5)) +
  geom_point(mapping = aes(x = firstx, y = desc(firsty))) + facet_grid(. ~ pageNumber)

ggplot(clusters_c %>% filter(pageNumber == 1)) +
  geom_point(mapping = aes(x = firstx, y = desc(firsty))) + facet_grid(. ~ decade)


# reading in a set of files for working with page start coordinates rather than quadrants
files <-list.files (path = "./data", pattern = '*.csv', full.names = TRUE)
clusters_c <- do.call(rbind, lapply(files, read_csv)) %>%
  select(cluster, text, date, title, placeOfPublication, p1seq, p1width, p1height, p1x, p1y, p1w, p1h, corpus) %>%
  filter(corpus == "ca") %>%
  select(-corpus) %>%
  rename(pageNumber = p1seq, pageWidth = p1width, pageHeight = p1height) %>%
  filter(pageNumber < 5) %>%
  mutate(title = gsub('(.*)\\.*\\(.*','\\1',title)) %>%
  mutate(decade = paste(substring(date, 1,3))) %>%
  mutate(decade = paste(decade, 0, sep="")) %>%
  filter(decade > 1820) %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))

#code below creates a factor that will allow us to visualize the pages in print order rather than sequential order
clusters_c$pageNumber <- factor(clusters_c$pageNumber, levels = c("4","1","2","3"))

ggplot(clusters_c %>% filter(p1y >= 0))+
  geom_point(mapping = aes(x = p1x, y = desc(p1y))) + 
  facet_wrap(~ pageNumber, ncol=2)

# this one gets closest to what I'd imagined, using the starting positions of each reprint. Still needs normalization
ggplot(clusters_c %>% filter(p1y >= 0 & p1x >=0 ), aes(p1x, desc(p1y))) +
  geom_bin2d(bins = 40) +
  facet_wrap(~ pageNumber, ncol=2)

ggplot(clusters_c %>% filter(p1y >= 0 & p1x >= 0), aes((p1x/pageWidth), desc((p1y/pageHeight)))) +
  geom_bin2d(bins = 40) +
  scale_fill_viridis(option = "A", trans = "sqrt") + 
  theme_bw() +
  facet_wrap(~ pageNumber, ncol=2) + 
  labs(x="Horizontal Page Position",y="Vertical Page Position",fill="Density of Reprints")

ggplot(clusters_c %>% filter(p1y >= 0 & p1x >= 0 & p1y <= pageHeight), aes((p1x/pageWidth), desc((p1y/pageHeight)))) +
  geom_hex(bins = 30) +
  scale_fill_viridis(option = "A", trans = "sqrt") + 
  theme_bw() +
  facet_wrap(~ pageNumber, ncol=2) + 
  labs(x="Horizontal Page Position",y="Vertical Page Position",fill="Density of Reprints")

# how about the mean location of each reprint?
clusters_c <- clusters_c %>%
  filter(p1y >= 0 & p1x >= 0) %>%
  mutate(startX = p1x/pageWidth, startY = (p1y/pageHeight)*(pageHeight/pageWidth)) %>%
  #filter(startX >= 1 | startY >= 1) %>%
  mutate(endX = (p1x + p1w) / pageWidth, endY = (p1y + p1h) / pageHeight) %>%
  mutate(meanX = (startX + endX) / 2, meanY = (startY + endY) / 2)

#mean location
ggplot(clusters_c %>% filter(meanX <= 1 & meanY <=1), aes(meanX, desc(meanY))) +
  geom_hex(bins = 40) +
  scale_fill_viridis(option = "A", trans = "sqrt") + 
  theme_bw() +
  facet_wrap(~ pageNumber, ncol=2) + 
  ggtitle("Mean location of reprinted texts on unfolded sheets") +
  labs(x="Horizontal Page Position",y="Vertical Page Position",fill="Density of Reprints") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22, hjust=0.5)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16)) + 
  theme(legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16)) +
  theme(legend.background = element_rect(color = "#efefef")) +
  theme(plot.caption = element_text(family = "Trebuchet MS", color="#666666", size=10, hjust = 0.5, margin = margin(15, 0, 15, 0))) +
  theme(axis.text = element_text(family = "Trebuchet MS", color="#aaaaaa", face="bold", size=10)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_line(color = "#efefef")) +
  theme(axis.ticks = element_line(color = "#efefef"))

# Select on particular titles

cluster_titles <- unique(clusters_c$title)
titleToSearch <- cluster_titles[267]

ggplot(clusters_c %>% filter(meanX <= 1 & meanY <=1) %>% filter(title == titleToSearch), aes(meanX, desc(meanY))) +
  geom_hex(bins = 18) +
  scale_fill_viridis(option = "A", trans = "sqrt") + 
  theme_bw() +
  facet_wrap(~ pageNumber, ncol=2) + 
  ggtitle(paste("Mean location of reprinted texts in the", titleToSearch, collapse = " ")) +
  labs(x="Horizontal Page Position",y="Vertical Page Position",fill="Density of Reprints") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22, hjust=0.5)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16)) + 
  theme(legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16)) +
  theme(legend.background = element_rect(color = "#efefef")) +
  theme(plot.caption = element_text(family = "Trebuchet MS", color="#666666", size=10, hjust = 0.5, margin = margin(15, 0, 15, 0))) +
  theme(axis.text = element_text(family = "Trebuchet MS", color="#aaaaaa", face="bold", size=10)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_line(color = "#efefef")) +
  theme(axis.ticks = element_line(color = "#efefef"))


#ggplot(clusters_c) + 
#  geom_point(mapping = aes(x = firstx, y = firsty), position = "jitter") +
#  geom_smooth(mapping = aes(x = firstx, y = firsty)) +
#  facet_grid(. ~ pageNumber)
