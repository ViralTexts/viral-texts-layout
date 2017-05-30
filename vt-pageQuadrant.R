library(tidyverse)
library(viridis)

# importing and transforming into a column with quadrants
clusters <- read_csv("./data/part-00000-7b87df26-7dc9-4656-8e32-1f4258f80dde.csv") %>% #[ , c("cluster", "text", "date", "title", "placeOfPublication", "seq", "pageLeft", "pageUpper", "corpus")] %>%
  # filter(corpus == "ca") %>%
  select(-corpus) %>%
  rename(pageNumber = seq) %>%
  mutate(title = gsub('(.*)\\.*\\(.*','\\1',title)) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) 
  #mutate(pagePosition = ifelse(pageLeft == "true" & pageUpper == "true", "UpperLeft", NA)) %>%
  #mutate(pagePosition = ifelse(pageLeft == "true" & pageUpper == "false", "LowerLeft", pagePosition)) %>%
  #mutate(pagePosition = ifelse(pageLeft == "false" & pageUpper == "true", "UpperRight", pagePosition)) %>%
  #mutate(pagePosition = ifelse(pageLeft == "false" & pageUpper == "false", "LowerRight", pagePosition)) %>%
  #select(-pageLeft, -pageUpper) %>%
  #mutate(pagePlusPosition = paste(pageNumber, pagePosition, sep = "_")) # %>%

#importing a larger set of CSV files
files <-list.files(path = "./data/", pattern = '*.csv', full.names = TRUE)
clusters <- do.call(rbind, lapply(files, read_csv)) %>%
  select(cluster, text, date, title, placeOfPublication, seq, pageLeft, pageUpper, corpus) %>%
  # at the moment the most reliable coordinate data comes from Chronicling America, so I limit the results to CA papers. In future I would hope to calculate page position across other corpora as well.
  filter(corpus == "ca") %>%
  select(-corpus) %>%
  rename(pageNumber = seq) %>%
  mutate(title = gsub('(.*)\\.*\\(.*','\\1',title)) %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))  %>%
  mutate(pagePosition = ifelse(pageLeft == "true" & pageUpper == "true", "UpperLeft", NA)) %>%
  mutate(pagePosition = ifelse(pageLeft == "true" & pageUpper == "false", "LowerLeft", pagePosition)) %>%
  mutate(pagePosition = ifelse(pageLeft == "false" & pageUpper == "true", "UpperRight", pagePosition)) %>%
  mutate(pagePosition = ifelse(pageLeft == "false" & pageUpper == "false", "LowerRight", pagePosition))

#determine the total number of page, pagePosition combinations
pageDist <- clusters %>%
  select(6,9) %>%
  gather(pageNumber, pagePosition) %>%
  group_by(pageNumber, pagePosition) %>%
  summarize(total = sum(pagePosition %in% c("LowerLeft","UpperLeft","LowerRight","UpperRight"))) %>%
  na.omit(clusters) %>%
  arrange(desc(total), pageNumber)
  # spread(pagePosition, total)

#plot page, PagePosition combinations
ggplot(pageDist %>% filter(pageNumber < 5), aes(pagePosition, desc(pageNumber))) + geom_tile(aes(fill = total), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")

ggplot(pageDist %>% filter(pageNumber < 5), aes(pagePosition, desc(pageNumber))) + geom_tile(aes(fill = total)) + scale_fill_viridis()

# summarize all the pageLeft / pageUpper combinations 
pageDist_2 <- clusters %>%
  select(6:8) %>%
  group_by(pageNumber, pageLeft, pageUpper) %>%
  filter(pageNumber < 5) %>%
  summarise(total = n()) %>%
  na.omit(pageLeft) %>%
  na.omit(pageUpper)

# plots the pageLeft/pageUpper combinations by page, ordered as they would be on said pages
pageDist_2$pageNumber <- factor(pageDist_2$pageNumber, levels = c("4","1","2","3"))
ggplot(pageDist_2, aes(pageLeft, pageUpper)) + scale_x_discrete(limits=c("true","false")) + geom_tile(aes(fill = total), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue") + facet_grid(. ~ pageNumber)

# or... 
pageDist_2$pageNumber <- factor(pageDist_2$pageNumber, levels = c("4","1","2","3"))
reprintPlot_all <- ggplot(pageDist_2, aes(pageLeft, pageUpper)) + 
  scale_x_discrete(limits=c("true","false")) + 
  geom_tile(aes(fill = total), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  facet_wrap(~ pageNumber, ncol=2) + 
  labs(title = "Distribution of Reprints", subtitle = "Across Chronicling America Newspaper Pages", x="Left or Right Side",y="Upper or Lower Half",fill="Density of Reprints") + 
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
  theme(legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 

# filter to specific publications; compare?
pageDist_hickman <- clusters %>%
  select(4,6:8) %>%
  filter(str_detect(title, "Hickman courier")) %>%
  group_by(pageNumber, pageLeft, pageUpper) %>%
  filter(pageNumber < 5) %>%
  summarise(total = n()) %>%
  na.omit(pageLeft) %>%
  na.omit(pageUpper)

pageDist_hickman$pageNumber <- factor(pageDist_hickman$pageNumber, levels = c("4","1","2","3"))
reprintPlot_hickman <- ggplot(pageDist_hickman, aes(pageLeft, pageUpper)) + scale_x_discrete(limits=c("true","false")) + geom_tile(aes(fill = total), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue") + facet_wrap(~ pageNumber, ncol=2) + 
  labs(title = "Distribution of Reprints", subtitle = "From the Hickman Courier (Hickman, KY)", x="Left or Right Side",y="Upper or Lower Half",fill="Density of Reprints") + 
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
  theme(legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 

pageDist_star <- clusters %>%
  select(4,6:8) %>%
  filter(str_detect(title, "Evening star")) %>%
  group_by(pageNumber, pageLeft, pageUpper) %>%
  filter(pageNumber < 5) %>%
  summarise(total = n()) %>%
  na.omit(pageLeft) %>%
  na.omit(pageUpper)

pageDist_star$pageNumber <- factor(pageDist_star$pageNumber, levels = c("4","1","2","3"))
reprintPlot_star <- ggplot(pageDist_star, aes(pageLeft, pageUpper)) + scale_x_discrete(limits=c("true","false")) + geom_tile(aes(fill = total), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue") + facet_wrap(~ pageNumber, ncol=2) + 
  labs(title = "Distribution of Reprints", subtitle = "From the Evening Star (Washington, DC)", x="Left or Right Side",y="Upper or Lower Half",fill="Density of Reprints") + 
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
  theme(legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 

pageDist_wheeling <- clusters %>%
  select(4,6:8) %>%
  filter(str_detect(title, "Wheeling daily intelligencer")) %>%
  group_by(pageNumber, pageLeft, pageUpper) %>%
  filter(pageNumber < 5) %>%
  summarise(total = n()) %>%
  na.omit(pageLeft) %>%
  na.omit(pageUpper)

pageDist_wheeling$pageNumber <- factor(pageDist_wheeling$pageNumber, levels = c("4","1","2","3"))
reprintPlot_wheeling <- ggplot(pageDist_wheeling, aes(pageLeft, pageUpper)) + scale_x_discrete(limits=c("true","false")) + geom_tile(aes(fill = total), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue") + facet_wrap(~ pageNumber, ncol=2) + 
  labs(title = "Distribution of Reprints", subtitle = "From the Wheeling Daily Intelligencer (Wheeling, VA/WV)", x="Left or Right Side",y="Upper or Lower Half",fill="Density of Reprints") + 
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) + 
  theme(legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))