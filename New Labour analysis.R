library(tidyverse)
library(gridExtra)

read_csv("/Users/matteo/Downloads/New Labour/GDPPC.csv") %>%
  filter(`Country Code` %in% c('GBR','OED')) %>%
  select(!(c(`Country Name`,`Indicator Name`,`Indicator Code`))) %>%
  pivot_longer(-`Country Code`, names_to = "Year", values_to = "value") -> data

ggplot(data[data$`Country Code` == 'GBR',], aes(x = as.numeric(Year), y = value)) +
  geom_line(colour = 'blue') +
  geom_smooth(method = 'lm', colour = "black") +
  scale_x_continuous(limits = c(1961,2019)) +
  xlab('Year') + ylab("Annual GDP per capita growth (%)") +
  guides(colour = guide_legend(title = NULL), linetype = guide_legend(title = NULL)) +
  theme_base()

data[data$`Country Code` == "GBR",] %>%
  mutate(Decade = substr(Year,1,3)) %>%
  group_by(Decade) %>%
  summarise(average = mean(value, na.rm = TRUE), groups = "keep") %>%
  mutate(Decade = paste0(Decade,'0s')) %>%
  filter(Decade != "2020s") %>%
  ggplot(aes(x = Decade, y = average)) +
  geom_col(colour = 'black', fill = 'grey80') +
  xlab(NULL) + ylab("Average annual GDP per capita growth") +
  labs(title = "Economic growth in the UK", 
       caption = "Source: World Bank national accounts data, and OECD National Accounts data files. Indicator NY.GDP.PCAP.KD.ZG") +
  theme_base() + theme(legend.position = "none")

data %>%
  group_by(Year) %>%
  mutate(Difference = value[`Country Code` == 'GBR'] - value[`Country Code` == 'OED']) %>%
  filter(`Country Code` == 'GBR') %>%
  select(Year, Difference) -> Difference

ggplot(Difference, aes(x = as.numeric(Year), y = Difference, fill = Difference)) +
  geom_col() +
  scale_fill_continuous(type = 'viridis') +
  scale_x_continuous(limits = c(1960,2019)) +
  xlab('Year') + ylab("Difference in annual GDP per capita growth\nbetween the UK and the OECD") +
  theme_base() + theme(legend.position = "none")

Difference %>%
  filter(as.numeric(Year) <= 2010 & as.numeric(Year) >= 1997) %>%
  ggplot(aes(x = as.numeric(Year), y = Difference)) +
  geom_col(colour = 'black', fill = "grey80") +
  scale_y_continuous(labels = c('-1', '0','1','2')) +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks = c(1997, 2001, 2005, 2009)) +
  xlab(NULL) + ylab("Difference in annual GDP per capita\ngrowth between UK and OECD (%)") +
  #labs(title = "Comparing the economic performance of New Labour and other OECD countries", 
  #     caption = "Source: World Bank national accounts data, and OECD National Accounts data files. Indicator NY.GDP.PCAP.KD.ZG") +
  theme_base(base_family = 'Times') + theme(legend.position = "none") -> differences


medianincome <- tibble(Year = c(seq.int(1997,2010,1)),
                       `Real income` = c(21729,22198,23633,24032,25553,26388,26853,27569,27872,27850,28269,27940,27977,27938))
ggplot(medianincome, aes(x = as.numeric(Year), y = `Real income`)) +
  geom_line(colour = 'black') +
  scale_x_continuous(breaks = c(1997, 2001, 2005, 2009)) +
  xlab(NULL) + ylab("Real median income after\ntaxes and benefits (£s)") +
  #labs(title = "The growth of median income under New Labour", 
  #     caption = "Source: ONS Living Costs and Food survey.") +
  theme_base(base_family = 'Times') + theme(legend.position = "none") -> medians
ggsave("/Users/matteo/Downloads/New Labour/performance.pdf", plot = medians, device='pdf', family = 'Times',
       dpi = 300, bg="transparent", 
       width=7, height=3.5)


poverty <- read.csv("/Users/matteo/Downloads/New Labour/Relative poverty.csv")
ggplot(poverty, aes(x = Year, y = Value, shape = Variable)) +
  geom_line(colour = 'black') +
  geom_point() +
  geom_vline(xintercept = c(1997), colour = 'black', size = 0.05) +
  xlab(NULL) + ylab("Percentage living in relative poverty") +
  #labs(caption = "Source: DWP Households Below Average Income.") +
  theme_base(base_family = 'Times') + theme(legend.position = "bottom", legend.title = element_blank()) -> poverties
ggsave("/Users/matteo/Downloads/New Labour/poverty.pdf", plot = poverties, device='pdf', family = 'Times',
       dpi = 300, bg="transparent", 
       width=7, height=3.5)



inequality <- read.csv("/Users/matteo/Downloads/New Labour/Inequality.csv")
ggplot(inequality[inequality$year<=2010,], aes(x = as.numeric(year), y = Gini)) +
  geom_line(colour = 'black') +
  geom_vline(xintercept = c(1997), colour = 'black', size = 0.05) +
  xlab(NULL) + ylab("Gini coefficient for income\nafter taxes and benefits") +
  theme_base(base_family = 'Times') + theme(legend.position = "none") -> p1
ggplot(inequality[inequality$year<=2010,], aes(x = as.numeric(year), y = Income)) +
  geom_line(colour = 'black') +
  geom_vline(xintercept = c(1997), colour = 'black', size = 0.05) +
  scale_y_continuous(limits = c(4,16), breaks = c(6,10,14)) +
  xlab(NULL) + ylab("The top 1%'s share\nof national income") +
  theme_base(base_family = 'Times') + theme(legend.position = "none") -> p2
grid.arrange(p1,p2, ncol = 1) -> inequalities
ggsave("/Users/matteo/Downloads/New Labour/inequality.pdf", plot = inequalities, device='pdf', family = 'Times',
       dpi = 300, bg="transparent", 
       width=7, height=5.5)

  
