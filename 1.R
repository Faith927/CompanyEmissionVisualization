

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
# library(hrbrthemes)
# library(babynames)
library(viridis)
library(ggrepel)
library(plotly)


# df <- read_excel("carbon.xlsx", sheet = "data")
df <- read_excel("emissions.xlsx", sheet = "Emissions")

df1 <- df[!(row.names(df) %in% c("T", "S", "I", "Investor", "Remaining", "Global", "State")), ]

df1 <- df1 %>%
  pivot_longer(cols = -c(Company, Status),
               names_to = "Year",
               values_to = "Emissions")



# df_filtered <- df1 %>% filter(Company %in% c("ExxonMobil Corp", "Chevron Corp", "Royal Dutch Shell PLC", "BP PLC", "Total SA", "ConocoPhillips", "BHP Billiton Ltd", "Lukoil OAO", "Rio Tinto", "Peabody Energy Corp"))
# 
# # view the filtered data
# df_filtered

years <- c(1998, 2015)
df2 <- df1[df1$Year %in% years, ]

df3 <- df2 %>%
  group_by(Company) %>%
  mutate(value_change = ifelse(Emissions[Year == 2015] > Emissions[Year == 1998], "Increased", "Decreased"))


as.numeric(df2$year)

# p1 <- ggplot(data = df2,
#              aes(x = year, y = value, group = Company)) +
#   geom_point() + geom_line() 
# p1


# Define colors for increased and decreased value_change
colors <- c("#3366CC", "#990000")

# Create the plot with different colors for increased and decreased value_change
p1 <- ggplot(data = df3,
             aes(x = year, y = value, group = Company, color = value_change)) +
  geom_point() + geom_line() +
  scale_color_manual(values = colors)

p1

p2 <- p1 + scale_size_continuous(range = c(1,3))
p2

# p3 <- p2 +
#   geom_text_repel(data = filter(df3, year == 1998),
#                   aes(label = paste0(Company, " - ", round(value, 2), " MtCO2e")),
#                   direction = "y", hjust = 1, colour = "dimgray",
#                   nudge_x = -1.75, size = 3) + 
# 
#   geom_text_repel(data = filter(df3, year == 2015),
#                   aes(label = paste0(round(value, 2), " MtCO2e", " - ", Company)),
#                   direction = "y", hjust = 0, colour = "dimgray",
#                   nudge_x = 1.75, size = 3)
# p3

p3 <- p2 +
  geom_text_repel(data = filter(df3, year == 1998),
                  aes(label = paste0(Company, " - ", round(value, 2), " MtCO2e")),
                  direction = "y", hjust = 1, colour = "dimgray",
                  nudge_x = -1.75, size = 3) + 
  geom_text_repel(data = filter(df3, year == 2015),
                  aes(label = paste0(round(value, 2), " MtCO2e", " - ", Company)),
                  direction = "y", hjust = 0, colour = "dimgray",
                  nudge_x = 1.75, size = 3)



p4 <- p3 + theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_x_discrete(expand = expansion(add = 1)) +
  labs(title = "MtCO2e Emissions", subtitle = "Top 10 investor companies with the worst MtCO2e emmissions in the world")
ggplotly(p4)
p4

#########################################33


# df <- df[, -which(names(df) == "Owned")]
# 
# df1 <- df %>% pivot_longer(names_to = "year", values_to = "value", cols = 2:29)
# 
# df_total <- df1[df1$Company %in% c("Remaining", "Total"), ]
# 
# df_investor <- df1[df1$Company %in% "Investor", ]
# 
# p1 <- ggplot(df_total, aes(x=year,y=value)) +
#   geom_bar(aes(fill=Company), position="stack", stat="identity")+
#   geom_line(data = df_investor, aes(x = year, y = value, color="Investor companies in top 100", group = 0)) +
#   scale_y_continuous(breaks = seq(0, 40, 5))
# 
# p2 <- p1 + 
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     axis.title.x = element_blank(),
#     axis.line.x = element_line(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     legend.spacing = unit(-0.2, "cm")
#   ) +
#   scale_x_discrete(expand = expansion(add = 1)) +
#   scale_fill_manual(values = c("#DCDCDC", "#990000"), 
#                     labels = c("Remaining Fossil Fuel Industry", "Top 100 GtCO2e emitting companies")) +
#   scale_color_manual(values = c("#E69F00"), 
#                      labels = c("Investor companies in top 100")) +
#   labs(
#     title = "Figure 4:", 
#     subtitle = "Operational and product GHG emissions of fossil fuel producing entities, 1988-2015",
#     y = "GtCO2e"
#   ) +
#   guides(
#     fill = guide_legend(title = NULL),
#     color = guide_legend(title = "")
#   )
# 
# p3  <- p2 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(breaks = seq(0, 40, 5), expand = c(0, 0), limits = c(0, 40)) 
# 
# p3

