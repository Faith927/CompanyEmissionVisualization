library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)

df <- read_excel("emissions.xlsx", sheet = "Emissions")

# df <- df[, -which(names(df) == "Owned")]

df1 <- df %>%
  pivot_longer(cols = -c(Company, Status),
               names_to = "Year",
               values_to = "Emissions")

df_total <- df1[df1$Company %in% c("Remaining", "Total", "Global"), ]

# df_investor <- df1[df1$Company %in% "Investor", ]
# 
# df_state <- df1[df1$Company %in% "State", ]

gp <- ggplot(df_total, aes(x = Year, y = Emissions, col = Company, group = Company)) +
  geom_line() +
  theme_minimal() 
# +
#   theme(panel.grid = element_blank())

# Rotate X-axis labels vertically
gp <- gp + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Convert ggplot object to plotly object
gp <- ggplotly(gp)

# Display the plotly object
gp



















ggplot(df_total, aes(x=year, y=value, col = Company, group = Company)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2") +
  theme_ipsum() +
  ggtitle("Evolution of something")

# df_wide <- df_total %>%
#   pivot_wider(names_from = Company, values_from = value)

p1 <- ggplot(df_total, aes(x=year,y=value)) +
  geom_bar(aes(fill=Company), position="stack", stat="identity")+
  geom_line(data = df_investor, aes(x = year, y = value, color="Investor companies in top 100", group = 0)) +
  scale_y_continuous(breaks = seq(0, 40, 5))
ggplotly(p1)

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
# p3
# 
