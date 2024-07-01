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

# df1 <- df[!(row.names(df) %in% c("T", "S", "I", "Investor", "Remaining", "Global", "State")), ]

subset1 <- df[!(as.numeric(row.names(df)) %in% c(101, 102, 103, 104, 105, 106, 107, 108)), ]

subset1 <- subset1 %>%
  pivot_longer(cols = -c(Company, Status),
               names_to = "Year",
               values_to = "Emissions")
years <- c(1988, 2015)

subset2 <- subset1[subset1$Year %in% years, ]

subset3 <- subset2 %>%
  group_by(Company) %>%
  mutate(change = ifelse(Emissions[Year == 2015] > Emissions[Year == 1988], "Increased", "Decreased"))

subset4 <- subset3[subset2$Year != 1988, ]

# Define custom colors
custom_colors <- c("#FF0000", "#00FF00")  # Example colors: red and green

# Create the bar chart with custom colors
v <- ggplot(subset4, aes(x = change, fill = Status)) +
  geom_bar(position = "dodge") +
  labs(x = "Change in Emissions", y = "Number of Companies") +
  ggtitle("Count of Companies by Change in Emissions, Grouped by Status") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5)
  ) +
  scale_y_continuous(expand = c(0, 0))

v

library(ggplot2)

# Modify the subset4 dataframe
subset4 <- subset4 %>%
  mutate(Status = ifelse(Status == "Increase", paste(Status, "Investor"), paste(Status, "State")))

# Define custom colors
custom_colors <- c("#FF0000", "#00FF00", "#FF0000", "#00FF00")  # Red for decrease, green for increase

# Create the bar chart with custom colors and dodge position
v <- ggplot(subset4, aes(x = change, fill = Status)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Change in Emissions", y = "Number of Companies") +
  ggtitle("Count of Companies by Change in Emissions, Grouped by Status") +
  scale_fill_manual(values = custom_colors, labels = c("State Decrease", "State Increase", "Investor Decrease", "Investor Increase")) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5)
  ) +
  scale_y_continuous(expand = c(0, 0))

v


# Subset the data for the top 10 emitting companies
value <- subset1 %>%
  group_by(Company) %>%
  summarise(total_emission = sum(Emissions)) %>%
  arrange(desc(total_emission))

result <- merge(value, subset3, by = "Company")

result1 <- arrange(result, desc(total_emission))

# Filter the emissions data for the top 10 emitting companies
top10 <- result1[1:20, ]
bottom10 <- result1[(nrow(result)-19):nrow(result1), ]



colors <- c("#3366CC", "#990000")

# Create the plot with different colors for increased and decreased value_change
p1 <- ggplot(data = top10,
             aes(x = Year, y = Emissions, group = Company, color = change)) +
  geom_point() + geom_line() +
  scale_color_manual(values = colors)

ggplotly(p1)

p2 <- ggplot(data = bottom10,
             aes(x = Year, y = Emissions, group = Company, color = change)) +
  geom_point() + geom_line() +
  scale_color_manual(values = colors)

ggplotly(p2)








# # Create the line graph
# g2 <- ggplot(filtered_emissions, aes(x = Year, y = Emissions, color = Company)) +
#   geom_point() +
#   labs(title = "Emission Values for Top 10 Emitting Companies",
#        x = "Year",
#        y = "Emission Value") +
#   theme_minimal()
# 
# g2
# # Convert the plot to interactive using plotly
# ggplotly(g2)
# 
# # ten least 
# 
# # Subset the data for the top 10 emitting companies
# bottom10 <- subset %>%
#   group_by(Company) %>%
#   summarise(total_emission = sum(Emissions)) %>%
#   arrange(desc(total_emission)) %>%
#   top_n(-10) %>%
#   pull(Company)
# 
# # Filter the emissions data for the top 10 emitting companies
# filtered <- df1 %>%
#   filter(Company %in% bottom10)
# 
# # Create the line graph
# g3 <- ggplot(filtered, aes(x = Year, y = Emissions, color = Company)) +
#   geom_line() +
#   labs(title = "Emission Values for Bottom 10 Emitting Companies",
#        x = "Year",
#        y = "Emission Value") +
#   theme_minimal()
# 
# # Convert the plot to interactive using plotly
# ggplotly(g3)
# g3
# 
# 
# 
