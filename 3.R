library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(readxl)
library(dplyr)
library(tidyverse)
library(viridis)
library(ggrepel)
library(shinyWidgets)
library(MASS)


# Define your data frame df1 here
df <- read_excel("emissions.xlsx", sheet = "Emissions")

df1 <- df %>%
  pivot_longer(cols = -c(Company, Status, Rank),
               names_to = "Year",
               values_to = "Emissions")

df_total <- df1[df1$Company %in% c("Total", "Global"), ]

df_total1 <- df1[df1$Company %in% c("State", "Investor"), ]


df2 <- df1 %>%
  filter(!Company %in% c("Global", "Remaining", "Total", "State", "Investor", "T", "S", "I"))


df2$Year <- as.numeric(df2$Year)

filtered_df <- df2 %>% filter(Year == 2015)

filtered_df$Rank_Group <- cut(filtered_df$Rank, breaks = c(0, 25, 50, 75, 100), labels = c("1-25", "26-50", "51-75", "76-100"), include.lowest = TRUE)


# df2$Rank_Group <- cut(df2$Rank, breaks = c(0, 25, 50, 75, 100), labels = c("1-25", "26-50", "51-75", "76-100"), include.lowest = TRUE)

# filtered_df <- df2 %>% filter(Status %in% c("Investor", "State"))

grouped_df <- filtered_df %>%
  group_by(Rank_Group, Status) %>%
  summarise(Total_Emissions = sum(Emissions))


company_counts <- df2 %>%
  filter(Year == 2015) %>%
  group_by(Rank_Group, Status) %>%
  summarise(Company_Count = n())

grouped_df <- left_join(grouped_df, company_counts, by = c("Rank_Group", "Status"))

b <- ggplot(grouped_df, aes(x = Rank_Group, y = round(Total_Emissions/1000, 2), fill = Status,
                            text = paste('Emissions: ', round(Total_Emissions/1000, 2), '\nNumber of Companies: ', Company_Count))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Rank Group", y = "Total GtCO2e Emissions", fill = "Status") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#FF7F0E", "#1F77B4"))  # Add custom colors

ggplotly(b, tooltip = c("text"))


########################################################

count <- merge(filtered_df, df3, by = c("Company", "Status", "Rank", "Year", "Emissions"))

count1 <- count %>%
  filter(Status %in% c("Investor", "State") & value_change %in% c("Increased", "Decreased")) %>%
  group_by(Rank_Group, Status, value_change) %>%
  summarise(Count = n())


count2 <- count1 %>%
  filter(Status == "Investor")

count3 <- count1 %>%
  filter(Status == "State")

ggplot(count2, aes(x = Rank_Group, y = Count, fill = value_change)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Rank Group", y = "Count", fill = "Emissions Change") +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E"), labels = c("Decreased", "Increased")) +
  theme_minimal() +
  guides(fill = guide_legend(title = "Emissions Change", override.aes = list(color = NULL)))

i <- ggplot(count2, aes(x = Rank_Group, y = Count, fill = value_change)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Rank Group", y = "Total GtCO2e Emissions", fill = "") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E")) + # Add custom colors
  labs(title = "Investor Emissions", subtitle = "Decrease vs. Increase")

ggplotly(i)






c <- ggplot(count3, aes(x = Rank_Group, y = Count, fill = value_change)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E")) + # Add custom colors
  labs(title = "State Emissions", subtitle = "Increase vs. Decrease")


ggplotly(c)



