library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(ggrepel)
library(plotly)


df <- read_excel("emissions.xlsx", sheet = "g")
View(df)
# df <- df[, -which(names(df) == "Owned")]

df1 <- df %>%
  pivot_longer(cols = -c(Company, Owned),
               names_to = "Year",
               values_to = "Value")

df1$year <- as.numeric(df1$Year)


g1 <- ggplot(df1, aes(x = Year, y = Value, color = Company)) +
  geom_line() +
  labs(title = "Multiple Line Chart", x = "Year", y = "Value") +
  theme_minimal()

ggplotly(g1, tooltip = c("x", "y", "color", "Owned"))



g <- ggplot(df1, aes(x = Year, y = Value, group = Company, color = Company, text = paste("Owned:", Owned))) +
  geom_line(show.legend = FALSE) +
  labs(title = "Examination of top 100 companies gtco2e emissions", x = "Year", y = "Gtco2e Emissions") +
  theme_minimal() +
  scale_color_viridis_d()

ggplotly(g, tooltip = c("x", "y", "color", "text"))



# Subset the data for the top 10 emitting companies
top10 <- df1 %>%
  group_by(Company) %>%
  summarise(total_emission = sum(value)) %>%
  arrange(desc(total_emission)) %>%
  top_n(10) %>%
  pull(Company)

# Filter the emissions data for the top 10 emitting companies
filtered_emissions <- df1 %>%
  filter(Company %in% top10)

# Create the line graph
g2 <- ggplot(filtered_emissions, aes(x = year, y = value, color = Company)) +
  geom_line() +
  labs(title = "Emission Values for Top 10 Emitting Companies",
       x = "Year",
       y = "Emission Value") +
  theme_minimal()

# Convert the plot to interactive using plotly
ggplotly(g2)

# ten least 

# Subset the data for the top 10 emitting companies
bottom10 <- df1 %>%
  group_by(Company) %>%
  summarise(total_emission = sum(value)) %>%
  arrange(desc(total_emission)) %>%
  top_n(-10) %>%
  pull(Company)

# Filter the emissions data for the top 10 emitting companies
filtered <- df1 %>%
  filter(Company %in% bottom10)

# Create the line graph
g3 <- ggplot(filtered, aes(x = year, y = value, color = Company)) +
  geom_line() +
  labs(title = "Emission Values for Bottom 10 Emitting Companies",
       x = "Year",
       y = "Emission Value") +
  theme_minimal()

# Convert the plot to interactive using plotly
ggplotly(g3)



