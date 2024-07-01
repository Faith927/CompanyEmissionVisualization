df <- read_excel("emissions.xlsx", sheet = "data")

df1 <- df %>%
  pivot_longer(cols = -c(Company, Status),
               names_to = "Year",
               values_to = "Emissions")

df1$year <- as.numeric(df1$Year)

# Subset the data for the top 10 emitting companies
top10 <- df1 %>%
  group_by(Company) %>%
  summarise(total_emission = sum(Emissions)) %>%
  arrange(desc(total_emission)) %>%
  top_n(10) %>%
  pull(Company)

# Filter the emissions data for the top 10 emitting companies
filtered_emissions <- df1 %>%
  filter(Company %in% top10)

# Create the line graph
g2 <- ggplot(filtered_emissions, aes(x = Year, y = Emissions, color = Company)) +
  geom_point() +
  labs(title = "Emission Values for Top 10 Emitting Companies",
       x = "Year",
       y = "Emission Value") +
  theme_minimal()

g2
# Convert the plot to interactive using plotly
ggplotly(g2)

# ten least 

# Subset the data for the top 10 emitting companies
bottom10 <- df1 %>%
  group_by(Company) %>%
  summarise(total_emission = sum(Emissions)) %>%
  arrange(desc(total_emission)) %>%
  top_n(-10) %>%
  pull(Company)

# Filter the emissions data for the top 10 emitting companies
filtered <- df1 %>%
  filter(Company %in% bottom10)

# Create the line graph
g3 <- ggplot(filtered, aes(x = Year, y = Emissions, color = Company)) +
  geom_line() +
  labs(title = "Emission Values for Bottom 10 Emitting Companies",
       x = "Year",
       y = "Emission Value") +
  theme_minimal()

# Convert the plot to interactive using plotly
ggplotly(g3)
g3



