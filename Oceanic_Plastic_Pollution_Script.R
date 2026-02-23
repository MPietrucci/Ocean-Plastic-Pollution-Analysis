install.packages("tidyverse")
library(tidyverse)
oceanicpp_df <- read_csv("data/Oceanic_Plastic_Pollution_Db.csv")

# Data Cleaning

oceanicpp_df <- oceanicpp_df %>% 
  rename(Plastic_Type = Platic_Type)

# Insight 1: observation counts, total plastic weight, and average values

# Observation counts by ocean region

observation <- oceanicpp_df %>% 
  count(Region)

ggplot(data = observation, mapping = aes(x = Region,y = n, fill = Region)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Plastic Pollution Observations by Region",
       x = "Ocean Region",
       y = "Observation Counts"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
    axis.title = element_text(face = "bold"), 
    axis.text = element_text(face = "bold"), 
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA))

ggsave("plastic_observations_by_region.png", 
       width = 10, 
       height = 6, 
       dpi = 300)

# Total plastic weight by ocean region

total_kg <- oceanicpp_df %>%
  group_by(Region) %>% 
  summarise(Total_kg = sum(Plastic_Weight_kg, na.rm = TRUE))

# Total plastic weight by ocean region

ggplot(data = total_kg, mapping = aes(x = Region,y = Total_kg/1000, fill = Region)) +
  geom_col() +
  geom_text(aes(label = round(Total_kg/1000, 1)), 
            vjust = -0.5, fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Plastic Distribution by Ocean Region", 
    x = "Ocean Region", 
    y = "Total Plastic Weight (ton)"
  ) +
  theme_minimal(base_size = 14) + 
  theme( plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
         axis.title = element_text(face = "bold"), 
         axis.text = element_text(face = "bold"), 
         legend.position = "none", 
         panel.background = element_rect(fill = "white", color = NA), 
         plot.background = element_rect(fill = "white", color = NA) )

ggsave("plastic_distribution_by_region.png", 
       width = 10, height = 6, dpi = 300)

# Average plastic weight per observation

media_df <- total_kg %>% 
  inner_join(observation, by = "Region") %>% 
  mutate(media = Total_kg / n)

# Average plastic weight per observation by ocean region

ggplot(data = media_df, mapping = aes(x = Region, y = media,fill = Region)) +
  geom_col() +
  geom_text(aes(label = round(media, 1)), 
            vjust = -0.5, fontface = "bold") + 
  scale_fill_brewer(palette = "Set2") +
  labs( title = "Average Plastic Weight per Observation by Ocean Region", 
        x = "Ocean Region", 
        y = "Average Plastic Weight (kg)" ) + 
  theme_minimal(base_size = 14) + 
  theme( plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
         axis.title = element_text(face = "bold"), 
         axis.text = element_text(face = "bold"), 
         legend.position = "none", 
         panel.background = element_rect(fill = "white", color = NA), 
         plot.background = element_rect(fill = "white", color = NA))

ggsave("average_plastic_weight_by_region.png",
       width = 10, height = 6, dpi = 300)

# Insight 2: distribution of plastic types across ocean regions and overall totals

# Plastic quantities by region and plastic type (kg, ton, pct)

plastic_type_total_kg_ton_pct <- oceanicpp_df %>%
  group_by(Region, Plastic_Type) %>% 
  summarise(Type_Total_kg = sum(Plastic_Weight_kg, na.rm = TRUE), .groups = "drop") %>% 
  mutate(Type_Total_ton = Type_Total_kg / 1000) %>% 
  group_by(Region) %>% 
  mutate(pct = Type_Total_ton / sum(Type_Total_ton)) %>% 
  ungroup()

# Stacked bar chart of plastic type distribution by region

ggplot(plastic_type_total_kg_ton_pct, 
       aes(x = Region, y = Type_Total_ton, fill = Plastic_Type)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(pct * 100, 1), "%")), 
    position = position_stack(vjust = 0.5), 
    size = 3, 
    fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Set2") + 
  scale_y_continuous(limits = c(0, 800)) +
  labs(title = "Plastic Type Distribution by Ocean Region", 
       x = "Ocean Region",
       y = "Total Plastic Weight (ton)", 
       fill = "Plastic Type") +
  theme_minimal(base_size = 14) + 
  theme( plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
         axis.title = element_text(face = "bold"), 
         axis.text = element_text(face = "bold"), 
         legend.position = "right", 
         panel.background = element_rect(fill = "white", color = NA), 
         plot.background = element_rect(fill = "white", color = NA), 
         axis.text.x = element_text(angle = 35, hjust = 1))

ggsave("plastic_type_distribution_by_region.png",
       width = 10, height = 6, dpi = 300)

# Insight 3: relationship between depth, plastic weight, and plastic type across ocean regions

ggplot(data = oceanicpp_df, mapping = aes(x = Depth_meters, y = Plastic_Weight_kg, color = Plastic_Type)) +
  geom_point(alpha = 0.7, size = 0.5) +
  facet_wrap(~Region) +
  scale_color_brewer(palette = "Set2") + 
  labs( title = "Correlation of Depth, Plastic Weight, and Plastic Type", 
        x = "Depth (meters)", 
        y = "Plastic Weight (kg)", color = "Plastic Type" ) +
  guides(color = guide_legend(override.aes = list(size = 4))) + 
  theme_minimal(base_size = 14) + 
  theme( plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
         axis.title = element_text(face = "bold"), 
         axis.text = element_text(face = "bold"),
         strip.text = element_text(face = "bold", size = 12),
         legend.title = element_text(face = "bold"), 
         legend.text = element_text(face = "bold"), 
         panel.background = element_rect(fill = "white", color = NA), 
         plot.background = element_rect(fill = "white", color = NA))

ggsave("depth_plastic_correlation_by_region.png",
       width = 10, height = 6, dpi = 300)

# Save processed tibbles as RDS files

saveRDS(media_df, "data/media_df.rds")
saveRDS(observation, "data/observation.rds")
saveRDS(oceanicpp_df, "data/oceanicpp_df.rds")
saveRDS(plastic_type_total_kg_ton_pct, "data/plastic_type_total_kg_ton_pct.rds")
saveRDS(total_kg, "data/total_kg.rds")

# Preview of processed tibbles

head(media_df, 10)
head(observation, 10)
head(oceanicpp_df, 10)
head(plastic_type_total_kg_ton_pct, 10)
head(total_kg, 10)
