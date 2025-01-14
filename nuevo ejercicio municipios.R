race <- election_data_tidy |> 
  filter(municipio %in% c("Alpedrete", "Moaña", "Cornellà de Llobregat", "Manacor", "Madrid", "Bilbao", "Vitoria-Gasteiz", "Gijón"))

# Step 1: Determine the winning party for each municipality and year
winners <- race %>%
  group_by(municipio, date) %>%
  arrange(desc(votes)) %>% 
  slice(1) %>% # Take the party with the most votes
  ungroup()

# Step 2: Identify if the winning party changed from the previous year
stability <- winners %>%
  arrange(municipio, date) %>%
  group_by(municipio) %>%
  mutate(
    previous_party = lag(party_recoded),                  # The winning party in the previous election
    party_change = ifelse(party_recoded == previous_party, 0, 1) # 1 if the party changed, 0 otherwise
  ) %>%
  ungroup()

# Step 3: Aggregate stability/chaos for each municipality
chaos_summary <- stability %>%
  group_by(municipio) %>%
  summarise(
    total_changes = sum(party_change, na.rm = TRUE), # Total number of changes
    total_elections = n() - 1,                      # Number of comparisons (elections - 1)
    chaos_rate = total_changes / total_elections    # Proportion of elections with a change
  )

# Step 4: Visualise the municipalities by chaos and stability
ggplot(chaos_summary, aes(x = municipio, y = chaos_rate, fill = municipio)) +
  geom_col() +
  labs(
    title = "Chaos Rate of Municipalities",
    x = "Municipality",
    y = "Chaos Rate (Proportion of Elections with Change)"
  ) +
  theme_minimal()


library(viridis)

chaos_withoutlabels <- ggplot(chaos_summary, aes(x = reorder(municipio, chaos_rate), y = chaos_rate, fill = chaos_rate)) +
  geom_col(width = 0.7, color = "black", alpha = 0.9) +
  scale_fill_viridis(option = "plasma", direction = -1, name = "Chaos Rate") +
  geom_text(aes(label = scales::percent(chaos_rate, accuracy = 1)), 
            hjust = -0.1, size = 4, fontface = "bold") +
  labs(
    title = "Chaos Rate de Municipios en Elecciones",
    subtitle = "Proporción de elecciones con cambio de partido ganador",
    x = "Municipio",
    y = "Chaos Rate (%)"
  ) +
  coord_flip() +  # Convierte el gráfico a horizontal
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )


chaos_withlabels <- ggplot(chaos_summary, aes(x = reorder(municipio, chaos_rate), y = chaos_rate, fill = chaos_rate)) +
  geom_col(width = 0.7, color = "black", alpha = 0.9) +
  scale_fill_viridis(option = "plasma", direction = -1, name = "Chaos Rate") +
  geom_text(aes(label = municipio), 
            hjust = 1.1, color = "white", size = 5, fontface = "bold") +
  geom_text(aes(label = scales::percent(chaos_rate, accuracy = 1)), 
            hjust = -0.1, size = 4, fontface = "bold") +
  labs(
    title = "Chaos Rate of Municipalities in Elections",
    subtitle = "Proportion of elections with a change in the winning party",
    x = "Municipality",
    y = "Chaos Rate (%)"
  ) +
  coord_flip() +  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),  
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )

ggsave("chaos_rate_municipalities.png", plot = chaos_withoutlabels, width = 10, height = 6, dpi = 300)
ggsave("chaos_rate_municipalities2.png", plot = chaos_withlabels, width = 10, height = 6, dpi = 300)
