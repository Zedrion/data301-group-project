library(tidyverse)

# Load the main pharmaceutical data

pharms <- read_csv(file.choose())

head(pharms)
glimpse(pharms)
summary(pharms)

# Check missing values
colSums(is.na(pharms))

# Check number of rows and duplicated rows
nrow(pharms)
nrow(distinct(pharms))


# Convert NumDisps to numeric

# Values reported as "<6" do not provide an exact number.
# They are treated as missing for numerical analysis.

pharms <- pharms %>%
  mutate(
    NumDisps_num = if_else(
      NumDisps == "<6",
      NA_real_,
      as.numeric(NumDisps)
    )
  )

# Number of "<6" values
sum(pharms$NumDisps == "<6", na.rm = TRUE)

# Missing values after conversion
sum(is.na(pharms$NumDisps_num))


# Identify the Top 10 Therapeutic Groups

top10 <- pharms %>%
  group_by(TherapeuticGrp2) %>%
  summarise(
    total_dispensings = sum(
      NumDisps_num,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(total_dispensings)) %>%
  slice_head(n = 10)

top10


# Figure 1: Top 10 Therapeutic Groups

fig1 <- ggplot(
  top10,
  aes(
    x = reorder(
      TherapeuticGrp2,
      total_dispensings
    ),
    y = total_dispensings
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    title = "Top 10 Therapeutic Groups by Initial Dispensings",
    x = "Therapeutic Group Level 2",
    y = "Total initial dispensings (2021–2024)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 9
    )
  )

fig1

ggsave(
  "top10_groups.png",
  fig1,
  width = 9,
  height = 6,
  dpi = 300
)


# Figure 2: Trends in the Top 10 Therapeutic Groups

trend_top10 <- pharms %>%
  filter(
    TherapeuticGrp2 %in% top10$TherapeuticGrp2
  ) %>%
  group_by(
    TherapeuticGrp2,
    YearDisp
  ) %>%
  summarise(
    total_dispensings = sum(
      NumDisps_num,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

trend_top10


fig2 <- ggplot(
  trend_top10,
  aes(
    x = YearDisp,
    y = total_dispensings,
    group = TherapeuticGrp2,
    colour = TherapeuticGrp2
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  scale_x_continuous(
    breaks = 2021:2024
  ) +
  labs(
    title = "Trends in Initial Dispensings for the Top 10 Therapeutic Groups",
    x = "Year",
    y = "Total initial dispensings",
    colour = "Therapeutic Group Level 2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold"
    ),
    legend.position = "right"
  )

fig2

ggsave(
  "trend_top10.png",
  fig2,
  width = 9,
  height = 6,
  dpi = 300
)


# Load district-level data

district_data <- read_csv(file.choose())

head(district_data)
glimpse(district_data)

nrow(district_data)

unique(district_data$District)


# Convert NumDisps to numeric

district_data <- district_data %>%
  mutate(
    NumDisps_num = if_else(
      NumDisps == "<6",
      NA_real_,
      as.numeric(NumDisps)
    )
  )


# Remove New Zealand total

district_data <- district_data %>%
  filter(
    District != "New Zealand"
  )

n_distinct(district_data$District)

unique(district_data$District)


# Figure 3: Total Initial Dispensings by District

district_total <- district_data %>%
  group_by(District) %>%
  summarise(
    total_dispensings = sum(
      NumDisps_num,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(total_dispensings))

district_total


fig3 <- ggplot(
  district_total,
  aes(
    x = reorder(
      District,
      total_dispensings
    ),
    y = total_dispensings
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    title = "Total Initial Dispensings by District",
    x = "District",
    y = "Total initial dispensings (2021–2024)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 9
    )
  )

fig3

ggsave(
  "district_total.png",
  fig3,
  width = 9,
  height = 6,
  dpi = 300
)


# Top 10 Therapeutic Groups by District

district_top10 <- district_data %>%
  filter(
    TherapeuticGrp2 %in% top10$TherapeuticGrp2
  ) %>%
  group_by(
    District,
    TherapeuticGrp2
  ) %>%
  summarise(
    total_dispensings = sum(
      NumDisps_num,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

nrow(district_top10)

district_top10


# Create district order

district_order <- district_total %>%
  arrange(total_dispensings) %>%
  pull(District)

district_top10 <- district_top10 %>%
  mutate(
    District = factor(
      District,
      levels = district_order
    ),
    TherapeuticGrp2 = factor(
      TherapeuticGrp2,
      levels = rev(top10$TherapeuticGrp2)
    )
  )


# Figure 4: Top 10 Therapeutic Groups by District

fig4 <- ggplot(
  district_top10,
  aes(
    x = TherapeuticGrp2,
    y = District,
    fill = total_dispensings
  )
) +
  geom_tile() +
  scale_fill_continuous(
    labels = scales::comma
  ) +
  labs(
    title = "Top 10 Therapeutic Groups by District",
    x = "Therapeutic Group Level 2",
    y = "District",
    fill = "Total initial dispensings"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    ),
    axis.text.y = element_text(
      size = 9
    )
  )

fig4

ggsave(
  "district_top10.png",
  fig4,
  width = 9,
  height = 7,
  dpi = 300
)


# Key results

top10

highest_districts <- district_total %>%
  slice_max(
    total_dispensings,
    n = 5
  )

highest_districts

lowest_districts <- district_total %>%
  slice_min(
    total_dispensings,
    n = 5
  )

lowest_districts


# Change from 2021 to 2024

change_top10 <- trend_top10 %>%
  group_by(
    TherapeuticGrp2
  ) %>%
  summarise(
    dispensing_2021 =
      total_dispensings[
        YearDisp == 2021
      ],
    dispensing_2024 =
      total_dispensings[
        YearDisp == 2024
      ],
    change =
      dispensing_2024 -
      dispensing_2021,
    percent_change =
      (
        dispensing_2024 -
          dispensing_2021
      ) /
      dispensing_2021 *
      100,
    .groups = "drop"
  ) %>%
  arrange(
    desc(change)
  )

change_top10


# Highest District + Therapeutic Group combinations

highest_combinations <- district_top10 %>%
  arrange(
    desc(total_dispensings)
  ) %>%
  slice_head(
    n = 10
  )

highest_combinations


# Lowest District + Therapeutic Group combinations

lowest_combinations <- district_top10 %>%
  arrange(
    total_dispensings
  ) %>%
  slice_head(
    n = 10
  )

lowest_combinations



