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


# Convert NumPpl to numeric

# Values reported as "<6" do not provide an exact number.
# They are treated as missing for numerical analysis.

pharms <- pharms %>%
  mutate(
    NumPpl_num = if_else(
      NumPpl == "<6",
      NA_real_,
      as.numeric(NumPpl)
    )
  )

# Number of "<6" values

sum(
  pharms$NumPpl == "<6",
  na.rm = TRUE
)

# Missing values after conversion

sum(is.na(pharms$NumPpl_num))


# Identify the Top 10 Therapeutic Groups

top10 <- pharms %>%
  group_by(TherapeuticGrp2) %>%
  summarise(
    total_patients = sum(
      NumPpl_num,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(total_patients)) %>%
  slice_head(n = 10)

top10


# Figure 1: Top 10 Therapeutic Groups

fig1 <- ggplot(
  top10,
  aes(
    x = reorder(
      TherapeuticGrp2,
      total_patients
    ),
    y = total_patients
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    title = "Top 10 Therapeutic Groups by Number of Patients",
    x = "Therapeutic Group Level 2",
    y = "Total patients (2021–2025)"
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
  "top10_groups_patient.png",
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
    total_patients = sum(
      NumPpl_num,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

trend_top10


fig2 <- ggplot(
  trend_top10,
  aes(
    x = YearDisp,
    y = total_patients,
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
    breaks = 2021:2025
  ) +
  labs(
    title = "Trends in Number of Patients for the Top 10 Therapeutic Groups",
    x = "Year",
    y = "Total patients",
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
  "trend_top10_patient.png",
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


# Convert NumPpl to numeric

district_data <- district_data %>%
  mutate(
    NumPpl_num = if_else(
      NumPpl == "<6",
      NA_real_,
      as.numeric(NumPpl)
    )
  )


# Remove New Zealand total

district_data <- district_data %>%
  filter(
    District != "New Zealand"
  )

n_distinct(district_data$District)

unique(district_data$District)


# Figure 3: Total Patients by District

district_total <- district_data %>%
  group_by(District) %>%
  summarise(
    total_patients = sum(
      NumPpl_num,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(total_patients))

district_total


fig3 <- ggplot(
  district_total,
  aes(
    x = reorder(
      District,
      total_patients
    ),
    y = total_patients
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    title = "Total Number of Patients by District",
    x = "District",
    y = "Total patients (2021–2025)"
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
  "district_total_patient.png",
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
    total_patients = sum(
      NumPpl_num,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

nrow(district_top10)

district_top10


# Create district order

district_order <- district_total %>%
  arrange(total_patients) %>%
  pull(District)

district_top10 <- district_top10 %>%
  mutate(
    District = factor(
      District,
      levels = district_order
    ),
    TherapeuticGrp2 = factor(
      TherapeuticGrp2,
      levels = rev(
        top10$TherapeuticGrp2
      )
    )
  )


# Figure 4: Top 10 Therapeutic Groups by District

fig4 <- ggplot(
  district_top10,
  aes(
    x = TherapeuticGrp2,
    y = District,
    fill = total_patients
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
    fill = "Total patients"
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
  "district_top10_patient.png",
  fig4,
  width = 9,
  height = 7,
  dpi = 300
)


# Key results

top10

highest_districts <- district_total %>%
  slice_max(
    total_patients,
    n = 5
  )

highest_districts

lowest_districts <- district_total %>%
  slice_min(
    total_patients,
    n = 5
  )

lowest_districts


# Change from 2021 to 2025

change_top10 <- trend_top10 %>%
  group_by(
    TherapeuticGrp2
  ) %>%
  summarise(
    patients_2021 =
      total_patients[
        YearDisp == 2021
      ],
    patients_2025 =
      total_patients[
        YearDisp == 2025
      ],
    change =
      patients_2025 -
      patients_2021,
    percent_change =
      (
        patients_2025 -
          patients_2021
      ) /
      patients_2021 *
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
    desc(total_patients)
  ) %>%
  slice_head(
    n = 10
  )

highest_combinations


# Lowest District + Therapeutic Group combinations

lowest_combinations <- district_top10 %>%
  arrange(
    total_patients
  ) %>%
  slice_head(
    n = 10
  )

lowest_combinations

