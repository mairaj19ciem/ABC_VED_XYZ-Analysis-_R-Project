
# Install required package
#install.packages(c("RMariaDB", "dplyr", "openxlsx"))

library(RMariaDB)
library(dplyr)
library(openxlsx)

# 1. Database Connection
# MySQL credentials
con <- dbConnect(RMariaDB::MariaDB(),
                 user = 'root',
                 password = 'Mairaj@1987',
                 dbname = 'inventory_db',
                 host = 'localhost')

# Fetch data
df <- dbGetQuery(con, "SELECT * FROM inventory_data")
dbDisconnect(con) # Always close the connection

# 2. Perform ABC, VED, and XYZ Analysis
inventory_analysis <- df %>%

  # --- ABC Analysis ---
  # Calculate total annual value and sort descending
  mutate(annual_value = unit_cost * annual_demand) %>%
  arrange(desc(annual_value)) %>%
  mutate(
    cumulative_value = cumsum(annual_value),
    cumulative_perc = cumulative_value / sum(annual_value),

    # Assign ABC categories (A: top 80%, B: next 15%, C: bottom 5%)
    abc_category = case_when(
      cumulative_perc <= 0.80 ~ 'A',
      cumulative_perc <= 0.95 ~ 'B',
      TRUE ~ 'C'
    )
  ) %>%

  # --- XYZ Analysis ---
  # Calculate Coefficient of Variation (CV) = Standard Deviation / Mean
  mutate(
    cv = std_dev_demand / mean_monthly_demand,

    # Assign XYZ categories (X: CV < 0.2, Y: 0.2 to 0.5, Z: CV > 0.5)
    xyz_category = case_when(
      cv <= 0.2 ~ 'X',
      cv <= 0.5 ~ 'Y',
      TRUE ~ 'Z'
    )
  ) %>%

  # --- VED Analysis ---
  mutate(ved_category = factor(ved_category, levels = c('V', 'E', 'D'))) %>%

  # Create a combined classification (e.g., "AVX", "CEZ",)
  mutate(abc_ved_xyz_class = paste(abc_category, ved_category, xyz_category))

# 3. Create the 27-Class Mapping Reference
# Using expand.grid to systematically generate all 27 combinations
class_matrix <- expand.grid(
  abc_category = c("A", "B", "C"),
  ved_category = c("V", "E", "D"),
  xyz_category = c("X", "Y", "Z")
) %>%
  # Sort logically: A's first, V's first, X's first to prioritize the most critical items
  arrange(abc_category, ved_category, xyz_category) %>%
  mutate(
    abc_ved_xyz_class = paste(abc_category, ved_category, xyz_category),
    # Assign Classes 1 through 27
    class_marking = paste("Class", row_number())
  ) %>%
  select(abc_ved_xyz_class, class_marking)

# 4. Join the Class Markings back to the main inventory data frame
final_inventory_report <- inventory_analysis %>%
  left_join(class_matrix, by = "abc_ved_xyz_class") %>%
  select(item_id, item_name, annual_value, abc_category, ved_category, cumulative_perc, xyz_category, abc_ved_xyz_class, class_marking) %>%
  arrange(class_marking)


head(final_inventory_report)

# 5. Summarize data
matrix_summary <- inventory_analysis %>%
  group_by(abc_category, ved_category, xyz_category) %>%
  summarise(item_count = n(), .groups = 'drop')

# 6. Combine into a list where with Excel sheet names
my_sheets <- list("Summary" = matrix_summary, "Analysis" = final_inventory_report)

# 7. Export the report
write.xlsx(my_sheets,"G:/My Drive/Major_Project/Main/ABC-XYZ_Analysis.xlsx")



