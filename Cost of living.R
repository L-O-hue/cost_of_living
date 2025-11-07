

# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(stringr)
library(RColorBrewer)
library(gganimate)   
library(gifski)      

## Preparation of dataset for visualisation
############################################
## Consumer Price Index Data (CPI Annual) ##
############################################

# Check available sheets to determine which to use
excel_sheets("ConsumerPriceIndex.xlsx") # Check available sheets

# Annual CPI information
CPI_annual_raw <- read_excel("ConsumerPriceIndex.xlsx",
                             sheet = "T3",
                             range = "A11:BM218") 

# Sanity check for annual CPI 
str(CPI_annual_raw) # Structure before 2024 is chr
head(CPI_annual_raw) # First rows
colnames(CPI_annual_raw) # Column names
any(CPI_annual_raw == "na", na.rm = T) # Presence of string "na"
colSums(CPI_annual_raw == "na", na.rm = T) # Check which columns contains "na"

# Convert data type of annual CPI to numeric
CPI_annual_raw <- CPI_annual_raw %>%
  mutate(across(-`Data Series`, as.character)) %>%   # Convert all columns to chr except first column
  mutate(across(-`Data Series`, ~na_if(., "na"))) %>%  # Convert na (string) to na
  mutate(across(-`Data Series`, ~as.numeric(.)))  # Convert all columns to numeric except first

# Check structure after conversion
str(CPI_annual_raw) 

# Convert from wide to long format for plotting
CPI_annual <- CPI_annual_raw %>%
  pivot_longer(cols = -'Data Series',
               names_to = "year",
               values_to = "CPI") %>%
  mutate(year = as.numeric(year)) # Convert Year from character to numeric
  

# Sanity check for cleaned CPI_annual
str(CPI_annual)
head(CPI_annual)

# Create new column for indices (indexed to base Year 2023)
CPI_annual <- CPI_annual %>%
  group_by(`Data Series`) %>%
  mutate(CPI_index_2023 = (CPI / CPI[year == 2023]) * 100) %>%
  ungroup()

# Data frame to store GST years and corresponding %
gst_years <- data.frame(year = c(1994, 2003, 2004, 2007,2023,2024), 
                        gst = c("GST 3% 1994", "GST 4% 2003", "GST 5% 2004",
                                "GST 7% 2007", "GST 8% 2023", "GST 9% 2024"))

# Plot CPI for all items (overall trend)
CPI_annual %>%
  filter(`Data Series` == "All Items" & year>=1980) %>% 
  ggplot(aes(x = year, y = CPI)) +
  geom_line(size = 0.8, color = "steelblue") +
  geom_point(size = 1.5, color = "steelblue") +
  geom_vline(data = gst_years, aes(xintercept = year),linetype = "dashed", 
             color = "black", alpha = 0.3, size =0.4) +
  geom_text(data = gst_years, aes(x = year, y = 80, label = gst),
            angle = 90, vjust = -0.5, size = 2.8, color = "red", alpha = 0.8) +
  geom_rect(data = gst_years,
            inherit.aes = FALSE,
            aes(xmin = year - 0.3, xmax = year + 0.3,
                ymin = -Inf, ymax = Inf),
            fill = "darkgrey", alpha = 0.3) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 2)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9)
  ) +
  labs(
    title = "How Singapore’s Cost of Living Has Risen Over Time",
    subtitle = "Consumer Price Index (CPI, 2024 base year), with GST hikes marked along the way",
    caption = "Source: Singapore Department of Statistics",
    x = "Year",
    y = "Comsumer Price Index"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15),      # Bold, larger title
    plot.subtitle = element_text(size = 12, face = "italic")  # Smaller, italic subtitle
  )

# Broad categories of interest
# All Items, Food, Clothing & Footwear, Housing & Utilities, Household Durables & Services
# Health, Transport, Information & Communication, Recreation, Sport & Culture
# Education, Miscellaneous Goods & Services

# Store categories for CPI
CPI_categories <- c("Food", "Clothing & Footwear", "Housing & Utilities", "Household Durables & Services",
                    "Health", "Transport", "Information & Communication",
                    "Recreation, Sport & Culture", "Education", "Miscellaneous Goods & Services")

# Plot CPI (absolute index) for main categories (base year 2024)
CPI_annual %>%
  filter(`Data Series` %in% CPI_categories & year>=1989) %>% 
  ggplot(mapping = aes(x = year, y = CPI, color = `Data Series`)) +
  geom_line(size = 0.5) +
  geom_point(size = 0.5) +
  geom_vline(data = gst_years, aes(xintercept = year),linetype = "dashed", 
             color = "black", alpha = 0.6, size =0.4) + # GST hikes
  scale_x_continuous(breaks = seq(1989, 2024, by = 5)) +
  facet_wrap(~ `Data Series`, scales ="free") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  ) +
  labs(
    title = "How Prices of Different Goods and Services Have Changed Over Time",
    subtitle = "Trends in Singapore since 1989, with GST hike years highlighted",
    caption = "Source: Singapore Department of Statistics",
    x = "Year",
    y = "Index",
    color = "Category"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),      # Bold, larger title
    plot.subtitle = element_text(size = 12, face = "italic")  # Smaller, italic subtitle
  )


###########################
## CPI % change (Annual) ##
###########################
# Annual CPI% information
CPI_percent_annual_raw <- read_excel("ConsumerPriceIndex.xlsx",
                             sheet = "T8",
                             range = "A11:BL218") 

# Sanity checks
str(CPI_percent_annual_raw)
head(CPI_percent_annual_raw)
any(CPI_percent_annual_raw == "na", na.rm = T) # Presence of string "na"
colSums(CPI_percent_annual_raw == "na", na.rm = T) # Check which columns contains "na"

# Convert data type of annual CPI% change to numeric
CPI_percent_annual_raw <- CPI_percent_annual_raw %>%
  mutate(across(-`Data Series`, as.character)) %>%   # Convert all columns to chr except first column
  mutate(across(-`Data Series`, ~na_if(., "na"))) %>%  # Convert na (string) to na
  mutate(across(-`Data Series`, ~as.numeric(.)))  # Convert all columns to numeric except first

# Convert from wide to long format
CPI_percent_annual <- CPI_percent_annual_raw %>%
  pivot_longer(cols = -'Data Series',
               names_to = "year",
               values_to = "CPI") %>%
  mutate(year = as.numeric(year))      # Convert Year from character to numeric

# Sanity check
str(CPI_percent_annual)


# Broad categories of interest
# All Items, Food, Clothing & Footwear, Housing & Utilities
# Health, Transport, Information & Communication,   Recreation, Sport & Culture
# Education, Miscellaneous Goods & Services

# Plot CPI % for all items (overall)
CPI_percent_annual %>%
  filter(`Data Series` == "All Items" & year>=1980) %>% # Year 1994 (first GST)
  ggplot(mapping = aes(x = year, y = CPI)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 1.5, color = "steelblue") +
  geom_vline(data = gst_years, aes(xintercept = year),linetype = "dashed", 
             color = "black", alpha = 0.7, size =0.4) + # GST hikes
  geom_text(data = gst_years, aes(x = year, y = 7, label = gst),
            angle = 90, vjust = -0.5, size = 2.8, color = "red", alpha = 0.6) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 2)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9)
  ) +
  labs(
    title = "Year-on-Year Change in Consumer Price Index (CPI)",
    subtitle = "Inflation trend in Singapore with GST hike years highlighted",
    caption = "Source: Singapore Department of Statistics",
    x = "Year",
    y = "Year-on-Year Percent Change (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),      # Bold, larger title
    plot.subtitle = element_text(size = 12, face = "italic")  # Smaller, italic subtitle
  )


# Plot CPI % for main categories
CPI_percent_annual %>%
  filter(`Data Series` %in% CPI_categories & year>=1992) %>% 
  ggplot(mapping = aes(x = year, y = CPI, color = `Data Series`)) +
  geom_line(size = 0.5) +
  geom_point(size = 0.5) +
  geom_vline(data = gst_years, aes(xintercept = year),linetype = "dashed", 
             color = "black", alpha = 0.6, size =0.4) + # GST hikes
  scale_x_continuous(breaks = seq(1992, 2024, by = 4)) +
  facet_wrap(~ `Data Series`, scales ="free") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  ) +
  labs(
    title = "Where Singaporeans Feel the Pinch",
    subtitle = "Year-on-year inflation by category of goods and services, with GST hikes highlighted",
    caption = "Source: Singapore Department of Statistics",
    x = "Year",
    y = "Year-on-Year CPI Percent Change (%)",
    color = "Category"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),      # Bold, larger title
    plot.subtitle = element_text(size = 12, face = "italic")  # Smaller, italic subtitle
  )


####################
## HDB Index Data ##
####################

# Import HDB csv
hdb_raw <- read.csv("HDBResalePriceIndex1Q2009100Quarterly.csv")

# Sanity check for raw file
str(hdb_raw) 
summary(hdb_raw)
sum(is.na(hdb_raw))

# Change quarter data type
hdb_raw$quarter <- as.yearqtr(hdb_raw$quarter, format = "%Y-Q%q")

# Save to new df (quarterly data)
hdb_quarter <- hdb_raw

# Aggregate quarter into yearly and save to new df (annual data) and create
# new index column based off year 2024
hdb_annual <- hdb_quarter %>%
  mutate(year = as.numeric(str_sub(quarter, 1, 4))) %>% # Extract year
  group_by(year) %>%
  summarise(index = mean(index, na.rm = TRUE)) %>% 
  mutate(hdb_index_2024 = (index / index[year == 2024]) * 100)

# Create a merged dataframe for CPI & HDB resale
CPI_hdb <- CPI_hdb <- left_join(CPI_annual, hdb_annual,by = "year")

# Plot HDB index with CPI
CPI_hdb %>%
  filter(`Data Series`=="All Items", year>=1990, year<=2024) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = CPI, color = "CPI"), size = 1) +
  geom_point(aes(y = CPI, color = "CPI"), size = 2) +
  geom_line(aes(y = hdb_index_2024, color = "HDB"), size = 1) +
  geom_point(aes(y = hdb_index_2024, color = "HDB"), size = 2) +
  geom_vline(
    data = gst_years %>% filter(year >= 1990, year<=2024),
    aes(xintercept = year),
    linetype = "dashed",
    color = "black",
    alpha = 0.3,
    size = 0.8
  ) +
  geom_rect(data = gst_years,
            inherit.aes = FALSE,
            aes(xmin = year - 0.2, xmax = year + 0.2,
                ymin = -Inf, ymax = Inf),
            fill = "darkgrey", alpha = 0.3) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 2)) +
  labs(
    title = "How Rising HDB Prices Shape the Cost of Living (2024 base year)",
    subtitle = "CPI and HDB resale index trends from 1990–2024, with GST revisions highlighted",
    caption = "Source: data.gov.sg; Singapore Department of Statistics",
    x = "Year",
    y = "Index",
    color = ""
  ) +
  scale_color_manual(values = c("CPI" = "blue", "HDB" = "red")) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9)
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),      # Bold, larger title
    plot.subtitle = element_text(size = 11, face = "italic")  # Smaller, italic subtitle
  )

#########################
## Median gross income ##
#########################

# Import income csv 
income_raw <- read.csv("MedianGrossMonthlyIncomeFromEmploymentofFullTimeEmployedResidentsTotal.csv")

# Sanity check
str(income_raw)
head(income_raw)
summary(income_raw)
sum(is.na(income_raw))

# Convert year and income columns to numeric and save to new df 
income_annual <- income_raw %>%
  mutate(across(c(year, med_income_incl_empcpf, med_income_excl_empcpf), as.numeric))

# Sanity check 
str(income_annual)

# Create new index column (base year 2023)
income_annual <- income_annual %>%
  mutate(income_index = (med_income_incl_empcpf / med_income_incl_empcpf[year == 2023]) * 100)

# Create a merged dataframe for CPI and income
CPI_income <- left_join(CPI_annual, income_annual,by = "year")

# Plot income with CPI over the years
CPI_income %>%
  filter(`Data Series`=="All Items", year>=2001, year<=2023) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = CPI_index_2023, color = "CPI"), size = 1) +
  geom_point(aes(y = CPI_index_2023, color = "CPI"), size = 2) +
  geom_line(aes(y = income_index, color = "Income"), size = 1) +
  geom_point(aes(y = income_index, color = "Income"), size = 2) +
  geom_vline(
    data = gst_years %>% filter(year >= 2001, year<=2023),
    aes(xintercept = year),
    linetype = "dashed",
    color = "black",
    alpha = 0.8,
    size = 0.5
  ) +
  scale_x_continuous(breaks = seq(2001, 2023, by = 1)) +
  labs(
    title = "Are Rising Incomes Enough to Match Rising Prices?",
    subtitle = "Median income index vs. CPI index (2001–2023, 2023 = 100), with GST hikes highlighted",
    caption = "Source: data.gov.sg; Singapore Department of Statistics\nNote: Income data for 2005 is not available",
    x = "Year",
    y = "Index",
    color = ""
  ) +
  scale_color_manual(values = c("CPI" = "blue", "Income" = "orange")) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),      # Bold, larger title
    plot.subtitle = element_text(size = 11, face = "italic")  # Smaller, italic subtitle
  )

#################
# Bank Interest #
#################

# Import bank interest csv
bank_raw <- read.csv("CurrentBanksInterestRatesEndOfPeriodMonthly.csv")

# Sanity check
str(bank_raw) # Data is in wide format
head(bank_raw)
summary(bank_raw)
sum(is.na(bank_raw))
any(bank_raw == "na", na.rm = T) # Presence of string "na"
colSums(bank_raw == "na", na.rm = T) # Check which columns contains "na"

# Data Cleaning
bank_month <- bank_raw %>%
  mutate(across(-`DataSeries`, as.character)) %>%   # Convert all columns to chr except first column
  mutate(across(-`DataSeries`, ~na_if(., "na"))) %>%  # Convert na (string) to na
  mutate(across(-`DataSeries`, ~as.numeric(.))) %>%  # Convert all columns to numeric except first
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year_month",
    values_to = "interest rate"
  ) %>%                         # Convert to long format
  mutate(year_month = str_remove(year_month, "X")) %>% # Remove the X in front of year
  mutate(
    year_month = as.Date(paste0(year_month, "01"), format = "%Y%b%d")) # Convert year_month into a date

# Sanity check for bank_month df
str(bank_month)

# Dataframe containing year-month gst 
GST_year_month <- data.frame(
  year_month = c("1994-04", "2003-01", "2004-01", "2007-07", "2023-01", "2024-01"),
  gst_rate   = c("GST 3% 1994", "GST 4% 2003", "GST 5% 2004", "GST 7% 2007","GST 8% 2023", "GST 9% 2024"))

# Convert to proper readable Date format
GST_year_month$year_month <- as.Date(paste0(GST_year_month$year_month, "-01"))

# Plotting facets of bank interest
bank_month %>%
  filter(DataSeries %in% c(
    "Government Securities - 5-Year Bond Yield",
    "Government Securities - 2-Year Bond Yield",
    "Government Securities - 1-Year Treasury Bills Yield"
  )) %>%
  ggplot(aes(x = year_month, y = `interest rate`, color = DataSeries)) +
  geom_line(size = 1) +
  geom_vline(data = GST_year_month, aes(xintercept = year_month),
             linetype = "dashed", color = "black", alpha = 0.6) +
  facet_wrap(~ DataSeries, scales = "free_y") +  # separate panel per bond type
  scale_x_date(
    date_breaks = "3 years",   # Every 3 years
    date_labels = "%Y"         # Show only the year
  ) +
  labs(title = "Rising and Falling Borrowing Costs in Singapore",
       subtitle = "Government bond and treasury yields (1988–2025), with GST hikes highlighted",
       x = "Year", 
       y = "Interest Rate (%)",
       color = "Bond Type") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),      # Bold, larger title
    plot.subtitle = element_text(size = 11, face = "italic")  # Smaller, italic subtitle
  )


# Create a new dataframe containing only aggregated interest (annual)
# for different bond types
bank_year <- bank_month %>%
  mutate(year = format(year_month, "%Y")) %>%  # Extract year
  group_by(DataSeries, year) %>%               # Group by category and year
  summarise(avg_interest_rate = mean(`interest rate`, na.rm = TRUE), .groups = "drop") %>% # average across months by groups
  mutate(year = as.numeric(year)) # convert year to numeric

# Sanity check for bank_year df
str(bank_year)
head(bank_year)  

# Plot aggregated interest
# Use this
bank_year %>%
  filter(DataSeries %in% c(
    "Government Securities - 5-Year Bond Yield",
    "Government Securities - 2-Year Bond Yield",
    "Government Securities - 1-Year Treasury Bills Yield"
  )) %>%
  ggplot(aes(x = year, y = avg_interest_rate, color = DataSeries)) +
  geom_line(size = 1) +
  geom_vline(data = gst_years, aes(xintercept = year),
             linetype = "dashed", color = "black", alpha = 0.6) +
  scale_x_continuous(breaks = seq(1988, 2025, by = 5)) +
  facet_wrap(~ DataSeries, scales = "free_y") +  # separate panel per bond type
  labs(title = "Government Bond and Treasury Yields with GST Hikes",
       subtitle = "Shows how borrowing costs changed over the years and marks when GST was increased",
       x = "Year", y = "Interest Rate (%)",
       color = "Bond Type ") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),      # Bold, larger title
    plot.subtitle = element_text(size = 11, face = "italic")  # Smaller, italic subtitle
  )

###############################
# Industrial Production Index #
###############################  

# Import industrial production data
ind_raw <- read.csv("IndexOfIndustrialProduction2015100Monthlybymanufacturingclusters.csv")

# Sanity checks
str(ind_raw)
head (ind_raw)
colnames(ind_raw) # Column names
any(ind_raw == "na", na.rm = T) # Presence of string "na"
colSums(ind_raw == "na", na.rm = T) # Check which columns contains "na"

# Data cleaning
ind_year <- ind_raw %>%
  select(-level_1) %>% # Remove level_1 column
  rename(cluster = level_2) %>% # Rename level_2 to cluster
  rename(year = month) %>% # Rename month to year
  mutate(year = as.numeric(substr(year, 1, 4))) %>% # Take first 4 characters to keep years
  rename(ind_index = value) %>% # Rename value to ind_index
  mutate(ind_index = na_if(ind_index, "na")) %>% # Replace with string "na" with NA
  mutate(ind_index = as.numeric(ind_index)) %>% # Make index numeric
  group_by(cluster, year) %>%               # Group by cluster and year
  summarise(avg_ind_index = mean(ind_index, na.rm = TRUE), .groups = "drop") # Get average index

# Sanity check after data clean
str(ind_year)
head(ind_year)
colSums(ind_year == "na", na.rm = T)

# Plot facet plot for different production clusters
ind_year %>%
  filter(year>=1992) %>% 
  ggplot(mapping = aes(x = year, y = avg_ind_index, color = cluster)) +
  geom_line(size = 0.5) +
  geom_point(size = 0.5) +
  scale_x_continuous(breaks = seq(1992, 2019, by = 3)) +
  facet_wrap(~ `cluster`, scales ="free") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  ) +
  labs(
    title = "Singapore’s Industrial Growth: Shifts Over the Past Three Decades",
    subtitle = "Trends in the Industrial Production Index (by cluster) from 1992 to 2019",
    caption = "Source: data.gov.sg",
    x = "Year",
    y = "Index",
    color = "Cluster"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),      # Bold, larger title
    plot.subtitle = element_text(size = 12, face = "italic")  # Smaller, italic subtitle
  )


##########################
# Income (by profession) #
##########################

# Import data
job_raw <- read.csv("MedianGrossMonthlyIncomeFromEmploymentIncludingEmployerCPFOfFullTimeEmployedResidentsByOccupationsAndSexEndJuneAnnual.csv")

# Sanity Check
str(job_raw)
any(job_raw == "na", na.rm = T) # Presence of string "na"
sum(is.na(job_raw)) # Presence of missing value

# Data cleaning and transformation
job_year <- job_raw %>%
  rename_with(~gsub("^X","",.x)) %>%      # Remove X from column
  rename(profession = `DataSeries`) %>%   # Rename DataSeries to profession
  separate(profession, into = c("profession", "gender"), sep = " - ") %>%
  pivot_longer(cols = -c(profession, gender),
               names_to = "year",
               values_to = "income") %>%  # Reshape data set to long
  mutate(year = as.numeric(year)) %>%     # Convert year data type to numeric
  mutate(income = as.numeric(income)) %>% # Convert income data type to numeric
  group_by(profession, year) %>%
  summarise(income = mean(income, na.rm = T), .groups = "drop") %>% # Avg across gender
  group_by(profession) %>%
  mutate(job_index = (income / income[year == 2023])*100) %>%
  ungroup()

# Cleaned dataframe - job_year

# Sanity Check for new df
str(job_year)

# Merge job_year with CPI data into new df CPI_job
CPI_job <- job_year %>%
  left_join(CPI_annual %>% filter(`Data Series`== "All Items"),
            by = "year")

# Plot the CPI_job
ggplot(CPI_job, aes(x = year)) +
  # Income lines
  geom_line(aes(y = job_index, color = profession), size = 0.8) +
  geom_point(aes(y = job_index, color = profession),size = 1) +
  scale_color_brewer(palette = "Set2") +
  # CPI line
  geom_line(aes(y = CPI_index_2023), color = "black",alpha = 0.6, size = 0.8, linetype = "dashed") +
  scale_x_continuous(breaks = seq(2001, 2023, by = 2)) +
  labs(
    title = "Do All Professions Keep Up with Rising Prices",
    subtitle = "Indexed incomes (2023 = 100) by profession, compared with CPI (dashed line)",
    caption = "Source: data.gov.sg; Singapore Department of Statistics",
    x = "Year",
    y = "Index",
    color = "Profession"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),      # Bold, larger title
    plot.subtitle = element_text(size = 11, face = "italic")  # Smaller, italic subtitle
  )


#############################
# Question 1b Animated plot #
#############################
# Base plot using CPI_hdb
p_anim1 <- CPI_hdb %>%
  filter(`Data Series`=="All Items", year>=1990, year<=2024) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = CPI, color = "CPI"), size = 1) +
  geom_point(aes(y = CPI, color = "CPI"), size = 2.5) +
  geom_line(aes(y = hdb_index_2024, color = "HDB"), size = 1) +
  geom_point(aes(y = hdb_index_2024, color = "HDB"), size = 2.5) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 2)) +
  labs(
    title = "How Rising HDB Prices Shape the Cost of Living (2024 base year)",
    subtitle = "CPI and HDB resale index trends from 1990–2024",
    caption = "Source: data.gov.sg; Singapore Department of Statistics",
    x = "Year",
    y = "Index",
    color = ""
  ) +
  scale_color_manual(values = c("CPI" = "blue", "HDB" = "red")) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9)
  ) +
  theme_minimal(base_size = 14) +
  transition_reveal(year) # Animate


# Render p_anim1
animate(
  p_anim1,
  width = 800, height = 600,
  fps = 35, duration = 15, # 15 seconds animation
  renderer = gifski_renderer()
)

# Save as gif file
anim_save("CPI_hdb.gif", animation = last_animation())
