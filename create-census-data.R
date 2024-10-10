
library(dplyr)
set.seed(123)

# Population size:
num_people <- 1000
start_year <- 2010
end_year <- start_year+10
years <- start_year:end_year

# Set age at first observation to a random value between 18 and 65:
initial_ages <- sample(18:65, num_people, replace = TRUE)

# Parent income and education level (constant):
parent_income <- sample(c('Low', 'Medium', 'High'), num_people, replace = TRUE)
parent_education <- sample(c('Primary', 'Secondary', 'Tertiary'), num_people, replace = TRUE)

# Define IDs for places of residence, education, and work:
neighborhoods <- paste0('N', 1:250)
education_places <- paste0('E', 1:25)
work_places <- paste0('W', 1:350)

# Initiate an empty data frame:
sample_population <- data.frame()

# Initialize tracking of place of residence, education, and work:
initial_residence <- sample(neighborhoods, num_people, replace = TRUE)
initial_work <- rep(NA, num_people)
initial_education <- ifelse(initial_ages >= 18 & initial_ages <= 25,
                            sample(education_places, num_people, replace = TRUE), NA)

# Track the number of changes
residence_changes <- rep(0, num_people)
work_changes <- rep(0, num_people)

# Assign gender once per person
n <- num_people
gender_vector <- rep(c("Male", "Female"), each = ceiling(n / 2))[1:n]
gender <- sample(gender_vector, n)

# Defining a function to apply the weights
weighted_sample_gender <- function(values, socio_economic, gender, weights_male, weights_female) {
  sapply(1:length(socio_economic), function(i) {
    se <- socio_economic[i]
    g <- gender[i]
    if (g == "Male") {
      sample(values, 1, prob = weights_male[[se]])
    } else {
      sample(values, 1, prob = weights_female[[se]])
    }
  })
}

# Add bias using weights:
neighborhood_weights <- list(
  "Low" = rep(1, 250),                    # Equal chance
  "Medium" = c(rep(3, 75), rep(1, 175)),  # More likely to be in the first 75 neighborhoods
  "High" = c(rep(10, 30), rep(1, 220))    # More likely to be in the first 30 neighborhoods
)

education_weights <- list(
  "Low" = rep(1, 25),                   # Equal chance
  "Medium" = c(rep(3, 10), rep(1, 15)), # More likely to be in the first 10 places of education
  "High" = c(rep(10, 8), rep(1, 17))    # More likely to be in the first 8 places of education
)

# Define work weights for males
work_weights_male <- list(
  "Low" = rep(1, 350),                    # Equal chance
  "Medium" = c(rep(3, 100), rep(1, 250)), # More likely to be in the first 100 places of employment
  "High" = c(rep(10, 50), rep(1, 300))    # More likely to be in the first 50 places of employment
)

# Define work weights for females (higher likelihood in first 30 workplaces)
work_weights_female <- list(
  "Low" = c(rep(2, 30), rep(1, 320)),                    # Twice as likely in first 30 workplaces
  "Medium" = c(rep(6, 30), rep(3, 70), rep(1, 250)),     # Higher weights in first 30 workplaces
  "High" = c(rep(20, 30), rep(1, 320))                   # Much higher likelihood in first 30 workplaces
)

# Loop over each year:
for (year in years) {
  # Increase age by 1 each year:
  ages <- initial_ages + (year - 2020)

  # Allow for both education and work at the same time for ages 18-30:
  place_of_education <- ifelse(ages >= 18 & ages <= 30,
                               weighted_sample_gender(education_places, parent_income, gender,
                                                      education_weights, education_weights),
                               NA)

  # Assign place of work for people aged 25+ (including those in education for ages 18-30):
  place_of_work <- ifelse(ages >= 25,
                          ifelse(is.na(initial_work),
                                 # Assign initial place of employment:
                                 weighted_sample_gender(work_places, parent_income, gender,
                                                        work_weights_male, work_weights_female),
                                 ifelse(work_changes < 4,
                                        # 20% chance to change work if num. changes still < 4:
                                        ifelse(runif(num_people) < 0.2,
                                               weighted_sample_gender(work_places, parent_income, gender,
                                                                      work_weights_male, work_weights_female),
                                               initial_work),
                                        initial_work)),
                          NA)

  # Update the tracking of number of places of employment:
  work_changes <- work_changes + ifelse(is.na(initial_work), 0, initial_work != place_of_work)
  initial_work <- ifelse(is.na(place_of_work), initial_work, place_of_work)

  # Assign place of residence, weighted:
  place_of_residence <- ifelse(residence_changes < 1,
                               # 10% chance to change residence if number of changes is still < 1:
                               ifelse(runif(num_people) < 0.1,
                                      weighted_sample_gender(neighborhoods, parent_income, gender,
                                                             neighborhood_weights, neighborhood_weights),
                                      initial_residence),
                               initial_residence)

  # Update the tracking of number of places of residence:
  residence_changes <- residence_changes + (initial_residence != place_of_residence)
  initial_residence <- place_of_residence

  # Add incomes
  base_income <- 20000  # Base income for lowest education level
  education_multiplier <- 10000  # Additional income per education level
  income_sd <- 5000  # Standard deviation of income

  income <- rnorm(
    n,
    mean = base_income + education_multiplier * (as.numeric(factor(parent_education)) - 1),
    sd = income_sd
  )

  # Add education
  education_sd <- 1  # Standard deviation for education levels

  education <- round(
    pmin(
      pmax(
        rnorm(
          n,
          mean = as.numeric(factor(parent_education)),
          sd = education_sd
        ),
        1  # Minimum education level
      ),
      3  # Maximum education level
    )
  )

  # Combine the data:
  year_data <- tibble(
    PersonID = 1:num_people,
    Year = year,
    Age = ages,
    Gender = gender,
    PersonalIncome = income,
    PersonalEducation = education,
    ParentIncome = parent_income,
    ParentEducation = parent_education,
    PlaceOfEducation = place_of_education,
    PlaceOfWork = place_of_work,
    PlaceOfResidence = place_of_residence
  )

  # Append yearly data to the complete dataframe:
  sample_population <- bind_rows(sample_population, year_data)
}

# Convert 'PersonalEducation' to categorical levels
sample_population <-
  sample_population %>%
  mutate(PersonalEducation = case_when(
    PersonalEducation == 1 ~ "Primary",
    PersonalEducation == 2 ~ "Secondary",
    PersonalEducation == 3 ~ "Tertiary")) %>%
  filter(!is.na(Year)) %>%
  arrange(PersonID, Year)

# Add 'Ethnicity' variable based on the previous instructions:


# Step 2: Calculate average 'PersonalIncome' and 'PersonalEducation_num' per 'PersonID'
person_scores <- sample_population %>%
  group_by(PersonID) %>%
  summarise(
    AvgIncome = mean(PersonalIncome, na.rm = TRUE),
    AvgEducation = mean(as.numeric(factor(ParentEducation)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Step 3: Compute a score inversely related to income and education
  mutate(
    Score = - (AvgIncome + AvgEducation)  # Negative sum so that lower income and education have higher scores
  )

# Step 4: Determine the threshold for the top 20% highest scores (lowest income and education)
threshold <- quantile(person_scores$Score, probs = 0.8, na.rm = TRUE)

# Step 5: Assign 'Ethnicity' based on the score
person_scores <- person_scores %>%
  mutate(
    Ethnicity = ifelse(Score >= threshold, 'non-native', 'native')
  )

# Step 6: Merge 'Ethnicity' back into 'sample_population'
sample_population <- sample_population %>%
  left_join(person_scores %>% select(PersonID, Ethnicity), by = 'PersonID')


# Add industry 

# Convert 'PersonalEducation', 'ParentEducation', and 'ParentIncome' to numeric levels
sample_population <- sample_population %>%
  mutate(
    PersonalEducation_num = case_when(
      PersonalEducation == 'Primary' ~ 1,
      PersonalEducation == 'Secondary' ~ 2,
      PersonalEducation == 'Tertiary' ~ 3,
      TRUE ~ NA_real_
    ),
    ParentEducation_num = case_when(
      ParentEducation == 'Primary' ~ 1,
      ParentEducation == 'Secondary' ~ 2,
      ParentEducation == 'Tertiary' ~ 3,
      TRUE ~ NA_real_
    ),
    ParentIncome_num = case_when(
      ParentIncome == 'Low' ~ 1,
      ParentIncome == 'Medium' ~ 2,
      ParentIncome == 'High' ~ 3,
      TRUE ~ NA_real_
    )
  )

# Filter out rows where 'PlaceOfWork' is NA (people not working)
workplace_metrics <- sample_population %>%
  filter(!is.na(PlaceOfWork)) %>%
  group_by(PlaceOfWork) %>%
  summarise(
    AvgPersonalEducation = mean(PersonalEducation_num, na.rm = TRUE),
    AvgPersonalIncome = mean(PersonalIncome, na.rm = TRUE),
    AvgParentIncome = mean(ParentIncome_num, na.rm = TRUE),
    AvgParentEducation = mean(ParentEducation_num, na.rm = TRUE),
    ProportionNonNative = mean(Ethnicity == 'non-native', na.rm = TRUE),
    TotalEmployees = n()
  ) %>%
  ungroup()

# Compute thresholds
thresholds <- list(
  HighEducation = quantile(workplace_metrics$AvgPersonalEducation, probs = 0.8, na.rm = TRUE),
  LowIncome = quantile(workplace_metrics$AvgPersonalIncome, probs = 0.2, na.rm = TRUE),
  LowParentIncome = quantile(workplace_metrics$AvgParentIncome, probs = 0.2, na.rm = TRUE),
  LowParentEducation = quantile(workplace_metrics$AvgParentEducation, probs = 0.2, na.rm = TRUE),
  HighNonNative = quantile(workplace_metrics$ProportionNonNative, probs = 0.8, na.rm = TRUE)
)

# Create flags for each industry
workplace_metrics <- workplace_metrics %>%
  mutate(
    Flag_A_B = AvgPersonalEducation >= thresholds$HighEducation,
    Flag_C = AvgPersonalIncome <= thresholds$LowIncome,
    Flag_D = (AvgParentIncome <= thresholds$LowParentIncome) & 
             (AvgParentEducation <= thresholds$LowParentEducation),
    Flag_E = ProportionNonNative >= thresholds$HighNonNative
  )

# Initialize 'Industry' as NA
workplace_metrics$Industry <- NA

# Assign Industry 'E'
workplace_metrics$Industry[workplace_metrics$Flag_E] <- 'E'

# Assign Industry 'D' where Industry is still NA
workplace_metrics$Industry[is.na(workplace_metrics$Industry) & workplace_metrics$Flag_D] <- 'D'

# Assign Industry 'C' where Industry is still NA
workplace_metrics$Industry[is.na(workplace_metrics$Industry) & workplace_metrics$Flag_C] <- 'C'

# Assign Industry 'A' or 'B' randomly to remaining workplaces with high education
set.seed(123)  # For reproducibility
indices_A_B <- which(is.na(workplace_metrics$Industry) & workplace_metrics$Flag_A_B)
workplace_metrics$Industry[indices_A_B] <- sample(c('A', 'B'), length(indices_A_B), replace = TRUE)

# Assign Industry 'A' or 'B' randomly to any remaining workplaces
indices_remaining <- which(is.na(workplace_metrics$Industry))
workplace_metrics$Industry[indices_remaining] <- sample(c('A', 'B'), length(indices_remaining), replace = TRUE)


# Merge 'Industry' back into 'sample_population'
sample_population <- sample_population %>%
  left_join(workplace_metrics %>% select(PlaceOfWork, Industry), by = 'PlaceOfWork')

rm(list=setdiff(ls(), "sample_population"))
