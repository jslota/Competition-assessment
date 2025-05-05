library(dplyr)
library(ggplot2)
library(ComplexHeatmap)
library(emmeans)

dt_inspect <- read.csv("Building_and_Safety_Inspections_20250505.csv") # Load inspection dataset
dt_meta <- read.csv("xnhu-aczu.csv")  # Load dataset with additional information

permit_list <- gsub("-", " ", dt_meta$pcis_permit) # Get list of permits with meta data
dt_inspect <- dt_inspect %>% filter(PERMIT %in% permit_list) # filter based on permit list

dt_meta$PERMIT <- gsub("-", " ", dt_meta$pcis_permit)

dt_inspect <- dt_inspect %>% # Add meta-data to inspection dataset
  left_join(dt_meta %>% select(PERMIT, street_name, contractors_business_name, contractor_city, census_tract), by = "PERMIT")

### Part 1 - show an interesting characteristic

pstatus.tab <- dt_inspect %>%  # make table counting permits by status
  group_by(Permit.Status) %>%
  count() %>%
  filter(n > 10) %>% # only show values with greater than 10 entries
  arrange(n)
pstatus.tab$Permit.Status <- factor(pstatus.tab$Permit.Status, levels = pstatus.tab$Permit.Status) # set permit status as factor so plot is arranged by n

pstatus.tab # display table

ggplot(pstatus.tab, aes(x=n, y=Permit.Status)) + # make plot of permit status
  geom_bar(stat = "identity") +
  theme_classic()

result.tab <- dt_inspect %>%  # make table counting permits by status
  group_by(Inspection.Result) %>%
  count() %>%
  filter(n > 100) %>% # only show values with greater than 100 entries
  arrange(n)
result.tab$Inspection.Result <- factor(result.tab$Inspection.Result, levels = result.tab$Inspection.Result)

ggplot(result.tab, aes(x=n, y=Inspection.Result)) + # make plot of permit status
  geom_bar(stat = "identity") +
  theme_classic()

### Part 2 - investigate geography

# N inspections by geography

census.tab <- dt_inspect %>%
  group_by(census_tract) %>%
  count() %>%
  arrange(n)
census.tab$census_tract <- factor(census.tab$census_tract, levels = census.tab$census_tract)

# 20 Most common census tracts
common_census <- census.tab[c(326:346),]$census_tract

ggplot(census.tab[c(326:346),], aes(x=n, y=census_tract)) + # make plot of permit status
  geom_bar(stat = "identity") +
  theme_classic()

# N inspections by geography by result

mat <- dt_inspect %>%
  filter(census_tract %in% common_census) %>%
  group_by(Inspection.Result, census_tract) %>%
  count() %>%
  reshape2::acast(Inspection.Result ~ census_tract,value.var = "n", fill = 0) %>%
  t()

mat <- mat/rowSums(mat)

Heatmap(mat, name = "proportion")

### Part 3 - model contractor location

dt_inspect %>%
  filter(contractor_city != "") %>%
  pull(contractor_city) %>%
  unique()

# Make dataset of violations by contractor location
dt_violate <- dt_inspect %>%
  filter(contractor_city != "") %>%
  mutate(violation = case_when(Inspection.Result %in% c("No Access for Inspection", "Corrections Issued",
                                                        "Not Ready for Inspection", "Order to Comply Issued") ~ 1,
                               .default = 0)) %>% # binarize inspection result - 1 for violation
  select(contractor_city, violation)

# Fit logistic regression model
logit_model <- glm(violation ~ contractor_city, # fit binarized inspection violation data to logistic regression model
                   family = binomial(link = "logit"), data = dt_violate)
res <- summary(logit_model)
res <- res$coefficients %>% as.data.frame() %>% arrange(`Pr(>|z|)`)
res
