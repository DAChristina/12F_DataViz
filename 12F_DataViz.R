library(tidyverse)
library(readxl)
library(epitools)

wd = "C:/Users/dac23/Downloads"
setwd(wd)

Strep12F <- read_excel("12F_international_incidence_v2.xlsx", col_names = T)
Vaccine <- read_excel("12F_international_incidence_v2.xlsx", sheet = "Vaccine", col_names = T)
# view(Strep12F)
glimpse(Strep12F)
unique(sort(Strep12F$Region))
unique(sort(Strep12F$Area))
unique(sort(Strep12F$Period))
unique(sort(Strep12F$Demographic))
unique(sort(Strep12F$Reference))

# Incidence components:
unique(sort(Strep12F$Count))
unique(sort(Strep12F$Total))

# Vaccines:
glimpse(Vaccine)
unique(sort(Vaccine$Vaccine))


# Things to consider before incidence analysis: ################################
# 1. Have to check why some data in <Total> have zero value.
# 2. <Region> & <Area> are fine
# 3. <Period> & <Demographic> is quite messy, need to cross-check the reference.

# Some arguments and grumblings ################################################
# Idk. I think it's impossible to group the data considering the various Perd & Demo value?
ZeroTot <- Strep12F %>% 
  filter(Total == 0) %>% 
  # view() %>% 
  glimpse()
# That's it. The source is located in Hong Kong (2017 & 2019).
# I checked the Ref again and the data's fine & make sense.

IntervalPerd <- Strep12F %>% 
  filter(grepl("-", Period)) %>% 
  view() %>% 
  glimpse()
# The data that have a messy interval Perd also have messy Age structures:
unique(sort(IntervalPerd$Region))
# [1] "Belgium" "Germany" "Israel"  "Japan"   "Kenya"   "Morocco" "USA"

# Q: Is the Perd-filtered messy data specific to these Region? (manually filter the data)
# If yes, the analysis should be done in country-specific approach.
# Belgium = y (1 Ref)
# Germany = n (3 Refs)
# Israel  = y
# Japan   = n (2 Refs)
# Kenya   = y           --> huge time range (1999-2010 AND 2012-2016)
# Morocco = y           --> in range of 4 OR 5 years (inconsistent)
# USA     = y           --> freaking weird data with age-grouping = "All" -_-)"

unique(sort(IntervalPerd$Demographic))
unique(IntervalPerd$Reference) # ugh

# Decision:
# 1. DataViz as simple as (Period, Incidence, col = ALL_Ages, Children, Adult, Elderly) col according to WHO
# 2. Data is separated based on visualisation:
#   2.1. Worldwide
#   2.2. Area
#   2.3. Region --> Region-based DataViz can be vary based on the original data (Germany & Japan are special cases)
# 3. Keypoints for DataViz components:
#   3.1. Period with intervals (e.g. 2000-2004) will use midpoints instead (e.g. 2002)
#   3.2. Incidence components (Count, Total) will be summed based on the group
#   3.3. Special to Hong Kong: Total with value == 0 will be ommitteed (for CI calculations)
#   3.3. Confidence Intervals then calculated for each dataframe
#   3.4. Vaccine data as geom_vline OR abline(v = PeriodValue)

# The said functions are FUN:
FunYearMid <- function(eyy, delimit = "-") {
  range <- strsplit(eyy, delimit)[[1]]
  
  if (length(range ==2)) {
    numb <- as.numeric(range)
    midp <- mean(numb, na.rm = T)
    
  } else {
    midp <- as.numeric(eyy)
  }
  
  return(midp)
}


# 2. DataViz ###################################################################
# 2.1. WorldWide
Strep12F_1ww_ALLages <- Strep12F %>% 
  # filter(Total != 0) %>% # Only required when we group the data specifically
  mutate(New_Period = sapply(Period, FUN = FunYearMid)) %>% 
  group_by(New_Period) %>% 
  summarise(sum_Count = sum(Count),
            sum_Total = sum(Total)) %>%
  ungroup() %>% 
  mutate(Conf_Int = binom.exact(sum_Count, sum_Total)) %>% 
  view() %>% 
  glimpse()

# <TOBECONTINUED>
Strep12F_1ww_GRages <- Strep12F %>% 
  # filter(Total != 0) %>% # Only required when we group the data specifically
  mutate(New_Period = sapply(Period, FUN = FunYearMid),
         New_Demographic = "TOBECONTINUED") %>% 
  group_by(New_Period) %>% 
  summarise(sum_Count = sum(Count),
            sum_Total = sum(Total)) %>%
  ungroup() %>% 
  mutate(Conf_Int = binom.exact(sum_Count, sum_Total)) %>% 
  view() %>% 
  glimpse()

max_up <- max(Strep12F_1ww_ALLages$Conf_Int$upper)+.01
plot(Strep12F_1ww_ALLages$New_Period, Strep12F_1ww_ALLages$Conf_Int$proportion,
     ylim = c(0, max_up),
     main = "The Incidence of 12F from Publictly-Available Data Worldwide")
segments(Strep12F_1ww_ALLages$New_Period, Strep12F_1ww_ALLages$Conf_Int$lower,
         Strep12F_1ww_ALLages$New_Period, Strep12F_1ww_ALLages$Conf_Int$upper, col = "black")



# 2.2. Facet-wrap by <Area>
# <tobecontinued>
Strep12F_2Area_ALLages <- Strep12F %>% 
  # filter(Total != 0) %>% # Only required when we group the data specifically
  mutate(New_Period = sapply(Period, FUN = FunYearMid)) %>% 
  group_by(Area, New_Period) %>% 
  summarise(sum_Count = sum(Count),
            sum_Total = sum(Total)) %>%
  ungroup() %>% 
  mutate(Conf_Int = binom.exact(sum_Count, sum_Total)) %>% 
  view() %>% 
  glimpse()

ggplot(Strep12F_2Area_ALLages, aes(x = New_Period, y = Conf_Int$proportion)) +
  geom_point() +
  geom_errorbar(aes(ymin = Conf_Int$lower, ymax = Conf_Int$upper),
                width = .1, color = "black") +
  ggtitle("The Incidence of 12F Grouped by Area") +
  facet_wrap(~ Area)


# 2.3. Facet-wrap by <Region>
Strep12F_3Region_ALLages <- Strep12F %>% 
  # filter(Total != 0) %>% # Only required when we group the data specifically
  mutate(New_Period = sapply(Period, FUN = FunYearMid)) %>% 
  group_by(Area, New_Period, Region) %>% 
  summarise(sum_Count = sum(Count),
            sum_Total = sum(Total)) %>%
  ungroup() %>% 
  mutate(Conf_Int = binom.exact(sum_Count, sum_Total)) %>% 
  view() %>% 
  glimpse()

unique(sort(Vaccine$Vaccine))
Vaccine$Vaccine <- factor(Vaccine$Vaccine,
                          levels = c("PCV7", "PCV10", "PCV13", "PCV10 & PCV13")) # coz of that weird automatic levels

ggplot(Strep12F_3Region_ALLages, aes(x = New_Period, y = Conf_Int$proportion)) +
  geom_point() +
  geom_errorbar(aes(ymin = Conf_Int$lower, ymax = Conf_Int$upper),
                width = .1, color = "black") +
  geom_vline(data = Vaccine, aes(xintercept = Period, colour = Vaccine), linetype = "dashed") +
  scale_color_manual(values = c("PCV7" = "red", "PCV10" = "green", "PCV13" = "blue", "PCV10 & PCV13" = "purple")) +
  ggtitle("The Incidence of 12F Specific to Regions") +
  facet_wrap(~ Region)



################################################################################
