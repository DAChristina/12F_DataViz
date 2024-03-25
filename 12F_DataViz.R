library(tidyverse)
library(readxl)

wd = "C:/Users/dac23/Downloads"
setwd(wd)

Strep12F <- read_excel("12F_international_incidence_v2.xlsx", col_names = T)
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


# Things to consider before incidence analysis: ################################
# 1. Have to check why some data in <Total> have zero value.
# 2. <Region> & <Area> are fine
# 3. <Period> & <Demographic> is quite messy, need to cross-check the reference.

# See the structure of the data first:
ViewAll <- Strep12F %>% 
  group_by(Area, Region, Period, Demographic) %>% 
  summarise(n_Demo = n()) %>% 
  view()

unique(sort(ViewAll$n_Demo))

# Idk. I think it's impossible to group the data by <Area> considering the various Perd & Demo value.



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
unique(IntervalPerd$Reference)

# mutate(0 in Total to -1)



################################################################################