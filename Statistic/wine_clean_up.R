require(tidyverse)
require(reshape2)

wine <- read.delim("wine_data.txt", header = T)
wine$wine <- factor(wine$wine)

# split table by variable
color     <- wine %>% filter(variable == "color")     %>% select(c(wine, value, name)) 
legs      <- wine %>% filter(variable == "legs")      %>% select(c(wine, value, name))
nose      <- wine %>% filter(variable == "nose")      %>% select(c(wine, value, name))
mouthfeel <- wine %>% filter(variable == "mouthfeel") %>% select(c(wine, value, name))
taste     <- wine %>% filter(variable == "taste")     %>% select(c(wine, value, name))

# create vector of unique values for each variable
color.unique <- color %>% {unique(color$value)} %>% as.vector() %>% sort()
legs.unique <- legs %>% {unique(legs$value)} %>% as.vector() %>% sort()
nose.unique <- nose %>% {unique(nose$value)} %>% as.vector() %>% sort()
mouthfeel.unique <- mouthfeel %>% {unique(mouthfeel$value)} %>% as.vector() %>% sort()
taste.unique <- taste %>% {unique(taste$value)} %>% as.vector() %>% sort()

# replace values as needed
wine[wine$value == "thin", "value"] <- "skinny"
wine[wine$value == "berrys", "value"] <- "berry"
wine[wine$value == "casis", "value"] <- "cassis"
wine[wine$value == c("catspee", "cat'spee"), "value"] <- "catpee"
wine[wine$value == "cheesey", "value"] <- "cheese"
wine[wine$value == "cistrusblossom", "value"] <- "citrusblossom"
wine[wine$value == c("grape", "grappy", "grapy"), "value"] <- "grapey"
wine[wine$value == c("juiciy", "jucy"), "value"] <- "juicy"
wine[wine$value == "quinche", "value"] <- "quince"
wine[wine$value == "redfruit", "value"] <- "redfruits"
wine[wine$value == c("sweettabaco", "sweettobaco"), "value"] <- "sweettobacco"
wine[wine$value == "tropical", "value"] <- "tropicalfruit"
wine[wine$value == "whiteflower", "value"] <- "whiteflowers"
wine[wine$value == "acidity", "value"] <- "acidic"
wine[wine$value == "agressive", "value"] <- "aggressive"
wine[wine$value == "astingent", "value"] <- "astringent"
wine[wine$value == "dilicate", "value"] <- "delicate"
wine[wine$value == "powefull", "value"] <- "powerful"
wine[wine$value == "velvet", "value"] <- "velvety"
wine[wine$value == "plastics", "value"] <- "plastic"
wine[wine$value == "thin", "value"] <- "skinny"
wine[wine$value == "stammy", "value"] <- "stemmy"
wine[wine$value == c("flower", "flowery"), "value"] <- "flowers"

# Check that is has worked
color     <- wine %>% filter(variable == "color")     %>% select(c(wine, value, name)) 
legs      <- wine %>% filter(variable == "legs")      %>% select(c(wine, value, name))
nose      <- wine %>% filter(variable == "nose")      %>% select(c(wine, value, name))
mouthfeel <- wine %>% filter(variable == "mouthfeel") %>% select(c(wine, value, name))
taste     <- wine %>% filter(variable == "taste")     %>% select(c(wine, value, name))

color.unique     <- color     %>% {unique(.$value)} %>% as.vector() %>% sort()
legs.unique      <- legs      %>% {unique(.$value)} %>% as.vector() %>% sort()
nose.unique      <- nose      %>% {unique(.$value)} %>% as.vector() %>% sort()
mouthfeel.unique <- mouthfeel %>% {unique(.$value)} %>% as.vector() %>% sort()
taste.unique     <- taste     %>% {unique(.$value)} %>% as.vector() %>% sort()

# the baseR way
wine.mouthfeel <- wine %>% filter(variable == "mouthfeel") %>%
  #filter(name == "dennis") %>%
  dcast(data = ., formula = wine ~ value, length)
write_delim(wine.mouthfeel, "mouthfeel.txt", delim = "\t")

wine.color <- wine %>% filter(variable == "color") %>%
  #filter(name == "dennis") %>%
  dcast(data = ., formula = wine ~ value, length)
write_delim(wine.color, "color.txt", delim = "\t")

wine.legs <- wine %>% filter(variable == "legs") %>%
  #filter(name == "dennis") %>%
  dcast(data = ., formula = wine ~ value, length)
write_delim(wine.legs, "legs.txt", delim = "\t")

wine.nose <- wine %>% filter(variable == "nose") %>%
  #filter(name == "dennis") %>%
  dcast(data = ., formula = wine ~ value, length)
write_delim(wine.nose, "nose.txt", delim = "\t")

wine.taste <- wine %>% filter(variable == "taste") %>%
  #filter(name == "dennis") %>%
  dcast(data = ., formula = wine ~ value, length)
write_delim(wine.taste, "taste.txt", delim = "\t")