# script to load and preprocess data
library(maptools)
library(ggplot2)
library(dplyr)
library(ggvis)


# load fish data
# load("../data/pike.RData")
# data <- pike
#
#
# load("../data/filled_data.Rdata")
# filled_data <- filled_data %>%
#   mutate( origin="interpolated" ) %>%
#   dplyr::select( ID, datetime, x, y, origin, group ) %>%
#   tbl_df()
#
#
# data <- data %>%
#   mutate( origin="original") %>%
#   select( ID, datetime, x, y, origin, group )
#
# data <- filled_data %>% bind_rows(data)
# groups <- data %>% group_by(ID) %>% summarise(group = first(group))
# data <- left_join(data %>% select(-group), groups) %>%
#   mutate(time = as.character(datetime))
#save(data,file="../data/shiny_data.Rdata")

load("../data/shiny_data.Rdata")
data <- data %>% mutate(ID = as.character(ID))


mintime = as.Date(min(data$datetime))
maxtime = as.Date(max(data$datetime))


groups <- as.character(unique(data$group))
all_ids <- as.character(unique(data$ID))
#names(all_ids) <- sapply(all_ids, FUN=substring, first=1, last=3)

# load coastline
coastline <- readShapeSpatial("../data/GIS_Dollnsee_dmarch/GISIGB/shps/coastline.shp")
coords <- coastline@lines[[1]]@Lines[[1]]@coords
lake <- data.frame( x=coords[,1], y=coords[,2])
