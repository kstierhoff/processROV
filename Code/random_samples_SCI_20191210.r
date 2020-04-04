# code for selecting random sampling blocks during the San Clemente Island white abalone survey
setwd("C:/Users/kevin.stierhoff/Desktop/SCI Survey/SCI_2019_R")
#read data from text files
sample.list <- read.csv("samples.csv")
blocks <- read.csv("fishnet_overlap_2016.csv")
# create a key in blocks
blocks$FID <- seq(0,dim(blocks)[1] - 1,1)
blocks$key <- as.factor(paste(blocks$patch_id,blocks$z_stratum))
#subset data to include only blocks within the sampling areas and depth strata
blocks <- droplevels(subset(blocks,blocks$key %in% sample.list$key))
sample.list <- droplevels(subset(sample.list,sample.list$key %in% blocks$key))

#initialize variables
FID <- numeric()
key <- factor()
patch_id <- FID
depth <- key

# i = sample.list$key[11]
#randomly sample blocks within each habitat patch/depth
for (i in sample.list$key) {
	#subset block data to include blocks from each strata
	block.temp <- droplevels(subset(blocks,blocks$key %in% i))
	#determine how many samples are needed
	no.samples <- sample.list$samples[which(sample.list$key %in% i)]
	#select random blocks
	FID.temp <- sample(block.temp$FID,no.samples)
	#append to earlier data
	FID <- c(FID,FID.temp)
}
#create data frame to merge with blocks data
FID <- data.frame(FID)

#merge two data frames
results <- merge(FID, blocks, all.x = TRUE)

# remove key column
results$key <- NULL

#write results to text file for use in GIS
write.csv(results, file = "random_blocks.csv",row.names = FALSE)

# Read sampling polygon layer
fishnet <- st_read("E:/GIS/gis_data/rov_surveys/sci_abalone/fishnet_100m_overlap.shp") %>% 
  filter(z_stratum %in% c("30-40m","40-50m","50-60m"))

fishnet.sub <- fishnet[results$FID, ]

st_write(fishnet.sub, "E:/GIS/gis_data/rov_surveys/sci_abalone/random_blocks-2019.shp", 
         delete_layer = TRUE)
