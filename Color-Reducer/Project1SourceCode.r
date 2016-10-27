## Authors

## Saurabh Gandhele (smg6512@rit.edu)
## Surabhi Marathe  (srm6226@rit.edu)
## Saurabh Wani     (saw4058@rit.edu)

## This program will read the source image and apply K-means clustering to replace the centers
## by the target colors assigned by the user.

## One time install commands for installing the R library packages 'jpeg and 'grid'  
#install.packages("jpeg")
#install.packages("grid")

# Use both the libraries 
library('jpeg')
library('grid')

# read the jpg image using function and store it in img
originalImage <- readJPEG("Trombone.jpg")

# create a copy of the img
transformedImage <- originalImage

# find out the dimensions of the given image and a summary
dimension <- dim(originalImage)
dimension <- dimension[1] * dimension[2]
dim(originalImage)
summary(originalImage)

# create a dataframe to convert the 3 dimensional RGB image into a 2 dimensional matrix
dataFrame <- data.frame(c(originalImage[,,1]), c(originalImage[,,2]), c(originalImage[,,3]))
dim(dataFrame)
summary(dataFrame)

# multiply the pixel values by 256 to get its RGB color 
dataFrame <- dataFrame * 256
summary(dataFrame)

# apply Kmeans clustering with no of centers = 5 so that we can map the 5 centers with our 5 target colors 
imageCluster = kmeans(dataFrame, 5)

# view the center of the clusters generated
imageCluster$centers

# convert this centers into a 2 D matrix
centers <- matrix(imageCluster$centers, nrow=5, ncol=3)

# specify the RGB values of our 5 target colors in a matrix. target color is given by c(i,i+5,i+10) value where 1 <= i <= 5
# first, write the 5 values for R color (i)
# second, write the 5 values for G color (i+5)
# third, write the 5 values for B color (i+10)
userColors <- matrix(c(255, 0, 43, 196, 113, 255, 0, 202, 148, 17, 255, 0, 133, 62, 214), nrow=5, ncol=3)

# create a copy of the userColors and the centers matrices
targetColors <- userColors
clusterCenters <-centers

# initialization of variables for storing and calculating intermediate results
count=1
index <- c(0, 0, 0, 0, 0)
temp <- c(999, 999, 999, 999, 999)

# iterate over the cluster to replace the centers with the user defined colors
# replace the centers by calculating the minimum distance between cluster centers and target colors 
 
for(i in 1:nrow(centers))
{
	for(j in 1:nrow(userColors))
	{
		if(j %in% index)
		{
		}
		else 
		{
			temp[j] = sqrt((userColors[i,1] - centers[j,1])^2 + (userColors[i,2] - centers[j,2])^2 + (userColors[i,3] - centers[j,3])^2)
		}
	}
	index[count] <- which.min(temp)
	temp <- c(999, 999, 999, 999, 999)
	clusterCenters[index[count],] <- targetColors[i,]
	count = count + 1
}

# convert the centers of cluster into a matrix with the original dimensions of image
originalCenters <- imageCluster$cluster
originalCenters <- matrix(imageCluster$cluster, nrow=dimension, ncol=3)
modifiedImgMat <- matrix(0, nrow=dimension, ncol=3)

# add the target color value to the original image pixel through the cluster centers
for(j in 1:dimension)
 {
	modifiedImgMat[j,1] <- clusterCenters[originalCenters[j],1]
	modifiedImgMat[j,2] <- clusterCenters[originalCenters[j],2]
	modifiedImgMat[j,3] <- clusterCenters[originalCenters[j],3]
 }

# check if the target color value changes are reflected in the modifiedImgMat
summary(modifiedImgMat)
 
# divide the target colors RGB values in modified matrix by 256 to get the original image colors in range 0:1 
modifiedImgMat <- modifiedImgMat / 256

# assign the new target colors to the original image in seperate RGB dimensions
transformedImage[,,1] <- modifiedImgMat[,1]
transformedImage[,,2] <- modifiedImgMat[,2]
transformedImage[,,3] <- modifiedImgMat[,3]

# display the transformed image with grid function 
grid.raster(transformedImage)

 
