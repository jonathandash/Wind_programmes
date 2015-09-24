#-------------------------------------------------------------------------------
# Name:      LiDAR Processing for wind risk project
#
# Author:      DashJ
#
# Created:     12/11/2014
# Copyright:   (c) DashJ 2014
# Licence:     <your licence>
#-------------------------------------------------------------------------------

# import things for use
import os
import liblas
import numpy
from liblas import file
from liblas import header as lasheader
from liblas import vlr
import gdal
import shapely
import sys
import math

HOME_DIR="D:\working"

#Switches
ReferenceDTM == 0
DTMsClipData == 0
Clip == 1
MkCloudmetrics == 1
MkGridmetrics == 1




os.chdir(HOME_DIR)

#LiDAR Processing
os.chdir('D:\WindRisk\Ground')

if ReferenceDTM = 1:
    os.system('blast2dem -i *.laz -step 25 -o wind_10.asc -elevation_meter -merged -cores 4') #Generate a single nice picture dtm for reference

if DMTsClipData = 1:
    os.system('las2dem -i *.laz -step 1 -odir D:\WindRisk\Ground -odtm -cores 4') #Generate dtm format ground surfaces for use with FUSION CLipData.

os.chdir('D:\WindRisk\Non_Ground')
if Clip = 1:
    os.system('clipdata /dtm:*.dtm /height /shape:1 *.laz corners.csv') #Clip and normalise the lidar in the study area's field plots.

if MkCloudmetrics = 1:
    os.system('laszip -i *.las -olaz -odir D:\WindRisk\Clips')
    os.chdir('D:\WindRisk\Clips')
    os.system('cloudmetrics /new /above /minht:0 /outlier:0,80 /id /strata:[0.5,2.5,6,10,20,30,40,50,60] /intstrata:[0.5,2.5,6,10,20,30,40,50,60] *.laz Cloudmetrics.csv')

if MkGridmetricsrid = 1:
    os.chdir('D:\WindRisk\Non_Ground')
    os.system(ReturnDensity ReturnD.dtm 30 *.laz)
    os.system('gridmetrics /buffer:20 /minht:0 /outlier:0,80 /strata:[0.5,2.5,6,10,20,30,40,50,60] /intstrata:[0.5,2.5,6,10,20,30,40,50,60] *.dtm 2 30 gridmetics.csv *.laz')
    ReturnDensity ReturnD.dtm 30 *.laz







