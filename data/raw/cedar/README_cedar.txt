READ ME for Cedar Lake data

All files in this folder are raw data files taken directly from the instruments in the field. Therefore, 
they include data that is not useful for the study of the lake, including air temperatures and oxygen before 
and after the sensor is placed or removed from the water. These data are cleaned using a combination of field 
notes that indicate when the instrument was removed or placed into the water. The cleaned files are present in the
"cleaned" data folder, which was used for data anlaysis and publication figures etc. 

Data file names are in the form "lakeName_instrumentType_depthMeasurement_dateCollected_fileStatus.csv"
lakeName = name of lake
instrumentType = instrument used to collect data, where "DOT" stands for a PME miniDOT clear logger, "hobo" stands for a HOBO U22 temperature logger, and "light_pendant" stands for a HOBO light pendant temperature and light logger
depthMeasurement = the depth that the sensor was placed, where "sediment" is a logger buried in the sediment attached to the anchor holding the thermistor chain, "1m" is 1m from the sediment, "2m" is 2m from the sediment, and the "light_pendant" was placed approximately 1m from the surface. 
dateCollected = the date that the data was retrieved from the instrument. The date is in the form YEAR.MONTH.DAY. Zeroes in month and day are used to ensure dates remain in consecutive order. This date will include data from the previous deployment to the retrieval date.
fileStatus = either raw or cleaned as described above. Only "cleaned" data were used in analysis.  
csv = file type

Note regarding miniDOT data from 10.10.2020 through 06.11.2021:
MiniDOT left at 1 minute intervals rather than 10 minute intervals when deployed. One minute intervals were
used during calibration and are normally reverted to 10 minutes for the field deployment, but were inadvertently
left at 1 min. Cleaned data were averaged to 1 hour intervals for analysis as with other data.

Note regarding light pendant from 10.10.2020 through 6.11.2021:
No data on light pendant. Appears to be a launching issue. May need to replace.

Update on light pendant 10.14.2021: Issues continued with light pendant. Needed replacement batteries. Also placed two additional pendants on a separate anchor in November of 2021. 