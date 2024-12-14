*Date of study*  
Enter when study happened here (e.g., January-June, 2015)

*Date of Public Archiving:*  
Enter when data should be (or was) publicly-archived following lab data management plan.  
*Last Modified:*  

06 Dec 2024

*Goal*

Investigate how extended growing seasons influence tree's capacity to grow to following year. And also get a Master's degree!

*Contributors*

Lizzie

Frederik Baumgarten

Xiaomao Wang

Deirdre Loughnan

Ken 

Justin

Britany Wu

Selena

Mark

Julie Sieg

*General Files*

| **File**                    | **Where**                                                                                                                                             | **What**                    |
| --------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------- |
| Explain what is in the file | Give a backed-up place where the data are (e.g., Wolkovich Lab Google Drive, WeldShare, Odyssey lab folder; github is okay). Link to item if possible | Explain what is in the file |

*Data and Code*

Give info on how to track down all locations given in table below (even if link fails). Two good examples given below -- delete these for your file!

**Github** [https://github.com/lizzieinvancouver/buds/tree/](https://github.com/lizzieinvancouver/buds/tree/master/analyses/data)  
**W: drive** Weldshare > Wolkovich Lab > Budburst Experiment 2015

| **File**                        | **Where**                                                                                                                       | **What**                                                                                                                                                                           |
| ------------------------------- | ------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| hoboLight (folder)              | /fuelinex/data/hoboLight/                                                                                                       | Hobo loggers that were set up at totem field and in the growth chambers                                                                                                            |
| hoboTemp (folder)               | /fuelinex/data/hoboTemp/                                                                                                        | Hobo loggers that were set up at totem field and in the growth chambers                                                                                                            |
| 2024MiscellaneousMonitoring.csv | [Github](https://github.com/christophe-rd/fuelinex/tree/07ce6a790eb73b4769eb29ebcd92ede950735c65/data/miscellaneous_monitoring) | miscellaneous monitoring spreadsheet. This is a condensed version of the different problems trees have had so far in 2024. E.g. dead apical shoot, chlorosis, light stress, mildew |
| 2024BudburstToBudset.csv        | [Github](https://github.com/christophe-rd/fuelinex/tree/07ce6a790eb73b4769eb29ebcd92ede950735c65/data/monitoring_phenology)     | Bi-weekly, weekly monitoring of phenological events for 2024 from budburst, to leaf unfolding to budset.                                                                           |
| 2024Senescence.csv              | [Github](https://github.com/christophe-rd/fuelinex/tree/07ce6a790eb73b4769eb29ebcd92ede950735c65/data/monitoring_phenology)     |                                                                                                                                                                                    |
| TotemField_30Years.csv          | [Github](https://github.com/christophe-rd/fuelinex/tree/71c1dd92573e9d1b47a0c1c4bd5273476025bbb1/data/totemFieldClimateData)    |                                                                                                                                                                                    |
| Jun2024TreeInventory.csv        | [Github](https://github.com/christophe-rd/fuelinex/tree/71c1dd92573e9d1b47a0c1c4bd5273476025bbb1/data/treeInventory)            | inventory of leftover trees next to the greenhouse                                                                                                                                 |
| TreeMeasurements.xlsx           | [Github](https://github.com/christophe-rd/fuelinex/tree/71c1dd92573e9d1b47a0c1c4bd5273476025bbb1/data/treeMeasurements)         | Diameter and height measurements                                                                                                                                                   |

| Cleaned file                  | Where                                                                                                                                   | What                                                                                         |
| ----------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| TotemField_30YearsCleaned.csv | [Github](https://github.com/christophe-rd/fuelinex/tree/71c1dd92573e9d1b47a0c1c4bd5273476025bbb1/analyses/output/totemFieldClimateData) | Cleaned version of climate data from Totem Field climate station. Needs to be moved to data. |
|                               |                                                                                                                                         |                                                                                              |

### Code

| Code           |                                                                                                                                 | What                                                     |
| -------------- | ------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------- |
| hobotemp_cc.R  | [Github](https://github.com/christophe-rd/fuelinex/blob/71c1dd92573e9d1b47a0c1c4bd5273476025bbb1/analyses/rcode/hobolight_cc.R) | Cleaning data and organizing temp data from hobo loggers |
| hobolight_cc.R | [Github](https://github.com/christophe-rd/fuelinex/tree/71c1dd92573e9d1b47a0c1c4bd5273476025bbb1/analyses/rcode)                | Cleaning and organizing light data from hobo loggers     |
|                |                                                                                                                                 |                                                          |
|                |                                                                                                                                 |                                                          |
|                |                                                                                                                                 |                                                          |



**Possible Extras:**

Any amendments to when public archiving happen should mentioned here and an asterisk given above where archiving date is given.

Be sure all your data is somewhere where it is backed up as per the data management plan.

Check this file for accuracy, and update as needed, every 6 months or sooner.

##### Questions:

1 entry for every single files, even if dozens of them?

should we use md or txt for metadata
