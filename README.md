### Puget Sound Crab Recreational Harvest Estimation 

This repository includes the code and data used to generate harvest estimates based on reported CRCs. There are two separate functions, one for summer estimates and another for winter estimates. All the data and inputs needed to run the functions are provided under the **CRC Data** folder. 
We have also supplied all the data and analysis used to produce the partitioning catch amoung Marine Areas to better align with the quota management areas used for harvest management. The partitioning survey data was used to produce the 2024 harvest estimates. These files can be found under the the **Partitioning Survey** folder. 


Currently, this repo also contains the data from the Puget Sound Dungnesess recreational crab creel project along with the associated metadata. This information can be found under the **Creel Data** folder.



**UPDATE**
11/15/2024 - An updated script was uploaded that incorporates data aggregation changes based on recommendations from the Joint State Tribal Technical Workging group ("FUNCTION_SW_CRC_Estimate - TWG Update.R"). An additional folder is also now included that contains parameters and templates that are read in to complete the new aggregation proceedures ("CRC Data//CRC Estimate Function Inputs"). 

**UPDATE**
11/22/2024 - An updated script was uploaded with revised data aggregation processes based on recommendations from the Joint State Tribal Technical Workging group ("FUNCTION_SW_CRC_Estimate - TWG UpdateV3.R"). An new seasons/areas template was also provided ("CRC Data//CRC Estimate Function Inputs"). 

**UPDATE**
4/22/2024 - An updated script was uploaded that is used to generate recreational catch estimates from raw CRC data and data from the 2023 Responsive Management survey to estimate the catch of non-respondents following recommendations from the Joint State Tribal Technical Working group ("FUNCTION_SW_CRC_Estimate - TWG Update_V4_nobias.R"). This updated code has removed the previously used non-response bias correction model. Associated parameter files needed to excetcute the calculations were also updated. 
