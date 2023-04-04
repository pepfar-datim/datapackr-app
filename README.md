# datapackr-app

**Repo Owner:** Scott Jackson [@jacksonsj](https://github.com/jacksonsj)


This repo hosts the code for the R Shiny front-end app for datapackr, allowing users to upload and validate Data Packs as well as visualize their targets.

**User guide**

***Login page** 

Login with your DATIM username and password. Your credentials will be securely transmitted to the app server, and you will be logged into DATIM. This process allows the app to fetch metadata and data from the server using your own permissions. No modifications or changes to your account or data which you may have permission to alter will be made. 
**Validating your DataPack**

Once you have successfully logged in, you can proceed to upload a copy of your DataPack. Currently only COP22 DataPacks, COP22 OPU DataPacks, COP23 Target Setting Tools and COP23 PSNUxIM Tools are supported by the app. Be sure that you upload an XLSX version of your DataPack. The app does not support XLSB formatted workbooks. If you are using an XLSB file, be sure to save it as an XLSX, and use this file for validation purposes. 

Once you have uploaded your file, the "Validate" button should become active. Press the button to begin the validation process. Depending on the size of your Excel file, this process may take several minutes.  

For further details, please consult the Target Setting Tool User Guide. 
