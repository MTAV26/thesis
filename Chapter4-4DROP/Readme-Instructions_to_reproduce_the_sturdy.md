#  <p align="center"> **Predicciones probabilísticas de sequías meteorológicas basadas en condiciones iniciales múltiples: <a href="https://matv.shinyapps.io/app_4DROP/" target="_blank_">4DROP</a>**</p>

<p align="center"> Miguel Ángel Torres-Vázquez; University of Murcia (miguelangel.torres@um.es).</p>
<p align="center"> Juan Pedro Montávez Gómez; University of Murcia (montavez@um.es).</p>
<p align="center"> Marco Turco; University of Murcia  (mturco@um.es).</p>

# 
**A. General instructions** 


This project is designed to be executed with R scripts. 
Execute script files in the order they are listed.

Data sources:

The "observed" precipitation data as collected by DROP members are available in the data section of this repository "https://github.com/MTAV26/thesis/tree/main/Chapter4-4DROP/Data". However, it is important to note that the GPCP dataset, identified as "gpcp_cdr_v23rB1_1981_2021.nc," exceeds the maximum 25 MB limit allowed for downloads on this website. To access this data, we recommend downloading it through the following web link: https://psl.noaa.gov/data/gridded/data.gpcp.html, or alternatively, establishing direct contact with the responsible author.

The following reference shows the data applied as predictors of 4DROP. 

| Dataset  | Source |
| :------------ |:---------------|
| CAMS\_OPI | http://www.cpc.ncep.noaa.gov/products/global_precip/html/wpage.cams_opi.html       |
| CHIRPS  (v2)    | http://chg.geog.ucsb.edu/data/chirps/        |
| CPC | https://www.esrl.noaa.gov/psd/data/gridded/data.cpc.globalprecip.html        |
| ERA5 (Ensemble mean) | https://www.ecmwf.int/en/forecasts/datasets/archive-datasets/reanalysis-datasets/era5      |
| GPCC  | https://www.dwd.de/EN/ourservices/gpcc/gpcc.html      |
| GPCP (V2.3)      | http://eagle1.umd.edu/GPCP_ICDR/Data/ |
| JRA-55  | http://jra.kishou.go.jp/JRA-55/index_en.html      |
| MERRA2  | https://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis2.html      |
| MSWEP | https://www.gloh2o.org/mswep/       |
| NCEP  | https://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis2.html     |
| PREC/L | https://www.esrl.noaa.gov/psd/data/gridded/data.precl.html       |
<br/>
<br/>

The script files have been organized with the goal of facilitating reproducibility. They are first sorted alphabetically and then numerically. However, it's important to note that due to the absence of GPCP data, these scripts will not run unless you download the data or contact the corresponding author, as mentioned earlier. If you choose to download them from the website, you will need to adjust the resolution to 2.5°x2.5° and select only the period between 1981 and 2020. If you have any questions or wish to provide feedback to the authors, we encourage you to get in touch with Miguel Ángel Torres-Vázquez via the email address provided above.

# 
**B. Description of script files**
Scripts for data preparation and cleaning.

**Development**
- A1_Loop_load_SPI.R: Reads precipitation data and calculates SPI6 for the eleven members of DROP.
- A1_load_DROP.R: Calculates the mean, spread, probability of moderate drought, and alert level for DROP.
- B1_Forecast_ESP_loop.R: Calculates ESP predictions for December-January-February (DJF), March-April-May (MAM), June-July-August (JJA), September-October-November (SON).
- B2_load_ESP.R: Calculates the mean, spread, probability of moderate drought, and alert level for 4DROP.
- C1_Load_S5_raw_all_members.R: Combines all SEAS5 predictions.
- C2_Load_S5_raw_ens.R: Same as above (this was a test).
- C3_bias_adj_all_members.R: Adjusts bias in predictions for each of the DROP sets.
- C4_Verify_S5_pre.R: Verifies that there are no errors with bias and correlation.
- C5_Forecast_S5_loop.R: Calculates S5 predictions for DJF, MAM, JJA, SON.
- C6_load_SEAS5.R: Calculates the mean, spread, probability of moderate drought, and alert level for S5.

**Deterministic validation**
- D1_Verify_COR_ESP_loop.R: Calculates the correlation of 4DROP for DJF, MAM, JJA, SON.
- D2_Verify_COR_SEAS5_loop.R: Calculates the correlation of S5 for DJF, MAM, JJA, SON.
- D3_Verify_COR_ens_mean_mb_loop.R: Calculates the correlation of 4DROP and S5 members for DJF, MAM, JJA, SON.
- D4_Boxplot_COR_detrended.R: Represents spatial correlation without trend in boxplot for 4DROP and S5 for DJF, MAM, JJA, SON.
- D4_Boxplot_COR_original.R: Represents spatial correlation with trend in boxplot for 4DROP and S5 for DJF, MAM, JJA, SON.
- D5_PLOT_COR.R: Represents spatial correlation maps of ESP(4DROP) and S5 for DJF, MAM, JJA, SON.

**Probabilistic verification**
- E1_Verify_Reliability_loop.R: Calculates the reliability of predictions for 4DROP and S5 for DJF, MAM, JJA, SON and represents the attribute diagram plot.
- E2_Boxplot_reliability.R: Represents reliability of predictions in boxplot for 4DROP and S5 for DJF, MAM, JJA, SON.
- F1_Verify_ROC_loop.R: Calculates the skill of predictions for 4DROP and S5 for DJF, MAM, JJA, SON and represents the ROC plot.
- F2_Boxplot_ROC.R: Represents the ROC of predictions in boxplot for 4DROP and S5 for DJF, MAM, JJA, SON.
- G1_BS.R: Calculates the Brier Score of predictions for 4DROP and S5 for DJF, MAM, JJA, SON.
- G2_BS_mb.R: Calculates the Brier Score of predictions for the members of 4DROP and S5 for DJF, MAM, JJA, SON.
- G3_BS_plot.R: Represents the BS maps of 4DROP and S5 for DJF, MAM, JJA, SON.

**Study case**
- H1_Caso_Estudio.R: Represents the maps of SPI6 mean, spread, probability of moderate drought, and alert level for DROP, 4DROP, and S5.
- H2_Caso_Estudio_Boxplot.R: Represents boxplots of SPI6 mean for DROP, 4DROP, and S5. In a grid over South America, with a lead time of two months for the entire time series 1981-2020 in JJA. Location: longitude = -63.75, latitude = -33.75.

- 
In the "cammon" folder, you will find additional script files that are useful for the execution of the main script files described above. This folder corresponds to the .R files for DROP (Turco et al., 2020).

**References**
Turco, M., Jerez, S., Donat, M. G., Toreti, A., Vicente-Serrano, S. M., & Doblas-Reyes, F. J. (2020). A global probabilistic dataset for monitoring meteorological droughts. Bulletin of the American Meteorological Society, 101(10), E1628-E1644.
