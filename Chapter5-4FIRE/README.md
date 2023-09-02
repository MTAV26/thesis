#  <p align="center"> **Predicciones estacionales probabilísticas de la actividad global de los incendios basadas en observaciones climáticas iniciales múltiples: <a href="https://matv.shinyapps.io/app_4FIRE/" target="_blank_">4FIRE</a>**</p>

<p align="center"> Miguel Ángel Torres-Vázquez; University of Murcia (miguelangel.torres@um.es).</p>
<p align="center"> Juan Pedro Montávez Gómez; University of Murcia (montavez@um.es).</p>
<p align="center"> Marco Turco; University of Murcia  (mturco@um.es).</p>

# 
**A. General instructions** 


This project is designed to be executed with R scripts. 
Execute script files in the order they are listed.

Data sources:

The "observed" precipitation data as collected by DROP members are available in the data section of this repository "https://github.com/MTAV26/thesis/tree/main/Chapter4-4FIRE/Data". However, it is important to note that the GPCP dataset, identified as "gpcp_cdr_v23rB1_1981_2021.nc," exceeds the maximum 25 MB limit allowed for downloads on this website. To access this data, we recommend downloading it through the following web link: https://psl.noaa.gov/data/gridded/data.gpcp.html, or alternatively, establishing direct contact with the responsible author.

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


In the case of the SEAS5-related data, they are available through the Copernicus Climate Data Store. To obtain this data, access can be gained through the following web link: [https://cds.climate.copernicus.eu/cdsapp#!/search?type=dataset](https://cds.climate.copernicus.eu/cdsapp#!/search?type=dataset).  If you have any questions or wish to provide feedback to the authors, we encourage you to get in touch with Miguel Ángel Torres-Vázquez via the email address provided above.


 
# **B. Description of script files**

Scripts for data preparation and cleaning.

**Development**

- A1_Loop_load_SPI.R: Reads precipitation data and calculates SPI6 for the eleven members of DROP.
- A2_Load_SPI_DROP.R: Calculates the mean for DROP.
- B1_model_BA_spi_spi_seasons_ALL.R: Calculates the climate - burned area (CLIBA) model for the eleven datasets and for DROP (CLIBA-DROP).
- image_mask.R: A function to obtain the mask (B1).

- C1_Load_S5_raw_all_members.R: Reads raw SEAS5 data.
- C2_bias_correction_all_members.R: Calculates bias correction for SEAS5.
- C3_Forecast_SPI3_S5_loop_all.R: Calculates SPI3 predictions.
- C4_Forecast_SPI6_S5_loop_all.R: Calculates SPI6 predictions.
- C5_Forecast_SPI12_S5_loop_all.R: Calculates SPI12 predictions.
- C6_Verify_cor_Forecast_S5_all.R: Calculates the correlation of S5 predictions with DROP.

- D1_forecast_BA_ens_DJF.R: Calculates the climate - burned area (CLIBA) model for S5 predictions of the eleven DROP datasets and for the ensemble mean (CLIBA-4FIRE) in January-February-March (DJF).
- D2_forecast_BA_ens_JJA.R: Calculates the climate - burned area (CLIBA) model for S5 predictions of the eleven DROP datasets and for the ensemble mean (CLIBA-4FIRE) in June-July-August (JJA).
- D3_forecast_BA_ens_MAM.R: Calculates the climate - burned area (CLIBA) model for S5 predictions of the eleven DROP datasets and for the ensemble mean (CLIBA-4FIRE) in March-April-May (MAM).
- D4_forecast_BA_ens_SON.R: Calculates the climate - burned area (CLIBA) model for S5 predictions of the eleven DROP datasets and for the ensemble mean (CLIBA-4FIRE) in September-October-November (SON).

**plot**

- F2_Plot_COR.R: Correlation between SPI predictions with MSWEP(S5) and the SPI of DROP for various lead times (1 to +4) in JJA and different accumulated time intervals (3, 6, and 12 months) during the period 2000-2020.
- F3_plot_ROC_DROP.R: ROC area of BA predictions for the CLIBA-DROP model.
- F4_plot_ROC_4FIRE.R: ROC area of burned area (BA) predictions for the CLIBA-4FIRE model.
- F5_plot_matrix.R: Skill area percentage of the global burnable area provided by the CLIBA model with observations and seasonal predictions.
- F6_Boxplot_DROP.R: Boxplots for different statistical skill methods of the CLIBA model using observations.
- F6_Boxplot_4FIRE.R: Boxplots for different statistical skill methods of the CLIBA model using seasonal predictions.
- F7_8_Plot_model_BA.R: Spatial distribution of parameters involved in the CLIBA model for one member of DROP (MSWEP), considering the four seasons and for coincident (CC) or antecedent (AC) climate variables.
- Sup_A_plot_roc_drop_robin_all.R: ROC area of burned area predictions for the CLIBA model for the eleven observations of DROP and the CLIBA-DROP model.
- Sup_B_plot_roc_4fire_robin_all.R: ROC area of burned area predictions for the CLIBA model for the eleven DROP datasets with S5 predictions and the CLIBA-4FIRE model.
- Sup_C_plot_BS_DROP_robin_all.R: Brier Score of burned area predictions for the CLIBA model for the eleven observations of DROP and the CLIBA-DROP model.
- Sup_D_plot_BS_4FIRE_robin_all.R: Brier Score of burned area predictions for the CLIBA model for the eleven DROP datasets with S5 predictions and the CLIBA-4FIRE model.
- Sup_E_plot_BS_REL_DROP_robin_all.R: Reliability Brier Score of burned area predictions for the CLIBA model for the eleven observations of DROP and the CLIBA-DROP model.
- Sup_F_plot_BS_REL_4FIRE_robin_all.R: Reliability Brier Score of burned area predictions for the CLIBA model for the eleven DROP datasets with S5 predictions and the CLIBA-4FIRE model.
- Sup_G_plot_BS_RES_DROP_robin_all.R: Resolution Brier Score of burned area predictions for the CLIBA model for the eleven observations of DROP and the CLIBA-DROP model.
- Sup_H_plot_BS_RES_4FIRE_robin_all.R: Resolution Brier Score of burned area predictions for the CLIBA model for the eleven DROP datasets with S5 predictions and the CLIBA-4FIRE model.



In the "cammon" folder, you will find additional script files that are useful for the execution of the main script files described above. This folder corresponds to the .R files for DROP (Turco et al., 2020).

**References**
Turco, M., Jerez, S., Donat, M. G., Toreti, A., Vicente-Serrano, S. M., & Doblas-Reyes, F. J. (2020). A global probabilistic dataset for monitoring meteorological droughts. Bulletin of the American Meteorological Society, 101(10), E1628-E1644.


<br/>

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Licencia de Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">Probabilistic forecasts for monitoring</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://matv.shinyapps.io/app_4DROP/" property="cc:attributionName" rel="cc:attributionURL">4DROP</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Reconocimiento-NoComercial 4.0 Internacional License</a>.

<a href="http://cran.r-project.org/web/packages/shiny" target="_blank_">shiny</a>: Chang, W., Cheng J., Allaire, J.J., Xie, Y. & McPherson, J. (2013). shiny: Web Application Framework for R. R package version 1.7.5

<a href="http://cran.r-project.org/web/packages/shinydashboard" target="_blank_">shinydashboard</a>: Chang, W. (2015). shinydashboard: Create Dashboards with Shiny. R package version 0.7.2
