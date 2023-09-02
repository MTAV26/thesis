#  <p align="center"> **Seguimiento y predicción probabilística de la sequía meteorológica de alta resolución en un entorno mediterráneo: <a href="https://matv.shinyapps.io/app_4SPAIN/" target="_blank_">4SPAIN</a>**</p>

<p align="center"> Miguel Ángel Torres-Vázquez; University of Murcia (miguelangel.torres@um.es).</p>
<p align="center"> Juan Pedro Montávez Gómez; University of Murcia (montavez@um.es).</p>
<p align="center"> Marco Turco; University of Murcia  (mturco@um.es).</p>

# 
**A. General instructions** 

This project is designed to be executed with R scripts.
Execute the script files in the order they are listed.

Data sources:
First, different precipitation datasets are compared after estimating meteorological drought. These datasets are available in the data section of this repository "https://github.com/MTAV26/thesis/tree/main/Chapter3-4SPAIN/Data". Secondly, the ERA5 database is used to obtain seasonal predictions of meteorological drought.

| Dataset  | Source |
| :------------ |:---------------|
| AEMET  (v1)    | http://www.aemet.es/es/serviciosclimaticos/cambio_climat/datos_diarios?w=2&w2=0      |
| EOBS  (v17)    | https://www.ecad.eu/download/ensembles/download.php/       |
| CHIRPS (v2)    | http://chg.geog.ucsb.edu/data/chirps/        |
| ERA5 (Ensemble mean) | https://www.ecmwf.int/en/forecasts/datasets/archive-datasets/reanalysis-datasets/era5      |
<br/>
<br/>

The script files have been organized with the aim of facilitating reproducibility. They are first sorted alphabetically and then numerically. If you have any questions or wish to provide feedback to the authors, we encourage you to contact Miguel Ángel Torres-Vázquez through the provided email address.

# 
**B. Description of script files**
Scripts for data preparation and cleaning.

**Precipitation Data Comparison (after estimating meteorological drought)**
- A_load_SPI.R: Read and convert precipitation data to SPI.
- B_Verify_COR_obs.R: Calculate the correlation.
- B1_Plot_COR_obs.R: Plot correlation maps.
- B2_Boxplot_COR_obs.R: Plot correlation boxplots.
- C_Verify_obs_MAE.R: Calculate the MAE.
- C1_Plot_MAE_obs.R: Plot MAE maps.
- C2_Boxplot_MAE_obs.R: Plot MAE boxplots.

**Forecast**
- D_Forecast_ESP_spain_1-12.R: Calculate ESP predictions.
- E_unir_RData.R: Merge all predictions into a single .Rdata file.

**Deterministic Validation**
- F_Verify_COR_esp.R: Calculate the correlation of predictions.
- F1_Boxplot_COR_esp.R: Plot correlation boxplots.
- G_Verify_MAE_esp.R: Calculate the MAE of predictions.
- G1_Boxplot_MAE_esp.R: Plot MAE boxplots.
- H_Verify_BIAS_esp.R: Calculate the BIAS of predictions.
- H1_Boxplot_BIAS_esp.R: Plot BIAS boxplots.
- I_Plot_COR_MAE_BIAS_esp.R: Plot spatial maps of correlation, MAE, and BIAS of predictions.

**Probabilistic Verification**
- J_Verify_Reliability_loop_esp.R: Calculate the reliability of predictions for 4SPAIN and plot the attribute diagram.
- J1_Boxplot_Reliability_esp.R: Plot reliability of predictions in boxplot for 4SPAIN.
- K_Verify_ROC_loop_esp.R: Calculate the skill of predictions for 4SPAIN and plot the ROC plot.
- K1_Boxplot_ROC_esp.R: Plot ROC of predictions in boxplot for 4SPAIN.

**Case Study**
- L_Caso_estudio.R: Represent the case study of September 2017.
- M_precip_seasont.R: Calculate seasonal precipitation for AEMET.

In the "cammon" folder, you will find additional script files that are useful for the execution of the main script files described above. This folder corresponds to the .R files for DROP (Turco et al., 2020).

**References**
Turco, M., Jerez, S., Donat, M. G., Toreti, A., Vicente-Serrano, S. M., & Doblas-Reyes, F. J. (2020). A global probabilistic dataset for monitoring meteorological droughts. Bulletin of the American Meteorological Society, 101(10), E1628-E1644.

<br/>

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Licencia de Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">Probabilistic forecasts for monitoring</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://matv.shinyapps.io/app_4DROP/" property="cc:attributionName" rel="cc:attributionURL">4DROP</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Reconocimiento-NoComercial 4.0 Internacional License</a>.

<a href="http://cran.r-project.org/web/packages/shiny" target="_blank_">shiny</a>: Chang, W., Cheng J., Allaire, J.J., Xie, Y. & McPherson, J. (2013). shiny: Web Application Framework for R. R package version 1.7.5

<a href="http://cran.r-project.org/web/packages/shinydashboard" target="_blank_">shinydashboard</a>: Chang, W. (2015). shinydashboard: Create Dashboards with Shiny. R package version 0.7.2
