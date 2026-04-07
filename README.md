silvaR <img src="man/figures/silvaR_logo_v1.png" align="right" width="160"/>
---
![license](https://img.shields.io/badge/Licence-GPL--3-blue.svg)
![R-CMD-check](https://github.com/DFRM-FF-UAK/silvaR/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)
[![codecov](https://codecov.io/gh/DFRM-FF-UAK/silvaR/branch/main/graph/badge.svg)](https://codecov.io/gh/DFRM-FF-UAK/silvaR)
[![CRAN status](https://www.r-pkg.org/badges/version/silvaR)](https://CRAN.R-project.org/package=silvaR)
![Downloads](https://cranlogs.r-pkg.org/badges/silvaR)

**Tools for processing and analyzing forest data from Polish forests**

**silvaR** is an R package providing a collection of tools useful for Polish foresters, forest managers, ecologists and environmental scientists working with stand-, plot- or tree-level data.  
The package supports data cleaning, standardization, attribute calculation and height/volume modelling using widely used Polish forestry equations.

---

## 📜 Citation

If you use **silvaR** in your scientific work, please cite the package as follows.

>Socha, J., Hawryło, P., Tymińska-Czabańska, L., Woda, M., & Janiec, P. (2026).
>Development of regionalised stand level volume increment models for the main forest forming tree species in Poland using GAM and artificial chronosequences of forest growth.
>Forest Ecology and Management, 603, 123433.
>https://doi.org/10.1016/j.foreco.2025.123433

---

# 🚀 Key Features

### **✔ Data cleaning and preparation**
- Standardizes tree species names (Latin, Polish, English), correcting typos and formatting inconsistencies.  
- Converts species names to official Polish forestry abbreviations.  
- Groups species by habitat conditions, natural region or growth characteristics.  
- Classifies plot/stand origin (upland vs lowland).  
- Prepares inputs required for calculating tree volume with Bruchwald equations.

### **🌲 Tree-level attribute calculation**
Estimate tree height and stem volume using empirical Polish forestry models:

> Bruchwald, A.; Rymer-Dudzińska, T.; Dudek, A.;  
> Michalak, K.; Wróblewski, L.; Zasada, M. (2000).  
> *Wzory empiryczne do określania wysokości i pierśnicowej liczby kształtu grubizny drzewa.*  
> **Sylwan**, 144, 5–12.

### **📐 Plot and stand evel metrics**
Calculate stand top height (TH) based on:

> Socha, J., Hawryło, P., Stereńczak, K., Miścicki, S.,  
> Tymińska-Czabańska, L., Młocek, W., & Gruba, P. (2020).  
> *Assessing the sensitivity of site index models developed using bi-temporal airborne laser scanning data to different top height estimates and grid cell sizes.*  
> **International Journal of Applied Earth Observation and Geoinformation**, 91, 102129.

and

> Hawryło, P., Socha, J., Wężyk, P., Ochał, W., Krawczyk, W.,  
> Miszczyszyn, J., & Tymińska-Czabańska, L. (2024).  
> *How to adequately determine the top height of forest stands based on airborne laser scanning point clouds?*  
> **Forest Ecology and Management**, 551, 121528.

### **✔ Top height (TH) estimation from raster CHM**
**silvaR** includes algorithms for top height estimation at any spatial resolution,
useful in calculating stand parameters, segmentation or ALS-based inventory workflows.


**Example:**

```r
library(silvaR)

chm <- terra::rast(system.file("inst/raster/chm.tif", 
                               package = "silvaR"))

th <- th_calc(chm, f = "h_sd")

par(mfrow = c(1, 2))
terra::plot(chm, main = "Canopy Height Model")
terra::plot(th, main = "Top Height 20x20m")

```
<img align="center" src="man/figures/th_calc_example.png">

---

## 🔧 Installation
**From GitHub**
```r
# install.packages("devtools")
devtools::install_github("DFRM-FF-UAK/silvaR")
```
---

## 📘 Basic usage example
### Stand level calculations

```r
library(silvaR)

# Example dataset
data(olkusz_v1)
data(olkusz_v2)

# Clean species names
olkusz_v1$species <- sp_clean(olkusz_v1$species)

# Predict tree volume growth in specific period of time
olkusz_v1$vol_growth <- v_growth(stand_id = olkusz_v1$stand_id, 
         years = 10, 
         age = olkusz_v1$age, 
         height = olkusz_v1$height, 
         volume = olkusz_v1$volume, 
         species = olkusz_v1$species, 
         region = olkusz_v1$region)
         
head(olkusz_v1)

# Estimate tree height at age 100
olkusz_v2$h_at_100 <- h_growth(T1 = olkusz_v2$age, 
                               T2 = 100, 
                               H1 = olkusz_v2$height, 
                               species = olkusz_v2$species)
                               
head(olkusz_v2)

```

### Plot level calculations

```r
library(silvaR)

# Load example data
data(PPdemo)
head(PPdemo)

# Clean species name
PPdemo$SPECIES_CD <- sp_clean(PPdemo$SPECIES_CD)

# Estimate tree height
PPdemo$TREE_HEIGHT <- h_tree(plot_id = PPdemo$SAMPLE_NR, 
                               tree_id = PPdemo$SAMPLE_RECORD_NR, 
                               species = PPdemo$SPECIES_CD, 
                               age = PPdemo$TREE_AGE, 
                               layer = PPdemo$STOREY_NR, 
                               dbh = PPdemo$BREAST_HEIGHT, 
                               height = PPdemo$TREE_HEIGHT)

# Estimate single tree volume
PPdemo$TREE_VOLUME <- v_tree(dbh = PPdemo$BREAST_HEIGHT, 
                          height = PPdemo$TREE_HEIGHT, 
                          species = PPdemo$SPECIES_CD)
                          
head(PPdemo)

## Plot single sample plot 
singlePP <- PPdemo[PPdemo$SAMPLE_NR == 72,]

plot_sample_plot(data = singlePP, plot_radius = 11.28)

```
<img align="left" src="man/figures/plot_sample_plot_example.png" width = 300>
<img align="center" src="man/figures/plot_sample_plot_example2.png" width = 300>
<img align="right" src="man/figures/plot_sample_plot_example3.png" width = 300>

---

## 📂 Package structure
```
silvaR/
├── R/                     # Package functions
├── man/                   # Documentation
│   └── figures/           # Logo, images for README/pkgdown
├── raw_data/              # Dictionares
├── data/                  # Example datasets
├── inst/                  # Additional resources
└── tests/                 # Unit tests (testthat)
```
---

## 🤝 Contributing
Contributions, bug reports and feature requests are welcome!
Please open an Issue or submit a Pull Request.

---

## 📄 License
This package is licensed under the GPL-3 license.

---

## 🏫 About
The **silvaR** package is developed at the [Faculty of Forestry](https://wl.urk.edu.pl/), University of Agriculture in Kraków, 
in collaboration with the [Bureau for Forest Management and Geodesy (BULiGL)](https://www.buligl.pl/), Kraków Regional Office.

---

<img src="man/figures/collab_logos.png" width = 250 align="left">
