[![DOI](https://img.shields.io/badge/DOI-10.3389%2Ffmed.2021.721515-blue)](https://doi.org/10.3389/fmed.2021.721515)

tve_malaria
================
`R` code and outputs to support [Carrasco-Escobar G](https://github.com/gcarrascoe), [Qquellon J](https://github.com/luzjazmin19), [Villa D](https://github.com/dievillano), [Cava R](https://github.com/renatocava), Llanos-Cuentas A, Benmarhnia T. Time-Varying Effects of Meteorological Variables on Malaria Epidemiology in the Context of Interrupted Control Efforts in the Amazon Rainforest, 2000-2017. Front Med (Lausanne). 2021 Sep 29;8:721515. doi: [10.3389/fmed.2021.721515](https://doi.org/10.3389/fmed.2021.721515). PMID: 34660633; PMCID: PMC8511324.

<p align="center">
<img src="analysis/figs/figure-2.png" alt="figure-2" width="500" height="600"/>
</p>

<p align="center">
<i><b>Figure 2. </b>Fitted time-varying meteorological effects on MIR by parasite species. Fitted time-varying meteorological effects due to P. vivax (red) and P. falciparum (light blue) and their 95% CI (shading of each colored curve). The period shaded in gray represents the PAMAFRO intervention.</i>
</p>

----------------

## Study description

In this study, we hypothesized that the interruption of malaria control interventions may modify the meteorological-malaria relationships over time (i.e., temporal changes in the dose-response between meteorological variables and malaria incidence). For instance, we assessed the extent that relationships between meteorological variables and malaria changed temporally using data of monthly malaria incidence due to *Plasmodium vivax* or *P. falciparum* in Loreto, Peru (2000–2017). Generalized additive models were used to explore how the effects of meteorological variables changed in magnitude before, during, and after the PAMAFRO intervention (2006–2010) - a Global Fund-sponsored project for the strengthening of malaria control and surveillance in multiple countries in Latin America.

----------------

## Repository structure

- [analysis](analysis) `R` markdowns and outputs.
  - [figs](analysis/figs) Main and supplementary figures.
    - [supplementary-figs](analysis/figs/supplementary-figs)  
      - [districts](analysis/figs/supplementary-figs/districts) Meteorological variables time-series plot for every district in Loreto, Peru.
      - [provinces](analysis/figs/supplementary-figs/provinces) Meteorological variables time-series plot aggregated for every province in Loreto, Peru.
    - [figure-1-with-map.jpg](analysis/figs/figure-1-with-map.jpg) Annual malaria incidence rates variation by parasite species (Peru map added).
    - [figure-1.pdf](analysis/figs/figure-1.pdf) Annual malaria incidence rates variation by parasite species.
    - [figure-2.png](analysis/figs/figure-2.png) Fitted time-varying meteorological effects on MIR by parasite species.
    - [figure-3.png](analysis/figs/figure-3.png) Fitted time-varying meteorological effects on MIR by parasite species at different time lags.
  - [00-data-preparation.Rmd](analysis/00-data-preparation.Rmd) `R` markdown for data preparation and preprocessing.
  - [01-exploratory-data-analysis.Rmd](analysis/01-exploratory-data-analysis.Rmd) `R` markdown for exploratory data analysis.
  - [02-time-vaying-coef-models.Rmd](analysis/02-time-vaying-coef-models.Rmd) `R` markdown for model building and interpretation.
- renv
- .Rprofile
- .gitignore
- README.md
- renv.lock
- [tve_malaria.Rproj](tve_malaria.Rproj) `R` project file.

----------------

## `R` environment and version

Detailed information about the `R` environment and version used for running this project.

```bash
               _                           
platform       x86_64-w64-mingw32          
arch           x86_64                      
os             mingw32                     
system         x86_64, mingw32             
status                                     
major          4                           
minor          1.0                         
year           2021                        
month          05                          
day            18                          
svn rev        80317                       
language       R                           
version.string R version 4.1.0 (2021-05-18)
nickname       Camp Pontanezen 
```
