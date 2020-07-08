# GAM Visualization

An interactive visualization of GAM models using R Shiny package.

A user will need basic familiarity with the R programming language to run this application.

## Description
Generalized Additive Models (GAMs) offer flexible ways to capture multi-variate relationships. In this interactive visualization, the user can select variables to build an arbitrarily complex GAM, then view the predicted surface plots and isocontour plots for two selected variables.

The intended context of this application is in neuroimaging. A dummy dataset has been supplied for illustrative purposes with the following variables:

* STUDY
* AGE
* SEX
* DIAGNOSIS
* TRAILMAKING (Cognitive test performance)
* MMSE (Cognitive test performance)
* ICV (Intravranial volume)
* TOTALBRAIN_VOLUME

## Main Visualization

The figure under the "Isocontours" tab offers a view of the relationship between three variables of interest.

One can examine previously-saved plots from the [paper](link_missing) to compare with the plots from ones own data.

An exmple plot is shown below, from the dummy dataset:

![alt text](img/1_contours_demo.png)

## Installation and Quick Start

1. Run `install.R` to ensure the necessary packages are installed.
2. Open the script `R/UI.R` in RStudio.
3. Launch the app from the upper right corner, where a button will say "Run App".

## Questions

Email Ray Pomponio: raymond (dot) pomponio (at) pennmedicine (dot) upenn (dot) edu


