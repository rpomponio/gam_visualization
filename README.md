# GAM Visualization of Brain Aging Charts

An interactive visualization of GAM models using R Shiny package.

A user will need basic familiarity with the R programming language to run this application.

Alternatively, you can run the [Web-only app](https://rpomponio.shinyapps.io/brain_aging_charts/).

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

1. Run `brain_aging_charts/install.R` to ensure the necessary packages are installed.
2. Open the script `brain_aging_charts/UI.R` in RStudio.
3. Launch the app from the upper right corner, where a button will say "Run App".
4. Upload a dataset using the dialog box "Upload a New Dataset"

## Questions

Email Raymond Pomponio: raymond (dot) pomponio (at) outlook (dot) com


