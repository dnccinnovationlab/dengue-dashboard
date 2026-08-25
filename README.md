# DNCC Dengue Dashboard & Spatial Data Repository

An interactive dashboard and spatial data repository for monitoring dengue outbreak patterns, environmental variables, and weather data across **Dhaka North City Corporation (DNCC)** wards.

## 📌 Overview

This project provides real-time and historical spatial visualization tools to track vector-borne disease risks in Dhaka North. By integrating DNCC ward boundary shapefiles with automated OpenWeather pipeline workflows, it enables data-driven public health surveillance and early warning interventions.

## 🛠 Features

* **Interactive Mapping:** Spatial visualization of DNCC ward boundaries and localized outbreak metrics.
* **Automated Weather Pipeline:** Scheduled GitHub Actions (`update_openweather_observed.yml`) fetching local weather parameters.
* **Epidemiological & GIS Analytics:** R-based scripts for processing geospatial datasets and tracking disease patterns.
* **Web Deployment:** Hosted directly via GitHub Pages.

## 📁 Repository Structure

```text
.
├── .github/workflows/                 # Automated GitHub Actions for data updates
├── R/                                 # Data processing and analytical R scripts
├── DNCC_Ward_Boundary_poly_urp_f.*    # Spatial shapefile components (SHP, DBF, PRJ, etc.)
└── index.html                         # Interactive dashboard frontend
