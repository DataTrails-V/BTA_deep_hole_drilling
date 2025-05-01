# Early Detection of Chatter and Spiraling in Drilling Operations

This project aims to develop a data-driven approach for the early detection of **chatter** and **spiraling** phenomena in drilling processes using sensor data and frequency domain analysis. By analyzing signals (e.g., torque) via Fast Fourier Transform (FFT), the project identifies patterns in the frequency spectrum that may indicate the onset of instability before it fully develops.

## Project Structure

- `chatter_detection.R`: Main R script containing the full implementation, including data preprocessing, FFT analysis, periodogram generation, and early warning logic.

## Features

- 📊 Integration and visualization of multi-sensor data (e.g., torque, axial force, rotational speed)
- 🔍 Signal analysis using Fast Fourier Transform (FFT)
- 📈 Periodogram generation to investigate frequency domain characteristics
- 🚨 Identification of early warning signs based on frequency patterns
- 🧠 Proposed logic for triggering alarms before chatter/spiraling begins

## Getting Started

### Prerequisites

Make sure the following R packages are installed:

```r
install.packages("tuneR")
