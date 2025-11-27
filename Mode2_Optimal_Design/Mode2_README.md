# Mode 2 - TSA Optimal Design

Mode 2 of the Temporary Storage Area Design Optimisation Tool (TSA-DOT) allows users to run full TSA design simulations using their own site-specific storm data, TSA geometry, and soil conditions.
It is intended for practical design, assessment, and optimisation of new or existing TSAs.

---

## 🎯 Purpose 

Use Mode 2 to:
-	Evaluate TSA performance using site-specific storm events, topography, and soil infiltration
-	Test ranges of TSA dimensions and outlet pipe designs
-	Assess performance across five flood-mitigation metrics
-	Apply custom metric weighting to prioritise outcomes based on local objectives
-	Identify the optimal TSA design from all combinations tested
-	Download results for reporting or further analysis

---

## 🚀 How to Use TSA-DOT (Mode 2)
Mode 2 runs inside the R Shiny application.

Information icons throughout the interface guide users through the workflow, design inputs, and metric definitions.

Start by clicking the TSA-DOT workflow info icon.

Users then:
1.	Select the storm event / upload custom Qin
2.	Enter contributing area (km²)
3.	Choose TSA geometry (Tarland template or custom inputs)
4.	Define TSA height range and number of steps
5.	Set outlet pipe design (diameter limits, number of steps, pipe positions)
6.	Specify soil infiltration rate
7.	Click Run Simulations

<img width="3959" height="2475" alt="TSADOT_Workflow" src="https://github.com/user-attachments/assets/f93d2288-4c56-4134-90a0-c61891a692f7" />

*Figure 1: TSA-DOT workflow*

---

## 🛠️ Flood-Metric Weighting & Interpretation

After simulations are complete, users can:
-	Adjust metric weightings to reflect priorities (e.g., shorter retention time for arable land)
-	View overall effectiveness scores
-	Inspect any design using the Design ID selector


**⚠ Important Note on Comparability**
The overall effectiveness score is only comparable within the same simulation.
- Overall effectivess scores are normalised and ranked across the tested designs. Therefore, a simulation with 50 designs vs. another with 100 designs will produce effectiveness scores that cannot be compared.
- **Always compare designs within a single run.**

---

## 📐 Flood-Mitigation Metrics Used in Mode 2

1.	Storage Efficiency Index (SEI)
2.	Mean Retention Time
3.	Peak Flow Attenuation
4.	Peak Flow Reduction
5.	Change in Peak Flow Travel Time

---

## 📊 Outputs

Mode 2 produces:
-	Overall effectiveness plot comparing all design combinations
-	Storm-event timeseries for any selected TSA design
-	Summary table of TSA parameters and flood-mitigation metrics
-	CSV exports for further analysis or reporting

---

## ▶️ How to Run Mode 2
1. Download the script:  
📄 [`TSA_DOT_Mode2.R`](./TSA_DOT_Mode2.R)
2. Open the script in RStudio  
3. Run all code:  
   **Windows:** Ctrl + A → Ctrl + Enter  
   **Mac:** Cmd + A → Cmd + Enter
