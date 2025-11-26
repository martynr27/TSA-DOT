# Mode 1 - Educational Tool

Mode 1 of the Temporary Storage Area Design Optimisation Tool (TSA-DOT) is an interactive learning tool for exploring how Temporary Storage Area (TSA) design parameters influence flood-mitigation performance.

This mode uses built-in hydrological and topographic data from the Tarland TSA (northeast Scotland), an agricultural soil bund designed to intercept near-surface runoff (Roberts et al., 2025).

Mode 1 is ideal for learning, training, demonstration, and understanding TSA functioning and performance during storm events.

**Purpose**
Use Mode 1 to:
-	Experiment with key TSA design parameters
-	Understand how TSAs attenuate runoff during storm events
-	See how design decisions affect flood-mitigation metrics
-	Test designs against future increased runoff scenarios

**Built-In Data**
Mode 1 includes:
-	Two inflow scenarios
	- October 2023 Storm Babet (observed)
  - +40% inflow scenario (future extreme storm)
-	Tarland bund topographic data
  - 1-m LiDAR DEM used to generate TSA geometry (bund height vs storage)

**User-Adjustable TSA Parameters**
Users can dynamically modify:
-	Maximum TSA height / storage capacity
-	Outlet pipe diameter
-	Outlet pipe height
-	Soil infiltration rate

These changes update plots and metrics in real time.

# Outputs
**1. Time-Series Plot (3 panels)**
-	Runoff attenuation: near-surface runoff (Qin) vs quick TSA outflow (Qout_quick). 
(quick TSA outflow = outlet pipe discharge + overflow)
-	Overflow
-	TSA stored water volume

**2. Volume–Depth Curve**
-	Geometry of the Tarland TSA 

**3. TSA Effectiveness Table**
Five flood-mitigation metrics are calculated:

**Storage Efficiency Index (SEI) **
Measures how effectively the TSA uses storage while avoiding overflow.
Values near 0 indicate efficient designs; positive values show under-utilised storage; negative values indicate overflow risk.

**Mean Retention Time**
Average time for the TSA to drain to near-empty (<5 m3).
A range of 10–20 hours is generally effective for allowing recovery between storms.

**Peak Flow Attenuation**
Additional storage capacity available within ±2 hours of peak inflow. Benchmarked against 1,000 m³/km² as an effective threshold.

**Peak Flow Reduction**
Difference between peak inflow and peak quick TSA outflow, indicating the TSA’s ability to reduce peak discharge rates.

**Change in Peak Flow Travel Time**
Delay between inflow and quick TSA outflow peaks, showing how the TSA shifts peak timing to mitigate downstream flooding.

# How to Run Mode 1
1.	Download the TSA_DOT_Mode1_Rcode
2.	Open the script in RStudio
3.	Run all code:
**Windows:** Ctrl + A → Ctrl + Enter
**Mac:** Cmd + A → Cmd + Enter

