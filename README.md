# TSA-DOT  

The Temporary Storage Area Design Optimisation Tool (TSA-DOT) is an R-based decision support tool for designing and optimising Temporary Storage Areas (TSAs) used in natural flood management. TSAs attenuate near-surface runoff during storm events, contributing to flood mitigation. However, guidance on how to optimise TSA designs for site-specific conditions is limited.

TSA-DOT addresses this gap by simulating TSA performance across multiple design combinations and evaluating effectiveness using five flood mitigation metrics:

- Storage Efficiency Index (SEI)
- Mean Retention Time
- Peak Flow Attenuation
- Peak Flow Reduction
- Change in Peak Flow Travel Time

Users can input site-specific data—including design storm events, local topography and storage potential, outlet pipe dimensions, and soil infiltration rates—making TSA-DOT suitable for designing new TSAs or retrofitting existing features. Metric weightings can be customised to reflect local priorities, supporting evidence-based and scalable TSA design decisions.

---

## 🔧 Modes

### **Mode 1 — Educational Tool (Built-in Example Data)**

Mode 1 uses built-in data from the Tarland TSA (northeast Scotland), an agricultural soil bund intercepting near-surface runoff.
- Includes the October 2023 storm and a 40% increased inflow scenario to explore potential future extremes.
- Allows users to adjust TSA height and storage, outlet pipe diameter and height, and soil infiltration rate to observe real-time impacts on flood mitigation metrics.

**Purpose:** Learning, demonstration, training, and introductory exploration of TSA design and runoff attenuation performance.

---

### **Mode 2 — Optimal Design (User-Defined Inputs)**

Mode 2 provides full customisation for operational design work. Users can:
- Upload site-specific design storm events (rainfall or inflow time series)
- Derive storage capacity from local topography (e.g., LiDAR DEM depth–volume curves)
- Define ranges for TSA height, storage volume, and outlet pipe diameter and height
- Specify soil infiltration rates
- Prioritise metrics with user-defined weightings
- Identify the optimal TSA design across all parameter combinations

**Purpose:** Designing new TSAs, retrofitting existing TSAs, and exploring TSA flood mitigation effectiveness.

---

## How to Cite

If you use TSA-DOT in your research, design work, teaching, or reporting, please cite:

