# TSA-DOT  

The Temporary Storage Area Design Optimisation Tool (TSA-DOT) is an R-based decision support tool for designing and optimising Temporary Storage Areas (TSAs) used in natural flood management. TSAs attenuate near-surface runoff during storm events, contributing to flood mitigation. However, guidance on how to optimise TSA designs for site-specific conditions is limited.

TSA-DOT addresses this gap by simulating TSA performance across multiple design combinations and evaluating effectiveness using five flood mitigation metrics:

- Storage Efficiency Index (SEI)
- Mean Retention Time
- Peak Flow Attenuation
- Peak Flow Reduction
- Change in Peak Flow Travel Time

Users can input site-specific data, including design storm events, local topography (depth to volume relationships), outlet pipe configurations, and soil infiltration rates, making TSA-DOT suitable for designing new TSAs or retrofitting existing ones. Metric weightings can be customised to reflect local priorities.

---

## 🔧 Modes

### **Mode 1 — Educational Tool (Built-in Example Data)**

Interactive mode using preloaded Tarland TSA hydrological and topographic data to demonstrate how design parameters influence TSA runoff attenuation performance.

**Purpose:** Learning, demonstration, training, and introductory exploration of TSA design and runoff attenuation performance.

📄 **Full Mode 1 Documentation → [Mode1_Educational_Tool/Mode1_README.md](Mode1_README.md)**  


### **Mode 2 — Optimal Design (User-Defined Inputs)**

Full optimisation mode using user-supplied storm events, topographic storage curves, and design parameter ranges. TSA-DOT evaluates all combinations and identifies the most effective TSA design based on the five metrics and user-defined weightings.

**Purpose:** Designing and optimising new TSAs or retrofitting existing TSAs using site-specific data.

📄 **Full Mode 2 Documentation → [Mode2/README.md](Mode2/README.md)**  

---

## How to Cite

If you use TSA-DOT in your research, design work, teaching, or reporting, please cite:
