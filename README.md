# TSA-DOT  

The Temporary Storage Area Design Optimisation Tool (TSA-DOT) is an R-based decision support tool for designing and optimising Temporary Storage Areas (TSAs) used in natural flood management. TSAs attenuate near-surface runoff during storm events, contributing to flood mitigation. However, guidance on how to optimise TSA designs for site-specific conditions is limited.

TSA-DOT addresses this gap by simulating the performance of multiple TSA design combinations (height, storage volume, outlet pipe configuration, soil infiltration) for a design storm event and evaluating effectiveness using five flood-mitigation metrics:

- Storage Efficiency Index (SEI)
- Mean Retention Time
- Peak Flow Attenuation
- Peak Flow Reduction
- Change in Peak Flow Travel Time

Users can input site-specific data such as design storm events, depth–volume curves, outlet configuration, and soil infiltration rates. Metric weightings can be customised to reflect local priorities, making TSA-DOT suitable for designing new TSAs or retrofitting existing ones.

<img width="3959" height="2475" alt="TSADOT_Workflow" src="https://github.com/user-attachments/assets/b1edad39-ae24-4cad-8abe-157bab244070" />

*(Roberts et al., 2025)*

For more details on the underlying methodology, see our manuscript:

---

## 🔧 Modes

### **Mode 1 — Educational Tool (Built-in Example Data)**

Interactive mode using preloaded Tarland TSA hydrological and topographic data to demonstrate how design parameters influence TSA runoff attenuation performance.

**Purpose:** Learning, demonstration, training, and introductory exploration of TSA design and runoff attenuation performance.

📁 **Mode 1 Folder → [Mode1_Educational_Tool](Mode1_Educational_Tool/)**


### **Mode 2 — Optimal Design (User-Defined Inputs)**

Full optimisation mode using user-supplied storm events, topographic depth-volume curves, and design parameter ranges. TSA-DOT evaluates all combinations and identifies the most effective TSA design based on the five metrics and user-defined weightings.

**Purpose:** Designing and optimising new TSAs or retrofitting existing TSAs using site-specific data.

📁 **Mode 2 Folder → [Mode2_Optimal_Design](Mode2_Optimal_Design/)**  

---

## 📄 Docs

**Full Tarland Site Description** — background and characteristics for the Tarland TSA used as the built-in example dataset in TSA-DOT.

📄 [View Tarland Site Description](docs/Tarland_Site_Description.md)

---

## How to Cite

If you use TSA-DOT in your research, design work, teaching, or reporting, please cite:
