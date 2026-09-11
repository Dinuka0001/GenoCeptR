# GenoCeptR

**GenoCeptR** is an interactive R Shiny application for exploring overlaps between differential gene expression (DE) result sets and performing downstream pathway enrichment analysis — all in one workspace.

Developed by **Dinuka Adasooriya**, Yonsei University College of Dentistry, Seoul, Korea.

---

## 🚀 Try GenoCeptR Online — No Installation Required

The easiest way to use GenoCeptR is the **online version**:

### 🌐 [Launch GenoCeptR Online](https://dinuka-genoceptr.share.connect.posit.cloud/)

You can run GenoCeptR directly in your web browser without installing:

- R
- RStudio
- R packages
- Docker
- Any local software

Simply open the link, upload or paste your gene-expression data, and start the analysis.

> **Recommended for most users:** Use the online version if you just want to analyze your data without setting up a local environment.

The online application is **GenoCeptR v3.0** and currently supports up to **8 datasets**.

---

## 🐳 Run GenoCeptR with Docker

If you prefer to run GenoCeptR locally, a pre-built Docker image is available through GitHub Container Registry (GHCR).

### Pull the published image

```bash
docker pull ghcr.io/dinuka0001/genoceptr:latest
```

### Run GenoCeptR

```bash
docker run --rm -p 3838:3838 ghcr.io/dinuka0001/genoceptr:latest
```

Then open:

**http://localhost:3838**

No R installation or manual R-package installation is required when using Docker.

### Run in the background

```bash
docker run -d \
  --name genoCeptR \
  -p 3838:3838 \
  ghcr.io/dinuka0001/genoceptr:latest
```

Open:

**http://localhost:3838**

Useful commands:

```bash
docker logs genoCeptR
docker stop genoCeptR
docker rm genoCeptR
```

### Use a different host port

If port `3838` is already being used:

```bash
docker run --rm -p 8080:3838 ghcr.io/dinuka0001/genoceptr:latest
```

Then open:

**http://localhost:8080**

### Docker image

The published container is available at:

**https://github.com/Dinuka0001/GenoCeptR/pkgs/container/genoceptr**

Current image:

```text
ghcr.io/dinuka0001/genoceptr:latest
```

The container exposes Shiny on port `3838`.

---

# Key Features

## Data Input

GenoCeptR supports multiple ways to provide gene lists and differential-expression results.

- Analyze **2–8 datasets** in a single session.
- Upload complete DE result files.
- Upload pre-filtered gene lists.
- Upload a single file containing multiple gene lists.
- Paste gene IDs or gene names directly into the application.
- Supported file formats include:
  - `.csv`
  - `.tsv`
  - `.txt`
  - `.xlsx`
- Automatic separator/sheet handling.
- Flexible column mapping for:
  - Gene ID
  - Gene name
  - Adjusted p-value
  - Log2 fold-change
- Adjustable adjusted p-value cutoff.
- Adjustable absolute log2FC cutoff.
- Optional gene-direction filtering:
  - All significant genes
  - Upregulated genes
  - Downregulated genes

---

## Set Overlap Visualization

Compare shared and unique genes across multiple datasets using several visualization methods:

- Venn diagrams
- Interactive Plotly-based Venn diagrams
- Euler diagrams
- UpSet plots
- Edwards' Venn diagrams

Visualization customization includes:

- Set labels
- Font sizes
- Set colors
- Colorblind-friendly palettes
- Fill transparency
- Borders
- Percentages
- Count gradients
- Plot-specific options

### Downloadable visualizations

Export plots as:

- PNG
- SVG
- PDF
- Interactive HTML where supported

---

## Overlap Summary

GenoCeptR provides detailed numerical summaries of your gene-set overlaps.

You can inspect:

- Number of genes in each dataset
- Exclusive Venn regions
- Inclusive intersections
- Shared genes
- Dataset-specific genes

Summary information can be downloaded for further analysis.

---

## Gene Lists

The **Gene Lists** workspace allows you to browse and export genes from individual regions or combined overlap sets.

Features include:

- Select individual overlap regions
- Show all genes
- Search gene tables
- Sort results
- Export selected or all genes
- Include associated DE statistics where available

Gene lists can be downloaded as CSV files.

---

# 🧬 Pathway Analysis

GenoCeptR includes a dedicated **Pathway Analysis** workspace for downstream over-representation analysis (ORA).

Pathway analysis can be performed using:

- A complete dataset
- An overlap region
- A manually pasted gene list

### Gene input types

Depending on the selected species/database, users can work with:

- Gene IDs
- Ensembl IDs
- Entrez IDs
- Gene names/symbols

### Gene direction

Pathway analysis can be restricted to:

- Upregulated genes
- Downregulated genes
- Both upregulated and downregulated genes

### Background gene set

You can use:

- An automatically generated species background
- A manually supplied background gene list

### Pathway databases

The application supports pathway/enrichment resources available through its enrichment backend, including commonly used resources such as:

- Gene Ontology (GO)
- KEGG
- Reactome
- WikiPathways
- Other supported pathway databases

### Pathway results

Results can be explored using:

- Sortable/searchable enrichment tables
- Bar plots
- Dot plots
- Lollipop plots
- Hierarchical clustering/dendrograms
- Pathway-gene networks

Exports include:

- PNG
- SVG
- PDF
- CSV
- Interactive HTML
- Network nodes
- Network edges

---

# Quick Start

## Option 1 — Use the Online Version ⭐ Recommended

No installation is required.

Open:

**https://dinuka-genoceptr.share.connect.posit.cloud/**

Then:

1. Open **Data Input**.
2. Select the number of datasets.
3. Choose an input method.
4. Upload or paste your gene lists/DE results.
5. Configure column mapping and filtering if using DE results.
6. Click **Generate Diagram**.
7. Explore:
   - Visualization
   - Overlap Summary
   - Gene Lists
   - Pathway Analysis
8. Download your results.

---

## Option 2 — Use Docker

Install Docker Desktop or Docker Engine, then:

```bash
docker pull ghcr.io/dinuka0001/genoceptr:latest
docker run --rm -p 3838:3838 ghcr.io/dinuka0001/genoceptr:latest
```

Open:

**http://localhost:3838**

---

## Option 3 — Run from the R Source Code

If you are developing GenoCeptR or want to modify the source code, clone the repository:

```bash
git clone https://github.com/Dinuka0001/GenoCeptR.git
cd GenoCeptR
```

Then open the project in RStudio or run:

```r
shiny::runApp(".")
```

You can also run:

```r
source("run_app.R")
```

---

# Requirements for Local R Installation

For running directly from source:

- R **≥ 4.2** recommended
- RStudio is optional
- Internet access is recommended when installing dependencies

Docker users do **not** need to install R or the application's R packages manually.

---

# Installing R Dependencies

The main dependencies include:

```r
pkgs <- c(
  "shiny",
  "bslib",
  "shinyjs",
  "colourpicker",
  "VennDiagram",
  "ggvenn",
  "dplyr",
  "DT",
  "shinyWidgets",
  "readxl",
  "openxlsx",
  "UpSetR",
  "eulerr",
  "ggplot2",
  "showtext",
  "ggVennDiagram",
  "plotly",
  "htmlwidgets"
)

install.packages(pkgs)
```

For Pathway Analysis and network-related functionality:

```r
install.packages(c(
  "gprofiler2",
  "ggdendro",
  "visNetwork",
  "igraph",
  "scales"
))
```

Then launch:

```r
shiny::runApp("path/to/GenoCeptR")
```

---

# Input Data Format

GenoCeptR accepts several input styles.

## 1. Differential Expression Result Files

Supported formats:

```text
.csv
.tsv
.txt
.xlsx
```

A typical DE result file may contain:

| Gene ID | Gene Name | Adjusted P-value | Log2 Fold Change |
|---|---|---:|---:|
| ENSMUSG000000... | GeneA | 0.001 | 2.15 |
| ENSMUSG000000... | GeneB | 0.014 | -1.72 |
| ENSMUSG000000... | GeneC | 0.032 | 1.34 |

The application allows users to map the relevant columns through the interface.

At minimum, a gene identifier column is required.

Adjusted p-value and log2 fold-change columns are needed when significance and direction filtering are used.

---

## 2. Pre-filtered Gene Lists

You can upload a simple text/CSV/TSV gene list.

For example:

```text
GeneA
GeneB
GeneC
GeneD
GeneE
```

One gene can be provided per line.

---

## 3. Single File Containing Multiple Gene Lists

GenoCeptR can also process one file containing multiple gene lists in different columns or sheets.

Example:

| Dataset_A | Dataset_B | Dataset_C |
|---|---|---|
| GeneA | GeneB | GeneA |
| GeneC | GeneD | GeneF |
| GeneE | GeneG | GeneH |

---

## 4. Paste Gene Lists

Gene identifiers or names can be pasted directly into the application.

Example:

```text
GeneA
GeneB
GeneC
GeneD
```

This is useful for quickly comparing gene lists from previous analyses.

---

# Typical Workflow

A typical GenoCeptR analysis follows this workflow:

```text
DE Results / Gene Lists
        │
        ▼
     Data Input
        │
        ▼
 Column Mapping & Filtering
        │
        ▼
 Generate Overlap Analysis
        │
        ├───────────────┐
        ▼               ▼
 Visualization     Overlap Summary
        │               │
        ▼               ▼
     Gene Lists      Shared Genes
        │
        ▼
 Pathway Analysis
        │
        ├── Enrichment Tables
        ├── Pathway Plots
        ├── Dendrogram
        └── Gene–Pathway Network
```

---

# Gene Identifier Considerations

For overlap analysis, make sure that gene identifiers are comparable across datasets.

For example, avoid directly comparing:

```text
ENSMUSG00000000001
```

against:

```text
GeneSymbol
```

unless the identifiers have first been converted to a common identifier system.

For pathway analysis, choose the appropriate:

- Species
- Genome/organism
- Gene identifier type
- Background gene set

The biological interpretation of pathway enrichment depends strongly on correct gene identifiers and species selection.

---

# Project Structure

```text
GenoCeptR/
├── app.R                         # Shiny entry point
├── global.R                      # Package loading and sourcing
├── run_app.R                     # Standalone launcher
├── DESCRIPTION                   # Project metadata/dependencies
├── Dockerfile                    # Docker image definition
├── CHANGELOG.md                  # Version history
├── LICENSE                       # MIT License
├── GenoCeptR.Rproj               # RStudio project
│
├── modules/                      # Shiny UI/server modules
│   ├── ui_styles.R
│   ├── ui_sidebar.R
│   ├── ui_tabs.R
│   ├── ui_pathway.R
│   ├── server_data_input.R
│   ├── server_data_input_generate.R
│   ├── server_plotting.R
│   ├── server_downloads.R
│   ├── server_downloads_overlaps.R
│   ├── server_outputs.R
│   └── server_pathway.R
│
├── utils/                        # Shared utility functions
│   ├── file_utils.R
│   ├── palette_utils.R
│   ├── overlap_utils.R
│   ├── plot_utils.R
│   └── pathway_utils.R
│
└── www/                          # Static assets, logos and images
```

---

# Docker Deployment

GenoCeptR is packaged using a Shiny Docker base image.

The Docker container:

- Provides the required R environment.
- Installs the application's dependencies.
- Copies the GenoCeptR source code into the container.
- Exposes Shiny on port `3838`.
- Starts the application automatically.

The published image is:

```text
ghcr.io/dinuka0001/genoceptr:latest
```

## Build the Docker image yourself

From the repository root:

```bash
docker build -t genoceptr .
```

Run it:

```bash
docker run --rm -p 3838:3838 genoceptr
```

Then open:

```text
http://localhost:3838
```

## Pull the published image

```bash
docker pull ghcr.io/dinuka0001/genoceptr:latest
```

This is recommended for users who only want to run GenoCeptR locally.

---

# Reproducibility

GenoCeptR can be used through three deployment approaches:

| Method | R installation | Docker installation | Best for |
|---|---:|---:|---|
| **Online** | ❌ | ❌ | Quick analysis / general users |
| **Docker** | ❌ | ✅ | Reproducible local deployment |
| **R source** | ✅ | ❌ | Developers / customization |

### Recommended choice

**Just want to use GenoCeptR?**

→ Use the **online version**.

**Want a local, isolated environment?**

→ Use **Docker**.

**Want to modify the application?**

→ Clone the **GitHub repository** and run from R.

---

# Online Application

The public online application is hosted through Posit Connect:

### 🌐 [https://dinuka-genoceptr.share.connect.posit.cloud/](https://dinuka-genoceptr.share.connect.posit.cloud/)

This version requires **no installation** and can be accessed directly from a modern web browser.

---

# Source Code

GitHub repository:

**https://github.com/Dinuka0001/GenoCeptR**

Docker / GitHub Container Registry:

**https://github.com/Dinuka0001/GenoCeptR/pkgs/container/genoceptr**

Online application:

**https://dinuka-genoceptr.share.connect.posit.cloud/**

---

# Screenshots

Screenshots and application images are maintained in the repository and the `www/` directory.

The online application also provides an interactive way to explore the full interface before installing GenoCeptR locally.

---

# Version

Current application version:

**GenoCeptR v3.0**

The v3.0 release includes:

- Multiple input methods for gene lists
- Single-file multi-list input
- Support for up to 8 datasets
- Enhanced Venn visualization
- Edwards' Venn diagrams
- Interactive Venn visualization
- Improved customization
- Improved data-input configuration
- Pathway Analysis
- Over-representation analysis
- Multiple pathway databases
- Interactive pathway visualizations
- Pathway dendrograms
- Gene–pathway network visualization

See [`CHANGELOG.md`](CHANGELOG.md) for version history.

---

# Citation

If you use GenoCeptR in your research, please cite the software/repository:

> Adasooriya, D. GenoCeptR: Gene Expression Overlap & Pathway Enrichment Analysis. Yonsei University College of Dentistry, Seoul, Korea.

Please also cite the relevant databases and R packages used in your analysis where appropriate.

---

# Contributing

Contributions, bug reports, feature requests, and suggestions are welcome.

Please use the GitHub repository:

**https://github.com/Dinuka0001/GenoCeptR**

When reporting an issue, please provide:

- GenoCeptR version
- Operating system
- Whether you used Online, Docker, or R
- Input file format
- Error message
- Steps required to reproduce the problem

---

# License

GenoCeptR is released under the **MIT License**.

See [`LICENSE`](LICENSE) for the full license text.

---

# Acknowledgments

GenoCeptR is built using the R and Shiny ecosystem and incorporates several open-source R packages for data processing, visualization, overlap analysis, and pathway enrichment.

Important components include:

- Shiny
- bslib
- ggvenn
- ggVennDiagram
- VennDiagram
- eulerr
- UpSetR
- Plotly
- gprofiler2
- visNetwork
- igraph
- ggplot2
- DT

Please consult the respective package documentation and citation information when using GenoCeptR in publications.

---

# Quick Access

| Resource | Link |
|---|---|
| 🌐 **Online — No installation** | https://dinuka-genoceptr.share.connect.posit.cloud/ |
| 💻 **GitHub source code** | https://github.com/Dinuka0001/GenoCeptR |
| 🐳 **Docker / GHCR** | https://github.com/Dinuka0001/GenoCeptR/pkgs/container/genoceptr |

### ⭐ Recommended

**For immediate use:**  
🌐 Open the [GenoCeptR Online Application](https://dinuka-genoceptr.share.connect.posit.cloud/)

**For local reproducible use:**  
🐳 Pull the Docker image:

```bash
docker pull ghcr.io/dinuka0001/genoceptr:latest
```

**For development:**  
💻 Clone the GitHub repository:

```bash
git clone https://github.com/Dinuka0001/GenoCeptR.git
```

---

## GenoCeptR

**Gene Expression Overlap & Pathway Enrichment Analysis**

Analyze → Compare → Visualize → Enrich

