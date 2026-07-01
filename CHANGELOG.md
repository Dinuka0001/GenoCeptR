# Changelog

All notable changes to GenoCeptR are documented in this file.

## [3.0.0] - 2026-07-01

Full modular rewrite of the application, plus a new pathway enrichment
workspace.

### Added
- **Pathway Analysis tab**: over-representation analysis via `gprofiler2::gost()`,
  with bar/dot/lollipop plots, dendrogram tree view, and gene–pathway network
  view (static + interactive HTML), all with dedicated export handlers.
- **Gene Direction filter** for pathway analysis, restricting the query gene
  list to up- or down-regulated genes based on the mapped log2FC column.
- **Interactive Plotly Venn diagram** with custom hover detail panel.
- **Colorblind-friendly palette presets** for Venn/Euler/UpSet and pathway plots.
- Modular codebase split into `modules/` (UI + server) and `utils/` (shared
  logic), replacing the single-file monolithic app.
- `bslib::page_sidebar()` based UI redesign with a card-based layout and a
  custom blue theme.

### Fixed
- Duplicate gene ID rows within a dataset now correctly aggregate adjusted
  p-value (minimum) and log2FC (mean) across **all** matching rows instead of
  arbitrarily using only the first match (gene table and overlap CSV exports).
- `gprofiler2::gost()` numeric namespace parameter was not respecting the
  "Gene ID" vs "Gene Symbol" input selector; gene ID queries now correctly use
  `ENTREZGENE_ACC`.
- Removed dead/unwired "Gene Selection Criteria" controls (Statistical Test,
  Fold Change Type) that had no effect on analysis.
- Fixed an off-by-one bug in pathway network edge calculation
  (`calculate_pathway_edges()`) that could break edge/network exports when
  only one enriched pathway was returned.
- All Pathway Analysis download handlers (plot/table/tree/network/edges/nodes)
  now guard against missing results and report errors via notification instead
  of failing silently or crashing.
- Standardized graphics-device cleanup (`on.exit(dev.off())`) across all
  diagram download handlers (Venn/Euler/UpSet/Edwards) to prevent leaked
  devices on error.

### Changed
- Reorganized sidebar into a dedicated "Data Input" workspace tab, separate
  from visualization and analysis output tabs.

---

## [2.1.0] - 2025

- Introduced `bslib` navigation-rail UI redesign proposal and custom blue
  color scheme.
- Added Euler diagrams (`eulerr`) and UpSet plots (`UpSetR`) as alternatives
  to classic Venn diagrams.
- Began modularizing the codebase (`ui_styles.R`, utility extraction).

## [2.0.0] - 2025

*(Released separately as "DE Venn Explorer" — https://github.com/Dinuka0001/DE-Venn-Explorer)*

- Support for 2–5 dataset overlap analysis with file upload or pasted gene
  list input modes.
- Added Sankey diagrams (`networkD3`) and gene direction filtering
  (up/down-regulated).
- Automatic column detection for uploaded DE result files.
- Increased upload size limit to 50 MB.

## [1.0.0] - 2024

- Initial standalone GSEA application (`GSEA app V1.R`): DESeq2 result upload,
  column mapping, and enrichment analysis (GO, KEGG, Reactome, Hallmark) via
  `clusterProfiler` / `ReactomePA` / `msigdbr`.
