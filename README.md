# BET 2026 Results

<p align="right">
  <a href="kflow.yaml"><img src="kflow-ready.svg" alt="Kflow ready task"></a>
</p>

Kflow task repository for BET 2026 report-ready figures, tables, and report
section seeds.

This repository is Kflow-ready: Kflow discovers and runs it from
[`kflow.yaml`](kflow.yaml), consumes upstream model payload artifacts, and
passes report-ready figures, tables, QMD seeds, and provenance to the report
task.

This task reads one or more upstream model payload jobs, finds
`model_payload.rds` files, and writes the result bundle:

- `outputs/figures/*.png`
- `outputs/tables/*.csv`
- `outputs/figure-index.csv`
- `outputs/table-index.csv`
- `outputs/report-ready/figures.qmd`
- `outputs/report-ready/tables.qmd`
- `outputs/report-ready/report-map.html`
- `outputs/plot-report.qmd` and `outputs/_review/plot-report.qmd` for
  detailed review source

The `_review/` folder intentionally duplicates the QMD review source and the
report-ready map so useful review files are easy to find in Kflow artifacts
even when the figure bundle is large. The large `plot-report.html` review is
not rendered by default; set `PLOT_RENDER_REVIEW_HTML=true` only for a one-off
interactive review run.

The results bundle is designed to be consumed directly by
`ofp-sam-bet-2026-report`. The report job copies the generated assets under
`generated/outputs/` and seeds `sections/Figures.qmd` and `sections/Tables.qmd`
only when those files are missing, so manual report edits are preserved. The
report repository commits only the report-ready QMD, referenced figure/table
files, and provenance metadata. Results artifacts keep the light report map and
QMD review source; the large review HTML is opt-in.

Open `outputs/report-ready/report-map.html` to browse generated figures and
tables without duplicating all images into a large self-contained HTML file.
To change report content, edit the seeded QMD files in the report
repository: remove blocks, reorder blocks, move appendix material, or rewrite
captions there.

## Common Job Config

These fields are the useful ones to change from Kflow:

| Field | Example | Meaning |
| --- | --- | --- |
| `TRIGGER_NEXT` | `false` | Build results only; do not launch the report task. |
| `FLOW_GROUP` | `bet-2026-base` | Short label shared by one results/report chain. |
| `JOB_TITLE` | `BET results` | Human title shown in Kflow. |
| `PLOT_TITLE` | `BET 2026 report-ready figures` | Detailed review title. |
| `PLOT_MAX_FISHERIES` | `18` | Maximum fishery-level diagnostics to export per registered family. |
| `PLOT_RENDER_REVIEW_HTML` | `false` | Render the large self-contained `plot-report.html` review. Keep false for normal Kflow runs. |
| `FLOW_SPECIES` | `BET` | Species code passed into captions and report config. |
| `FLOW_SPECIES_LABEL` | `bigeye tuna` | Species label passed into captions and report config. |
| `FLOW_ASSESSMENT_YEAR` | `2026` | Assessment year passed into captions and report config. |
| `REPORT_QMD` | `assessment-report.qmd` | Report entrypoint to use if the downstream report task runs. |
| `REPORT_FILE_STEM` | `bet-2026-report` | Output filename stem for the downstream report task. |
| `PLOT_OPTIMIZE_FIGURES` | `true` | Optimize generated plot files for smaller artifacts and reports. |
| `PLOT_PNGQUANT_QUALITY` | `60-85` | Lossy PNG quality range when `pngquant` is available. |
| `PLOT_WEBP_QUALITY` | `72` | WebP sidecar quality for HTML. |
| `PLOT_JPEG_QUALITY` | `82` | JPEG sidecar quality for PDF rendering. |
