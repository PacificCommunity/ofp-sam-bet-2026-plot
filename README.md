# BET 2026 Results

<p align="right">
  <a href="kflow.yaml"><img src="kflow-ready.svg" alt="Kflow ready task"></a>
</p>

Kflow task for building report-ready BET 2026 figures, tables, captions, and
QMD section seeds from upstream model payloads.

## Workflow Role

```text
ofp-sam-bet-2026-stepwise -> ofp-sam-bet-2026-results -> ofp-sam-bet-2026-report
```

This task reads one or more upstream artifacts containing `model_payload.rds`,
builds the report figure/table bundle, and passes that bundle to the report
task.

The MFCL Shiny local app can now act as a curation layer. Open the app from a
results job, adjust model selections, overlays, facets, uncertainty controls,
and figure/table inclusion, then save `report-selection.json`. A follow-up
results run can read that small selection file with `PLOT_REPORT_SELECTION` or
`MFCLSHINY_REPORT_SELECTION_FILE` and rebuild the report-ready outputs from the
same Shiny state.

The app runs in local Docker on your computer. Kflow reads the needed model
payloads from the submitter over SSH instead of starting Shiny on the submitter.

## Main Outputs

```text
outputs/figures/
outputs/tables/
outputs/figure-index.csv
outputs/table-index.csv
outputs/report-selection.json
outputs/analysis-manifest.json
outputs/report-ready/figures.qmd
outputs/report-ready/tables.qmd
outputs/report-ready/report-map.html
outputs/plot-report.qmd
outputs/_review/plot-report.qmd
outputs/_review/report-map.html
```

Open `outputs/report-ready/report-map.html` to browse the generated assets. The
selection file controls inclusion, main/appendix placement, captions, and the
captured Shiny input state. The report repo still receives editable QMD section
seeds for final manual wording.

`analysis-manifest.json` is deliberately small: it records available analysis
layers such as model runs, likelihood profiles, Hessian checks, jitter, and
self-tests without copying large seed-level objects. Heavy artifacts stay in
their upstream outputs and are loaded only when a plot needs them.

The large self-contained `plot-report.html` review is off by default. Enable it
only for a one-off review run with `PLOT_RENDER_REVIEW_HTML=true`.

## Figure Size

Normal Kflow runs optimize generated images for smaller artifacts and smaller
reports. PNGs remain the fallback; WebP/JPEG sidecars are used where they help
HTML or PDF output.

## Run

Kflow runs:

```bash
bash run.sh
```

For local testing, put upstream model payload artifacts under `inputs/`, then
run the same command.

## Common Kflow Config

| Field | Typical value | Purpose |
| --- | --- | --- |
| `TRIGGER_NEXT` | `true` | Launch the report task after results complete. |
| `FLOW_GROUP` | `bet-2026-base` | Shared label for one stepwise/results/report chain. |
| `PLOT_MAX_FISHERIES` | `18` | Limit fishery-level diagnostic plots per plot family. |
| `PLOT_REPORT_SELECTION` | `report-selection.json` | Optional Shiny curation manifest to replay before rendering. |
| `PLOT_RENDER_REVIEW_HTML` | `false` | Render the large review HTML. Keep false for normal runs. |
| `PLOT_OPTIMIZE_FIGURES` | `true` | Optimize generated plot files. |
| `PLOT_PNGQUANT_QUALITY` | `60-85` | Lossy PNG quality range when `pngquant` is available. |
| `PLOT_WEBP_QUALITY` | `72` | WebP sidecar quality for HTML. |
| `PLOT_JPEG_QUALITY` | `82` | JPEG sidecar quality for PDF rendering. |
| `KFLOW_RUNTIME_PACKAGES` | `mfclshiny=PacificCommunity/mfclshiny@...` | mfclshiny version used for plot generation. |
| `MFCLSHINY_SELECTION_PUBLISH_CMD` | unset | Optional local-app hook for saving and publishing a selection to the next curated layer. |
