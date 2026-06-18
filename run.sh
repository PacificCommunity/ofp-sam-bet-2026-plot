#!/usr/bin/env bash
set -euo pipefail

OUT_DIR="${OUTPUT_DIR:-outputs}"
INPUT_DIR="${INPUT_DIR:-inputs}"
ROOT="$(pwd)"

runtime_packages_enabled() {
  case "${KFLOW_RUNTIME_PACKAGES:-}" in
    ""|0|false|FALSE|no|NO|off|OFF|none|NONE|skip|SKIP) return 1 ;;
    *) return 0 ;;
  esac
}

prepare_runtime_packages() {
  runtime_packages_enabled || return 0
  export R_LIBS_USER="${R_LIBS_USER:-${ROOT}/.R-library}"
  export KFLOW_RUNTIME_LIBRARY="${KFLOW_RUNTIME_LIBRARY:-${R_LIBS_USER}}"
  export KFLOW_RUNTIME_STATE_DIR="${KFLOW_RUNTIME_STATE_DIR:-${ROOT}/.kflow-runtime-cache}"
  mkdir -p "${R_LIBS_USER}" "${KFLOW_RUNTIME_STATE_DIR}"
  if [[ -x /usr/local/bin/30-update-kflow-runtime-packages ]]; then
    bash /usr/local/bin/30-update-kflow-runtime-packages
  else
    echo "[kflow-runtime-update] Runtime updater not found; using bundled packages." >&2
  fi
}

mkdir -p "${OUT_DIR}" "${INPUT_DIR}"

echo "BET plot task"
echo "Input directory: ${INPUT_DIR}"
echo "Output directory: ${OUT_DIR}"

prepare_runtime_packages
Rscript R/build_plots.R
