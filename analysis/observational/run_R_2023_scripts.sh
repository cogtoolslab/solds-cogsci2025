#!/bin/bash

set -euo pipefail

# Manually specify the .R scripts to run, in desired order
SCRIPTS=(
  #"01_survey_data_processing.R"
  "02_eoc_data_processing.R"
  "03_engagement_processing.R"
  #"04_merge_data_sources.R"
  #"05_analysis.R"
)

# Execute each script in order
for script in "${SCRIPTS[@]}"; do
  if [[ -f "$script" ]]; then
    echo "Running $script..."
    Rscript "$script"
    echo "$script completed successfully."
  else
    echo "Warning: $script not found or not a regular file, skipping."
  fi
  echo "-----------------------------"
done

echo "All tasks done."
