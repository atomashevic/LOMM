# Benchmark Reporting Policy Summary

Input directories: benchmark_outputs_revision_dynega, benchmark_outputs_revision_graphicalvar, benchmark_outputs_revision_gimme

Rows: 3960

Policy: DynEGA and GraphicalVAR are the main full-grid analysis. GIMME is reported only as a bounded sensitivity and feasibility comparator with explicit timeout accounting.

## Main Full-Grid Analysis

This section excludes GIMME by policy.

### By Method

method_compare | method_family | community_algorithm | uni_method | n_conditions | n_ok | n_timeout | n_error | success_rate | exact_rate_all | exact_rate_ok | cor_mean | nrmse_mean | assignment_mean | runtime_mean
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
DynEGA[louvain|expand] | dynEGA | louvain | expand | 1920 | 1920 | 0 | 0 | 1 | 0.973958333333333 | 0.973958333333333 | 0.654441753679275 | 0.867271441800875 | 0.980938946759259 | 3.60818177083333
GraphicalVAR | graphicalVAR | louvain | NA | 1920 | 1872 | 48 | 0 | 0.975 | 0.872916666666667 | 0.895299145299145 | 0.649436706529485 | 0.864844120134706 | 0.94836553300095 | 191.398286979167

### By Method Fit

fit_method_compare | method_family | n_fits | n_ok_fits | n_timeout_fits | n_error_fits | fit_success_rate | fit_runtime_mean | fit_runtime_median | fit_runtime_max
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
DynEGA[louvain|expand] | dynEGA | 1920 | 1920 | 0 | 0 | 1 | 3.60818177083333 | 3.12999999999988 | 12.028
GraphicalVAR structure fit | graphicalVAR | 1920 | 1872 | 48 | 0 | 0.975 | 191.398286979167 | 142.3445 | 775.729

### By Mode And n

method_compare | scenario_mode | scenario_nfac | n_conditions | n_ok | n_timeout | n_error | success_rate | exact_rate_all | cor_mean | nrmse_mean | runtime_mean
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
DynEGA[louvain|expand] | high_single_stable | 2 | 160 | 160 | 0 | 0 | 1 | 1 | 0.772842658683024 | 0.842305331817685 | 2.6448125
DynEGA[louvain|expand] | high_single_stable | 3 | 160 | 160 | 0 | 0 | 1 | 1 | 0.660090784587819 | 0.967357446936286 | 2.99034374999999
DynEGA[louvain|expand] | high_single_stable | 4 | 160 | 160 | 0 | 0 | 1 | 1 | 0.568450359527468 | 1.00734431665152 | 3.47991249999998
DynEGA[louvain|expand] | high_switch_stable | 2 | 160 | 160 | 0 | 0 | 1 | 1 | 0.695811446527149 | 0.742361904087121 | 2.65723125
DynEGA[louvain|expand] | high_switch_stable | 3 | 160 | 160 | 0 | 0 | 1 | 1 | 0.662219939940424 | 0.816265183257266 | 3.297275
DynEGA[louvain|expand] | high_switch_stable | 4 | 160 | 160 | 0 | 0 | 1 | 1 | 0.645147942006963 | 0.846626004651045 | 4.19489999999999
DynEGA[louvain|expand] | high_switch_volatile | 2 | 160 | 160 | 0 | 0 | 1 | 1 | 0.839261200181628 | 0.510764569734296 | 3.89147499999999
DynEGA[louvain|expand] | high_switch_volatile | 3 | 160 | 160 | 0 | 0 | 1 | 1 | 0.795198730789253 | 0.578903966559613 | 3.53951874999999
DynEGA[louvain|expand] | high_switch_volatile | 4 | 160 | 160 | 0 | 0 | 1 | 1 | 0.778404129664814 | 0.597062000516485 | 4.35179375000001
DynEGA[louvain|expand] | low_single_stable | 2 | 160 | 160 | 0 | 0 | 1 | 0.98125 | 0.540520568113789 | 1.09467612002045 | 2.54214375
DynEGA[louvain|expand] | low_single_stable | 3 | 160 | 160 | 0 | 0 | 1 | 0.91875 | 0.477376999457261 | 1.17923151045863 | 3.73341875
DynEGA[louvain|expand] | low_single_stable | 4 | 160 | 160 | 0 | 0 | 1 | 0.7875 | 0.417976284671708 | 1.22435894692011 | 5.97535625
GraphicalVAR | high_single_stable | 2 | 160 | 160 | 0 | 0 | 1 | 1 | 0.772842658683024 | 0.842305331817685 | 114.8001
GraphicalVAR | high_single_stable | 3 | 160 | 160 | 0 | 0 | 1 | 1 | 0.660090784587819 | 0.967357446936286 | 213.38839375
GraphicalVAR | high_single_stable | 4 | 160 | 146 | 14 | 0 | 0.9125 | 0.9125 | 0.565597799663564 | 1.01360818567264 | 403.74275625
GraphicalVAR | high_switch_stable | 2 | 160 | 160 | 0 | 0 | 1 | 1 | 0.695811446527149 | 0.742361904087121 | 90.76931875
GraphicalVAR | high_switch_stable | 3 | 160 | 160 | 0 | 0 | 1 | 1 | 0.662219939940424 | 0.816265183257266 | 182.58584375
GraphicalVAR | high_switch_stable | 4 | 160 | 154 | 6 | 0 | 0.9625 | 0.9625 | 0.642691579588895 | 0.850626808422384 | 306.3104625
GraphicalVAR | high_switch_volatile | 2 | 160 | 160 | 0 | 0 | 1 | 1 | 0.839261200181628 | 0.510764569734296 | 88.21530625
GraphicalVAR | high_switch_volatile | 3 | 160 | 160 | 0 | 0 | 1 | 1 | 0.795198730789253 | 0.578903966559613 | 175.82804375
GraphicalVAR | high_switch_volatile | 4 | 160 | 132 | 28 | 0 | 0.825 | 0.825 | 0.757549011001408 | 0.632817556412908 | 345.6026875
GraphicalVAR | low_single_stable | 2 | 160 | 160 | 0 | 0 | 1 | 0.75625 | 0.532669883686609 | 1.08404918165036 | 54.2968
GraphicalVAR | low_single_stable | 3 | 160 | 160 | 0 | 0 | 1 | 0.51875 | 0.461825752146771 | 1.14525013983551 | 94.39245625
GraphicalVAR | low_single_stable | 4 | 160 | 160 | 0 | 0 | 1 | 0.5 | 0.418812498228822 | 1.16569822512444 | 226.847275

## GIMME Feasibility And Sensitivity

GIMME rows share one structure fit across community variants; fit-level tables avoid counting the same expensive fit as independent variant runs.

### Row-Level Status By Variant

method_compare | method_family | community_algorithm | uni_method | n_conditions | n_ok | n_timeout | n_error | success_rate | exact_rate_all | exact_rate_ok | cor_mean | nrmse_mean | assignment_mean | runtime_mean
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
GIMME[louvain] | gimme | louvain | NA | 120 | 21 | 96 | 3 | 0.175 | 0.158333333333333 | 0.904761904761905 | 0.613587652154806 | 0.944304468782412 | 0.96031746031746 | 364.270241666667

### Fit-Level Status

fit_method_compare | method_family | n_fits | n_ok_fits | n_timeout_fits | n_error_fits | fit_success_rate | fit_runtime_mean | fit_runtime_median | fit_runtime_max
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
GIMME structure fit | gimme | 120 | 21 | 96 | 3 | 0.175 | 364.270241666667 | 360.5675 | 600.586

### Fit-Level Status By Scenario

scenario | scenario_mode | scenario_nfac | N | TimeP | NVAR | Error | Q | Loadings | n_fits | n_ok_fits | n_timeout_fits | n_error_fits | fit_success_rate | fit_runtime_mean | fit_runtime_median | fit_runtime_max
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
high_single_stable_2d | high_single_stable | 2 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 0 | 10 | 0 | 0 | 360.6188 | 360.616 | 360.689
high_single_stable_3d | high_single_stable | 3 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 0 | 10 | 0 | 0 | 360.5768 | 360.5865 | 360.619
high_single_stable_4d | high_single_stable | 4 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 0 | 10 | 0 | 0 | 360.5603 | 360.5665 | 360.59
high_switch_stable_2d | high_switch_stable | 2 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 10 | 0 | 0 | 1 | 298.1225 | 299.564 | 342.696
high_switch_stable_3d | high_switch_stable | 3 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 0 | 10 | 0 | 0 | 360.5979 | 360.6005 | 360.613
high_switch_stable_4d | high_switch_stable | 4 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 0 | 10 | 0 | 0 | 360.5658 | 360.575 | 360.61
high_switch_volatile_2d | high_switch_volatile | 2 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 0 | 10 | 0 | 0 | 360.5132 | 360.544 | 360.548
high_switch_volatile_3d | high_switch_volatile | 3 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 0 | 10 | 0 | 0 | 360.5377 | 360.5445 | 360.564
high_switch_volatile_4d | high_switch_volatile | 4 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 0 | 10 | 0 | 0 | 360.5502 | 360.5515 | 360.571
low_single_stable_2d | low_single_stable | 2 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 10 | 0 | 0 | 1 | 248.1367 | 258.2215 | 299.294
low_single_stable_3d | low_single_stable | 3 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 1 | 6 | 3 | 0.1 | 435.8678 | 600.5445 | 600.586
low_single_stable_4d | low_single_stable | 4 | 50 | 100 | 3 | 0.025 | 0.75 | 0.6 | 10 | 0 | 10 | 0 | 0 | 504.5952 | 600.522 | 600.586

## Legacy Combined Tables

The CSV files `full_suite_summary_by_method.csv`, `full_suite_summary_by_family_fit.csv`, and `full_suite_summary_by_mode_n.csv` are retained for audit/backward compatibility and include all methods.
