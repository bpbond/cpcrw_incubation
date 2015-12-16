# data

This folder contains raw data from the Picarro analyzer, field data on the cores, and notes and measurements by @apeyton.

* `CoreData.xlsx` - the spreadsheet that gets updated each time cores are weighed or measured. We then manually save the 'valvemap.csv' sheet to the `valvemap.csv` file.
* `cpcrw_cores.csv` - field data about the experimental cores: Date (sample date), Core (core code), Nearest_location (nearest location), MaxDepth_cm (depth of active layer at that location), MaxDepth_type (to permafrost or rock?), MaxCoreDepth_cm (max depth of core sampled), OrganicThickness_cm (thickness of organic layer), Odd (a boolean noting whether this core was 'odd' in some respect), Notes
* `removals.csv` - defines observations to be removed/excluded from the dataset. Data are "removed" because we know there's something wrong (e.g. analyzer was left running overnight); they're "excluded" because they're outliers or otherwise suspect. For more details see `2.5-remove.R`.
* `treatments.csv` - defines mapping of cores to treatments and incubation chambers (temperatures). These were randomly assigned at the beginning of the experiment and should never change.
* `valvemap.csv` - data on which Picarro valves were used to measure cores, and the mass of the cores, for each measurement. FIelds include Date and Time (PDT), Time_set_start_UTC (machine time, i.e. UTC, observation was started), Set (not really used), MPVPosition (Picarro valve number), Core (core measured, should match exactly to one in `treatments.csv`), Mass_g (core mass), Notes.
