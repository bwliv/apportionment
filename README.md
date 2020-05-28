Congressional apportionment simulations for Columbia Journalism School's NewsCounts project, finding how state population swings (more specifically, census undercounts) could have gained/lost states additional seats

Analysis was conducted outside of markdown document due to running time, all source files included here.

This is a static simulation - it does not need to be rerun with updated data.

For the full analysis and summary: bwliv.github.io/apportionment

Guide to files below - when two files are noted, one uses 2010 population data, while the other uses 2018 American Community Survey estimates to roughly estimate 2020 population

# statepops2010 + 2018ACS
aforementioned population data from 2010 and 2018 (which will be used as estimation for 2020)

# apportioning + apportioning2018nums
source code for deterministic simulations generating estimates for changes state undercount needed to gain or lose house seats

# seat_loss_gain + seat_loss_gain_2018
results from the above

# calculatetotals + calculatetotals2020
generates summaries using these numbers, yielding the below pairs of tables

# seatlosses + seatlosses2018
estimates for population (i.e. census count) change needed to gain/lose seat

# seatlosses_per + seatlosses_per2018
same as above, but as percentage of state population

# state_pers_within_uc + state_pers_within_uc2018
finds states that could have gained/lost a seat because of undercount of lack thereof

# apportionment
markdown/project for these files

# undercount
undercounting data for the 2010 census (see source files for original .pdf source)

# archive_source_and_draft_files
used to generate data in main repo