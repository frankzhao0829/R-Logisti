## Load Egoras Raw Data Files into DB from Remote

The `egoras_load.sql` script can only load data files when they are located in the DB server. To upload the data files into DB from remote. Here is the steps:

1. Add Version column manually (TODO, add a screenshot maybe)

2. Use BCP utility to upload the csv/tsv files to the DB

   - For CSV files:

   ```
   bcp DB.DBO.TABLENAME in "PATH_TO_RAW_DATA_FILE.csv" -S YOURDBHOST -t, -T -c -F2
   ```
  
   - For TSV (TXT) files:

   ```
   bcp DB.DBO.TABLENAME in "PATH_TO_RAW_DATA_FILE.csv" -S YOURDBHOST -t\t -T -c -F2
   ```
