Basic overview of the code framework:

AliasMatch_Main('Config.json') - runs a test

AliasMatch_Main - Main method, steps through 5 submethods that will eventually comprise the full suite of matching functionality.  Takes one argument, the path to a configuration file (currently JSON format)

- LoadLibrariesAndSourceFiles - Loads any relevant R packages and sources all of the necessary files.  If you add any new R files in the program flow, add the file and necessary packages to this to ensure proper sourcing at runtime.

- LoadData - Loads up the hierarchy, alias, personal alias, and data tables, and cuts down to the nameset_id specified in config.  For the moment, all files assumed to be csv.  

- FormatData - Preprocessing of string fields to ease later matching.  For now, converts all strings to lowercase.  In the future, could also preformat to address other common causes of mismatch - spaces and punctuation, doubled consonants, cardinal directions (N <-> north), # vs. word (3 <-> three).  

- MatchData - the main matching script.  Follows a fallthrough pattern to prevent rematching of already-matched names.  In the future, fallthrough could be broken when all names are matched, and the fallthrough code could be turned into a loop over function calls rather than inline calls.  Matching function implementations are in the file "MatchingFunctions.R".  For the moment, the code falls through as follows:
    Exact match to full dotname in hierarchy tables
    Exact match to full dotname in alias tables
    Sequential, top-down exact matching to alias tables (highest to lowest admin level).

Fuzzy matching should follow next in the fallthrough process.


SaveOutput - wrapper for all data saving operations.


Future work: Between MatchData and SaveOutput, there should be post-processing steps.  Produce report of matching performance, update the personal alias table, etc. 


If all have been matched, break from MatchData.
For Exact_TopDown_Match - 




The configuration file specifies the following fields:
FileSource: Currently, only "FromFiles" is implemented, the case when all necessary tables are accessed from file.  Future incarnations should include "FromDatabase" to pull all from database, and eventually be able to pull some tables from file and others from DB

Nameset_ID: The nameset_ID to use.  Only a single nameset_id is implemented now, though perhaps support for multiple nameset IDs could be added in the future.

Default Prefix: The hierarchy and name tables often encode higher admin levels than the data will (e.g., continent-level).  This attaches a default prefix to the dotnames that will be constructed from the data, to ensure matchability with the heirarchy/alias tables from the highest level.

Database: Place to eventually put all of the necessary info for DB access

Data_File_Columns: ordered list of the columns in the data file that store admin-level names, e.g. ["COUNTRY", "PROVINCE", "DISTRICT"] lets the software know that these three columns are the relevant names for matching, and that they are ordered in this way from highest to lowest admin level.

InputFiles: Self-explanatory. 

Output: Configures output files.  For the moment, has one member, "SaveFile", where the final output will be written as csv.  