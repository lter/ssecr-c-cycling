## Contributing Guidelines for the carbon cycling SSECR working group
This is for internal contributing dynamics between members of the C cycling group. anyone else .....
### Version Control & GitHub
We are working on separate scripts for our individual local data processing. 
Data analysts who are making the main script can take separate shifts. 
If someone is going to work on the main script on the side, create a branch and pull request requires the data analyst to review.

We have a data folder in a separate drive: [Google Drive](https://drive.google.com/drive/u/1/folders/1UQRLxuDRN9Qr8JCxVXwFKRl7qu-0MI4g)

Put the author name within your code.


As much as possible, use the style of "Conventional Commits" in your commit messages. See here for a nice summary of the highlights of that approach.


- For minor changes (< 5 min): (1) communicate with the team to avoid merge conflicts (2) make commits directly to main

- For major changes (> 5 min): (1) communicate with the team to avoid merge conflicts (2) open a branch with an intuitive but concise name (3) open a pull request when you are confident your edits are largely complete (4) request a review from a teammate (5) make any edits they suggest (6) merge the PR, and (7) delete the branch.

- For both: (1) communicate in the Whatsapp group chat when you expect to begin and end the change and (2) react to your Whatsapp message with a “👍” when you end your change.

### Style Guide

For folder/file names and in code, please follow the following style tips:

- Use all lowercase
- Related files/objects should have a shared file prefix (e.g., `wrangle_...`)
    - For files that have an inherent order, use zero-padded numbers as the prefix (e.g., `01_`, `02_`, etc.)
- Use underscores (`_`) for separating major pieces of information and hyphens (`-`) in lieu of spaces (e.g., `01_find-area.R`, `harmonize_spp-rich-info.py`, etc.)

Example: 00_Data.collection_v1.R 01_Stats.detrend_v1.R

Raw data should be organized by LTER site following the data template; and named as:
LTER_Description_YYYYstart-YYYYend_raw.csv

Gdrive structure: 
- Data/rawdata/LTER_Description_YYYYstart-YYYYend_raw.csv
	- Data/YX/side products.csv
	- Data/mastersheet.csv

Cleaned data can be organized in 1 sheet?...

### File Paths

When we write code--for ourselves or for working groups--we use **operating system (OS) agnostic file paths**.

In R, this means using the `file.path()` function (from base R) to stitch together elements (e.g., `file.path("data", "raw_2024.csv")`). In Python it means using the `join` function from the `path` module of the `os` library (e.g., `os.path.join("data", "raw_2024.csv")`).

If you plan on re-using a file path, assign it to an object/variable (R/Python) to avoid typos/errors when re-using the file path.
