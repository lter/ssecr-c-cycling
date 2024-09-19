## Contributing Guidelines for the carbon cycling SSECR working group

This is for internal contributing dynamics between members of C cycling group. anyone else ......
### Version Control & GitHub
We are working on separate scripts for our individual local data processing. Data analyst who is making the main script can make separate shifts.
we are having a data drive in a separate drive: Gdrive
naming conventions:
SLUG
00_Data.collection_v1.R
As much as possible, use the style of "[Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/)" in your commit messages. See [here](https://njlyon0.github.io/tips/commits.html) for a nice summary of the highlights of that approach.

For **minor changes**, (1) communicate with the team to avoid merge conflicts and (2) make commits directly to `main`

For **_major_ changes**, (1) communicate with the team to avoid merge conflicts, (2) open a branch with an intuitive but concise name, and (3) open a pull request when you are confident your edits are largely complete. (4) Request a review from a teammate, (5) make any edits they suggest, (6) merge the PR, and (7) delete the branch.

### Style Guide

For folder/file names and in code, please follow the following style tips:

- Use all lowercase
- Related files/objects should have a shared file prefix (e.g., `wrangle_...`)
    - For files that have an inherent order, use zero-padded numbers as the prefix (e.g., `01_`, `02_`, etc.)
- Use underscores (`_`) for separating major pieces of information and hyphens (`-`) in lieu of spaces (e.g., `01_find-area.R`, `harmonize_spp-rich-info.py`, etc.)

### File Paths

When we write code--for ourselves or for working groups--we use **operating system (OS) agnostic file paths**.

In R, this means using the `file.path()` function (from base R) to stitch together elements (e.g., `file.path("data", "raw_2024.csv")`). In Python it means using the `join` function from the `path` module of the `os` library (e.g., `os.path.join("data", "raw_2024.csv")`).

If you plan on re-using a file path, assign it to an object/variable (R/Python) to avoid typos/errors when re-using the file path.
