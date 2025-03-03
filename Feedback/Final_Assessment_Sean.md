
# Final Assessment for Sean

- The overall project structure adhered to the guidelines of CMEE coursework. Weekly directories (e.g., `week1`, `week2`, etc.).
- *Florida practical report was missing*
- README files were included in the main directory and weekly directories. The main README was updated with additional details about the repository and its structure over the weeks. 
- However, the weekly README files lacked sufficient details in some cases, especially about individual scripts. 
- The `.gitignore` file was progressively updated but could still exclude more unnecessary files (e.g., logs and temporary files).

## Week 1
- Scripts were basic but functional, showing an understanding of shell scripting fundamentals.
- Issues noted:
  - The `CountLines.sh` script had a parameter-handling issue, which should be resolved using proper error handling.
  - Redundant or unused code fragments were found in some scripts.
- Generated PDFs from LaTeX were present in the repository, unnecessarily increasing its size.

## Week 2
- Docstrings and comments were inconsistent. Some scripts lacked module-level or function-level docstrings entirely.
- Scripts threw some warnings about missing files (e.g., `Resource.csv`).
 - You could have formatted the output of certain scripts to be  more neat / organised / informative (compare with my solutions) -- for example `lc1.py` is perfectly functional, but the formatting of the output could have been improved.

## Week 3
- Scripts were functionally accurate, and visualizations generated matched their intended purpose.
- However, some warnings regarding missing files (e.g., `../Data/Resource.csv`) persisted.
- The R script for Florida weather autocorrelation was functional but lacked modularity. Functions should be used to encapsulate repetitive code for clarity. Sampling in a loop was unnecessary - see solution scipt.
- The LaTeX report was  massing.
- Your Groupwork practicals were all in order, and your group did well in collaborating on it based on the commit/merge/pull history. Check the groupwork feedback pushed to your group repo for more details.   
  - The Autocorrelation practical code was efficient, providing a correct answer to the question. The  provided statistical and biological/ecological interpretations in the report could have been stronger; had a somewhat weak conclusion at the end.

## Git Practices
- Commits were frequent but lacked descriptive messages in many instances (e.g., "Final push"). Detailed commit messages improve traceability.
- Large binary files (e.g., PDFs) were committed, unnecessarily inflating the repository size (125 MB by Week 4).
- Proper branching was not evident, which could have streamlined collaboration.

## Overall Assessment

Overall, OK job. 

Code readability improved with the inclusion of docstrings and comments, though consistency remained an issue.

Also a key omission - the florida practical writeup.

Some of your scripts retained fatal errors which could nave been easily fixed - work on being more vigilant and persistent in chasing down errors the future.

It was a tough set of weeks, but I hope they have given you a great start towards further training, a quantitative masters dissertation, and ultimately a career in quantitative biology! 


### (Provisional) Mark
 *58*