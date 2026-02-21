--- simplified
Important rules to remember: Project name is Img Factory 1.6

1. To avoid confusion, the file must include #this belongs in [dir]/ [filename] or goes in root /[filename] - Version: [number] of updates to file, Keep the existing file header comments. "X-Seti - $MONTH$DATE 2025 -" rest of the header information.

Example: 
# X-Seti - August 14 2025 - IMG Factory 1.6 - COL Table Population Methods
"""
COL Table Population Methods - Handles populating the main table widget with COL file data.
"""

List all def functions in alphabetical order, and class sections, like for example:

##Methods list -
# _load_col_file
# _populate_col_table_enhanced
# _setup_col_tab
# setup_col_tab_integration
# _setup_col_table_structure
# _update_col_info_bar_enhanced
# _validate_col_file

##class COLParser: -
#__init__
# def log
# _is_multi_model_archive

Each method gets its own #vers [number]. Increase the version number so we can keep track of method changes.

Example: 
def validate_col_file(main_window, file_path): #vers 1

2. The header's file name [filename] must match the save file. Keep filenames simple and unchanged. Avoid using words like 
   "Enhanced", 
   "Fallback", 
   "Improved", 
   "Fixed", 
   "Fix", 
   "Patch", 
   "Patched",
   "Updated",
   "Integrated", 
   "Clean"
Anywhere in the file, filename, method or functions. This will avoid confusion and file function duplication. Each set of related functions has its file.

3. Shared functions go in methods/ 
Themes .json files go in themes/
Core important /single-use functions go in core/ 
Editors go in components/ 
and GUI-related functions go in gui/
No emojis, only SVG-generated icons. 
 
4. "CRITICAL: When fixing bugs, you must preserve 100% of the original functionality. Do not simplify. Also, check for the original file first before creating a fix or update.

5. No patch files, check for duplicate functions and give a warning, suggest removal of duplicates, consolidate functions that can be shared and placed into the methods/ folder.

5. No patch or quick fix files! - Lots of patch files can make it hard to find problems, each file with a simple name indicating its functions. 

6. On the "Continue" prompt,  only make edits to complete the script.

7. Keep all replies short and to the point, as we have limited data on our pay plan.

8. No Conflicts: Keep track of functions in the project files. No duplicate functions. Check existing files and functions first before creating newer functions. 

9.  No fallback code - Works or doesn't work - no middle ground

10. Always ask first before creating files, suggesting ideas, but let me decide.

11. Read the Changelog file. List of TODO and functions fixed.

12. Finally, use the sed command to make changes to files (small fixes only), instead of recreating the file, being mindful of bandwidth and session limits. 

13. For large fixes, show the full fixed method; just tell me the file to swap the method.

14. The FIX_SUMMARY.md, update any file changes in the ChangeLog file (Keeping the same style and format), No lists, No summary files, show anything in the artefact.

15. I have limited bandwidth, so keep replies short and to the point.

16. No guessing, check you have access to GitHub repos or ask for the files first, before anything else is done. And confirm this with me.

## Workflow
- Clone the repo first, confirm access before any work
- Use the Claudia token to push directly to GitHub
- Syntax-check Python files before pushing: python3 -c "import ast; ast.parse(open('file.py').read()); print('OK')"
- sed for small fixes, full method shown for large fixes
- Always pull before push to avoid conflicts
- Confirm the plan with the user before making changes
- Keep replies short, no walls of text
- No artifacts, no summary files - changes go straight to GitHub
