# Word Caption Fix Instructions

## Problem
When Quarto renders to DOCX format, figure and table captions are created as
normal text paragraphs rather than proper Microsoft Word Caption objects. This
causes the List of Figures (LOF) and List of Tables (LOT) to appear blank.

## Solution
Use the provided VBA macro to automatically convert all Quarto-generated
captions to proper Word Caption objects.

## Step-by-Step Instructions

### 1. Open the Document
- Open `final-report_2025-09.docx` in Microsoft Word

### 2. Access the VBA Editor
- Press **Alt+F11** (Windows) or **Fn+Option+F11** (Mac) to open the VBA Editor

### 3. Create a New Module
- In the VBA Editor, go to **Insert > Module**
- A new blank code window will appear

### 4. Import the Macro Code
- Open `ConvertQuartoCaptionsToWord.bas` in a text editor
- Copy the entire contents
- Paste into the new module window in VBA Editor

### 5. Run the Main Macro
- In the VBA Editor, place your cursor anywhere within the `ConvertAllCaptions` subroutine
- Press **F5** or click the **Run** button (green play icon)
- The macro will process all captions and display progress in the status bar

### 6. Review Results
- A message box will appear showing:
  - Number of figure captions converted
  - Number of table captions converted
  - Time elapsed
- Click **OK**

### 7. Verify the Fix
- Close the VBA Editor (Alt+Q)
- Scroll to the List of Figures (after Table of Contents)
- Scroll to the List of Tables
- Both should now be populated with proper entries

### 8. Save the Document
- Save the document: **Ctrl+S** (Windows) or **Cmd+S** (Mac)
- The captions are now permanent Word Caption objects

## Troubleshooting

### If LOF/LOT are still blank:
1. Right-click on "List of Figures" or "List of Tables"
2. Select **Update Field**
3. Choose **Update entire table**

### If some captions weren't converted:
Run the troubleshooting macro to identify missed captions:
1. In VBA Editor, run `ListAllCaptionCandidates`
2. Press **Ctrl+G** to view the Immediate Window
3. Review the list of detected caption candidates
4. Manually convert any missed captions:
   - Select the caption text
   - Go to **References > Insert Caption**
   - Choose appropriate label (Figure or Table)
   - Paste the caption text

### If you want to convert only figures or only tables:
Instead of running `ConvertAllCaptions`, run:
- `ConvertFigureCaptionsOnly` - converts only figure captions
- `ConvertTableCaptionsOnly` - converts only table captions

## Technical Details

### What the Macro Does
1. **Scans all paragraphs** in the document looking for caption-like text
2. **Identifies figure captions** by detecting:
   - Keywords: "Figure", "Map", "Screenshot", "Plot"
   - Proximity to images
   - Small font sizes typical of captions
   - Caption or Figure Caption paragraph styles
3. **Identifies table captions** by detecting:
   - Paragraphs immediately preceding Word tables
   - Keywords: "Table", specific table titles from your report
   - Colon-prefixed text (`:` from Quarto markdown tables)
4. **Cleans caption text** by removing:
   - Quarto cross-reference labels like `{#fig-label}` or `{#tbl-label}`
   - Existing numbering like "Figure 1:" or "Table 2:"
   - Extra whitespace and line breaks
5. **Creates proper Word Captions** using `Selection.InsertCaption`
6. **Updates all fields** to refresh cross-references
7. **Regenerates LOF and LOT** to populate the lists

### Caption Detection Patterns

**Figure Captions:**
- Contains "Figure", "Map", "Screenshot", or "Plot"
- Has nearby inline shapes (images)
- Uses Caption or Figure Caption style
- Has smaller font size (< 11pt) near images
- Less than 500 characters (excludes body text)

**Table Captions:**
- Immediately precedes a Word table object
- Contains "Table" or specific report table titles
- Starts with colon (`:` from Quarto)
- Less than 300 characters

## Notes
- The macro is non-destructive: it only converts plain text to Caption objects
- Original text content is preserved exactly (minus Quarto artifacts)
- Numbering is automatically managed by Word's Caption system
- You can safely re-run the macro if needed (it will skip existing captions)
- The VBA code remains in the document until you remove it from the VBA Editor

## Expected Results

### Before Running Macro:
- List of Figures: blank
- List of Tables: blank
- Captions appear as normal text paragraphs

### After Running Macro:
- List of Figures: populated with all 13 figures
- List of Tables: populated with both tables
- Captions are proper Word Caption objects
- Cross-references work correctly throughout document
- Automatic renumbering if figures/tables are reordered
