' Attribute VB_Name = "ConvertQuartoCaptionsToWord"
'================================================================================
' ConvertQuartoCaptionsToWord.bas
'
' PURPOSE: Convert Quarto-generated figure and table captions to proper
'          Microsoft Word Caption objects, then regenerate List of Figures
'          and List of Tables.
'
' USAGE:
'   1. Open final-report_2025-09.docx in Microsoft Word
'   2. Press Alt+F11 to open VBA Editor
'   3. Insert > Module
'   4. Copy and paste this entire code
'   5. Run the macro: ConvertAllCaptions
'   6. Save and close document
'
' AUTHOR: Claude Code
' DATE: 2025-01-04
'================================================================================

Option Explicit

' Main subroutine - Run this one
Sub ConvertAllCaptions()
    Dim startTime As Double
    Dim endTime As Double
    Dim figCount As Integer
    Dim tblCount As Integer

    startTime = Timer

    ' Disable screen updating for speed
    Application.ScreenUpdating = False
    ' Application.DisplayStatusBar = True

    StatusBar = "Converting Quarto captions to Word captions..."

    ' Convert figures first
    figCount = ConvertFigureCaptions()

    ' Convert tables second
    tblCount = ConvertTableCaptions()

    ' Update all fields in document
    StatusBar = "Updating all fields in document..."
    UpdateAllFields

    ' Regenerate Table of Figures
    StatusBar = "Regenerating List of Figures..."
    RegenerateTableOfFigures

    ' Regenerate Table of Tables
    StatusBar = "Regenerating List of Tables..."
    RegenerateTableOfTables

    ' Re-enable screen updating
    Application.ScreenUpdating = True

    endTime = Timer

    ' Report results
    MsgBox "Caption Conversion Complete!" & vbCrLf & vbCrLf & _
           "Figures converted: " & figCount & vbCrLf & _
           "Tables converted: " & tblCount & vbCrLf & vbCrLf & _
           "Time elapsed: " & Format(endTime - startTime, "0.00") & " seconds", _
           vbInformation, "Conversion Complete"

    StatusBar = "Done!"
End Sub


'================================================================================
' FIGURE CAPTION CONVERSION
'================================================================================

Function ConvertFigureCaptions() As Integer
    Dim para As Paragraph
    Dim figNum As Integer
    Dim captionText As String
    Dim imgShape As InlineShape
    Dim foundImage As Boolean
    Dim convertedCount As Integer
    Dim i As Long

    ' Collections to store bookmarks and data
    Dim captionBookmarks() As String
    Dim captionImageIndexes() As Long
    Dim captionTexts() As String
    Dim captionCount As Integer
    Dim bmName As String
    Dim imgIndex As Long

    figNum = 1
    convertedCount = 0
    captionCount = 0
    imgIndex = 0

    StatusBar = "Identifying figure captions..."

    ' First pass: identify all caption candidates and mark with bookmarks
    For Each para In ActiveDocument.Paragraphs
        captionText = Trim(para.Range.Text)

        ' Skip empty paragraphs
        If Len(captionText) >= 5 Then
            ' Look for common figure caption patterns
            If IsFigureCaptionCandidate(captionText, para) Then
                foundImage = False

                ' Check current paragraph for inline images
                If para.Range.InlineShapes.Count > 0 Then
                    imgIndex = para.Range.InlineShapes(1).Range.Start
                    foundImage = True
                End If

                ' Check previous paragraph for inline images
                If Not foundImage And Not para.Previous Is Nothing Then
                    If para.Previous.Range.InlineShapes.Count > 0 Then
                        imgIndex = para.Previous.Range.InlineShapes(1).Range.Start
                        foundImage = True
                    End If
                End If

                ' Store for later processing
                If foundImage Then
                    captionCount = captionCount + 1
                    ReDim Preserve captionBookmarks(1 To captionCount)
                    ReDim Preserve captionImageIndexes(1 To captionCount)
                    ReDim Preserve captionTexts(1 To captionCount)

                    ' Create a unique bookmark for this caption paragraph
                    bmName = "TempCaption_" & captionCount
                    ActiveDocument.Bookmarks.Add Name:=bmName, Range:=para.Range

                    captionBookmarks(captionCount) = bmName
                    captionImageIndexes(captionCount) = imgIndex
                    captionTexts(captionCount) = CleanCaptionText(captionText, "Figure")
                End If
            End If
        End If
    Next para

    StatusBar = "Converting " & captionCount & " figure captions..."

    ' Second pass: convert captions (process in reverse to avoid range issues)
    For i = captionCount To 1 Step -1
        If ActiveDocument.Bookmarks.Exists(captionBookmarks(i)) Then
            Dim captionRange As Range
            Dim targetImage As InlineShape
            Dim searchStart As Long
            Dim searchEnd As Long

            Set captionRange = ActiveDocument.Bookmarks(captionBookmarks(i)).Range

            ' Find the image near this caption (before or in the same paragraph)
            Set targetImage = Nothing
            searchStart = captionRange.Start - 10000 ' Look up to 10000 chars before
            If searchStart < 0 Then searchStart = 0
            searchEnd = captionRange.End

            For Each imgShape In ActiveDocument.InlineShapes
                If imgShape.Range.Start >= searchStart And imgShape.Range.Start <= searchEnd Then
                    ' Found an image near the caption
                    Set targetImage = imgShape
                    Exit For
                End If
            Next imgShape

            If Not targetImage Is Nothing Then
                ' Store the caption text before any modifications
                Dim savedCaption As String
                savedCaption = captionTexts(i)

                On Error Resume Next

                ' Check if caption range contains the image
                Dim captionHasImage As Boolean
                captionHasImage = (captionRange.InlineShapes.Count > 0)

                If captionHasImage Then
                    ' Image is IN the caption paragraph - need to be careful
                    ' Just replace the text portion after the image
                    Dim textAfterImage As Range
                    Set textAfterImage = captionRange.Duplicate
                    textAfterImage.Start = targetImage.Range.End
                    textAfterImage.Text = ""

                    ' Delete the bookmark
                    If ActiveDocument.Bookmarks.Exists(captionBookmarks(i)) Then
                        ActiveDocument.Bookmarks(captionBookmarks(i)).Delete
                    End If

                    ' Insert proper caption
                    targetImage.Range.InsertCaption _
                        Label:="Figure", _
                        Title:=": " & savedCaption, _
                        Position:=wdCaptionPositionBelow
                Else
                    ' Image is NOT in caption paragraph - safe to delete entire paragraph
                    captionRange.Delete

                    ' Delete the bookmark
                    If ActiveDocument.Bookmarks.Exists(captionBookmarks(i)) Then
                        ActiveDocument.Bookmarks(captionBookmarks(i)).Delete
                    End If

                    ' Insert proper caption
                    targetImage.Range.InsertCaption _
                        Label:="Figure", _
                        Title:=": " & savedCaption, _
                        Position:=wdCaptionPositionBelow
                End If

                On Error GoTo 0

                convertedCount = convertedCount + 1
                StatusBar = "Converted " & convertedCount & " of " & captionCount & " figure captions..."
            End If
        End If
    Next i

    ConvertFigureCaptions = convertedCount
End Function


'================================================================================
' TABLE CAPTION CONVERSION
'================================================================================

Function ConvertTableCaptions() As Integer
    Dim tbl As Table
    Dim para As Paragraph
    Dim tblNum As Integer
    Dim captionText As String
    Dim convertedCount As Integer
    Dim searchRange As Range
    Dim i As Long

    ' Collections to store bookmarks and data
    Dim captionBookmarks() As String
    Dim tableIndexes() As Long
    Dim captionTexts() As String
    Dim captionCount As Integer
    Dim bmName As String

    tblNum = 1
    convertedCount = 0
    captionCount = 0

    StatusBar = "Identifying table captions..."

    ' First pass: identify all caption candidates and mark with bookmarks
    For Each tbl In ActiveDocument.Tables
        ' Look for caption in paragraph after table
        If tbl.Range.Paragraphs.Count > 0 Then
            ' Check paragraph immediately after table
            Set searchRange = tbl.Range
            searchRange.Collapse Direction:=wdCollapseEnd
            searchRange.Move Unit:=wdParagraph, Count:=1

            If searchRange.Paragraphs.Count > 0 Then
                captionText = Trim(searchRange.Paragraphs(1).Range.Text)

                ' Check if this looks like a table caption
                If IsTableCaptionCandidate(captionText, searchRange.Paragraphs(1)) Then
                    ' Store for later processing
                    captionCount = captionCount + 1
                    ReDim Preserve captionBookmarks(1 To captionCount)
                    ReDim Preserve tableIndexes(1 To captionCount)
                    ReDim Preserve captionTexts(1 To captionCount)

                    ' Create a unique bookmark for this caption paragraph
                    bmName = "TempTableCaption_" & captionCount
                    ActiveDocument.Bookmarks.Add Name:=bmName, Range:=searchRange.Paragraphs(1).Range

                    captionBookmarks(captionCount) = bmName
                    tableIndexes(captionCount) = tbl.Range.Start
                    captionTexts(captionCount) = CleanCaptionText(captionText, "Table")
                End If
            End If
        End If
    Next tbl

    StatusBar = "Converting " & captionCount & " table captions..."

    ' Second pass: convert captions (process in reverse to avoid range issues)
    For i = captionCount To 1 Step -1
        If ActiveDocument.Bookmarks.Exists(captionBookmarks(i)) Then
            Dim captionRange As Range
            Dim targetTable As Table
            Dim searchStart As Long
            Dim searchEnd As Long

            Set captionRange = ActiveDocument.Bookmarks(captionBookmarks(i)).Range

            ' Find the table near this caption (should be right before it)
            Set targetTable = Nothing
            searchStart = captionRange.Start - 10000 ' Look up to 10000 chars before
            If searchStart < 0 Then searchStart = 0
            searchEnd = captionRange.Start

            For Each tbl In ActiveDocument.Tables
                If tbl.Range.Start >= searchStart And tbl.Range.End <= searchEnd Then
                    ' Found a table before the caption
                    Set targetTable = tbl
                    ' Keep looking - we want the closest one (last match)
                End If
            Next tbl

            If Not targetTable Is Nothing Then
                ' Store the caption text before any modifications
                Dim savedCaption As String
                savedCaption = captionTexts(i)

                On Error Resume Next

                ' Delete the old caption text (tables are rarely in the same paragraph as caption)
                captionRange.Delete

                ' Delete the bookmark
                If ActiveDocument.Bookmarks.Exists(captionBookmarks(i)) Then
                    ActiveDocument.Bookmarks(captionBookmarks(i)).Delete
                End If

                ' Insert proper Word caption
                targetTable.Range.InsertCaption _
                    Label:="Table", _
                    Title:=": " & savedCaption, _
                    Position:=wdCaptionPositionAbove

                On Error GoTo 0

                convertedCount = convertedCount + 1
                StatusBar = "Converted " & convertedCount & " of " & captionCount & " table captions..."
            End If
        End If
    Next i

    ConvertTableCaptions = convertedCount
End Function


'================================================================================
' HELPER FUNCTIONS - Caption Detection
'================================================================================

Function IsFigureCaptionCandidate(ByVal txt As String, ByVal para As Paragraph) As Boolean
    Dim result As Boolean
    result = False

    ' Remove line breaks and extra spaces
    txt = Replace(txt, vbCr, "")
    txt = Replace(txt, vbLf, "")
    txt = Trim(txt)

    ' Check for common figure caption patterns
    If InStr(1, txt, "Figure", vbTextCompare) > 0 Then
        result = True
    ElseIf Left(txt, 3) = "Map" Then
        result = True
    ElseIf InStr(1, txt, "Screenshot", vbTextCompare) > 0 Then
        result = True
    ElseIf Left(txt, 4) = "Plot" Then
        result = True
    ElseIf para.Style = "Caption" Or para.Style = "Figure Caption" Then
        result = True
    ' Check if small font (Quarto often uses smaller font for captions)
    ElseIf para.Range.Font.Size < 11 Then
        ' Additional check: is there an image nearby?
        If para.Range.InlineShapes.Count > 0 Then
            result = True
        ElseIf Not para.Previous Is Nothing Then
            If para.Previous.Range.InlineShapes.Count > 0 Then
                result = True
            End If
        End If
    End If

    ' Exclude if it looks like body text (too long)
    If Len(txt) > 500 Then
        result = False
    End If

    IsFigureCaptionCandidate = result
End Function


Function IsTableCaptionCandidate(ByVal txt As String, ByVal para As Paragraph) As Boolean
    Dim result As Boolean
    result = False

    ' Remove line breaks and extra spaces
    txt = Replace(txt, vbCr, "")
    txt = Replace(txt, vbLf, "")
    txt = Trim(txt)

    ' Check for common table caption patterns
    If InStr(1, txt, "Table", vbTextCompare) > 0 Then
        result = True
    ElseIf Left(txt, 1) = ":" Then ' Quarto markdown tables often start with :
        result = True
    ElseIf para.Style = "Caption" Or para.Style = "Table Caption" Then
        result = True
    ElseIf InStr(1, txt, "Species distribution", vbTextCompare) > 0 Then
        result = True
    ElseIf InStr(1, txt, "Extinction risk", vbTextCompare) > 0 Then
        result = True
    ElseIf InStr(1, txt, "Dataset", vbTextCompare) > 0 Then
        result = True
    End If

    ' Exclude if it looks like body text (too long)
    If Len(txt) > 800 Then
        result = False
    End If

    IsTableCaptionCandidate = result
End Function


'================================================================================
' HELPER FUNCTIONS - Caption Text Cleaning
'================================================================================

Function CleanCaptionText(ByVal txt As String, ByVal captionType As String) As String
    Dim cleanedText As String

    ' Remove line breaks
    cleanedText = Replace(txt, vbCr, "")
    cleanedText = Replace(txt, vbLf, "")

    ' Remove existing "Figure X:" or "Table X:" prefixes
    cleanedText = RemoveNumberedPrefix(cleanedText, "Figure")
    cleanedText = RemoveNumberedPrefix(cleanedText, "Table")

    ' Remove leading colon if present (from Quarto markdown tables)
    If Left(cleanedText, 1) = ":" Then
        cleanedText = Trim(Mid(cleanedText, 2))
    End If

    ' Remove trailing paragraph marks
    If Right(cleanedText, 1) = Chr(13) Then
        cleanedText = Left(cleanedText, Len(cleanedText) - 1)
    End If

    ' Clean up extra spaces
    cleanedText = Trim(cleanedText)

    ' Remove any Quarto cross-reference labels like {#fig-label} or {#tbl-label}
    cleanedText = RemoveCrossRefLabels(cleanedText)

    CleanCaptionText = cleanedText
End Function


Function RemoveNumberedPrefix(ByVal txt As String, ByVal prefix As String) As String
    Dim pattern As String
    Dim i As Integer
    Dim result As String

    result = txt

    ' Try to match "Figure 1:", "Figure 1 :", "Figure 1-", etc.
    If InStr(1, result, prefix, vbTextCompare) = 1 Then
        ' Find where the number ends
        i = Len(prefix) + 1

        ' Skip whitespace
        Do While i <= Len(result) And Mid(result, i, 1) = " "
            i = i + 1
        Loop

        ' Skip digits
        Do While i <= Len(result) And IsNumeric(Mid(result, i, 1))
            i = i + 1
        Loop

        ' Skip optional separator (: or -)
        Do While i <= Len(result) And (Mid(result, i, 1) = ":" Or _
                                        Mid(result, i, 1) = "-" Or _
                                        Mid(result, i, 1) = " ")
            i = i + 1
        Loop

        ' Return the rest
        result = Trim(Mid(result, i))
    End If

    RemoveNumberedPrefix = result
End Function


Function RemoveCrossRefLabels(ByVal txt As String) As String
    Dim result As String
    Dim startPos As Integer
    Dim endPos As Integer

    result = txt

    ' Remove {#fig-...} labels
    Do
        startPos = InStr(result, "{#fig-")
        If startPos > 0 Then
            endPos = InStr(startPos, result, "}")
            If endPos > 0 Then
                result = Left(result, startPos - 1) & Mid(result, endPos + 1)
            Else
                Exit Do
            End If
        End If
    Loop While startPos > 0

    ' Remove {#tbl-...} labels
    Do
        startPos = InStr(result, "{#tbl-")
        If startPos > 0 Then
            endPos = InStr(startPos, result, "}")
            If endPos > 0 Then
                result = Left(result, startPos - 1) & Mid(result, endPos + 1)
            Else
                Exit Do
            End If
        End If
    Loop While startPos > 0

    RemoveCrossRefLabels = Trim(result)
End Function


'================================================================================
' UPDATE FIELDS AND REGENERATE LISTS
'================================================================================

Sub UpdateAllFields()
    Dim doc As Document
    Dim fld As Field
    Dim sect As Section
    Dim hdrFtr As HeaderFooter

    Set doc = ActiveDocument

    ' Update fields in main document
    For Each fld In doc.Fields
        fld.Update
    Next fld

    ' Update fields in headers and footers
    For Each sect In doc.Sections
        For Each hdrFtr In sect.Headers
            For Each fld In hdrFtr.Range.Fields
                fld.Update
            Next fld
        Next hdrFtr

        For Each hdrFtr In sect.Footers
            For Each fld In hdrFtr.Range.Fields
                fld.Update
            Next fld
        Next hdrFtr
    Next sect
End Sub


Sub RegenerateTableOfFigures()
    Dim doc As Document
    Dim rng As Range
    Dim toc As TableOfFigures
    Dim foundTOF As Boolean
    Dim para As Paragraph

    Set doc = ActiveDocument
    foundTOF = False

    ' Try to find existing Table of Figures
    For Each toc In doc.TablesOfFigures
        toc.Update
        foundTOF = True
        Exit For
    Next toc

    ' If no TOF found, try to find where to insert one
    If Not foundTOF Then
        ' Look for "List of Figures" heading
        For Each para In doc.Paragraphs
            If InStr(1, para.Range.Text, "List of Figures", vbTextCompare) > 0 Or _
               InStr(1, para.Range.Text, "LOF", vbTextCompare) > 0 Then

                ' Insert after this heading
                Set rng = para.Range
                rng.Collapse Direction:=wdCollapseEnd

                ' Insert Table of Figures
                doc.TablesOfFigures.Add _
                    Range:=rng, _
                    Caption:="Figure", _
                    IncludeLabel:=True, _
                    UseHeadingStyles:=False, _
                    UseFields:=True

                foundTOF = True
                Exit For
            End If
        Next para
    End If

    If Not foundTOF Then
        MsgBox "Warning: Could not find existing List of Figures to update." & vbCrLf & _
               "You may need to insert it manually: References > Insert Table of Figures", _
               vbExclamation, "List of Figures Not Found"
    End If
End Sub


Sub RegenerateTableOfTables()
    Dim doc As Document
    Dim rng As Range
    Dim toc As TableOfFigures
    Dim foundTOT As Boolean
    Dim para As Paragraph

    Set doc = ActiveDocument
    foundTOT = False

    ' Try to find existing Table of Tables
    ' (Note: Word stores these as TablesOfFigures with Label="Table")
    For Each toc In doc.TablesOfFigures
        If toc.Caption = "Table" Then
            toc.Update
            foundTOT = True
            Exit For
        End If
    Next toc

    ' If no TOT found, try to find where to insert one
    If Not foundTOT Then
        ' Look for "List of Tables" heading
        For Each para In doc.Paragraphs
            If InStr(1, para.Range.Text, "List of Tables", vbTextCompare) > 0 Or _
               InStr(1, para.Range.Text, "LOT", vbTextCompare) > 0 Then

                ' Insert after this heading
                Set rng = para.Range
                rng.Collapse Direction:=wdCollapseEnd

                ' Insert Table of Tables
                doc.TablesOfFigures.Add _
                    Range:=rng, _
                    Caption:="Table", _
                    IncludeLabel:=True, _
                    UseHeadingStyles:=False, _
                    UseFields:=True

                foundTOT = True
                Exit For
            End If
        Next para
    End If

    If Not foundTOT Then
        MsgBox "Warning: Could not find existing List of Tables to update." & vbCrLf & _
               "You may need to insert it manually: References > Insert Table of Figures > Table", _
               vbExclamation, "List of Tables Not Found"
    End If
End Sub


'================================================================================
' UTILITY FUNCTIONS
'================================================================================

Function IsNumeric(ByVal char As String) As Boolean
    IsNumeric = (char >= "0" And char <= "9")
End Function


'================================================================================
' ALTERNATIVE: Run figure and table conversions separately
'================================================================================

Sub ConvertFigureCaptionsOnly()
    Application.ScreenUpdating = False
    Dim count As Integer
    count = ConvertFigureCaptions()
    UpdateAllFields
    RegenerateTableOfFigures
    Application.ScreenUpdating = True
    MsgBox "Converted " & count & " figure captions.", vbInformation
End Sub


Sub ConvertTableCaptionsOnly()
    Application.ScreenUpdating = False
    Dim count As Integer
    count = ConvertTableCaptions()
    UpdateAllFields
    RegenerateTableOfTables
    Application.ScreenUpdating = True
    MsgBox "Converted " & count & " table captions.", vbInformation
End Sub


'================================================================================
' TROUBLESHOOTING: List all potential caption candidates
'================================================================================

Sub ListAllCaptionCandidates()
    Dim para As Paragraph
    Dim output As String
    Dim count As Integer

    output = "FIGURE CAPTION CANDIDATES:" & vbCrLf & String(50, "=") & vbCrLf
    count = 0

    For Each para In ActiveDocument.Paragraphs
        If IsFigureCaptionCandidate(para.Range.Text, para) Then
            count = count + 1
            output = output & count & ". " & Left(para.Range.Text, 100) & vbCrLf
        End If
    Next para

    output = output & vbCrLf & "TABLE CAPTION CANDIDATES:" & vbCrLf & String(50, "=") & vbCrLf
    count = 0

    For Each para In ActiveDocument.Paragraphs
        If IsTableCaptionCandidate(para.Range.Text, para) Then
            count = count + 1
            output = output & count & ". " & Left(para.Range.Text, 100) & vbCrLf
        End If
    Next para

    ' Output to Immediate Window (Ctrl+G to view)
    Debug.Print output

    MsgBox "Caption candidates listed in Immediate Window (Ctrl+G to view)", vbInformation
End Sub
