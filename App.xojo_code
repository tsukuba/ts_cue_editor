#tag Class
Protected Class App
Inherits Application
	#tag Event
		Sub Open()
		  
		  App.AutoQuit = True
		  
		  // Read Settings
		  
		  
		  // Set Encoding
		  cueReadEncoding = Encodings.ShiftJIS
		  
		  
		  // Read Files
		  Dim args() As String
		  args = Split(System.CommandLine, """ """)
		  args.Remove(0)
		  
		  If args.Ubound >= 0 Then
		    args(args.Ubound) = Left(args(args.Ubound), Len(args(args.Ubound)) -1)
		    
		    Dim files() As FolderItem
		    For Each file As String In args
		      files.Append(New FolderItem(file))
		    Next
		    
		    // Clear
		    fileListClear(10)
		    
		    fileList(files)
		    
		  End If
		  
		  
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h1
		Protected Sub cueFileLoad(file As FolderItem)
		  
		  Dim cue As TextInputStream
		  
		  cue = TextInputStream.Open(file)
		  
		  If file <> Nil And file.Exists Then
		    // Read Cue Sheet File
		    cue.Encoding = cueReadEncoding
		    Dim line() As String
		    Dim readTrack As UInt8 = 0
		    
		    // CUE SHEET DATA
		    Dim cue_rem As String //Comment
		    Dim cue_cdtextfile As String //CDTextFile(.cdt/.ccd)
		    Dim cue_catalog As String //Catalog
		    Dim cue_title As String //Title
		    Dim cue_performer As String //Performer
		    Dim cue_songwriter As String //Songwriter
		    Dim cue_file As String //FileName
		    Dim cue_file_type As String //FileType
		    
		    // CUE SHEET TRACK DATA
		    Dim cue_track() As String //TrackIndexCheck
		    Dim cue_track_rem() As String //Track Comments
		    Dim cue_track_title() As String //Track Titles
		    Dim cue_track_performer() As String //Track Performer
		    Dim cue_track_songwriter() As String //Track Songwriter
		    Dim cue_track_isrc() As String //Track ISRC
		    Dim cue_track_index_0() As String //Track Index 0
		    Dim cue_track_index_1() As String //Track Index 1
		    Dim cue_track_pregap() As String //Track Pregap
		    Dim cue_track_postgap() As String //Track Postgap
		    Dim cue_track_flags() As String //Track Flags
		    
		    While Not cue.EOF
		      // Split Command
		      line = Split(Trim(cue.ReadLine)," ")
		      Dim cmd As String
		      cmd = Lowercase(line(0))
		      line.Remove(0)
		      
		      Select Case cmd
		        
		        // REM
		      Case "rem"
		        If readTrack = 0 Then
		          // Comment
		          If cue_rem <> "" Then
		            cue_rem = cue_rem + EndOfLine + Join(line, " ")
		          Else
		            cue_rem = Join(line, " ")
		          End If
		        Else
		          // Track Comment
		          If cue_track_rem.Ubound < readTrack Then
		            ReDim cue_track_rem(readTrack - 1)
		          End If
		          If cue_track_rem(readTrack - 1) <> "" Then
		            cue_track_rem(readTrack - 1) = cue_track_rem(readTrack - 1) + EndOfLine + Join(line, " ")
		          Else
		            cue_track_rem(readTrack - 1) = Join(line, " ")
		          End If
		        End If
		        
		        // CDTEXTFILE
		      Case "cdtextfile"
		        If cue_cdtextfile = "" Then
		          cue_cdtextfile = removeQuotes(Join(line, " "))
		        Else
		          MsgBox("ERROR: CD Text File is Overlapped " + file.Name)
		          Quit()
		        End If
		        
		        // CATALOG
		      Case "catalog"
		        cue_catalog = removeQuotes(Join(line, " "))
		        
		        // ISRC
		      Case "isrc"
		        If cue_track_isrc.Ubound < readTrack Then
		          ReDim cue_track_isrc(readTrack - 1)
		        End If
		        cue_track_isrc(readTrack - 1) = removeQuotes(Join(line, " "))
		        
		        // TITLE
		      Case "title"
		        If readTrack = 0 Then
		          cue_title = removeQuotes(Join(line, " "))
		        Else
		          If cue_track_title.Ubound < readTrack Then
		            ReDim cue_track_title(readTrack - 1)
		          End If
		          cue_track_title(readTrack - 1) = removeQuotes(Join(line, " "))
		        End If
		        
		        // PERFORMER
		      Case "performer"
		        If readTrack = 0 Then
		          cue_performer = removeQuotes(Join(line, " "))
		        Else
		          If cue_track_performer.Ubound < readTrack Then
		            ReDim cue_track_performer(readTrack - 1)
		          End If
		          cue_track_performer(readTrack - 1) = removeQuotes(Join(line, " "))
		        End If
		        
		        // SONGWRITER
		      Case "songwriter"
		        If readTrack = 0 Then
		          cue_songwriter = removeQuotes(Join(line, " "))
		        Else
		          If cue_track_songwriter.Ubound < readTrack Then
		            ReDim cue_track_songwriter(readTrack - 1)
		          End If
		          cue_track_songwriter(readTrack - 1) = removeQuotes(Join(line, " "))
		        End If
		        
		        // FILE
		      Case "file"
		        If cue_file_type = "" And cue_file = "" Then
		          cue_file_type = line.Pop
		          cue_file =  removeQuotes(Join(line, " "))
		        Else
		          MsgBox("ERROR: File is Overlapped " + file.Name)
		          Quit()
		        End If
		        
		        // TRACK
		      Case "track"
		        If Lowercase(line(1)) = "audio" Then
		          readTrack = Val(removeQuotes(line(0)))
		          If cue_track.Ubound < readTrack Then
		            ReDim cue_track(readTrack - 1)
		          End If
		          If cue_track(readTrack - 1) = "" Then
		            cue_track(readTrack - 1) = removeQuotes(line(0))
		          Else
		            //  Overlap
		            MsgBox("ERROR: Track is Overlapped " + file.Name)
		            Quit()
		          End If
		        End If
		        
		        // INDEX
		      Case "index"
		        If readTrack > 0 Then
		          Dim index As UInt8
		          index = Val(line(0))
		          line.Remove(0)
		          If index = 0 Then
		            If cue_track_index_0.Ubound < readTrack Then
		              ReDim cue_track_index_0(readTrack - 1)
		            End If
		            cue_track_index_0(readTrack - 1) = removeQuotes(Join(line, " "))
		          ElseIf index = 1 Then
		            If cue_track_index_1.Ubound < readTrack Then
		              ReDim cue_track_index_1(readTrack - 1)
		            End If
		            cue_track_index_1(readTrack - 1) = removeQuotes(Join(line, " "))
		          End If
		        End If
		        
		        // PREGAP
		      Case "pregap"
		        If readTrack > 0 Then
		          If cue_track_pregap.Ubound < readTrack Then
		            ReDim cue_track_pregap(readTrack - 1)
		          End If
		          cue_track_pregap(readTrack - 1) = removeQuotes(Join(line, " "))
		        End If
		        
		        // POSTGAP
		      Case "postgap"
		        If readTrack > 0 Then
		          If cue_track_postgap.Ubound < readTrack Then
		            ReDim cue_track_postgap(readTrack - 1)
		          End If
		          cue_track_postgap(readTrack - 1) = removeQuotes(Join(line, " "))
		        End If
		        
		        // FLAGS
		      Case "flags"
		        If readTrack > 0 Then
		          If cue_track_flags.Ubound < readTrack Then
		            ReDim cue_track_flags(readTrack - 1)
		          End If
		          cue_track_flags(readTrack - 1) = removeQuotes(Join(line, " "))
		        End If
		        
		      End Select
		    Wend
		    
		    // Track Count Check
		    If cue_track.Ubound + 1 <> readTrack Then
		      MsgBox("ERROR: Invalid Tracks " + file.Name)
		      Quit()
		    End If
		    
		    // Output
		    
		    
		    
		    
		    
		    
		    
		    
		    
		    
		    
		    
		    
		  End If
		  
		  cue.Close
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub fileCueAdd(file As FolderItem)
		  
		  fileCueList.Append(file)
		  
		  cueFileLoad(file)
		  
		  frmMain.lstFile.AddRow(file.Name)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub fileCueClear()
		  
		  ReDim fileCueList(-1)
		  frmMain.lstFile.DeleteAllRows
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub fileCueRemove()
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub fileList(files() As FolderItem)
		  
		  For Each file As FolderItem In files
		    If file <> Nil And file.Exists = True Then
		      If file.Directory Then
		        // Directory
		        fileSearchDir(file)
		      Else
		        // File
		        If Lowercase(file.Name.Right(4)) = ".cue" Then
		          fileCueAdd(file)
		        End If
		      End If
		    End If
		  Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub fileListClear(mode As UInt8)
		  
		  // 10 - args
		  // 100 - btnAddFile
		  // 110 - btnAddFolder
		  // 200 - From lstFile
		  
		  
		  fileCueClear()
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub fileSearchDir(folder As FolderItem)
		  
		  Dim itemCount As Integer
		  Dim file As FolderItem
		  itemCount = folder.Count
		  
		  For i As Integer = 1 To itemCount
		    file = folder.Item(i)
		    If file <> Nil And file.Exists = True Then
		      If file.Directory Then
		        fileSearchDir(file)
		      Else
		        If Lowercase(file.Name.Right(4)) = ".cue" Then
		          fileCueAdd(file)
		        End If
		      End If
		    End If
		  Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function removeQuotes(text As String) As String
		  
		  text = Trim(text)
		  
		  If Left(text, 1) = Right(text, 1) Then
		    If Left(text, 1) = Chr(34) Or Left(text, 1) = Chr(39) Then
		      Return Mid(text, 2, Len(text) - 2)
		    End If
		  End If
		  
		  Return text
		  
		End Function
	#tag EndMethod


	#tag Property, Flags = &h1
		Protected cueReadEncoding As TextEncoding
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected fileCueList() As FolderItem
	#tag EndProperty


	#tag Constant, Name = kEditClear, Type = String, Dynamic = False, Default = \"\xE5\x89\x8A\xE9\x99\xA4", Scope = Public
		#Tag Instance, Platform = Windows, Language = Default, Definition  = \"\xE5\x89\x8A\xE9\x99\xA4"
		#Tag Instance, Platform = Linux, Language = Default, Definition  = \"\xE5\x89\x8A\xE9\x99\xA4"
	#tag EndConstant

	#tag Constant, Name = kFileQuit, Type = String, Dynamic = False, Default = \"\xE7\xB5\x82\xE4\xBA\x86", Scope = Public
		#Tag Instance, Platform = Windows, Language = Default, Definition  = \"\xE7\xB5\x82\xE4\xBA\x86"
	#tag EndConstant

	#tag Constant, Name = kFileQuitShortcut, Type = String, Dynamic = False, Default = \"", Scope = Public
		#Tag Instance, Platform = Mac OS, Language = Default, Definition  = \"Cmd+Q"
		#Tag Instance, Platform = Linux, Language = Default, Definition  = \"Ctrl+Q"
	#tag EndConstant


	#tag ViewBehavior
	#tag EndViewBehavior
End Class
#tag EndClass
