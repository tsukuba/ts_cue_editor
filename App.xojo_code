#tag Class
Protected Class App
Inherits Application
	#tag Event
		Sub Open()
		  
		  App.AutoQuit = True
		  
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
		Protected Sub fileCueAdd(file As FolderItem)
		  
		  fileCueList.Append(file)
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
		  // 100 - Menu
		  // 110 - Button
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
