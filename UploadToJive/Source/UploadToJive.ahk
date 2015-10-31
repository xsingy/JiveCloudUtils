
;***********
; the below classes and functions are used to parse Jive responses

class JSON
{
	/* Function:    parse
	 * Deserialize a string containing a JSON document to an AHK object.
	 * Syntax:
	 *     json_obj := JSON.parse( src [, jsonize:=false ] )
	 * Parameter(s):
	 *     src      [in] - String containing a JSON document
	 *     jsonize  [in] - If true, objects {} and arrays [] are wrapped as
	 *                     JSON.object and JSON.array instances respectively.
	 */
	parse(src, jsonize:=false) {
		;// Pre-validate JSON source before parsing
		if ((src := Trim(src, " `t`n`r")) == "") ;// trim whitespace(s)
			throw "Empty JSON source"
		first := SubStr(src, 1, 1), last := SubStr(src, 0)
		if !InStr("{[""tfn0123456789-", first) ;// valid beginning chars
		|| !InStr("}]""el0123456789", last) ;// valid ending chars
		|| (first == "{" && last != "}") ;// if starts w/ '{' must end w/ '}'
		|| (first == "[" && last != "]") ;// if starts w/ '[' must end w/ ']'
		|| (first == """" && last != """") ;// if starts w/ '"' must end w/ '"'
		|| (first == "n" && last != "l") ;// assume 'null'
		|| (InStr("tf", first) && last != "e") ;// assume 'true' OR 'false'
		|| (InStr("-0123456789", first) && !InStr("0123456789", last)) ;// number
			throw "Invalid JSON format"

		esc_seq := {
		(Join
			"""": """",
			"/": "/",
			"b": "`b",
			"f": "`f",
			"n": "`n",
			"r": "`r",
			"t": "`t"
		)}
		i := 0, strings := []
		while (i := InStr(src, """",, i+1)) {
			j := i
			while (j := InStr(src, """",, j+1)) {
				str := SubStr(src, i+1, j-i-1)
				StringReplace, str, str, \\, \u005C, A
				if (SubStr(str, 0) != "\")
					break
			}
			if !j
				throw "Missing close quote(s)"
			src := SubStr(src, 1, i) . SubStr(src, j+1)
			k := 0
			while (k := InStr(str, "\",, k+1)) {
				ch := SubStr(str, k+1, 1)
				if InStr("""btnfr/", ch, 1)
					str := SubStr(str, 1, k-1) . esc_seq[ch] . SubStr(str, k+2)
				
				else if (ch == "u") {
					hex := "0x" . SubStr(str, k+2, 4)
					if !(A_IsUnicode || (Abs(hex) < 0x100))
						continue ;// throw Exception() ???
					str := SubStr(str, 1, k-1) . Chr(hex) . SubStr(str, k+6)
				
				} else throw "Invalid escape sequence: '\" ch "'"
			}
			strings.Insert(str)
		}
		;// Check for missing opening/closing brace(s)
		if InStr(src, "{") || InStr(src, "}") {
			StringReplace, dummy, src, {, {, UseErrorLevel
			c1 := ErrorLevel
			StringReplace, dummy, src, }, }, UseErrorLevel
			c2 := ErrorLevel
			if (c1 != c2)
				throw "Missing " . Abs(c1-c2) . (c1 > c2 ? "clos" : "open") . "ing brace(s)"
		}
		;// Check for missing opening/closing bracket(s)
		if InStr(src, "[") || InStr(src, "]") {
			StringReplace, dummy, src, [, [, UseErrorLevel
			c1 := ErrorLevel
			StringReplace, dummy, src, ], ], UseErrorLevel
			c2 := ErrorLevel
			if (c1 != c2)
				throw "Missing " . Abs(c1-c2) . (c1 > c2 ? "clos" : "open") . "ing bracket(s)"
		}
		t := "true", f := "false", n := "null", null := ""
		jbase := jsonize ? {"{":JSON.object, "[":JSON.array} : {"{":0, "[":0}
		, pos := 0
		, key := "", is_key := false
		, stack := [tree := []]
		, is_arr := Object(tree, 1)
		, next := first ;// """{[01234567890-tfn"
		while ((ch := SubStr(src, ++pos, 1)) != "") {
			if InStr(" `t`n`r", ch)
				continue
			if !InStr(next, ch)
				throw "Unexpected char: '" ch "'"
			
			is_array := is_arr[obj := stack[1]]
			
			if InStr("{[", ch) {
				val := (proto := jbase[ch]) ? new proto : {}
				, obj[is_array? NumGet(&obj+4*A_PtrSize)+1 : key] := val
				, ObjInsert(stack, 1, val)
				, is_arr[val] := !(is_key := ch == "{")
				, next := is_key ? """}" : """{[]0123456789-tfn"
			}

			else if InStr("}]", ch) {
				ObjRemove(stack, 1)
				, next := is_arr[stack[1]] ? "]," : "},"
			}

			else if InStr(",:", ch) {
				if (obj == tree)
					throw "Unexpected char: '" ch "' -> there is no container object."
				next := """{[0123456789-tfn", is_key := (!is_array && ch == ",")
			}

			else {
				if (ch == """") {
					val := ObjRemove(strings, 1)
					if is_key {
						key := val, next := ":"
						continue
					}

				} else {
					val := SubStr(src, pos, (SubStr(src, pos) ~= "[\]\},\s]|$")-1)
					, pos += StrLen(val)-1
					if InStr("tfn", ch, 1) {
						if !(val == %ch%)
							throw "Expected '" %ch% "' instead of '" val "'"
						val := %val%
					
					} else if (Abs(val) == "") {
						throw "Invalid number: " val
					}
					val += 0
				}
				obj[is_array? NumGet(&obj+4*A_PtrSize)+1 : key] := val
				, next := is_array ? "]," : "},"
			}
		}
		return tree[1]
	}
	/* Function:    stringify
	 * Serialize an object to a JSON formatted string.
	 * Syntax:
	 *     json_str := JSON.stringify( obj [, indent:="" ] )
	 * Parameter(s):
	 *     obj      [in] - The object to stringify.
	 *     indent   [in] - Specify string(s) to use as indentation per level.
 	 */
	stringify(obj:="", indent:="", lvl:=1) {
		if IsObject(obj) {
			if (ObjGetCapacity(obj) == "") ;// COM,Func,RegExMatch,File object
				throw "Unsupported object type"
			is_array := 0
			for k in obj
				is_array := (k == A_Index)
			until !is_array

			if (Abs(indent) != "") {
				if (indent < 0)
					throw "Indent parameter must be a postive integer"
				spaces := indent, indent := ""
				Loop % spaces
					indent .= " "
			}
			indt := ""
			Loop, % indent ? lvl : 0
				indt .= indent

			lvl += 1, out := "" ;// make #Warn happy
			for k, v in obj {
				if IsObject(k) || (k == "")
					throw "Invalid JSON key"
				if !is_array
					out .= ( ObjGetCapacity([k], 1) ? JSON.stringify(k) : q . k . q ) ;// key
					    .  ( indent ? ": " : ":" ) ;// token + padding
				out .= JSON.stringify(v, indent, lvl) ;// value
				    .  ( indent ? ",`n" . indt : "," ) ;// token + indent
			}
			
			if (out != "") {
				out := Trim(out, ",`n" indent)
				if (indent != "")
					out := "`n" . indt . out . "`n" . SubStr(indt, StrLen(indent)+1)
			}
			
			return is_array ? "[" out "]" : "{" out "}"
		}
		;// Not a string - assume number -> integer or float
		if (ObjGetCapacity([obj], 1) == "") ;// returns an integer if 'obj' is string
			return InStr("01", obj) ? (obj ? "true" : "false") : obj
		;// null
		else if (obj == "")
			return "null"
		;// String
		; if obj is float
		; 	return obj
		esc_seq := {
		(Join
		    """": "\""",
		    "/":  "\/",
		    "`b": "\b",
		    "`f": "\f",
		    "`n": "\n",
		    "`r": "\r",
		    "`t": "\t"
		)}
		
		StringReplace, obj, obj, \, \\, A
		for k, v in esc_seq
			StringReplace, obj, obj, %k%, %v%, A

		while RegExMatch(obj, "[^\x20-\x7e]", wstr) {
			ucp := Asc(wstr), hex := "\u", n := 16
			while ((n-=4) >= 0)
				hex .= Chr( (x := (ucp >> n) & 15) + (x < 10 ? 48 : 55) )
			StringReplace, obj, obj, %wstr%, %hex%, A
		}
		return obj 
	}
	
	class object
	{
		
		__New(args*) {
			ObjInsert(this, "_", [])
			if ((count := NumGet(&args+4*A_PtrSize)) & 1)
				throw "Invalid number of parameters"
			Loop, % count//2
				this[args[A_Index*2-1]] := args[A_Index*2]
		}

		__Set(key, val, args*) {
			ObjInsert(this._, key)
		}

		Insert(key, val) {
			return this[key] := val
		}
		/* Buggy - remaining integer keys are not adjusted
		Remove(args*) { 
			ret := ObjRemove(this, args*), i := -1
			for index, key in ObjClone(this._) {
				if ObjHasKey(this, key)
					continue
				ObjRemove(this._, index-(i+=1))
			}
			return ret
		}
		*/
		Count() {
			return NumGet(&(this._)+4*A_PtrSize) ;// Round(this._.MaxIndex())
		}

		stringify(indent:="") {
			return JSON.stringify(this, indent)
		}

		_NewEnum() {
			static proto := {"Next":JSON.object.Next}
			return {
			(LTrim Join
				"base": proto,
				"enum": this._._NewEnum(),
				"obj":  this
			)}
		}

		Next(ByRef key, ByRef val:="") {
			if (ret := this.enum.Next(i, key))
				val := this.obj[key]
			return ret
		}
	}
		
	class array
	{
			
		__New(args*) {
			args.base := this.base
			return args
		}

		stringify(indent:="") {
			return JSON.stringify(this, indent)
		}
	}
}

;decodes a URI encoded string
uriDecode(str) {
	Loop
		If RegExMatch(str, "i)(?<=%)[\da-f]{1,2}", hex)
			StringReplace, str, str, `%%hex%, % Chr("0x" . hex), All
		Else Break
	Return, str
}

;encodes a string into a URI
uriEncode(Uri)
{
	VarSetCapacity(Var, StrPut(Uri, "UTF-8"), 0)
	StrPut(Uri, &Var, "UTF-8")
	f := A_FormatInteger
	SetFormat, IntegerFast, H
	While Code := NumGet(Var, A_Index - 1, "UChar")
		If (Code >= 0x30 && Code <= 0x39 ; 0-9
			|| Code >= 0x41 && Code <= 0x5A ; A-Z
			|| Code >= 0x61 && Code <= 0x7A) ; a-z
			Res .= Chr(Code)
		Else
			Res .= "%" . SubStr(Code + 0x100, -1)
	SetFormat, IntegerFast, %f%
	Return, Res
}

;finds if an object has a specific key
ObjHasKey(aObj, aKey) {
    for key in aObj
        if(key = aKey)
            return, true, ErrorLevel := 0
    return, false, errorlevel := 1
}

;finds if an array has a specific value
ArrayHasValue(aArr, aVal){
	for each, value in aArr
		if(value = aVal)
            return, true, ErrorLevel := 0
    return, false, errorlevel := 1
}

;keeps the last k lines of a string
StrTail(k,str) ;; Inspired by Laszlo (http://www.autohotkey.com/forum/topic6928.html)
	{
	Loop,Parse,str,`n
		{
		i := Mod(A_Index,k)
		L%i% = %A_LoopField%
		}
	L := L%i%
	Loop,% k-1
		{
		If i < 1
			SetEnv,i,%k%
		i-- ;Mod does not work here 
		L := L%i% "`n" L
		}
	Return L
	}

;merges several arrays into a single array (no duplicate values)
Arr_merge(p*) {
    res := Object()
    For each, obj in p
        For each, value in obj
            if(!ArrayHasValue(res,value))
				res.Insert(value)
    return res
}

;converts an array into a string
arrayToString(arr)
{   for each, value in arr
        res .= ", " value
    return "[" SubStr(res, 3) "]"
}

;split a string into an array
StrSplit(ByRef InputVar, Delimiters="", OmitChars="")
{
   o := []
   Loop, Parse, InputVar, % Delimiters, % OmitChars
      o.Insert(A_LoopField)
   return o
}

;sends information to the user interface
wInfo(info)
{
    global infoString= StrTail(30,global infoString . "`n" . info)
    GuiControl,, MyText, %infoString%
    
}

;sends a line of text to the out.txt file
wPrint(printLine)
{
FileAppend, %printLine%`n, out.txt
}


/*************************************************************************************************************
 *
 *   PROGRAM EXECUTION STARTS HERE
 *   
 */

;***********
; The following two lines allow to hide the CURL program windows that the are used to communicate with Jive
DllCall("AllocConsole")
WinHide % "ahk_id " DllCall("GetConsoleWindow", "ptr")

/*************************************************************************************************************
 *
 *  Deploy CURL Windows 32 bits
 *  put the CURL.EXE file in the same directory as the script before compiling.
 *  when the executable runs it will create the file in the same place.
 *  
 *  CURL is needed to communicate between this program and JIVE    
 *  
 *  Source CURL executable is here: 
 *  http://curl.haxx.se/download.html
 *   
 */
  
FileInstall,CURL.EXE,CURL.EXE

/*************************************************************************************************************
 * 
 * Initiate variables
 * 
 */   

FileDelete, out.txt
infoString:=""
debugString:=""

/*************************************************************************************************************
 *
 *  Get parameters
 *  
 */  

if 1=?
  {
  ;first parameter when executing the file is a question mark
  MsgBox Upload multiple files into Jive.`n`nUsage: start the Command Prompt (cmd.exe), and execute the following command:`n`nUploadToJive.exe "[Jive URL]" "[path and pattern]" "[tags to add]" "[categories to add]" "[place or place ID]" [0: skip file if exists; 1: replace file if exists] UserID password "extra CURL parameters" `nExample:`nUploadToJive.exe "https://mycompany.firmenich.com" "c:\temp\*.png" "blue,green" "products" "my great place" 0 XVS pmYPAzzW2d "-x http://zprox.com:3456 --proxy-user XVS:pmYPAzzW2d -k`n``nNote that if the string <userID> or <password> is used in the additional CURL parameters it will be replaced by the UserID and the Password" 
  ExitApp
  }
else
  {
  ;get the parameter values into the variables - concatenate with the variables themselves in case of looping
  jiveInstanceURL=%1%
  filePattern=%2%
  tags=%3%
  categories=%4%
  placeName=%5%
  updateExisting=%6%
  userID=%7%
  password=%8%
  curlExtra=%9%
  }

;if there is an UploadToJive.cfg file in the same directory, parse it to get any parameter provided in JSON format
;this allows to pre-populate the UI and is more convenient for basic users who actually always use the same settings
;only variables provided will be populated
;parameters can be provided as a mix of command line and config file - the app will merge values provided as command
;line with the values provided in the config file
;***
;To upload multiple files with different parameters, provide the attributes in a multi levels
;level 0: list
;level 1: placeName
;level 1: list
;level 2: fileName, tags, categories[, subject][, text]  
;example: "list" : [{"placeName" : "Jive Anywhere test group","list" : [{"filePattern" : "c:/temp/s*.jpg","tags":["blue","green"]},{"filePattern" : "c:/temp/k*.png","tags":["purple","lemon"],"text":"<body><div class=\"jive-rendered-content\"><p>adding some description</p><p><strong>some bold text</strong></p><p style=\"min-height: 8pt; padding: 0px;\">&nbsp;</p><table><tbody><tr><td style=\";\">and a cell in a table</td><td style=\";\">another celle</td></tr><tr><td style=\";\">and a 3 cell in a table</td><td style=\";\">another 4 cell</td></tr></tbody></table></div></body>"}]}]
FileRead, Config, UploadToJive.cfg
if not ErrorLevel  ; The config file was loaded.
{
	jsonObj := JSON.parse( Config )
	jiveInstanceURL:=jsonObj.jiveInstanceURL . jiveInstanceURL
	filePattern:=jsonObj.filePattern . filePattern
	tags:=jsonObj.tags . tags
	categories:=jsonObj.categories . categories
	placeName:=jsonObj.placeName . placeName
	updateExisting:=jsonObj.updateExisting . updateExisting
	userID:=jsonObj.userID . userID
	password:=jsonObj.password . password
	curlExtra:=jsonObj.curlExtra . curlExtra
	;set debug to 1 to output Jive server response to the out.txt file
	debug:=jsonObj.debug
	;set prompt to "authenticate" to only prompt for username and password
	;set prompt to "none" for no prompt at all
	prompt:=jsonObj.prompt
	;to upload many files in many places, store the parameters for each place in a JSON list, and the parameters for each file in a sub list
	placeList:=jsonObj.list
	Config =  ; Free the memory.
}


;loop through the process until the user cancels
Loop
{
	loopStart:

	;to record the number of upload errors
	errorCount:=0

	;this is to store when there was a failure and the user wants to start again
	startAgain:=0

	/*************
	* GUI to capture user input if the first 8 variables were not provided as parameters
	* If some variables were provided as parameters or in the config file they will show pre-populated in the UI 
	*/

	if ( (0<8) and (prompt<>"none") )
	{
		; get parameters when not provided as command line
		  
		if (prompt<>"authenticate") 
		{
			Gui, Add, Text, x6 y10 w160 h30 0x2, Jive URL
			Gui, Add, Edit, x176 y6 w180 vjiveInstanceURL, %jiveInstanceURL%
			Gui, Add, Text, x362 y10, ex: https://mycompany.jiveon.com

			Gui, Add, Text, x10 y40 w160 h30 0x2 , Path and pattern of files to upload
			Gui, add, Edit, x176 y36 w180 vfilePattern, %filePattern% 
			Gui, Add, Text, x362 y40, ex: g:\temp\*.png

			Gui, Add, Text, x10 y70 w160 h30 0x2 , Add the following tags
			Gui, add, Edit, x176 y66 w180 vtags, %tags% 
			Gui, Add, Text, x362 y70, ex: blue,large

			Gui, Add, Text, x10 y100 w160 h30 0x2 , Add the following categories
			Gui, add, Edit, x176 y96 w180 vcategories, %categories%
			Gui, Add, Text, x362 y100, ex: products,blue line

			Gui, Add, Text, x6 y130 w160 h30 0x2, Destination place (name or ID)
			Gui, Add, Edit, x176 y126 w180 vplaceName, %placeName%
			Gui, Add, Text, x362 y130, ex: my place

			Gui, Add, Text, x10 y160 w160 h30 0x2, When a file exists in destination

			if updateExisting=1
			Gui, add, DropDownList, x176 y156 w180 vupdateExisting, skip it|update it with source file||
			else
			Gui, add, DropDownList, x176 y156 w180 vupdateExisting, skip it||update it with source file
		}
		Gui, Add, Text, x10 y190 w160 h30 0x2, User ID
		Gui, add, Edit, x176 y186 w180 vuserID, %userID%
		Gui, Add, Text, x362 y190, ex: XID 

		Gui, Add, Text, x6 y220 w160 h30 0x2, Password
		Gui, Add, Edit, x176 y216 w180 vpassword password, %password%
		Gui, Add, Text, x362 y220, ex: myPA55worD 

		
		if (prompt<>"authenticate") 
		{
			Gui, Add, Text, x6 y250 w160 h30 0x2, More CURL parameters
			Gui, Add, Edit, x176 y246 w180 h20 vcurlExtra, %curlExtra%
			Gui, Add, Text, x362 y250, ex: -x http://proxy.mycompany.com:8123 --proxy-user XID:myPA55worD -k
		}
		 
		Gui, add, Button, gSubmit x176 y280 w180, OK 
		Gui, show

		return

		;if escape, close or cancel is used, exit the app gracefully
		GuiEscape:
		GuiClose:
		ButtonCancel:
		ExitApp
		
	}
	
	;if submit button was used, clear the UI
	Submit:
	Gui, Submit, NoHide
	Gui, Destroy
	
	;Replace userID and password within curlExtra parameters if <userID> or <password> is provided
	StringReplace, curlExtra, curlExtra, <userID>, %userID%, All       
	StringReplace, curlExtra, curlExtra, <password>, %password%, All

	;**********
	;GUI to display output
	Gui +LastFound +ToolWindow 
	Gui, Color, FFFFFF
	Gui Font, s10 Bold
	Gui, Add, Text, , Uploading files matching pattern %filePattern% into group %placeName% 
	Gui Font, s8 BoldNormal 
	Gui Color, BBBBBB 
	Gui, Add, Text, w600 h390 vMyText -Background, ttttttt 
	Gui, Show, % "x" . A_ScreenWidth/2-310 . " y" . A_ScreenHeight/2-210 . " w620 h440"      
	;This variable will store information to display while the app runs
	infoString := ""

	/*************************************************************************************************************************
	* record place and file parameters in a placeList object if the place and file data was not provided through uploadToJive.cfg
	*/
	
	if (placeList<>"") {
		;nothing to do: list of places and file attributes already in placeList array
	} else {
		placeList := [{}]
		placeList[1].placeName:= placeName
		placeList[1].list := [{}]
		placeList[1].list[1].filePattern:= filePattern
		placeList[1].list[1].tags:= StrSplit(tags, "`,")
		placeList[1].list[1].categories:= StrSplit(categories, "`,")
		
		p1:= placeList[1].placeName
	}
		
	/*************************************************************************************************************************
	*
	*   Loop through all target places.
	*  
	*/
	
	for each, value in placeList
    {
		placeName:= value.placeName
		fileList:= value.list
	
		;**********
		;check if the place ID was provided, if so build place UI, otherwise, get the place ID by searching through all matching places

		if updateExisting is not integer
			updateExisting := (updateExisting="skip it")?0:1
		
		if placeName is integer 
		{
			placeURI := jiveInstanceURL . "/api/core/v3/places/" . placeName
		}
		else
		{
			/*
			*   Get place URI
			*/ 
			wInfo("**********************************" )
			wInfo("Getting place ID of """ . placeName . """")

			;start at first result, and download list of places by batch of 25
			startIndex := 0
			pageSize := 25

			;loop until there is no more place result
			Loop {
				;CURL command to get the list of matching places
				cURL_command := "CURL -i -u " . userID . ":" . password . " -k " . (curlExtra = "" ? "" : " " . curlExtra . " ") . " """ . jiveInstanceURL . "/api/core/v3/places?count=" . pageSize . "&startIndex=" . startIndex . "&filter=search%28" . uriEncode(placeName) . "%29"""
				;wPrint(cURL_command . "`n")

				;call the CURL command
				Cmd := ComObjCreate("WScript.Shell")
				CmdRun := Cmd.Exec(ComSpec . " /c cURL.exe " . cURL_command)

				;catch the output
				output := CmdRun.StdOut.ReadAll()

				;send to out file for debugging
				if(debug="1")
				wPrint(output . "`n")

				;clean up the response to use only the JSON response part
				position := RegExMatch(output,";\R{")
				;}
				jsonStr := SubStr(output,position+2)

				;error handling
				;check if the server sent a response
				if (position=0) {
					;oops there is no clean response from Jive server
					;display the response in the user interface

					GuiControl, Hide, MyText
					Gui, Add, ActiveX, w500 h500 x0 y30 vdoc, HTMLFile
					doc.write("Error connecting to Jive<br><br>Output:<br><hr><br>" . output)
					Gui, Show, w500 h500 Center, HTML Gui
					ComObjConnect(doc, Document)

					MsgBox, 4,,Error connecting to Jive. Try again?

					Gui, Destroy

					IfMsgBox Yes
						{
						startAgain = true
						break
						}

					ExitApp
				}

				if(startAgain) {
				  Goto, loopStart
				}

				;convert the JSON response into an object
				jsonObj := JSON.parse( jsonStr )

				;if the JSON response is an empty list (=no result or no more result) stop looping
				if (jsonObj.list.MaxIndex()="")
				  break

				;go through the list of results
				For index, valueP in jsonObj.list
				{
				  ;wInfo( "  " . valueP.name )
				  ;check if the result is the expected place
				  if(valueP.name = placeName)
				  {
					  ;capture the placeURI
					  placeURI := valueP.resources.self.ref
					  wInfo("Place ID of """ . valueP.name . """ : " . valueP.placeID)
					  break
				  }
				}
			  
				;if the place was not found, keep looping, otherwise, exit the loop
				if(placeURI="")
				{
				startIndex := startIndex + pageSize
				} else {
				  break
				}

			}

			if(startAgain) {
				Goto, loopStart
			}

			;if after going through all results the place is not found, exit
			if(placeURI="") {
				MsgBox, 4,,Place "%placeName%" not found. Try again?

				Gui, Destroy

				IfMsgBox Yes
				{
					Goto, loopStart
				}

				ExitApp
			}
		  
		}

		/*************************************************************************************************************************
		*
		*   Get list of files already existing in place
		*  
		*/

		wInfo("**********************************" )
		wInfo("*** Getting list of files already existing in """ . placeName . """")

		startIndex := 0
		pageSize := 100

		;will store the list of files found and some of their attributes in an array
		existingFiles := {}

		Loop{

		  cURL_command := "CURL -i -u " . userID . ":" . password . " -k " . (curlExtra = "" ? "" : curlExtra . " ") . """" . jiveInstanceURL . "/api/core/v3/contents?count=" . pageSize . "&startIndex=" . startIndex . "&fields=name,subject,content,tags,categories,parent&filter=place%28" . placeURI . "%29&filter=type%28file%29"""
				
		  ;wPrint(cURL_command . "`n")
		  
		  Cmd := ComObjCreate("WScript.Shell")
		  CmdRun := Cmd.Exec(ComSpec . " /c cURL.exe " . cURL_command)
		  output := CmdRun.StdOut.ReadAll()
		  position := RegExMatch(output,";\R{")
		  ;}
		  jsonStr := SubStr(output,position+2)
		  
		  if(debug="1")
			wPrint(output . "`n")
		  
		  ;error handling
		  ;check if the server sent the list of content
		  if (position=0) {
			;oops there is no clean response from Jive server
			;display the response in the user interface            
			GuiControl, Hide, MyText
			Gui, Add, ActiveX, w500 h500 x0 y30 vdoc, HTMLFile
			doc.write("Error connecting to Jive<br><br>Output:<br><hr><br>" . output)
			Gui, Show, w500 h500 Center, HTML Gui
			ComObjConnect(doc, Document)             
			MsgBox, 4,,Error connecting to Jive. Try again?
			Gui, Destroy            
			IfMsgBox Yes
				{
				startAgain = true
				break
				}             
			ExitApp
		  }          
		  if(startAgain) {
			 break
		  }
		  
		  ;now parse the response
		  jsonObj := JSON.parse( jsonStr )
		  
		  ;if there are no content items or no more items to fetch, break the loop
		  if (jsonObj.list.MaxIndex()="")
			  break
		  
		  ;verify that the list of results is really coming from the target place (if the ID provided is actually the ID of a person Jive replies with the list of files of the person)
		  if (jsonObj.list[1].parent <> placeURI ){
			  MsgBox, 4,,The destination place ID you provided is not a place ID.`n`nMaybe you'll be more lucky using the destination place NAME instead of its ID. Try again?
			  Gui, Destroy
			  IfMsgBox Yes
				  {
				  startAgain = true
				  break
				  }
			  ExitApp
		  }
		  
		  ;this is the list of content items. Store it.
		  Loop % jsonObj.list.MaxIndex()
			{
			  ;storing the name of each file, reference, subject, description, tags and categories
			  existingFiles.Insert( uriDecode(jsonObj.list[A_Index].name) , [uriDecode(jsonObj.list[A_Index].resources.self.ref), jsonObj.list[A_Index].subject, jsonObj.list[A_Index].content.text, jsonObj.list[A_Index].tags, jsonObj.list[A_Index].categories ] )
			  wInfo( "  " . uriDecode(jsonObj.list[A_Index].name) ) 
			}
		  
		  startIndex := startIndex + pageSize
		  
		}

		;if the procedure was aborted, go back to input screen
		if(startAgain) {
		 goto loopstart
		}

		
		
		/*************************************************************************************************************************
		*
		*   Loop through all files in fileList.
		*  
		*/
		
		for feach, fvalue in fileList
		{
			filePattern:= fvalue.filePattern
			tagsA:= fvalue.tags
			categoriesA:= fvalue.categories
		
			/*************************************************************************************************************************
			*
			*   Prepare file data
			*  
			*/

			;clean up the filePattern in case user used forward slash instead of backward slash
			StringReplace, filePattern, filePattern, /, \, All

			/*************************************************************************************************************************
			*
			*   Upload files
			*  
			*/

			wInfo("**********************************" )
			wInfo("*** Uploading files into " . placeName )

			Loop, %filePattern%
			{
				;if the file already exists
				;check if the file already exists in Jive with the zip extention
				addZIPext := ( ObjHasKey(existingFiles, A_LoopFileName . ".zip" ) ) ? ".zip"  : ""
				;check if the file already exists in Jive (or with the zip extension)
				if ( ObjHasKey(existingFiles, A_LoopFileName) or (addZIPext<>"") ) 
				{
					;if the option is the update the file, do it, otherwise, skip the file
					if updateExisting
					{
					  
						wInfo( A_LoopFileName . " is getting updated with the latest version.")

						;using "cou.tmp" to temporarily store the JSON string for the CURL command (as it would be too long to be in a command)
						FileDelete, cou.tmp

						if (fvalue.text<>"") {
							contentString := fvalue.text
						} else {
							contentString := existingFiles[A_LoopFileName][3]
						}

						if (fvalue.subject<>"") {
							subject := fvalue.subject
						} else {
							subject := existingFiles[A_LoopFileName . addZIPext][2]
						}
						
						;concatenate existing file tags with user provided file tags - also do this with categories
						contentTags := arrayToString(Arr_merge(existingFiles[A_LoopFileName][4],tagsA))
						contentCategories := arrayToString(Arr_merge(existingFiles[A_LoopFileName][5],categoriesA))

						;build the json string            
						contentJson := "{""type"":""file"",""subject"":'" . subject . "',""visibility"":""place"",""parent"":'" . placeURI . "',""content"":{""type"":'text/html',""text"":""" . contentString . """},""tags"":" . contentTags . ",""categories"":" . contentCategories . "}"            

						;for debugging: the JSON string in clear
						if(debug="1")
						  wPrint( "JSON string sent to the server:`n" . contentJson . "`n" )

						;storing the JSON string into the temporary file
						FileAppend, %contentJson% , cou.tmp

						;build the CURL command
						cURL_command := "CURL -X PUT -i -u " . userID . ":" . password . " -k " . (curlExtra = "" ? "" : curlExtra . " ") . "-F file1=@""" . A_LoopFileFullPath . """ -F ""json=@cou.tmp;type=application/json"" " . existingFiles[A_LoopFileName . withZIPext][1]
						CmdRun := Cmd.Exec(ComSpec . " /c cURL.exe " . cURL_command)
						output := CmdRun.StdOut.ReadAll() 
						;wPrint(cURL_command . "`n")

						position := RegExMatch(output,"\R\R{")
						;}
						jsonStr := SubStr(output,position+2)

						if(debug="1")
						  wPrint(output . "`n")

						jsonObj := JSON.parse( jsonStr )

						if(jsonObj.error<>""){
							wInfo( "  ... Error " . jsonObj.error.status . " : " . jsonObj.error.message )
							errorCount++
						} else {
							wInfo( "  ... updated successfully" )
						}

						;delete the temporary JSON file
						FileDelete, cou.tmp            
					}
					else
					  wInfo( A_LoopFileName . " skipped (already existed).")
				}
				else
				{
					;if the file does not exists - create it!
					
					wInfo( A_LoopFileName . " does not exist yet and will be created.")
					FileDelete, cou.tmp            
					
					contentTags := arrayToString(tagsA)
					contentCategories := arrayToString(categoriesA)
					
					if (fvalue.text<>"") {
						contentString := fvalue.text
					} else {
						contentString := ""
					}
					
					if (fvalue.subject<>"") {
						subject := fvalue.subject
					} else {
						subject := A_LoopFileName
					}
								
					contentJson := "{""type"":""file"",""subject"":'" . subject . "',""visibility"":""place"",""parent"":'" . placeURI . "',""content"":{""type"":'text/html',""text"":""" . contentString . """},""tags"":" . contentTags . ",""categories"":" . contentCategories . "}"            
					if(debug="1")
						wPrint( "JSON string sent to the server:`n" . contentJson . "`n" )
					FileAppend, %contentJson% , cou.tmp
					cURL_command := "CURL -i -u " . userID . ":" . password . " -k " . (curlExtra = "" ? "" : curlExtra . " ") . "-F file1=@""" . A_LoopFileFullPath . """ -F ""json=@cou.tmp;type=application/json"" " . jiveInstanceURL . "/api/core/v3/contents"
					CmdRun := Cmd.Exec(ComSpec . " /c cURL.exe " . cURL_command)
					;wPrint(cURL_command . "`n")
					output := CmdRun.StdOut.ReadAll()
					if(debug="1")
						wPrint(output . "`n")
					
					position := RegExMatch(output,"\R\R{")
					;}
					jsonStr := SubStr(output,position+2)
					
					if(debug="1")
						wPrint(output . "`n")
					
					jsonObj := JSON.parse( jsonStr )
					
					if(jsonObj.error<>""){
						wInfo( "  ... Error " . jsonObj.error.status . " : " . jsonObj.error.message )
						errorCount++
					} else {
						wInfo( "  ... uploaded successfully" )
					}
				}
				  
				;verify that the error is not caused by an invalid Place ID (if the ID provided was actually the ID of a person Jive replies with the list of files of the person)
				if(jsonObj.error<>"")
				{
					je:=jsonObj.error.message
					IfInString, je, Invalid place ID
					{
						MsgBox, 4,,The destination place ID you provided is not a place ID.`n`nMaybe you'll be more lucky using the destination place NAME instead of its ID. Try again?
						Gui, Destroy
						IfMsgBox Yes
							{
							startAgain = true
							break
							}
						ExitApp
					}
				}
			}
			
		}
		
	}

	if(startAgain) {
	  Goto, loopStart
	}
			
	;wInfo(debugString)
	wInfo("**********************************" )
	wInfo("Operation complete") 
	wInfo("**********************************" )
	wInfo(".")
	MsgBox, 4,, Operation complete with %errorCount% errors. Upload more files?

	Gui, Destroy

	IfMsgBox Yes
	  {
		goto, loopStart
	  }
	else 
	  ExitApp

}


