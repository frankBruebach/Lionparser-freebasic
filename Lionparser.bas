'' copyright loewenherz (frank b.), 2024-2025, nov 28-11-2025
'' "lionparser.bas" mini-interpreter starts, internal version v14bGO, update 6, 27-10-2025, 28-10-2025
'' compiled with freebasic 1.10.1, 32-bit and 64-bit
'' fbc lionparser.bas
''
Function includes(fileName as string) as string

    dim f as integer
    dim lines as string
    dim contents as string

    f = freefile()
    if open(fileName for input as #f) <> 0 then
        return "[Error: Could not open " + fileName + "]"
    end if

    while not eof(f)
        lines = ""
        line input #f, lines
        contents &= lines & chr(10)
    wend

    close #f
    return contents
End Function


' ------------------ TOKEN TYPES ------------------
Enum TokenType
    TOKEN_IDENTIFIER
    TOKEN_NUMBER
    TOKEN_OPERATOR
    TOKEN_STRING
    TOKEN_STR 
    TOKEN_ZSTRING 'new
    TOKEN_ASSIGN
    TOKEN_PRINT
    TOKEN_SLEEP
    TOKEN_EOF
    TOKEN_UNKNOWN
    TOKEN_MODULUS
    TOKEN_LESS
    TOKEN_GREATER
    TOKEN_LPAREN
    TOKEN_RPAREN
    TOKEN_IF
    TOKEN_THEN
    TOKEN_TRUE
    TOKEN_FALSE
    TOKEN_ELSE
    TOKEN_ENDIF
    TOKEN_LEN
    TOKEN_LEN1
    TOKEN_SIZEOF
    TOKEN_TYPE
    TOKEN_TYPE_END
    TOKEN_DIM 
    TOKEN_AS 
    TOKEN_EOL
    TOKEN_PLUS
    TOKEN_ASTERISK 
    TOKEN_EQUAL 
    TOKEN_COMMA
    TOKEN_CONST
    TOKEN_INCLUDE 'new
    TOKEN_DEFINE 'new
    TOKEN_BYTE 'new
    TOKEN_WSTRING 'new
    TOKEN_ALLOCATE 'new 
    TOKEN_DEALLOCATE
    TOKEN_PTR ''new 
    TOKEN_FUNCTION 'new 
    TOKEN_ENDFUNCTION 'new 
    TOKEN_RETURN 'new 
End Enum

Type Token
    As Integer kind
    As String value
    As Integer typ
End Type

Dim Shared As Token tokens(0 To 1000)
Dim Shared As Integer tokenCount = 0

Dim Shared As Integer Ptr symbolIntPtrs(0 To 255)
Dim Shared As String Ptr symbolStrPtrs(0 To 255)

' ------------------ SYMBOL TABLE ------------------
Dim Shared As String symbolTable(0 To 255)
Dim Shared As String symbolNames(0 To 255)
Dim Shared As String symbolTypes(0 To 255)
Dim Shared As Integer symbolDeclaredSize(0 To 255)
Dim Shared As Integer symbolCount = 0
Dim Shared As Integer symbolSizes(100)   ' Für ZString & Co.

' --- Simple memory simulation for PTR variables ---
'--- MEMORY SIMULATION ----------------
Dim Shared As String memorySlots(1 To 1000)
Dim Shared As Integer memoryUsed = 0

Function AllocMem(ByVal size As Integer) As String
    memoryUsed += 1
    memorySlots(memoryUsed) = Space(size)
    Return "&" & Str(1000 + memoryUsed * 100)
End Function

Function GetMemIndex(ByVal addr As String) As Integer
    If Left(addr, 1) = "&" Then
        Dim baseAddr As Integer = Val(Mid(addr, 2))
        Return (baseAddr - 1000) \ 100
    Else
        Return 0
    End If
End Function

Function FreeMem(ByVal addr As String) As string
    Dim idx As Integer = GetMemIndex(addr)
    If idx > 0 And idx <= memoryUsed Then
        memorySlots(idx) = ""
        Print "[Memory freed at " & addr & "]"
    Else
        Print "Error: invalid address for deallocate() -> " & addr
    End If
End Function

Function SimAllocate(size As Integer) As Integer
    memoryUsed += 1
    If memoryUsed > 255 Then
        Print "[Error: Out of memory]"
        Return 0
    End If
    memorySlots(memoryUsed) = Space(size)
    Return memoryUsed
End Function

Dim Shared lastExprType As String

'------------------ GLOBALS ------------------
' Symbol table
' Funktionen
Dim Shared As String functionNames(0 To 255)
Dim Shared As String functionBodies(0 To 255)
Dim Shared As String functionParams(0 To 255)
Dim Shared As String functionReturnTypes(0 To 255)
Dim Shared As Integer functionParamCount(0 To 255)
Dim Shared As String functionParamNames(0 To 255, 0 To 15)
Dim Shared As String functionParamTypes(0 To 255, 0 To 15)
Dim Shared As Integer functionCount = 0

'------------------ HELPER ------------------
Function GetSymbolIndex(names As String) As Integer
    For i As Integer = 0 To symbolCount - 1
        If symbolNames(i) = names Then Return i
    Next
    symbolNames(symbolCount) = names
    symbolTypes(symbolCount) = ""
    symbolDeclaredSize(symbolCount) = 0
    symbolCount += 1
    Return symbolCount - 1
End Function

Function Max(a As Integer, b As Integer) As Integer
    If a > b Then Return a
    Return b
End Function

Function IsLetter(ch As String) As Boolean
    Return (ch >= "a" And ch <= "z") Or (ch >= "A" And ch <= "Z")
End Function

Function IsDigit(ch As String) As Boolean
    Return (ch >= "0" And ch <= "9")
End Function

Function GetFunctionBody(fname as string) as string
    For i As Integer = 0 To functionCount - 1
        If LCase(functionNames(i)) = LCase(fname) Then
            Return functionBodies(i)
        End If
    Next
    Return "" ' nicht gefunden
End Function

Function IsNumeric(s As String) As Boolean
    Dim As Double tmp = Val(s)
    Return (s <> "" AndAlso Str(tmp) <> "0" Or s = "0")
End Function

'' TOKENIZER
'' Tokenizer (erweitert)
Sub Tokenize(lines As String)
    Dim As Integer i = 0
    tokenCount = 0

    Do While i < Len(lines)
        Dim As String ch = Mid(lines, i + 1, 1)

        If Mid(lines, i + 1, 7) = "#define" Then
    tokens(tokenCount).kind = TOKEN_DEFINE
    tokens(tokenCount).value = "#define"
    tokenCount += 1
    i += 7
    Continue Do
ElseIf Mid(lines, i + 1, 8) = "#include" Then
    tokens(tokenCount).kind = TOKEN_INCLUDE
    tokens(tokenCount).value = "#include"
    tokenCount += 1
    i += 8
    Continue Do
End If
    
        ' --- Skip whitespace ---
        If ch = " " Or ch = Chr(9) Or ch = Chr(13) Or ch = Chr(10) Then
            i += 1
            Continue Do

        ' --- Skip comment lines (starting with apostrophe) ---
        ElseIf ch = "'" Then
            ' Skip till end of line
            Do While i < Len(lines) AndAlso Mid(lines, i + 1, 1) <> Chr(10)
                i += 1
            Loop
            i += 1
            Continue Do

        ' --- Identifiers or keywords ---
        ElseIf IsLetter(ch) Then
            Dim As String ident = ""
            Do While i < Len(lines) AndAlso (IsLetter(Mid(lines, i + 1, 1)) Or IsDigit(Mid(lines, i + 1, 1)))
                ident &= Mid(lines, i + 1, 1)
                i += 1
            Loop
            tokens(tokenCount).value = ident
            Select Case LCase(ident)
                Case "print": tokens(tokenCount).kind = TOKEN_PRINT
                Case "sleep": tokens(tokenCount).kind = TOKEN_SLEEP
                Case "if": tokens(tokenCount).kind = TOKEN_IF
                Case "then": tokens(tokenCount).kind = TOKEN_THEN
                Case "true": tokens(tokenCount).kind = TOKEN_TRUE
                Case "false": tokens(tokenCount).kind = TOKEN_FALSE
                Case "else": tokens(tokenCount).kind = TOKEN_ELSE
                Case "endif": tokens(tokenCount).kind = TOKEN_ENDIF
                Case "len": tokens(tokenCount).kind = TOKEN_LEN
                Case "sizeof": tokens(tokenCount).kind = TOKEN_SIZEOF
                Case "dim": tokens(tokenCount).kind = TOKEN_DIM
                Case "as": tokens(tokenCount).kind = TOKEN_AS
            	 Case "str": tokens(tokenCount).kind = TOKEN_STR
				    Case "ptr": tokens(tokenCount).kind = TOKEN_PTR	 
                Case "zstring": tokens(tokenCount).kind = TOKEN_ZSTRING
					 Case "wstring": tokens(tokenCount).kind = TOKEN_WSTRING
            	 Case "#define": tokens(tokenCount).kind = TOKEN_DEFINE
            	 Case "long": tokens(tokenCount).kind = TOKEN_TYPE
            	 Case "single": tokens(tokenCount).kind = TOKEN_TYPE
            	 Case "short": tokens(tokenCount).kind = TOKEN_TYPE	
            	 Case "any": tokens(tokenCount).kind = TOKEN_TYPE
            	 Case "function": tokens(tokenCount).kind = TOKEN_FUNCTION
					 Case "endfunction": tokens(tokenCount).kind = TOKEN_ENDFUNCTION
            	 Case "return": tokens(tokenCount).kind = TOKEN_RETURN
            	 Case "const": tokens(tokenCount).kind = TOKEN_CONST
            	 Case "allocate": tokens(tokenCount).kind = TOKEN_ALLOCATE
            	 Case "deallocate": tokens(tokenCount).kind = TOKEN_DEALLOCATE	
            	 Case Else: tokens(tokenCount).kind = TOKEN_IDENTIFIER	
            	'Case "unknown": tokens(tokenCount).kind = TOKEN_IDENTIFIER
            End Select
            tokenCount += 1

If ident = "zstring" Or ident = "wstring" Then
    ' erwarte: * <Zahl>
    If Mid(lines, i+1, 1) = "*" Then
        i += 1
        tokens(tokenCount).kind = TOKEN_ASTERISK
        tokens(tokenCount).value = "*"
        tokenCount += 1

        ' Zahl danach einlesen
        Dim num As String = ""
        While i < Len(lines) And IsDigit(Mid(lines, i+1, 1))
            num &= Mid(lines, i+1, 1)
            i += 1
        Wend
        tokens(tokenCount).kind = TOKEN_NUMBER
        tokens(tokenCount).value = num
        tokenCount += 1
    End If
End If

        ' --- Numbers ---
ElseIf IsDigit(ch) Then
    Dim As String num = ""
    Dim As Integer hasDot = 0
    Do While i < Len(lines)
        Dim As String ch2 = Mid(lines, i + 1, 1)
        If IsDigit(ch2) Then
            num &= ch2
            i += 1
        ElseIf ch2 = "." And hasDot = 0 Then
            hasDot = 1
            num &= ch2
            i += 1
        Else
            Exit Do
        End If
    Loop
    tokens(tokenCount).kind = TOKEN_NUMBER
    tokens(tokenCount).value = num
    tokenCount += 1

        ' --- String literal ---
        ElseIf ch = """" Then
            Dim As String strx = ""
            i += 1
            Do While i < Len(lines) AndAlso Mid(lines, i + 1, 1) <> """"
                strx &= Mid(lines, i + 1, 1)
                i += 1
            Loop
            i += 1
            tokens(tokenCount).kind = TOKEN_STRING
            tokens(tokenCount).value = strx
            tokenCount += 1

'-------------------------------------------------------------- //
        ElseIf ch = "@" Then
    tokens(tokenCount).kind = TOKEN_OPERATOR
    tokens(tokenCount).value = "@"
    tokenCount += 1
    i += 1
'-------------------------------------------------------------- //
        ' --- ZString literal ---
        ElseIf ch = "`" Then
            Dim As String zstr = ""
            i += 1
            Do While i < Len(lines) AndAlso Mid(lines, i + 1, 1) <> "`"
                zstr &= Mid(lines, i + 1, 1)
                i += 1
            Loop
            i += 1
            tokens(tokenCount).kind = TOKEN_ZSTRING
            tokens(tokenCount).value = zstr
            tokenCount += 1

        ' --- Assignment or operator ---
        ElseIf ch = "=" Then
            tokens(tokenCount).kind = TOKEN_ASSIGN
            tokens(tokenCount).value = "="
            tokenCount += 1
            i += 1

        ElseIf ch = "+" Or ch = "-" Or ch = "*" Or ch = "/" Or ch = "%" Then
    			tokens(tokenCount).kind = TOKEN_OPERATOR
    			tokens(tokenCount).value = ch
    			tokenCount += 1
    			i += 1

        ElseIf ch = "," Then
            tokens(tokenCount).kind = TOKEN_COMMA
            tokens(tokenCount).value = ","
            tokenCount += 1
            i += 1

        ElseIf ch = "*" Or ch = "+" Or ch = "-" Or ch = "/" Or ch = "%" Then
    			tokens(tokenCount).kind = TOKEN_OPERATOR
    			tokens(tokenCount).value = ch
    			tokenCount += 1
    			i += 1

        ElseIf ch = "<" Then
            tokens(tokenCount).kind = TOKEN_LESS
            tokens(tokenCount).value = "<"
            tokenCount += 1
            i += 1

        ElseIf ch = ">" Then
            tokens(tokenCount).kind = TOKEN_GREATER
            tokens(tokenCount).value = ">"
            tokenCount += 1
            i += 1

        ElseIf ch = "(" Then
            tokens(tokenCount).kind = TOKEN_LPAREN
            tokens(tokenCount).value = "("
            tokenCount += 1
            i += 1

        ElseIf ch = ")" Then
            tokens(tokenCount).kind = TOKEN_RPAREN
            tokens(tokenCount).value = ")"
            tokenCount += 1
            i += 1

        ' --- Unknown character ---
        Else
            tokens(tokenCount).kind = TOKEN_UNKNOWN
            tokens(tokenCount).value = ch
            tokenCount += 1
            i += 1
        End If
    Loop

    tokens(tokenCount).kind = TOKEN_EOF
End Sub

Function Calc(leftVal As String, rightVal As String, op As String) As String
    Dim l As Double = Val(leftVal)
    Dim r As Double = Val(rightVal)
    Select Case op
        Case "+": Return Str(l + r)
        Case "-": Return Str(l - r)
        Case "*": Return Str(l * r)
        Case "/": Return Str(l / r)
        Case "%": Return Str(Fix(l) Mod Fix(r))
        Case Else
            Print "Error: Unknown operator '" & op & "'"
            Return "0"
    End Select
End Function

''
' einfach: uebergebe current index als parameter
Function PeekKind(curIndex As Integer, offset As Integer = 0) As Integer
    If curIndex + offset >= tokenCount Then
        Return TOKEN_EOF
    End If
    Return tokens(curIndex + offset).kind
End Function

Function PeekValue(curIndex As Integer, offset As Integer = 0) As String
    If curIndex + offset >= tokenCount Then
        Return ""
    End If
    Return tokens(curIndex + offset).value
End Function

'----------------------------------------
' Hilfsfunktionen f�r ZSTRING Variablen
'----------------------------------------
Sub HandleZstringPtr(varName As String)
    Dim varIdx As Integer = GetSymbolIndex(varName)
    symbolTypes(varIdx) = "zstring ptr"
    symbolTable(varIdx) = "NULL"  ' Platzhalter f�r Zeiger
End Sub

Sub HandleZstringArray(varName As String, size As Integer)
    Dim varIdx As Integer = GetSymbolIndex(varName)
    symbolTypes(varIdx) = "zstring"
    symbolDeclaredSize(varIdx) = size
    symbolTable(varIdx) = String(size - 1, Chr(0))  ' Nullterminierter String
End Sub

'' PARSE STATEMENT
' --- Parser ---

'======================= LIONPARSER PART 2 PATCHED =======================
'---------------- EvalExpression ----------------
Function EvalExpression(ByRef index As Integer) As String
    Dim result As String
    Dim resultType As String = "int"

    ' --- Linke Seite auswerten ---
    Select Case tokens(index).kind
        Case TOKEN_NUMBER
            result = tokens(index).value
            index += 1

        Case TOKEN_IDENTIFIER
            ' Funktionsaufruf?
            If index + 1 < tokenCount And tokens(index + 1).kind = TOKEN_LPAREN Then
                Dim fname As String = tokens(index).value
                index += 2 ' Skip name + "("
                
                Dim args(0 To 15) As String
                Dim argCount As Integer = 0
                While tokens(index).kind <> TOKEN_RPAREN
                    args(argCount) = EvalExpression(index)
                    argCount += 1
                    If tokens(index).kind = TOKEN_COMMA Then index += 1
                Wend
                index += 1 ' skip ")"
                
                ' Funktion finden
                Dim fi As Integer = -1
                For k As Integer = 0 To functionCount - 1
                    If LCase(functionNames(k)) = LCase(fname) Then fi = k: Exit For
                Next
                If fi = -1 Then
                    Print "Error: function not found: " & fname
                    Return "0"
                End If
                
                ' Parameter einbinden
                For p As Integer = 0 To functionParamCount(fi) - 1
                    Dim pname As String = functionParamNames(fi, p)
                    Dim ptype As String = functionParamTypes(fi, p)
                    Dim varIdx As Integer = GetSymbolIndex(pname)
                    symbolTypes(varIdx) = ptype
                    If p <= argCount - 1 Then symbolTable(varIdx) = args(p) Else symbolTable(varIdx) = "0"
                Next
                
                ' Backup Tokens
                Dim oldTokens(0 To 1000) As Token
                Dim oldCount As Integer = tokenCount
                For i As Integer = 0 To oldCount - 1: oldTokens(i) = tokens(i): Next
                
                Tokenize(functionBodies(fi))
                Dim subIndex As Integer = 0
                Dim retVal As String = "0"
                While subIndex < tokenCount
                    If tokens(subIndex).kind = TOKEN_RETURN Then
                        subIndex += 1
                        retVal = EvalExpression(subIndex)
                        Exit While
                    Else
                        subIndex += 1
                    End If
                Wend
                
                ' Restore Tokens
                For i As Integer = 0 To oldCount - 1: tokens(i) = oldTokens(i): Next
                tokenCount = oldCount
                
                Return retVal
            Else
                ' Normale Variable
                Dim varIdx As Integer = GetSymbolIndex(tokens(index).value)
                result = symbolTable(varIdx)
                index += 1
            End If
        
   Case TOKEN_ALLOCATE ''"allocate"
    index += 1
    If tokens(index).kind = TOKEN_LPAREN Then
        index += 1
        Dim sizeVal As String = EvalExpression(index)
        If tokens(index).kind = TOKEN_RPAREN Then index += 1
        Dim memIndex As Integer = SimAllocate(Val(sizeVal))
        Return Str(memIndex) ' simulate pointer address
    Else
        Print "[Error: Missing '(' after allocate]"
    End If

            ''Case TOKEN_ALLOCATE
            ''index += 1
            ''If tokens(index).kind = TOKEN_LPAREN Then index += 1
            ''Dim sizeVal As String = EvalExpression(index)
            ''If tokens(index).kind = TOKEN_RPAREN Then index += 1
            ''result = AllocMem(Val(sizeVal))

        Case TOKEN_DEALLOCATE
            index += 1
            If tokens(index).kind = TOKEN_LPAREN Then index += 1
            Dim addr As String = EvalExpression(index)
            If tokens(index).kind = TOKEN_RPAREN Then index += 1
            FreeMem(addr)
            result = "0"

    
        '---------------- LEN ----------------
        Case TOKEN_LEN
            index += 1
            If tokens(index).kind = TOKEN_LPAREN Then index += 1
            Dim innerName As String = tokens(index).value
            Dim varIdx As Integer = GetSymbolIndex(innerName)
            Dim vals As String = symbolTable(varIdx)
            Dim t As String = symbolTypes(varIdx)
            Dim resultLen As Integer
            If Left(t, 7) = "zstring" Then
                If Right(vals, 1) = Chr(0) Then resultLen = Len(vals) - 1 Else resultLen = Len(vals)
            Else
                resultLen = Len(vals)
            End If
            index += 1
            If tokens(index).kind = TOKEN_RPAREN Then index += 1
            Return Str(resultLen)

        '---------------- SIZEOF ----------------
        Case TOKEN_SIZEOF
            index += 1
            If index < tokenCount And tokens(index).kind = TOKEN_LPAREN Then index += 1
            Dim varName As String = tokens(index).value
            Dim varIdx As Integer = GetSymbolIndex(varName)
            Dim resultSize As Integer
            Select Case LCase(symbolTypes(varIdx))
                Case "zstring", "wstring": resultSize = symbolDeclaredSize(varIdx)
                Case "int", "integer", "long", "short", "single": resultSize = 4
                Case "double": resultSize = 8
                Case "byte": resultSize = 1
                Case "str": resultSize = Len(symbolTable(varIdx))
                Case Else: resultSize = 0
            End Select
            index += 1
            If index < tokenCount And tokens(index).kind = TOKEN_RPAREN Then index += 1
            Return Str(resultSize)

        '---------------- Address of @var ----------------
        Case TOKEN_OPERATOR
            If tokens(index).value = "@" Then
                index += 1
                Dim varname As String = tokens(index).value
                Dim varIdx As Integer = GetSymbolIndex(varname)
                index += 1
                Return Str(varIdx)

            '---------------- Dereference *ptr ----------------
            ElseIf tokens(index).value = "*" Then
                index += 1
                Dim addr As String = EvalExpression(index)
                Dim ptrIdx As Integer = Val(addr)
                If ptrIdx >= 0 And ptrIdx < symbolCount Then
                    result = symbolTable(ptrIdx)
                Else
                    result = "0"
                    Print "Fehler: Ung�ltiger Pointer!"
                End If
                index += 1
                Return result
            End If

        '---------------- STRING LITERAL ----------------
        Case TOKEN_STRING
            result = tokens(index).value
            resultType = "str"
            index += 1

        '---------------- STR() ----------------
        Case TOKEN_STR
            index += 1
            If tokens(index).kind = TOKEN_LPAREN Then index += 1
            Dim innerVal As String = EvalExpression(index)
            If tokens(index).kind = TOKEN_RPAREN Then index += 1
            Dim temp As String = Str(Val(innerVal))
            If Left(temp, 1) = " " Then temp = Mid(temp, 2)
            result = temp
            resultType = "str"

        Case Else
            Print "Unexpected token in expression: "; tokens(index).value
            index += 1
            Return "0"
    End Select

    '---------------- Operatoren ----------------
    While index < tokenCount And tokens(index).kind = TOKEN_OPERATOR
        Dim op As String = tokens(index).value
        index += 1
        Dim rightVal As String = EvalExpression(index)
        Dim rightType As String = "int"
        If Not IsNumeric(rightVal) Then rightType = "str"

        Select Case op
            Case "+": If resultType = "str" Or rightType = "str" Then result &= rightVal: resultType = "str" Else result = Str(Val(result) + Val(rightVal)): resultType = "int"
            Case "&": result = Str(result) & Str(rightVal): resultType = "str"
            Case "-": result = Str(Val(result) - Val(rightVal)): resultType = "int"
            Case "*": result = Str(Val(result) * Val(rightVal)): resultType = "int"
            Case "/": If Val(rightVal) = 0 Then Print "Error: Division by zero": result = "0" Else result = Str(Val(result) / Val(rightVal)): resultType = "int"
            Case "%": If Val(rightVal) = 0 Then Print "Error: Modulus by zero": result = "0" Else result = Str(Fix(Val(result)) Mod Fix(Val(rightVal))): resultType = "int"
            Case Else: Print "Error: Unknown operator '" & op & "'": result = "0": resultType = "int"
        End Select
    Wend

    Return result
End Function

'---------------- ParseStatement ----------------
Sub ParseStatement(ByRef index As Integer)
    If index >= tokenCount Then Exit Sub
    Dim t As Token = tokens(index)

    Select Case t.kind
        Case TOKEN_IDENTIFIER
            Dim id As String = LCase(t.value)
            index += 1

            '--- Zuweisung / *p = ... / *p = *q ---
            If index < tokenCount And tokens(index).kind = TOKEN_ASSIGN Then
                ' normale Variable
                index += 1
                Dim expr As String = EvalExpression(index)
                Dim varIdx As Integer = GetSymbolIndex(id)

                ' zstring fix
                If symbolTypes(varIdx) = "zstring" Then
                    Dim sz As Integer = symbolDeclaredSize(varIdx)
                    If sz <= 0 Then sz = Len(expr) + 1
                    expr = Left(expr, Max(0, sz - 1)) & Chr(0)
                End If

                symbolTable(varIdx) = expr

            ElseIf index < tokenCount - 3 And tokens(index).value = "*" And tokens(index+2).kind = TOKEN_ASSIGN Then
                ' Pointer copy: *p = *q
                Dim dstName As String = id
                Dim srcName As String = tokens(index + 3).value
                Dim dstIdx As Integer = GetSymbolIndex(dstName)
                Dim srcIdx As Integer = GetSymbolIndex(srcName)
                Dim srcAddr As Integer = Val(symbolTable(srcIdx))
                Dim dstAddr As Integer = Val(symbolTable(dstIdx))
                If srcAddr >= 1 And srcAddr <= memoryUsed And dstAddr >= 1 And dstAddr <= memoryUsed Then
                    symbolTable(dstAddr) = symbolTable(srcAddr)
                End If
                index += 4

            Else
                Print "Error: Unsupported assignment"
            End If

        Case TOKEN_PRINT
            index += 1
            Dim expr As String = EvalExpression(index)
            Print expr

        Case TOKEN_DIM
            index += 1
            ' DIM Parsing handled in Parse() fully

        Case Else
            Print "Unbekannte Anweisung oder Token: " & t.value
            index += 1
    End Select
End Sub

'---------------- Parse ---------------- //
'' PARSE BLOCK    
Sub Parse()
    Dim As Integer index = 0

    While index < tokenCount
        Select Case tokens(index).kind
            Case TOKEN_DIM
                index += 1
                If index >= tokenCount Then
                    Print "Error: Unexpected end after DIM"
                    Exit Sub
                End If

                If tokens(index).kind = TOKEN_AS Then
                    index += 1
                    If index >= tokenCount Then
                        Print "Error: Unexpected end after AS"
                        Exit Sub
                    End If

                    Select Case LCase(tokens(index).value)

'------------------------ INTEGER / LONG ------------------------
                        '------------------------ INTEGER / LONG ------------------------
Case "integer", "long"
    index += 1
    Do
        ' Vor Variablennamen: optional "ptr"
        Dim isPtr As Integer = 0
        If index < tokenCount And tokens(index).kind = TOKEN_PTR Then
            isPtr = 1
            index += 1
        End If

        If index >= tokenCount Or tokens(index).kind <> TOKEN_IDENTIFIER Then
            Print "Error: Expected variable name after Integer/Long"
            Exit Sub
        End If

        Dim varname As String = tokens(index).value
        Dim varIdx As Integer = GetSymbolIndex(varname)

        If isPtr Then
            symbolTypes(varIdx) = "int ptr"
            symbolTable(varIdx) = "NULL"
        Else
            symbolTypes(varIdx) = "int"
            symbolTable(varIdx) = "0"
        End If

        index += 1

        ' Initialisierung?
        If index < tokenCount And tokens(index).kind = TOKEN_ASSIGN Then
            index += 1
            Dim value As String = EvalExpression(index)
            symbolTable(varIdx) = value
        End If

        ' Kommata?
        If index < tokenCount And tokens(index).kind = TOKEN_COMMA Then
            index += 1
        Else
            Exit Do
        End If
    Loop

'------------------------ DOUBLE ------------------------
                        Case "double"
                            index += 1
                            Do
                                If index >= tokenCount Or tokens(index).kind <> TOKEN_IDENTIFIER Then
                                    Print "Error: Expected variable name after Double"
                                    Exit Sub
                                End If

                                Dim varname As String = tokens(index).value
                                Dim varIdx As Integer = GetSymbolIndex(varname)
                                symbolTypes(varIdx) = "double"
                                symbolTable(varIdx) = "0.0"
                                index += 1

                                ' Initialisierung?
                                If index < tokenCount And tokens(index).kind = TOKEN_ASSIGN Then
                                    index += 1
                                    symbolTable(varIdx) = EvalExpression(index)
                                End If

                                If index < tokenCount And tokens(index).kind = TOKEN_COMMA Then
                                    index += 1
                                Else
                                    Exit Do
                                End If
                            Loop

'------------------------ SINGLE ------------------------
                        Case "single"
                            index += 1
                            Do
                                If index >= tokenCount Or tokens(index).kind <> TOKEN_IDENTIFIER Then
                                    Print "Error: Expected variable name after Single"
                                    Exit Sub
                                End If

                                Dim varname As String = tokens(index).value
                                Dim varIdx As Integer = GetSymbolIndex(varname)
                                symbolTypes(varIdx) = "single"
                                symbolTable(varIdx) = "0.0"
                                index += 1

                                ' Initialisierung?
                                If index < tokenCount And tokens(index).kind = TOKEN_ASSIGN Then
                                    index += 1
                                    symbolTable(varIdx) = EvalExpression(index)
                                End If

                                If index < tokenCount And tokens(index).kind = TOKEN_COMMA Then
                                    index += 1
                                Else
                                    Exit Do
                                End If
                            Loop

'------------------------ SHORT ------------------------
                        Case "short"
                            index += 1
                            Do
                                If index >= tokenCount Or tokens(index).kind <> TOKEN_IDENTIFIER Then
                                    Print "Error: Expected variable name after Short"
                                    Exit Sub
                                End If

                                Dim varname As String = tokens(index).value
                                Dim varIdx As Integer = GetSymbolIndex(varname)
                                symbolTypes(varIdx) = "short"
                                symbolTable(varIdx) = "0"
                                index += 1

                                ' Initialisierung?
                                If index < tokenCount And tokens(index).kind = TOKEN_ASSIGN Then
                                    index += 1
                                    symbolTable(varIdx) = EvalExpression(index)
                                End If

                                If index < tokenCount And tokens(index).kind = TOKEN_COMMA Then
                                    index += 1
                                Else
                                    Exit Do
                                End If
                            Loop

'------------------------ BYTE ------------------------
                        Case "byte"
                            index += 1
                            Do
                                If index >= tokenCount Or tokens(index).kind <> TOKEN_IDENTIFIER Then
                                    Print "Error: Expected variable name after Byte"
                                    Exit Sub
                                End If

                                Dim varname As String = tokens(index).value
                                Dim varIdx As Integer = GetSymbolIndex(varname)
                                symbolTypes(varIdx) = "byte"
                                symbolTable(varIdx) = "0"
                                index += 1

                                ' Initialisierung?
                                If index < tokenCount And tokens(index).kind = TOKEN_ASSIGN Then
                                    index += 1
                                    Dim v As String = EvalExpression(index)
                                    Dim n As Integer = Val(v)
                                    If n < 0 Or n > 255 Then n = n And &HFF
                                    symbolTable(varIdx) = Str(n)
                                End If

                                If index < tokenCount And tokens(index).kind = TOKEN_COMMA Then
                                    index += 1
                                Else
                                    Exit Do
                                End If
                            Loop

'------------------------ STRING ------------------------
                        Case "string"
                            index += 1
                            Do
                                If index >= tokenCount Or tokens(index).kind <> TOKEN_IDENTIFIER Then
                                    Print "Error: Expected variable name after String"
                                    Exit Sub
                                End If

                                Dim varname As String = tokens(index).value
                                Dim varIdx As Integer = GetSymbolIndex(varname)
                                symbolTypes(varIdx) = "str"
                                symbolTable(varIdx) = ""
                                index += 1

                                ' Initialisierung?
                                If index < tokenCount And tokens(index).kind = TOKEN_ASSIGN Then
                                    index += 1
                                    symbolTable(varIdx) = EvalExpression(index)
                                End If

                                If index < tokenCount And tokens(index).kind = TOKEN_COMMA Then
                                    index += 1
                                Else
                                    Exit Do
                                End If
                            Loop

'------------------------ ZSTRING ------------------------
 Case "zstring"
    index += 1

    ' Pr�fen auf Stern + Zahl (zstring * 15)
    Dim declSize As Integer = 0
    If index < tokenCount And tokens(index).kind = TOKEN_ASTERISK Then
        index += 1
        If index < tokenCount And tokens(index).kind = TOKEN_NUMBER Then
            declSize = Val(tokens(index).value)
            index += 1
        End If
    End If

    Do
        Dim isPtr As Integer = 0
        If index < tokenCount And tokens(index).kind = TOKEN_PTR Then
            isPtr = 1
            index += 1
        End If

        If index >= tokenCount Or tokens(index).kind <> TOKEN_IDENTIFIER Then
            Print "Error: Expected variable name after ZSTRING"
            Exit Sub
        End If

        Dim varname As String = tokens(index).value
        Dim varIdx As Integer = GetSymbolIndex(varname)
        index += 1

        If isPtr Then
            symbolTypes(varIdx) = "zstring ptr"
            symbolTable(varIdx) = "NULL"
        Else
            symbolTypes(varIdx) = "zstring"
            symbolDeclaredSize(varIdx) = declSize
            If declSize > 0 Then
                symbolTable(varIdx) = String(declSize - 1, Chr(0))
            Else
                symbolTable(varIdx) = ""
            End If
        End If

        ' Optional Initialisierung
        If index < tokenCount And tokens(index).kind = TOKEN_ASSIGN Then
            index += 1
            Dim initValue As String = EvalExpression(index)
            If isPtr Then
                symbolTable(varIdx) = initValue
            Else
                Dim sz As Integer = IIf(declSize > 0, declSize, Len(initValue) + 1)
                symbolTable(varIdx) = Left(initValue, sz - 1) & Chr(0)
            End If
        End If

        If index < tokenCount And tokens(index).kind = TOKEN_COMMA Then
            index += 1
        Else
            Exit Do
        End If
    Loop

'------------------------ WSTRING ------------------------
                        Case "wstring"
                            index += 1
                            Do
                                If index + 1 >= tokenCount Or tokens(index).kind <> TOKEN_IDENTIFIER Then
                                    Print "Error: Expected variable name after WString"
                                    Exit Sub
                                End If

                                Dim varname As String = tokens(index).value
                                Dim varIdx As Integer = GetSymbolIndex(varname)
                                symbolTypes(varIdx) = "wstring"
                                symbolTable(varIdx) = ""
                                index += 1

                                If index < tokenCount And tokens(index).kind = TOKEN_ASSIGN Then
                                    index += 1
                                    symbolTable(varIdx) = EvalExpression(index) & Chr(0)
                                End If

                                If index < tokenCount And tokens(index).kind = TOKEN_COMMA Then
                                    index += 1
                                Else
                                    Exit Do
                                End If
                            Loop

'------------------------ ANY ------------------------
                        Case "any"
                            Print "Warning: 'Any' type accepted but not supported."
                            index += 1
                            If index >= tokenCount Or tokens(index).kind <> TOKEN_IDENTIFIER Then
                                Print "Error: Expected variable name after ANY"
                                Exit Sub
                            End If
                            index += 1

                        Case Else
                            Print "Error: Unsupported type after AS"
                            Exit Sub
                    End Select

                Else
                    Print "Error: Expected 'AS' after DIM"
                    Exit Sub
                End If

'------------------------------------ Andere Tokens ------------------------
Case TOKEN_CONST
    index += 1
    If tokens(index).kind <> TOKEN_IDENTIFIER Then
        Print "Error: Expected identifier after CONST"
        Exit Sub
    End If
    Dim cname As String = tokens(index).value
    Dim cIdx As Integer = GetSymbolIndex(cname)
    index += 1
    If tokens(index).kind <> TOKEN_ASSIGN Then
        Print "Error: Expected '=' in CONST"
        Exit Sub
    End If
    index += 1
    Dim cval As String = EvalExpression(index)
    symbolTypes(cIdx) = "const"
    symbolTable(cIdx) = cval

            Case TOKEN_IDENTIFIER
                Dim varIdx As Integer = GetSymbolIndex(tokens(index).value)
                index += 1
                If index < tokenCount And tokens(index).kind = TOKEN_ASSIGN Then
                    index += 1
                    Dim assignedValue As String = EvalExpression(index)

                    If symbolTypes(varIdx) = "zstring" Then
    							Dim sz As Integer = symbolDeclaredSize(varIdx)
    							If sz <= 0 Then sz = Len(assignedValue) + 1
    							assignedValue = Left(assignedValue, Max(0, sz - 1)) & Chr(0)
                    End If

                    symbolTable(varIdx) = assignedValue
                Else
                    'Print "Error: Expected '=' after variable name"
                    Exit Sub
                End If

'neu function...endfunction, begin it return werten einfach

Case TOKEN_FUNCTION
                index += 1
                Dim fname As String = tokens(index).value
                index += 1
                
                ' Parameterliste
                Dim paramList As String = ""
                If index < tokenCount And tokens(index).kind = TOKEN_LPAREN Then
                    index += 1
                    While tokens(index).kind <> TOKEN_RPAREN
                        Dim pname As String = tokens(index).value
                        index += 1
                        If tokens(index).kind = TOKEN_AS Then
                            index += 1
                            Dim ptype As String = tokens(index).value
                            functionParamNames(functionCount, functionParamCount(functionCount)) = pname
                            functionParamTypes(functionCount, functionParamCount(functionCount)) = LCase(ptype)
                            functionParamCount(functionCount) += 1
                        End If
                        If tokens(index).kind = TOKEN_COMMA Then index += 1
                    Wend
                    index += 1 ' skip ")"
                End If
                
                ' R�ckgabewert optional
                Dim returnType As String = "int"
                If index < tokenCount And tokens(index).kind = TOKEN_AS Then
                    index += 1
                    returnType = tokens(index).value
                    index += 1
                End If
                
                ' Funktionsrumpf sammeln
                Dim sb As String = ""
                Dim startIdx As Integer = index
                While index < tokenCount And tokens(index).kind <> TOKEN_ENDFUNCTION
                    sb &= tokens(index).value & " "
                    index += 1
                Wend
                
                ' Speichern
                functionNames(functionCount) = fname
                functionBodies(functionCount) = Trim(sb)
                functionReturnTypes(functionCount) = LCase(returnType)
                functionCount += 1
                
                ' Skip ENDFUNCTION
                If index < tokenCount And tokens(index).kind = TOKEN_ENDFUNCTION Then index += 1

'neu
Case TOKEN_DEFINE
    index += 1
    If tokens(index).kind <> TOKEN_IDENTIFIER Then
        Print "Error: Expected identifier after #define"
        Exit Sub
    End If
    Dim defName As String = tokens(index).value
    Dim defIdx As Integer = GetSymbolIndex(defName)
    index += 1
    Dim defValue As String = EvalExpression(index)
    symbolTypes(defIdx) = "define"
    symbolTable(defIdx) = defValue

            Case TOKEN_PRINT
                index += 1
                Dim printResult As String = EvalExpression(index)
                Print printResult

            Case TOKEN_SLEEP
                Sleep
                index += 1

            Case TOKEN_IF
                index += 1
                Dim condition As String = EvalExpression(index)

                If index >= tokenCount Or tokens(index).kind <> TOKEN_THEN Then
                    Print "Error: Expected 'then'"
                    Exit Sub
                End If
                index += 1

                If Val(condition) <> 0 Then
                    While index < tokenCount AndAlso tokens(index).kind <> TOKEN_ELSE AndAlso tokens(index).kind <> TOKEN_ENDIF
                        ParseStatement(index)
                    Wend
                    If index < tokenCount And tokens(index).kind = TOKEN_ELSE Then
                        While index < tokenCount And tokens(index).kind <> TOKEN_ENDIF
                            index += 1 ' skip ELSE branch
                        Wend
                    End If
                Else
                    While index < tokenCount AndAlso tokens(index).kind <> TOKEN_ELSE AndAlso tokens(index).kind <> TOKEN_ENDIF
                        index += 1 ' skip IF branch
                    Wend
                    If index < tokenCount And tokens(index).kind = TOKEN_ELSE Then
                        index += 1
                        While index < tokenCount And tokens(index).kind <> TOKEN_ENDIF
                            ParseStatement(index)
                        Wend
                    End If
                End If

                If index < tokenCount And tokens(index).kind = TOKEN_ENDIF Then
                    index += 1
                Else
                    Print "Error: Expected 'endif'"
                    Exit Sub
                End If
					Case TOKEN_EOF
    						Exit While

               'Case TOKEN_NUMBER, TOKEN_STRING, TOKEN_ZSTRING
                'Dim result As String = EvalExpression(index)
                 'Print result

            Case Else
                Print "Error: Unexpected token in parse loop: " & tokens(index).value
                index += 1
        End Select
    Wend
End Sub


for i as integer = 1 to memoryUsed
    print "mem("; i; ") = ["; memorySlots(i); "]"
next

Sub DumpMemory()
    Print "--- Memory Dump ---"
    For i As Integer = 1 To memoryUsed
        If memorySlots(i) <> "" Then
            Print "Slot "; i; " -> ["; memorySlots(i); "]"
        End If
    Next
    Print "-------------------"
End Sub
'--------------------------------------- //
Dim As String inputLine
Dim As String programBlock = ""

Print "Little LionParser (Multiline Mode)"
Print "Type your code line-by-line or.."
Print "Type your code multiline copy paste."

Print "Type 'run' to execute, 'clear' to reset, or 'exit' to quit."

' includes function using
Do
    Line Input "> ", inputLine
    Select Case LCase(Trim(inputLine))
        Case "run"
            If Len(Trim(programBlock)) > 0 Then
                Tokenize(programBlock)
                Parse()
                programBlock = ""
            Else
                Print "No code to run."
            End If

        Case "clear"
            programBlock = ""
            Print "Code buffer cleared."

        Case "exit"
            Exit Do

        Case Else
            ' ---- MOVE THE #INCLUDE CHECK HERE ----
            If LCase(Left(Trim(inputLine), 8)) = "#include" Then
                Dim incFile As String = Trim(Mid(Trim(inputLine), 9))
                incFile = Trim(incFile, """") ' Remove quotes
                Dim fileContent As String = includes(incFile)

                If Left(fileContent, 7) = "[Error:" Then
                    Print fileContent
                Else
                    programBlock &= fileContent & Chr(10)
                    Print "[Included: " & incFile & "]"
                End If
            Else
                programBlock &= inputLine & Chr(10)
            End If
    End Select
Loop

'
DumpMemory()


'ends / 31-07-2025, 18-08-2025, 19-08-2025, 20-08-2025, 23-08-2025, 25-08-2025, 08-09-2025, 19-09-25, lionparser v10d
' 28-10-2025
