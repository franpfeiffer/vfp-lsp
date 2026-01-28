* Test file for code actions

* Test 1: Unterminated string
LOCAL cText
cText = "Hello world

* Test 2: Misspelled keyword
FUNCION Test
    RETURN .T.
ENDFUNC

* Test 3: Unclosed block
IF .T.
    ? "Test"

* Test 4: Orphaned END
? "Another test"
ENDIF

* Test 5: Multiple issues
PROCEDUR MyProc
    cMsg = 'Hello
    FOR i = 1 TO 10
        ? i
