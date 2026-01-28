* Test file for document formatting

FUNCTION Test
? "Hello"
  FOR i = 1 TO 10
    IF i > 5
      ? i
    ENDIF
    ENDFOR
      RETURN .T.
    ENDFUNC

PROCEDURE MyProc
  LOCAL x, y
  x = 1
  y = 2
  IF x == y
? "Equal"
  ELSE
? "Not equal"
  ENDIF
  ENDPROC

* Test with nested blocks
IF .T.
  FOR j = 1 TO 5
    WHILE j > 0
      ? j
      j = j - 1
    ENDWHILE
    ENDFOR
    ENDIF

* Test TEXT block (should preserve formatting)
TEXT
  This content
    should NOT
      be formatted
      ENDTEXT
