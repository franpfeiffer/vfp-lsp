" Vim syntax file
" Language: Visual FoxPro
" Maintainer: VFP-LSP Team
" Latest Revision: 2024

if exists("b:current_syntax")
  finish
endif

" Case insensitive
syn case ignore

" Comments
syn match vfpComment "^\s*\*.*$"
syn match vfpComment "&&.*$"
syn match vfpCommentNote "^\s*NOTE\s\+.*$"

" Preprocessor
syn match vfpPreProc "^\s*#\(DEFINE\|DEF\|UNDEF\|IF\|IFDEF\|IFNDEF\|ELSE\|ELIF\|ENDIF\|INCLUDE\)\b"

" Strings
syn region vfpString start=+"+ skip=+\\.+ end=+"+
syn region vfpString start=+'+ skip=+\\.+ end=+'+
syn region vfpString start=+\[+ end=+\]+

" Numbers
syn match vfpNumber "\<\d\+\>"
syn match vfpNumber "\<\d\+\.\d*\([eE][+-]\?\d\+\)\?\>"
syn match vfpNumber "\<0[xXhH][0-9a-fA-F]\+\>"

" Boolean/Null constants
syn match vfpConstant "\.T\.\|\.TRUE\."
syn match vfpConstant "\.F\.\|\.FALSE\."
syn match vfpConstant "\.NULL\."

" Date literals
syn match vfpDate "{\^[^}]\+}"

" Logical operators
syn match vfpOperator "\.AND\.\|\.OR\.\|\.NOT\."

" Control flow keywords
syn keyword vfpConditional IF ELSE ELSEIF ENDIF THEN
syn keyword vfpConditional DO CASE ENDCASE OTHERWISE
syn keyword vfpRepeat WHILE ENDDO FOR EACH ENDFOR NEXT TO STEP IN
syn keyword vfpRepeat SCAN ENDSCAN
syn keyword vfpStatement EXIT LOOP RETURN

" Exception handling
syn keyword vfpException TRY CATCH FINALLY ENDTRY THROW

" Function/Procedure keywords
syn keyword vfpDefine FUNCTION ENDFUNC PROCEDURE ENDPROC
syn keyword vfpDefine LPARAMETERS PARAMETERS

" Class keywords
syn keyword vfpStructure DEFINE CLASS ENDDEFINE AS OF OLEPUBLIC
syn keyword vfpStructure PROTECTED HIDDEN

" Variable declarations
syn keyword vfpStorageClass LOCAL PRIVATE PUBLIC DIMENSION DECLARE EXTERNAL

" Special keywords
syn keyword vfpKeyword WITH ENDWITH TEXT ENDTEXT
syn keyword vfpKeyword USE ALIAS CLOSE CLEAR RELEASE STORE
syn keyword vfpKeyword THIS THISFORM THISFORMSET DODEFAULT NODEFAULT

" SQL keywords
syn keyword vfpSQL SELECT FROM WHERE ORDER BY GROUP HAVING
syn keyword vfpSQL INTO CURSOR TABLE ARRAY
syn keyword vfpSQL INSERT UPDATE DELETE SET VALUES
syn keyword vfpSQL JOIN INNER LEFT RIGHT OUTER FULL CROSS ON
syn keyword vfpSQL DISTINCT TOP UNION ALL
syn keyword vfpSQL AND OR NOT LIKE BETWEEN IS NULL
syn keyword vfpSQL CREATE ALTER DROP ADD COLUMN INDEX TAG
syn keyword vfpSQL ASC DESC NOFILTER READWRITE

" Built-in functions (partial list)
syn keyword vfpFunction ABS ACLASS ACOPY ADIR AELEMENT AFIELDS ALEN
syn keyword vfpFunction ALIAS ALLTRIM ASC AT BETWEEN BOF CDOW CHR
syn keyword vfpFunction CMONTH CREATEOBJECT CTOD DATE DATETIME DAY
syn keyword vfpFunction DELETED DTOC DTOS EMPTY EOF EVALUATE FILE
syn keyword vfpFunction FOUND GETENV GOMONTH IIF INLIST INT ISDIGIT
syn keyword vfpFunction ISNULL LEFT LEN LOWER LTRIM MAX MESSAGEBOX
syn keyword vfpFunction MIN MOD MONTH NVL PADL PADR PROPER RAT
syn keyword vfpFunction RECCOUNT RECNO REPLICATE RIGHT ROUND RTRIM
syn keyword vfpFunction SECONDS SEEK SELECT SPACE SQRT STR STRTOFILE
syn keyword vfpFunction STUFF SUBSTR SYS TIME TRANSFORM TRIM TYPE
syn keyword vfpFunction UPPER USED VAL VARTYPE VERSION YEAR

" Highlighting
hi def link vfpComment Comment
hi def link vfpCommentNote Comment
hi def link vfpPreProc PreProc
hi def link vfpString String
hi def link vfpNumber Number
hi def link vfpConstant Constant
hi def link vfpDate Constant
hi def link vfpOperator Operator
hi def link vfpConditional Conditional
hi def link vfpRepeat Repeat
hi def link vfpStatement Statement
hi def link vfpException Exception
hi def link vfpDefine Define
hi def link vfpStructure Structure
hi def link vfpStorageClass StorageClass
hi def link vfpKeyword Keyword
hi def link vfpSQL Keyword
hi def link vfpFunction Function

let b:current_syntax = "vfp"
