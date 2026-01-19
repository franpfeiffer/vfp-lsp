* Sample Visual FoxPro Program
* This file demonstrates various VFP language features

#DEFINE MAX_ITEMS 100
#DEFINE APP_NAME "My Application"

* ============================================================================
* Function: GetCustomerName
* Purpose: Retrieves customer name from database
* ============================================================================
FUNCTION GetCustomerName(nCustomerId)
    LOCAL cName, nCount
    cName = ""
    nCount = 0

    IF EMPTY(nCustomerId)
        RETURN ""
    ENDIF

    SELECT name FROM customers ;
        WHERE id = nCustomerId ;
        INTO CURSOR csrResult

    IF _TALLY > 0
        cName = csrResult.name
    ENDIF

    USE IN SELECT("csrResult")

    RETURN cName
ENDFUNC

* ============================================================================
* Procedure: ProcessOrders
* Purpose: Process all pending orders
* ============================================================================
PROCEDURE ProcessOrders
    LPARAMETERS dStartDate, dEndDate

    LOCAL nTotal, oOrder
    nTotal = 0

    SELECT orders.id, orders.total, customers.name ;
        FROM orders ;
        INNER JOIN customers ON orders.custid = customers.id ;
        WHERE orders.orderdate BETWEEN dStartDate AND dEndDate ;
        AND orders.status = "PENDING" ;
        ORDER BY orders.orderdate ;
        INTO CURSOR csrOrders

    SCAN
        nTotal = nTotal + csrOrders.total

        && Update order status
        UPDATE orders SET status = "PROCESSED" ;
            WHERE id = csrOrders.id

        IF nTotal > 10000
            EXIT
        ENDIF
    ENDSCAN

    USE IN SELECT("csrOrders")
ENDPROC

* ============================================================================
* Class: MyButton
* Purpose: Custom button class with enhanced functionality
* ============================================================================
DEFINE CLASS MyButton AS CommandButton
    Caption = "Click Me"
    Width = 100
    Height = 30
    lClicked = .F.
    nClickCount = 0

    PROCEDURE Init
        THIS.nClickCount = 0
        DODEFAULT()
    ENDPROC

    PROCEDURE Click
        THIS.lClicked = .T.
        THIS.nClickCount = THIS.nClickCount + 1
        MESSAGEBOX("Button clicked " + TRANSFORM(THIS.nClickCount) + " times!")
    ENDPROC

    PROCEDURE Reset
        THIS.lClicked = .F.
        THIS.nClickCount = 0
    ENDPROC
ENDDEFINE

* ============================================================================
* Class: DataProcessor
* Purpose: Handles data processing with error handling
* ============================================================================
DEFINE CLASS DataProcessor AS Custom
    PROTECTED cErrorMessage
    cErrorMessage = ""

    PROCEDURE ProcessData
        LPARAMETERS aData

        LOCAL i, nValue

        TRY
            FOR i = 1 TO ALEN(aData)
                nValue = aData[i]

                DO CASE
                    CASE nValue < 0
                        THIS.cErrorMessage = "Negative value found"
                        THROW THIS.cErrorMessage

                    CASE nValue = 0
                        LOOP

                    CASE nValue > MAX_ITEMS
                        EXIT

                    OTHERWISE
                        * Process normal value
                        THIS.ProcessItem(nValue)
                ENDCASE
            ENDFOR

        CATCH TO oException
            THIS.cErrorMessage = oException.Message
            RETURN .F.

        FINALLY
            * Cleanup code here
            RELEASE aData
        ENDTRY

        RETURN .T.
    ENDPROC

    PROTECTED PROCEDURE ProcessItem
        LPARAMETERS nValue
        * Process individual item
    ENDPROC
ENDDEFINE

* ============================================================================
* Main Program
* ============================================================================
#IF VERSION(5) >= 900
    * VFP 9.0+ specific code
    LOCAL oProcessor
    oProcessor = CREATEOBJECT("DataProcessor")
#ELSE
    * Older version fallback
    NOTE This code runs on older VFP versions
#ENDIF

* Using WITH block for object property access
WITH THISFORM
    .Caption = APP_NAME
    .Width = 800
    .Height = 600
    .AutoCenter = .T.
ENDWITH

* Date literal examples
LOCAL dToday, tNow
dToday = {^2024-01-15}
tNow = {^2024-01-15 10:30:00}

* Text merge example
TEXT TO lcOutput NOSHOW TEXTMERGE
Dear <<lcCustomerName>>,

Thank you for your order #<<nOrderId>>.
Your total is $<<nTotal>>.

Sincerely,
The Team
ENDTEXT

* Array operations
DIMENSION aItems[10]
aItems[1] = "First"
aItems[2] = "Second"

* Different string delimiters
LOCAL cStr1, cStr2
cStr1 = "Double quoted string"
cStr2 = 'Single quoted string'

* Logical operators
IF .NOT. EMPTY(cStr1) .AND. (nValue > 0 .OR. lFlag)
    ? "Condition met"
ENDIF

* Hexadecimal values
LOCAL nHex1, nHex2
nHex1 = 0xFF
nHex2 = 0hABCD

* DO WHILE loop
nCount = 0
DO WHILE nCount < 10
    nCount = nCount + 1
    IF nCount = 5
        LOOP
    ENDIF
ENDDO

* FOR EACH loop (VFP 9+)
LOCAL oCollection
FOR EACH oItem IN oCollection
    ? oItem.Name
ENDFOR

RETURN
