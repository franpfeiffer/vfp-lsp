* Preprocessor Directive Examples
* Demonstrates VFP preprocessor features

* Simple constant definitions
#DEFINE VERSION_MAJOR 1
#DEFINE VERSION_MINOR 0
#DEFINE VERSION_BUILD 123

#DEFINE APP_NAME "My VFP Application"
#DEFINE COPYRIGHT "Copyright 2024"

* Computed constant
#DEFINE VERSION_STRING APP_NAME + " v" + STR(VERSION_MAJOR) + "." + STR(VERSION_MINOR)

* Boolean constants
#DEFINE DEBUG_MODE .T.
#DEFINE RELEASE_MODE .F.

* Path constants
#DEFINE DATA_PATH "C:\Data\"
#DEFINE CONFIG_FILE DATA_PATH + "config.dbf"

* Macro definitions with parameters
#DEFINE SQUARE(x) ((x) * (x))
#DEFINE MAX(a,b) IIF((a) > (b), (a), (b))
#DEFINE MIN(a,b) IIF((a) < (b), (a), (b))
#DEFINE BETWEEN(val,lo,hi) ((val) >= (lo) .AND. (val) <= (hi))

* Conditional compilation
#DEFINE PLATFORM "WINDOWS"

#IF PLATFORM = "WINDOWS"
    #DEFINE PATH_SEPARATOR "\"
    #DEFINE NEWLINE CHR(13) + CHR(10)
#ELSE
    #DEFINE PATH_SEPARATOR "/"
    #DEFINE NEWLINE CHR(10)
#ENDIF

* Version-based conditional compilation
#IF VERSION(5) >= 900
    * VFP 9.0+ specific features
    #DEFINE HAS_COLLECTION .T.
    #DEFINE HAS_TRY_CATCH .T.
#ELSE
    #DEFINE HAS_COLLECTION .F.
    #DEFINE HAS_TRY_CATCH .F.
#ENDIF

* Debug vs Release builds
#IFDEF DEBUG_MODE
    #DEFINE LOG(msg) STRTOFILE(TRANSFORM(DATETIME()) + ": " + (msg) + CHR(13) + CHR(10), "debug.log", .T.)
    #DEFINE ASSERT(cond) IF .NOT. (cond) THEN MESSAGEBOX("Assertion failed: " + #cond)
#ELSE
    #DEFINE LOG(msg)
    #DEFINE ASSERT(cond)
#ENDIF

* Feature flags
#DEFINE FEATURE_EXPORT_PDF
#DEFINE FEATURE_EXPORT_EXCEL
#UNDEF FEATURE_EXPORT_CSV

#IFDEF FEATURE_EXPORT_PDF
    * PDF export is enabled
    #DEFINE PDF_DPI 300
#ENDIF

#IFNDEF FEATURE_EXPORT_CSV
    * CSV export is disabled
    NOTE CSV export feature is not available
#ENDIF

* Include files
#INCLUDE "foxpro.h"
#INCLUDE "common.h"

* Nested conditionals
#IF VERSION(5) >= 800
    #IF VERSION(5) >= 900
        #DEFINE VFP_VERSION "9.0"
    #ELSE
        #DEFINE VFP_VERSION "8.0"
    #ENDIF
#ELIF VERSION(5) >= 700
    #DEFINE VFP_VERSION "7.0"
#ELSE
    #DEFINE VFP_VERSION "6.0 or earlier"
#ENDIF

* Using defined constants in code
FUNCTION TestPreprocessor
    LOCAL nResult, nValue

    nValue = 5
    nResult = SQUARE(nValue)
    ? "Square of", nValue, "is", nResult

    * Conditional code based on preprocessor
    #IFDEF DEBUG_MODE
        LOG("TestPreprocessor started")
        ? "Running in DEBUG mode"
    #ELSE
        ? "Running in RELEASE mode"
    #ENDIF

    * Using MIN/MAX macros
    ? "Max of 10 and 20:", MAX(10, 20)
    ? "Min of 10 and 20:", MIN(10, 20)

    * Using BETWEEN macro
    IF BETWEEN(nValue, 1, 10)
        ? nValue, "is between 1 and 10"
    ENDIF

    * Feature-based code
    #IFDEF FEATURE_EXPORT_PDF
        ? "PDF export available at", PDF_DPI, "DPI"
    #ENDIF

    #IFDEF FEATURE_EXPORT_EXCEL
        ? "Excel export available"
    #ENDIF

    #IFNDEF FEATURE_EXPORT_CSV
        ? "CSV export not available"
    #ENDIF

    RETURN .T.
ENDFUNC

* Constants can be undefined and redefined
#UNDEF DEBUG_MODE
#DEFINE DEBUG_MODE .F.

* Multi-line macro (using line continuation)
#DEFINE CREATE_CUSTOMER(name, email) ;
    INSERT INTO customers (name, email) ;
    VALUES ((name), (email))

RETURN
