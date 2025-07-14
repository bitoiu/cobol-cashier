*> COMMON.cpy - Common Constants and Variables
*> This copybook defines common constants and variables

*> File Status Codes
01  FILE-STATUS-CODES.
    05  WS-FILE-STATUS          PIC X(2).
        88  FILE-SUCCESS        VALUE '00'.
        88  FILE-NOT-FOUND      VALUE '23'.
        88  FILE-AT-END         VALUE '10'.
        88  FILE-DUPLICATE      VALUE '22'.
        88  FILE-ERROR          VALUE '30' THRU '99'.

*> Screen Constants
01  SCREEN-CONSTANTS.
    05  SCREEN-LINES            PIC 9(2) VALUE 24.
    05  SCREEN-COLUMNS          PIC 9(2) VALUE 80.
    05  CLEAR-SCREEN            PIC X(4) VALUE X'1B5B324A'.
    05  CURSOR-HOME             PIC X(3) VALUE X'1B5B48'.

*> System Constants
01  SYSTEM-CONSTANTS.
    05  SYSTEM-NAME             PIC X(30) VALUE 'COBOL BANK LEDGER SYSTEM'.
    05  SYSTEM-VERSION          PIC X(10) VALUE 'V1.0'.
    05  MAX-MENU-OPTION         PIC 9(1) VALUE 7.
    05  YES-NO-INDICATOR        PIC X(1).
        88  YES-ANSWER          VALUE 'Y' 'y'.
        88  NO-ANSWER           VALUE 'N' 'n'.

*> Date and Time Working Variables
01  DATE-TIME-VARIABLES.
    05  WS-CURRENT-DATE         PIC X(8).
    05  WS-CURRENT-TIME         PIC X(8).
    05  WS-FORMATTED-DATE       PIC X(10).
    05  WS-FORMATTED-TIME       PIC X(8).

*> Error Messages
01  ERROR-MESSAGES.
    05  ERR-INVALID-ACCOUNT     PIC X(50) VALUE 
        'ERROR: Invalid account number'.
    05  ERR-INSUFFICIENT-FUNDS  PIC X(50) VALUE 
        'ERROR: Insufficient funds for transaction'.
    05  ERR-ACCOUNT-NOT-FOUND   PIC X(50) VALUE 
        'ERROR: Account not found'.
    05  ERR-INVALID-AMOUNT      PIC X(50) VALUE 
        'ERROR: Invalid transaction amount'.
    05  ERR-FILE-ERROR          PIC X(50) VALUE 
        'ERROR: File operation failed'.

*> Success Messages
01  SUCCESS-MESSAGES.
    05  MSG-ACCOUNT-CREATED     PIC X(50) VALUE 
        'SUCCESS: Account created successfully'.
    05  MSG-TRANSACTION-COMPLETE PIC X(50) VALUE 
        'SUCCESS: Transaction completed'.
    05  MSG-BALANCE-UPDATED     PIC X(50) VALUE 
        'SUCCESS: Balance updated'. 
