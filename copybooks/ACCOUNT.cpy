*> ACCOUNT.cpy - Account Master Record Definition
*> This copybook defines the structure of an account record
01  ACCOUNT-RECORD.
    05  ACCOUNT-NUMBER          PIC 9(10).
    05  ACCOUNT-TYPE            PIC X(1).
        88  CHECKING-ACCOUNT    VALUE 'C'.
        88  SAVINGS-ACCOUNT     VALUE 'S'.
    05  ACCOUNT-STATUS          PIC X(1).
        88  ACTIVE-ACCOUNT      VALUE 'A'.
        88  INACTIVE-ACCOUNT    VALUE 'I'.
    05  ACCOUNT-HOLDER.
        10  HOLDER-FIRST-NAME   PIC X(20).
        10  HOLDER-LAST-NAME    PIC X(20).
        10  HOLDER-ADDRESS      PIC X(50).
        10  HOLDER-PHONE        PIC X(15).
    05  ACCOUNT-BALANCE         PIC S9(13)V99.
    05  ACCOUNT-OPEN-DATE       PIC X(8).
    05  LAST-TRANSACTION-DATE   PIC X(8).
    05  FILLER                  PIC X(10).

*> Working Storage Variables for Account Operations
01  WS-ACCOUNT-VARIABLES.
    05  WS-ACCOUNT-NUMBER       PIC 9(10).
    05  WS-ACCOUNT-BALANCE      PIC S9(13)V99.
    05  WS-ACCOUNT-TYPE         PIC X(1).
    05  WS-ACCOUNT-STATUS       PIC X(1).
    05  WS-HOLDER-NAME          PIC X(41).
    05  WS-ACCOUNT-FOUND        PIC X(1).
        88  ACCOUNT-FOUND       VALUE 'Y'.
        88  ACCOUNT-NOT-FOUND   VALUE 'N'.

*> Constants
01  ACCOUNT-CONSTANTS.
    05  MAX-ACCOUNT-NUMBER      PIC 9(10) VALUE 9999999999.
    05  MIN-ACCOUNT-NUMBER      PIC 9(10) VALUE 1000000000. 