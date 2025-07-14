*> TRANS.cpy - Transaction Record Definition
*> This copybook defines the structure of a transaction record
01  TRANSACTION-RECORD.
    05  TRANS-ID                PIC 9(12).
    05  TRANS-DATE              PIC X(8).
    05  TRANS-TIME              PIC X(8).
    05  TRANS-TYPE              PIC X(1).
        88  DEPOSIT-TRANS       VALUE 'D'.
        88  WITHDRAWAL-TRANS    VALUE 'W'.
        88  TRANSFER-OUT-TRANS  VALUE 'T'.
        88  TRANSFER-IN-TRANS   VALUE 'I'.
    05  FROM-ACCOUNT            PIC 9(10).
    05  TO-ACCOUNT              PIC 9(10).
    05  TRANS-AMOUNT            PIC S9(13)V99.
    05  TRANS-DESCRIPTION       PIC X(50).
    05  TELLER-ID               PIC X(10).
    05  FILLER                  PIC X(5).

*> Working Storage Variables for Transaction Operations
01  WS-TRANSACTION-VARIABLES.
    05  WS-TRANS-ID             PIC 9(12).
    05  WS-TRANS-DATE           PIC X(8).
    05  WS-TRANS-TIME           PIC X(8).
    05  WS-TRANS-TYPE           PIC X(1).
    05  WS-FROM-ACCOUNT         PIC 9(10).
    05  WS-TRANS-AMOUNT         PIC S9(13)V99.
    05  WS-TRANS-DESCRIPTION    PIC X(50).
    05  WS-TELLER-ID            PIC X(10).
    05  WS-TRANS-COUNT          PIC 9(5).

*> Transaction Constants
01  TRANSACTION-CONSTANTS.
    05  MAX-TRANS-ID            PIC 9(12) VALUE 999999999999.
    05  DEFAULT-TELLER-ID       PIC X(10) VALUE 'TELLER001'.
    05  TRANS-SUCCESS           PIC X(1) VALUE 'Y'.
    05  TRANS-FAILURE           PIC X(1) VALUE 'N'. 