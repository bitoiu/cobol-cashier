IDENTIFICATION DIVISION.
PROGRAM-ID. BANKLED.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT ACCOUNT-FILE ASSIGN TO 'data/accounts.dat'
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS ACCOUNT-NUMBER
        FILE STATUS IS WS-FILE-STATUS.
    
    SELECT TRANSACTION-FILE ASSIGN TO 'data/trans.dat'
        ORGANIZATION IS SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS WS-TRANS-FILE-STATUS.

DATA DIVISION.
FILE SECTION.
FD  ACCOUNT-FILE.
01  ACCOUNT-RECORD.
    05  ACCOUNT-NUMBER          PIC 9(10).
    05  ACCOUNT-TYPE            PIC X(1).
    05  ACCOUNT-STATUS          PIC X(1).
    05  ACCOUNT-HOLDER.
        10  HOLDER-FIRST-NAME   PIC X(20).
        10  HOLDER-LAST-NAME    PIC X(20).
        10  HOLDER-ADDRESS      PIC X(50).
        10  HOLDER-PHONE        PIC X(15).
    05  ACCOUNT-BALANCE         PIC S9(13)V99.
    05  ACCOUNT-OPEN-DATE       PIC X(8).
    05  LAST-TRANSACTION-DATE   PIC X(8).
    05  FILLER                  PIC X(10).

FD  TRANSACTION-FILE.
01  TRANSACTION-RECORD.
    05  TRANS-ID                PIC 9(12).
    05  TRANS-DATE              PIC X(8).
    05  TRANS-TIME              PIC X(8).
    05  TRANS-TYPE              PIC X(1).
    05  FROM-ACCOUNT            PIC 9(10).
    05  TO-ACCOUNT              PIC 9(10).
    05  TRANS-AMOUNT            PIC S9(13)V99.
    05  TRANS-DESCRIPTION       PIC X(50).
    05  TELLER-ID               PIC X(10).
    05  FILLER                  PIC X(5).

WORKING-STORAGE SECTION.
COPY COMMON.

*> Account Status Variables
01  WS-ACCOUNT-STATUS-FLAGS.
    05  WS-ACCOUNT-FOUND        PIC X(1).
        88  ACCOUNT-FOUND       VALUE 'Y'.
        88  ACCOUNT-NOT-FOUND   VALUE 'N'.

01  WS-MENU-CHOICE              PIC 9(1).
01  WS-CONTINUE-FLAG            PIC X(1) VALUE 'Y'.
    88  CONTINUE-PROCESSING     VALUE 'Y'.
    88  STOP-PROCESSING         VALUE 'N'.

01  WS-TRANS-FILE-STATUS        PIC X(2).
01  WS-NEXT-ACCOUNT-NUM         PIC 9(10) VALUE 1000000006.
01  WS-NEXT-TRANS-ID            PIC 9(12) VALUE 1.

01  WS-SCREEN-FIELDS.
    05  WS-INPUT-ACCOUNT        PIC 9(10).
    05  WS-INPUT-AMOUNT         PIC 9(11)V99.
    05  WS-INPUT-AMOUNT-DISPLAY PIC ZZZ,ZZZ,ZZ9.99.
    05  WS-BALANCE-DISPLAY      PIC ZZZ,ZZZ,ZZ9.99.
    05  WS-INPUT-FIRST-NAME     PIC X(20).
    05  WS-INPUT-LAST-NAME      PIC X(20).
    05  WS-INPUT-ADDRESS        PIC X(50).
    05  WS-INPUT-PHONE          PIC X(15).
    05  WS-INPUT-ACCOUNT-TYPE   PIC X(1).
    05  WS-TO-ACCOUNT           PIC 9(10).
    05  WS-FROM-ACCOUNT-TEMP    PIC 9(10).
    05  WS-FROM-BALANCE         PIC S9(13)V99.
    05  WS-TO-BALANCE           PIC S9(13)V99.
    05  WS-FROM-BALANCE-DISPLAY PIC ZZZ,ZZZ,ZZ9.99.
    05  WS-TO-BALANCE-DISPLAY   PIC ZZZ,ZZZ,ZZ9.99.

01  DEFAULT-TELLER-ID           PIC X(10) VALUE 'TELLER001'.

*> Account Selection Variables
01  WS-ACCOUNT-SELECTION.
    05  WS-ACCOUNT-COUNT        PIC 9(2) VALUE 0.
    05  WS-SELECTED-OPTION      PIC 9(2).
    05  WS-ACCOUNT-SELECTION-INPUT PIC X(3).
    05  WS-ACCOUNT-LIST OCCURS 10 TIMES.
        10  WS-LIST-ACCOUNT-NUM PIC 9(10).
        10  WS-LIST-HOLDER-NAME PIC X(41).
        10  WS-LIST-ACCOUNT-TYPE PIC X(1).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    PERFORM INITIALIZE-PROGRAM
    PERFORM MAIN-MENU-LOOP UNTIL STOP-PROCESSING
    PERFORM CLEANUP-PROGRAM
    STOP RUN.

INITIALIZE-PROGRAM.
    DISPLAY CLEAR-SCREEN
    DISPLAY CURSOR-HOME
    OPEN I-O ACCOUNT-FILE
    IF NOT FILE-SUCCESS
        OPEN OUTPUT ACCOUNT-FILE
        CLOSE ACCOUNT-FILE
        OPEN I-O ACCOUNT-FILE
    END-IF.

MAIN-MENU-LOOP.
    PERFORM DISPLAY-MAIN-MENU
    ACCEPT WS-MENU-CHOICE
    
    EVALUATE WS-MENU-CHOICE
        WHEN 1
            PERFORM CREATE-ACCOUNT-MODULE
        WHEN 2
            PERFORM ACCOUNT-INFO-MODULE
        WHEN 3
            PERFORM DEPOSIT-MODULE
        WHEN 4
            PERFORM WITHDRAWAL-MODULE
        WHEN 5
            PERFORM TRANSFER-MODULE
        WHEN 6
            SET STOP-PROCESSING TO TRUE
        WHEN OTHER
            DISPLAY 'Invalid choice. Please try again.'
            DISPLAY 'Press ENTER to continue...'
            ACCEPT WS-MENU-CHOICE
    END-EVALUATE.

DISPLAY-MAIN-MENU.
    DISPLAY CLEAR-SCREEN
    DISPLAY CURSOR-HOME
    DISPLAY '================================================================='
    DISPLAY '                 COBOL BANK LEDGER SYSTEM'
    DISPLAY '                        Version 1.0'
    DISPLAY '================================================================='
    DISPLAY ' '
    DISPLAY '1. Create New Account'
    DISPLAY '2. Account Information'
    DISPLAY '3. Deposit'
    DISPLAY '4. Withdrawal'
    DISPLAY '5. Transfer'
    DISPLAY '6. Exit'
    DISPLAY ' '
    DISPLAY 'Please select an option (1-6): ' WITH NO ADVANCING.

*> New Account Selection Paragraph
SELECT-ACCOUNT.
    DISPLAY CLEAR-SCREEN
    DISPLAY CURSOR-HOME
    DISPLAY '================================================================='
    DISPLAY '                        SELECT ACCOUNT'
    DISPLAY '================================================================='
    DISPLAY ' '
    
    PERFORM LOAD-ACCOUNT-LIST
    
    IF WS-ACCOUNT-COUNT = 0
        DISPLAY 'No accounts found in the system.'
        DISPLAY 'Please create an account first.'
        MOVE 0 TO WS-INPUT-ACCOUNT
    ELSE
        PERFORM DISPLAY-ACCOUNT-LIST
        PERFORM GET-ACCOUNT-SELECTION
    END-IF.

*> Selection for transfer FROM account
SELECT-ACCOUNT-FROM.
    DISPLAY CLEAR-SCREEN
    DISPLAY CURSOR-HOME
    DISPLAY '================================================================='
    DISPLAY '                    SELECT SOURCE ACCOUNT'
    DISPLAY '          (Account to transfer money FROM)'
    DISPLAY '================================================================='
    DISPLAY ' '
    
    PERFORM LOAD-ACCOUNT-LIST
    
    IF WS-ACCOUNT-COUNT = 0
        DISPLAY 'No accounts found in the system.'
        DISPLAY 'Please create an account first.'
        MOVE 0 TO WS-INPUT-ACCOUNT
    ELSE
        PERFORM DISPLAY-ACCOUNT-LIST
        PERFORM GET-SOURCE-ACCOUNT
    END-IF.

*> Selection for transfer TO account
SELECT-ACCOUNT-TO.
    DISPLAY CLEAR-SCREEN
    DISPLAY CURSOR-HOME
    DISPLAY '================================================================='
    DISPLAY '                  SELECT DESTINATION ACCOUNT'
    DISPLAY '          (Account to transfer money TO)'
    DISPLAY '================================================================='
    DISPLAY ' '
    
    PERFORM LOAD-ACCOUNT-LIST
    
    IF WS-ACCOUNT-COUNT = 0
        DISPLAY 'No accounts found in the system.'
        DISPLAY 'Please create an account first.'
        MOVE 0 TO WS-INPUT-ACCOUNT
    ELSE
        PERFORM DISPLAY-ACCOUNT-LIST
        PERFORM GET-DESTINATION-ACCOUNT
    END-IF.

LOAD-ACCOUNT-LIST.
    MOVE 0 TO WS-ACCOUNT-COUNT
    MOVE LOW-VALUES TO ACCOUNT-RECORD
    START ACCOUNT-FILE KEY NOT < ACCOUNT-NUMBER
    PERFORM READ-NEXT-ACCOUNT
    PERFORM UNTIL FILE-AT-END OR WS-ACCOUNT-COUNT >= 10
        ADD 1 TO WS-ACCOUNT-COUNT
        MOVE ACCOUNT-NUMBER TO WS-LIST-ACCOUNT-NUM(WS-ACCOUNT-COUNT)
        STRING HOLDER-FIRST-NAME DELIMITED BY SPACE
               ' ' DELIMITED BY SIZE
               HOLDER-LAST-NAME DELIMITED BY SPACE
               INTO WS-LIST-HOLDER-NAME(WS-ACCOUNT-COUNT)
        MOVE ACCOUNT-TYPE TO WS-LIST-ACCOUNT-TYPE(WS-ACCOUNT-COUNT)
        PERFORM READ-NEXT-ACCOUNT
    END-PERFORM.

READ-NEXT-ACCOUNT.
    READ ACCOUNT-FILE NEXT RECORD AT END
        SET FILE-AT-END TO TRUE
    END-READ.

DISPLAY-ACCOUNT-LIST.
    DISPLAY 'Available Accounts:'
    DISPLAY '-------------------'
    
    PERFORM VARYING WS-SELECTED-OPTION FROM 1 BY 1 
            UNTIL WS-SELECTED-OPTION > WS-ACCOUNT-COUNT
        DISPLAY WS-SELECTED-OPTION ' ' 
                WS-LIST-HOLDER-NAME(WS-SELECTED-OPTION)(1:20)
    END-PERFORM
    DISPLAY '-------------------'.

GET-ACCOUNT-SELECTION.
    DISPLAY ' '
    DISPLAY 'Enter account number (1-' WS-ACCOUNT-COUNT ') or B to go Back: ' WITH NO ADVANCING
    ACCEPT WS-ACCOUNT-SELECTION-INPUT
    
    IF WS-ACCOUNT-SELECTION-INPUT = 'B' OR WS-ACCOUNT-SELECTION-INPUT = 'b'
        MOVE 0 TO WS-INPUT-ACCOUNT
    ELSE
        MOVE FUNCTION NUMVAL(WS-ACCOUNT-SELECTION-INPUT) TO WS-SELECTED-OPTION
        IF WS-SELECTED-OPTION >= 1 AND WS-SELECTED-OPTION <= WS-ACCOUNT-COUNT
            MOVE WS-LIST-ACCOUNT-NUM(WS-SELECTED-OPTION) TO WS-INPUT-ACCOUNT
        ELSE
            DISPLAY 'Invalid selection. Please try again.'
            DISPLAY 'Press ENTER to continue...'
            ACCEPT WS-MENU-CHOICE
            MOVE 0 TO WS-INPUT-ACCOUNT
        END-IF
    END-IF.

GET-SOURCE-ACCOUNT.
    DISPLAY ' '
    DISPLAY 'Enter SOURCE account number (1-' WS-ACCOUNT-COUNT ') or B to go Back: ' WITH NO ADVANCING
    ACCEPT WS-ACCOUNT-SELECTION-INPUT
    
    IF WS-ACCOUNT-SELECTION-INPUT = 'B' OR WS-ACCOUNT-SELECTION-INPUT = 'b'
        MOVE 0 TO WS-INPUT-ACCOUNT
    ELSE
        MOVE FUNCTION NUMVAL(WS-ACCOUNT-SELECTION-INPUT) TO WS-SELECTED-OPTION
        IF WS-SELECTED-OPTION >= 1 AND WS-SELECTED-OPTION <= WS-ACCOUNT-COUNT
            MOVE WS-LIST-ACCOUNT-NUM(WS-SELECTED-OPTION) TO WS-INPUT-ACCOUNT
        ELSE
            DISPLAY 'Invalid selection. Please try again.'
            DISPLAY 'Press ENTER to continue...'
            ACCEPT WS-MENU-CHOICE
            MOVE 0 TO WS-INPUT-ACCOUNT
        END-IF
    END-IF.

GET-DESTINATION-ACCOUNT.
    DISPLAY ' '
    DISPLAY 'Enter DESTINATION account number (1-' WS-ACCOUNT-COUNT ') or B to go Back: ' WITH NO ADVANCING
    ACCEPT WS-ACCOUNT-SELECTION-INPUT
    
    IF WS-ACCOUNT-SELECTION-INPUT = 'B' OR WS-ACCOUNT-SELECTION-INPUT = 'b'
        MOVE 0 TO WS-INPUT-ACCOUNT
    ELSE
        MOVE FUNCTION NUMVAL(WS-ACCOUNT-SELECTION-INPUT) TO WS-SELECTED-OPTION
        IF WS-SELECTED-OPTION >= 1 AND WS-SELECTED-OPTION <= WS-ACCOUNT-COUNT
            MOVE WS-LIST-ACCOUNT-NUM(WS-SELECTED-OPTION) TO WS-INPUT-ACCOUNT
        ELSE
            DISPLAY 'Invalid selection. Please try again.'
            DISPLAY 'Press ENTER to continue...'
            ACCEPT WS-MENU-CHOICE
            MOVE 0 TO WS-INPUT-ACCOUNT
        END-IF
    END-IF.

CREATE-ACCOUNT-MODULE.
    DISPLAY CLEAR-SCREEN
    DISPLAY CURSOR-HOME
    DISPLAY '================================================================='
    DISPLAY '                     CREATE NEW ACCOUNT'
    DISPLAY '================================================================='
    DISPLAY ' '
    
    DISPLAY 'Enter First Name: ' WITH NO ADVANCING
    ACCEPT WS-INPUT-FIRST-NAME
    
    DISPLAY 'Enter Last Name: ' WITH NO ADVANCING
    ACCEPT WS-INPUT-LAST-NAME
    
    DISPLAY 'Enter Address: ' WITH NO ADVANCING
    ACCEPT WS-INPUT-ADDRESS
    
    DISPLAY 'Enter Phone Number: ' WITH NO ADVANCING
    ACCEPT WS-INPUT-PHONE
    
    DISPLAY 'Account Type (C-Checking, S-Savings): ' WITH NO ADVANCING
    ACCEPT WS-INPUT-ACCOUNT-TYPE
    
    PERFORM CREATE-NEW-ACCOUNT
    
    DISPLAY ' '
    DISPLAY 'Press ENTER to continue...'
    ACCEPT WS-MENU-CHOICE.

CREATE-NEW-ACCOUNT.
    MOVE WS-NEXT-ACCOUNT-NUM TO ACCOUNT-NUMBER
    MOVE WS-INPUT-ACCOUNT-TYPE TO ACCOUNT-TYPE
    MOVE 'A' TO ACCOUNT-STATUS
    MOVE WS-INPUT-FIRST-NAME TO HOLDER-FIRST-NAME
    MOVE WS-INPUT-LAST-NAME TO HOLDER-LAST-NAME
    MOVE WS-INPUT-ADDRESS TO HOLDER-ADDRESS
    MOVE WS-INPUT-PHONE TO HOLDER-PHONE
    MOVE ZERO TO ACCOUNT-BALANCE
    PERFORM GET-CURRENT-DATE-TIME
    MOVE WS-CURRENT-DATE TO ACCOUNT-OPEN-DATE
    MOVE WS-CURRENT-DATE TO LAST-TRANSACTION-DATE
    
    WRITE ACCOUNT-RECORD
    
    IF FILE-SUCCESS
        DISPLAY MSG-ACCOUNT-CREATED
        DISPLAY 'Account Number: ' ACCOUNT-NUMBER
        ADD 1 TO WS-NEXT-ACCOUNT-NUM
    ELSE
        DISPLAY ERR-FILE-ERROR
        DISPLAY 'File Status: ' WS-FILE-STATUS
    END-IF.

ACCOUNT-INFO-MODULE.
    PERFORM SELECT-ACCOUNT
    
    IF WS-INPUT-ACCOUNT > 0
        PERFORM READ-ACCOUNT-RECORD
        
        IF ACCOUNT-FOUND
            DISPLAY CLEAR-SCREEN
            DISPLAY CURSOR-HOME
            DISPLAY '================================================================='
            DISPLAY '                      ACCOUNT INFORMATION'
            DISPLAY '================================================================='
            DISPLAY ' '
            DISPLAY 'Account Number: ' ACCOUNT-NUMBER
            DISPLAY 'Account Holder: ' HOLDER-FIRST-NAME ' ' HOLDER-LAST-NAME
            DISPLAY 'Account Type: ' ACCOUNT-TYPE
            DISPLAY 'Account Opened: ' ACCOUNT-OPEN-DATE
            DISPLAY ' '
            MOVE ACCOUNT-BALANCE TO WS-BALANCE-DISPLAY
            DISPLAY '***************************************************************'
            DISPLAY '*                    CURRENT BALANCE                          *'
            DISPLAY '*                       $' WS-BALANCE-DISPLAY '              *'
            DISPLAY '***************************************************************'
            DISPLAY ' '
            DISPLAY 'Recent Transaction History:'
            DISPLAY '-----------------------------------------------------------------'
            DISPLAY 'Date       Description                          Amount'
            DISPLAY '-----------------------------------------------------------------'
            PERFORM DISPLAY-TRANSACTION-HISTORY
            DISPLAY '-----------------------------------------------------------------'
            DISPLAY ' '
            DISPLAY '>>> Press ENTER to return to main menu <<<'
            ACCEPT WS-MENU-CHOICE
        ELSE
            DISPLAY ERR-ACCOUNT-NOT-FOUND
            DISPLAY ' '
            DISPLAY '>>> Press ENTER to return to main menu <<<'
            ACCEPT WS-MENU-CHOICE
        END-IF
    END-IF.

READ-ACCOUNT-RECORD.
    MOVE WS-INPUT-ACCOUNT TO ACCOUNT-NUMBER
    READ ACCOUNT-FILE
    IF FILE-SUCCESS
        SET ACCOUNT-FOUND TO TRUE
    ELSE
        SET ACCOUNT-NOT-FOUND TO TRUE
    END-IF.

DEPOSIT-MODULE.
    PERFORM SELECT-ACCOUNT
    
    IF WS-INPUT-ACCOUNT > 0
        PERFORM READ-ACCOUNT-RECORD
        
        IF ACCOUNT-FOUND
            DISPLAY ' '
            DISPLAY 'Enter Deposit Amount: $' WITH NO ADVANCING
            ACCEPT WS-INPUT-AMOUNT
            
            IF WS-INPUT-AMOUNT > ZERO
                PERFORM PROCESS-DEPOSIT
            ELSE
                DISPLAY ERR-INVALID-AMOUNT
            END-IF
        ELSE
            DISPLAY ERR-ACCOUNT-NOT-FOUND
        END-IF
    END-IF
    
    DISPLAY ' '
    DISPLAY 'Press ENTER to continue...'
    ACCEPT WS-MENU-CHOICE.

PROCESS-DEPOSIT.
    ADD WS-INPUT-AMOUNT TO ACCOUNT-BALANCE
    PERFORM GET-CURRENT-DATE-TIME
    MOVE WS-CURRENT-DATE TO LAST-TRANSACTION-DATE
    
    REWRITE ACCOUNT-RECORD
    
    IF FILE-SUCCESS
        PERFORM LOG-TRANSACTION
        DISPLAY MSG-TRANSACTION-COMPLETE
        MOVE ACCOUNT-BALANCE TO WS-BALANCE-DISPLAY
        DISPLAY 'New Balance: $' WS-BALANCE-DISPLAY
    ELSE
        DISPLAY ERR-FILE-ERROR
    END-IF.

WITHDRAWAL-MODULE.
    PERFORM SELECT-ACCOUNT
    
    IF WS-INPUT-ACCOUNT > 0
        PERFORM READ-ACCOUNT-RECORD
        
        IF ACCOUNT-FOUND
            DISPLAY ' '
            DISPLAY 'Enter Withdrawal Amount: $' WITH NO ADVANCING
            ACCEPT WS-INPUT-AMOUNT
            
            IF WS-INPUT-AMOUNT > ZERO
                IF WS-INPUT-AMOUNT <= ACCOUNT-BALANCE
                    PERFORM PROCESS-WITHDRAWAL
                ELSE
                    DISPLAY ERR-INSUFFICIENT-FUNDS
                END-IF
            ELSE
                DISPLAY ERR-INVALID-AMOUNT
            END-IF
        ELSE
            DISPLAY ERR-ACCOUNT-NOT-FOUND
        END-IF
    END-IF
    
    DISPLAY ' '
    DISPLAY 'Press ENTER to continue...'
    ACCEPT WS-MENU-CHOICE.

PROCESS-WITHDRAWAL.
    SUBTRACT WS-INPUT-AMOUNT FROM ACCOUNT-BALANCE
    PERFORM GET-CURRENT-DATE-TIME
    MOVE WS-CURRENT-DATE TO LAST-TRANSACTION-DATE
    
    REWRITE ACCOUNT-RECORD
    
    IF FILE-SUCCESS
        PERFORM LOG-TRANSACTION
        DISPLAY MSG-TRANSACTION-COMPLETE
        MOVE ACCOUNT-BALANCE TO WS-BALANCE-DISPLAY
        DISPLAY 'New Balance: $' WS-BALANCE-DISPLAY
    ELSE
        DISPLAY ERR-FILE-ERROR
    END-IF.

TRANSFER-MODULE.
    PERFORM SELECT-ACCOUNT-FROM
    MOVE WS-INPUT-ACCOUNT TO WS-FROM-ACCOUNT-TEMP
    
    IF WS-FROM-ACCOUNT-TEMP > 0
        MOVE WS-FROM-ACCOUNT-TEMP TO WS-INPUT-ACCOUNT
        PERFORM READ-ACCOUNT-RECORD
        
        IF ACCOUNT-FOUND
            PERFORM SELECT-ACCOUNT-TO
            MOVE WS-INPUT-ACCOUNT TO WS-TO-ACCOUNT
            
            IF WS-TO-ACCOUNT > 0
                *> Restore FROM account for processing
                MOVE WS-FROM-ACCOUNT-TEMP TO WS-INPUT-ACCOUNT
                PERFORM READ-ACCOUNT-RECORD
                
                DISPLAY ' '
                DISPLAY 'Enter Transfer Amount: $' WITH NO ADVANCING
                ACCEPT WS-INPUT-AMOUNT
                
                IF WS-INPUT-AMOUNT > ZERO
                    IF WS-INPUT-AMOUNT <= ACCOUNT-BALANCE
                        PERFORM PROCESS-TRANSFER
                    ELSE
                        DISPLAY ERR-INSUFFICIENT-FUNDS
                    END-IF
                ELSE
                    DISPLAY ERR-INVALID-AMOUNT
                END-IF
            END-IF
        ELSE
            DISPLAY ERR-ACCOUNT-NOT-FOUND
        END-IF
    END-IF
    
    DISPLAY ' '
    DISPLAY 'Press ENTER to continue...'
    ACCEPT WS-MENU-CHOICE.

PROCESS-TRANSFER.
    *> Process FROM account - subtract amount
    SUBTRACT WS-INPUT-AMOUNT FROM ACCOUNT-BALANCE
    PERFORM GET-CURRENT-DATE-TIME
    MOVE WS-CURRENT-DATE TO LAST-TRANSACTION-DATE
    
    REWRITE ACCOUNT-RECORD
    
    IF FILE-SUCCESS
        MOVE ACCOUNT-BALANCE TO WS-FROM-BALANCE
        PERFORM LOG-TRANSACTION
        
        *> Now process TO account - add amount
        MOVE WS-TO-ACCOUNT TO ACCOUNT-NUMBER
        READ ACCOUNT-FILE
        
        IF FILE-SUCCESS
            ADD WS-INPUT-AMOUNT TO ACCOUNT-BALANCE
            MOVE WS-CURRENT-DATE TO LAST-TRANSACTION-DATE
            
            REWRITE ACCOUNT-RECORD
            
            IF FILE-SUCCESS
                MOVE ACCOUNT-BALANCE TO WS-TO-BALANCE
                
                *> Display transfer summary
                DISPLAY ' '
                DISPLAY MSG-TRANSACTION-COMPLETE
                DISPLAY '================================================================='
                DISPLAY '                     TRANSFER SUMMARY'
                DISPLAY '================================================================='
                MOVE WS-FROM-BALANCE TO WS-FROM-BALANCE-DISPLAY
                MOVE WS-TO-BALANCE TO WS-TO-BALANCE-DISPLAY
                MOVE WS-INPUT-AMOUNT TO WS-INPUT-AMOUNT-DISPLAY
                DISPLAY 'Amount Transferred: $' WS-INPUT-AMOUNT-DISPLAY
                DISPLAY ' '
                DISPLAY 'FROM Account: ' WS-FROM-ACCOUNT-TEMP
                DISPLAY 'New Balance: $' WS-FROM-BALANCE-DISPLAY
                DISPLAY ' '
                DISPLAY 'TO Account: ' WS-TO-ACCOUNT
                DISPLAY 'New Balance: $' WS-TO-BALANCE-DISPLAY
                DISPLAY '================================================================='
            ELSE
                DISPLAY ERR-FILE-ERROR ' (TO account)'
            END-IF
        ELSE
            DISPLAY ERR-FILE-ERROR ' (TO account not found)'
        END-IF
    ELSE
        DISPLAY ERR-FILE-ERROR ' (FROM account)'
    END-IF.



DISPLAY-TRANSACTION-HISTORY.
    OPEN INPUT TRANSACTION-FILE
    IF WS-TRANS-FILE-STATUS = '00'
        PERFORM READ-TRANSACTION-RECORD
        PERFORM UNTIL WS-TRANS-FILE-STATUS = '10'
            IF FROM-ACCOUNT = WS-INPUT-ACCOUNT OR
               TO-ACCOUNT = WS-INPUT-ACCOUNT
                PERFORM DISPLAY-TRANSACTION-DETAIL
            END-IF
            PERFORM READ-TRANSACTION-RECORD
        END-PERFORM
        CLOSE TRANSACTION-FILE
    ELSE
        DISPLAY 'ERROR opening trans file. Status: ' WS-TRANS-FILE-STATUS
    END-IF.

READ-TRANSACTION-RECORD.
    READ TRANSACTION-FILE AT END
        MOVE '10' TO WS-TRANS-FILE-STATUS
    NOT AT END
        MOVE '00' TO WS-TRANS-FILE-STATUS
    END-READ.

DISPLAY-TRANSACTION-DETAIL.
    MOVE TRANS-AMOUNT TO WS-INPUT-AMOUNT-DISPLAY
    IF TRANS-TYPE = 'D' OR TRANS-TYPE = 'I'
        DISPLAY TRANS-DATE(5:2) '/' TRANS-DATE(7:2) '/' TRANS-DATE(1:4) ' '
                TRANS-DESCRIPTION(1:40) ' +$' WS-INPUT-AMOUNT-DISPLAY
    ELSE
        DISPLAY TRANS-DATE(5:2) '/' TRANS-DATE(7:2) '/' TRANS-DATE(1:4) ' '
                TRANS-DESCRIPTION(1:40) ' -$' WS-INPUT-AMOUNT-DISPLAY
    END-IF.

LOG-TRANSACTION.
    OPEN EXTEND TRANSACTION-FILE
    
    MOVE WS-NEXT-TRANS-ID TO TRANS-ID
    MOVE WS-CURRENT-DATE TO TRANS-DATE
    MOVE WS-CURRENT-TIME TO TRANS-TIME
    MOVE WS-INPUT-ACCOUNT TO FROM-ACCOUNT
    MOVE WS-TO-ACCOUNT TO TO-ACCOUNT
    MOVE WS-INPUT-AMOUNT TO TRANS-AMOUNT
    MOVE DEFAULT-TELLER-ID TO TELLER-ID
    
    EVALUATE WS-MENU-CHOICE
        WHEN 3
            MOVE 'D' TO TRANS-TYPE
            MOVE 'DEPOSIT' TO TRANS-DESCRIPTION
        WHEN 4
            MOVE 'W' TO TRANS-TYPE
            MOVE 'WITHDRAWAL' TO TRANS-DESCRIPTION
        WHEN 5
            MOVE 'T' TO TRANS-TYPE
            MOVE 'TRANSFER' TO TRANS-DESCRIPTION
    END-EVALUATE
    
    WRITE TRANSACTION-RECORD
    ADD 1 TO WS-NEXT-TRANS-ID
    
    CLOSE TRANSACTION-FILE.

GET-CURRENT-DATE-TIME.
    ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
    ACCEPT WS-CURRENT-TIME FROM TIME.

CLEANUP-PROGRAM.
    CLOSE ACCOUNT-FILE. 
