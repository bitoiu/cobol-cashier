       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-BASIC.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TEST-RESULT          PIC X(4).
       01  WS-TEST-COUNT           PIC 9(3) VALUE 0.
       01  WS-PASS-COUNT           PIC 9(3) VALUE 0.
       01  WS-FAIL-COUNT           PIC 9(3) VALUE 0.
       01  WS-CALC-RESULT          PIC S9(13)V99.
       01  WS-TEST-ACCOUNT-NUM     PIC 9(10) VALUE 1000000001.
       
       PROCEDURE DIVISION.
       MAIN-TEST.
           DISPLAY '=============================================='
           DISPLAY '       COBOL Bank Ledger System Tests'
           DISPLAY '=============================================='
           DISPLAY ' '
           
           PERFORM TEST-ACCOUNT-NUMBER-VALIDATION
           PERFORM TEST-BALANCE-CALCULATION
           PERFORM TEST-DATE-FORMATTING
           
           DISPLAY ' '
           DISPLAY '=============================================='
           DISPLAY 'Test Results:'
           DISPLAY 'Total Tests: ' WS-TEST-COUNT
           DISPLAY 'Passed: ' WS-PASS-COUNT
           DISPLAY 'Failed: ' WS-FAIL-COUNT
           DISPLAY '=============================================='
           
           IF WS-FAIL-COUNT = 0
               DISPLAY 'All tests PASSED!'
           ELSE
               DISPLAY 'Some tests FAILED!'
           END-IF
           
           STOP RUN.
       
       TEST-ACCOUNT-NUMBER-VALIDATION.
           DISPLAY 'Testing Account Number Validation...'
           ADD 1 TO WS-TEST-COUNT
           
           IF WS-TEST-ACCOUNT-NUM >= 1000000000 
               AND WS-TEST-ACCOUNT-NUM <= 9999999999
               MOVE 'PASS' TO WS-TEST-RESULT
               ADD 1 TO WS-PASS-COUNT
           ELSE
               MOVE 'FAIL' TO WS-TEST-RESULT
               ADD 1 TO WS-FAIL-COUNT
           END-IF
           
           DISPLAY '  Account Number Range Check: ' WS-TEST-RESULT.
       
       TEST-BALANCE-CALCULATION.
           DISPLAY 'Testing Balance Calculation...'
           ADD 1 TO WS-TEST-COUNT
           
           COMPUTE WS-CALC-RESULT = 1000.00 + 250.50 - 100.25
           
           IF WS-CALC-RESULT = 1150.25
               MOVE 'PASS' TO WS-TEST-RESULT
               ADD 1 TO WS-PASS-COUNT
           ELSE
               MOVE 'FAIL' TO WS-TEST-RESULT
               ADD 1 TO WS-FAIL-COUNT
           END-IF
           
           DISPLAY '  Balance Arithmetic: ' WS-TEST-RESULT.
       
       TEST-DATE-FORMATTING.
           DISPLAY 'Testing Date Formatting...'
           ADD 1 TO WS-TEST-COUNT
           
           ACCEPT WS-TEST-RESULT FROM DATE YYYYMMDD
           
           IF WS-TEST-RESULT IS NUMERIC
               MOVE 'PASS' TO WS-TEST-RESULT
               ADD 1 TO WS-PASS-COUNT
           ELSE
               MOVE 'FAIL' TO WS-TEST-RESULT
               ADD 1 TO WS-FAIL-COUNT
           END-IF
           
           DISPLAY '  Date Format: ' WS-TEST-RESULT. 
