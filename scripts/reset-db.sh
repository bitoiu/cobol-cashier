#!/bin/bash

# COBOL Bank Ledger System - Database Reset Script
# This script resets the database to its initial state

echo "=============================================="
echo "   COBOL Bank Ledger System Database Reset"
echo "=============================================="
echo ""

# Check if we're in the right directory
if [ ! -f "Makefile" ] || [ ! -d "src" ]; then
    echo "Error: This script must be run from the project root directory"
    echo "Please navigate to the ledger-cobol directory and try again"
    exit 1
fi

echo "Resetting database to baseline state..."

# Remove existing data files
if [ -f "data/accounts.dat" ]; then
    rm -f data/accounts.dat
    echo "✓ Removed accounts.dat"
fi

if [ -f "data/trans.dat" ]; then
    rm -f data/trans.dat
    echo "✓ Removed trans.dat"
fi

# Remove any index files that might exist
if [ -f "data/accounts.idx" ]; then
    rm -f data/accounts.idx
    echo "✓ Removed accounts.idx"
fi

# Create data directory if it doesn't exist
mkdir -p data

# Create empty data files
touch data/accounts.dat
touch data/trans.dat

echo "✓ Created empty data files"

# Create sample baseline accounts and transactions
echo "Creating baseline accounts and transactions..."

# Create a simple COBOL program to initialize sample data
cat > /tmp/init-sample.cob << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INIT-SAMPLE.
       
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
       01  WS-FILE-STATUS              PIC X(2).
       01  WS-TRANS-FILE-STATUS        PIC X(2).
       01  WS-CURRENT-DATE             PIC X(8).
       01  WS-TRANS-ID                 PIC 9(12) VALUE 1.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           
           OPEN OUTPUT ACCOUNT-FILE
           OPEN OUTPUT TRANSACTION-FILE
           
           MOVE 1000000001 TO ACCOUNT-NUMBER
           MOVE 'C' TO ACCOUNT-TYPE
           MOVE 'A' TO ACCOUNT-STATUS
           MOVE 'MONTEIRO' TO HOLDER-FIRST-NAME
           MOVE 'SILVA' TO HOLDER-LAST-NAME
           MOVE '123 MAIN ST' TO HOLDER-ADDRESS
           MOVE '555-1234' TO HOLDER-PHONE
           MOVE 4523.75 TO ACCOUNT-BALANCE
           MOVE WS-CURRENT-DATE TO ACCOUNT-OPEN-DATE
           MOVE WS-CURRENT-DATE TO LAST-TRANSACTION-DATE
           WRITE ACCOUNT-RECORD
           
           MOVE 1000000002 TO ACCOUNT-NUMBER
           MOVE 'S' TO ACCOUNT-TYPE
           MOVE 'A' TO ACCOUNT-STATUS
           MOVE 'ANDERSON' TO HOLDER-FIRST-NAME
           MOVE 'CLARK' TO HOLDER-LAST-NAME
           MOVE '456 OAK AVE' TO HOLDER-ADDRESS
           MOVE '555-5678' TO HOLDER-PHONE
           MOVE 3241.50 TO ACCOUNT-BALANCE
           MOVE WS-CURRENT-DATE TO ACCOUNT-OPEN-DATE
           MOVE WS-CURRENT-DATE TO LAST-TRANSACTION-DATE
           WRITE ACCOUNT-RECORD
           
           MOVE 1000000003 TO ACCOUNT-NUMBER
           MOVE 'C' TO ACCOUNT-TYPE
           MOVE 'A' TO ACCOUNT-STATUS
           MOVE 'JOHNSON' TO HOLDER-FIRST-NAME
           MOVE 'BROWN' TO HOLDER-LAST-NAME
           MOVE '789 PINE RD' TO HOLDER-ADDRESS
           MOVE '555-9012' TO HOLDER-PHONE
           MOVE 2895.25 TO ACCOUNT-BALANCE
           MOVE WS-CURRENT-DATE TO ACCOUNT-OPEN-DATE
           MOVE WS-CURRENT-DATE TO LAST-TRANSACTION-DATE
           WRITE ACCOUNT-RECORD
           
           MOVE 1000000004 TO ACCOUNT-NUMBER
           MOVE 'S' TO ACCOUNT-TYPE
           MOVE 'A' TO ACCOUNT-STATUS
           MOVE 'WILLIAMS' TO HOLDER-FIRST-NAME
           MOVE 'DAVIS' TO HOLDER-LAST-NAME
           MOVE '321 ELM ST' TO HOLDER-ADDRESS
           MOVE '555-2468' TO HOLDER-PHONE
           MOVE 4789.10 TO ACCOUNT-BALANCE
           MOVE WS-CURRENT-DATE TO ACCOUNT-OPEN-DATE
           MOVE WS-CURRENT-DATE TO LAST-TRANSACTION-DATE
           WRITE ACCOUNT-RECORD
           
           MOVE 1000000005 TO ACCOUNT-NUMBER
           MOVE 'C' TO ACCOUNT-TYPE
           MOVE 'A' TO ACCOUNT-STATUS
           MOVE 'GARCIA' TO HOLDER-FIRST-NAME
           MOVE 'RODRIGUEZ' TO HOLDER-LAST-NAME
           MOVE '654 MAPLE AVE' TO HOLDER-ADDRESS
           MOVE '555-1357' TO HOLDER-PHONE
           MOVE 3456.80 TO ACCOUNT-BALANCE
           MOVE WS-CURRENT-DATE TO ACCOUNT-OPEN-DATE
           MOVE WS-CURRENT-DATE TO LAST-TRANSACTION-DATE
           WRITE ACCOUNT-RECORD
          
           *> Create sample transactions for all accounts
           PERFORM CREATE-SAMPLE-TRANSACTIONS
           
           CLOSE ACCOUNT-FILE
           CLOSE TRANSACTION-FILE
           STOP RUN.

       CREATE-SAMPLE-TRANSACTIONS.
           *> Transactions for Account 1 - Monteiro Silva
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250101' TO TRANS-DATE
           MOVE '08000000' TO TRANS-TIME
           MOVE 'D' TO TRANS-TYPE
           MOVE 1000000001 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 5000.00 TO TRANS-AMOUNT
           MOVE 'INITIAL DEPOSIT' TO TRANS-DESCRIPTION
           MOVE 'TELLER001' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250102' TO TRANS-DATE
           MOVE '10300000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000001 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 50.00 TO TRANS-AMOUNT
           MOVE 'ATM WITHDRAWAL - BANK OF AMERICA' TO TRANS-DESCRIPTION
           MOVE 'ATM001' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250103' TO TRANS-DATE
           MOVE '14150000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000001 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 125.50 TO TRANS-AMOUNT
           MOVE 'AMAZON.COM' TO TRANS-DESCRIPTION
           MOVE 'POS001' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250103' TO TRANS-DATE
           MOVE '16300000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000001 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 301.00 TO TRANS-AMOUNT
           MOVE 'WALMART SUPERCENTER' TO TRANS-DESCRIPTION
           MOVE 'POS002' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           *> Transactions for Account 2 - Anderson Clark
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250101' TO TRANS-DATE
           MOVE '09000000' TO TRANS-TIME
           MOVE 'D' TO TRANS-TYPE
           MOVE 1000000002 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 3500.00 TO TRANS-AMOUNT
           MOVE 'INITIAL DEPOSIT' TO TRANS-DESCRIPTION
           MOVE 'TELLER002' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250102' TO TRANS-DATE
           MOVE '11450000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000002 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 75.50 TO TRANS-AMOUNT
           MOVE 'TARGET STORES' TO TRANS-DESCRIPTION
           MOVE 'POS003' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250103' TO TRANS-DATE
           MOVE '18200000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000002 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 183.00 TO TRANS-AMOUNT
           MOVE 'WHOLE FOODS MARKET' TO TRANS-DESCRIPTION
           MOVE 'POS004' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           *> Transactions for Account 3 - Johnson Brown
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250101' TO TRANS-DATE
           MOVE '10000000' TO TRANS-TIME
           MOVE 'D' TO TRANS-TYPE
           MOVE 1000000003 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 3000.00 TO TRANS-AMOUNT
           MOVE 'INITIAL DEPOSIT' TO TRANS-DESCRIPTION
           MOVE 'TELLER003' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250102' TO TRANS-DATE
           MOVE '12150000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000003 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 45.25 TO TRANS-AMOUNT
           MOVE 'STARBUCKS #4521' TO TRANS-DESCRIPTION
           MOVE 'POS005' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250102' TO TRANS-DATE
           MOVE '17300000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000003 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 59.50 TO TRANS-AMOUNT
           MOVE 'CHEVRON GAS STATION' TO TRANS-DESCRIPTION
           MOVE 'POS006' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           *> Transactions for Account 4 - Williams Davis
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250101' TO TRANS-DATE
           MOVE '11000000' TO TRANS-TIME
           MOVE 'D' TO TRANS-TYPE
           MOVE 1000000004 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 5000.00 TO TRANS-AMOUNT
           MOVE 'INITIAL DEPOSIT' TO TRANS-DESCRIPTION
           MOVE 'TELLER001' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250102' TO TRANS-DATE
           MOVE '13200000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000004 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 89.90 TO TRANS-AMOUNT
           MOVE 'NETFLIX SUBSCRIPTION' TO TRANS-DESCRIPTION
           MOVE 'ACH001' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250103' TO TRANS-DATE
           MOVE '09450000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000004 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 121.00 TO TRANS-AMOUNT
           MOVE 'VERIZON WIRELESS' TO TRANS-DESCRIPTION
           MOVE 'ACH002' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           *> Transactions for Account 5 - Garcia Rodriguez
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250101' TO TRANS-DATE
           MOVE '12000000' TO TRANS-TIME
           MOVE 'D' TO TRANS-TYPE
           MOVE 1000000005 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 4000.00 TO TRANS-AMOUNT
           MOVE 'INITIAL DEPOSIT' TO TRANS-DESCRIPTION
           MOVE 'TELLER002' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250102' TO TRANS-DATE
           MOVE '15300000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000005 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 234.20 TO TRANS-AMOUNT
           MOVE 'COSTCO WHOLESALE' TO TRANS-DESCRIPTION
           MOVE 'POS007' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250103' TO TRANS-DATE
           MOVE '10150000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000005 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 45.00 TO TRANS-AMOUNT
           MOVE 'CHIPOTLE MEXICAN GRILL' TO TRANS-DESCRIPTION
           MOVE 'POS008' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE '20250103' TO TRANS-DATE
           MOVE '16450000' TO TRANS-TIME
           MOVE 'W' TO TRANS-TYPE
           MOVE 1000000005 TO FROM-ACCOUNT
           MOVE 0 TO TO-ACCOUNT
           MOVE 264.00 TO TRANS-AMOUNT
           MOVE 'HOME DEPOT' TO TRANS-DESCRIPTION
           MOVE 'POS009' TO TELLER-ID
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID.
EOF

# Compile and run the sample data creation program
if cobc -x -o /tmp/init-sample /tmp/init-sample.cob 2>/dev/null; then
    /tmp/init-sample
    rm -f /tmp/init-sample /tmp/init-sample.cob
    echo "✓ Created 5 baseline accounts with transaction history:"
    echo "  - Account 1000000001: Monteiro Silva (Checking) - $4,523.75"
    echo "  - Account 1000000002: Anderson Clark (Savings) - $3,241.50"
    echo "  - Account 1000000003: Johnson Brown (Checking) - $2,895.25"
    echo "  - Account 1000000004: Williams Davis (Savings) - $4,789.10"
    echo "  - Account 1000000005: Garcia Rodriguez (Checking) - $3,456.80"
    echo "✓ Created sample transactions for all accounts"
else
    echo "✗ Failed to create baseline accounts (GnuCOBOL may not be installed)"
    echo "  You can create accounts manually using the application"
fi

echo ""
echo "=============================================="
echo "Database reset completed successfully!"
echo "You can now run the application with: make run"
echo "==============================================" 