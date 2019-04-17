       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTROL as "CONTROL".
       AUTHOR.     George Yang.

       PROCEDURE DIVISION.
       
      *==============================================================
      * The top level of the project
      * call 4 sub programs to 
      * 1. convert files to indexed files
      * 2. update invent file according to transactions
      * 3. generate inventory report and re-order report
      *==============================================================
       100-CREATE-REPORTS.
           CALL "CONVERT".
           CALL "UPDATEINV".
           CALL "INVENTORY".
           CALL "REORDER".

           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERT.
       AUTHOR.     George Yang.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENT-FILE
               ASSIGN TO "D:\COBOL\INVENT6.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PART-NUMBER
               FILE STATUS IS STATUS-FIELD.
           SELECT SEQ-INVENT
               ASSIGN TO "D:\COBOL\INVFILE3.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SUPPLIER-FILE
               ASSIGN TO "D:\COBOL\SUPPLIERI.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SUPPLIER-CODE-IN
               FILE STATUS IS STATUS-FIELD.
           SELECT SEQ-SUPPLIER
               ASSIGN TO "D:\COBOL\SUPPLIER4.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENT-FILE.
       01  INVENT-RECORD.
           05 PART-NUMBER    PIC 9(7)  VALUE ZERO.
           05 PART-NAME      PIC X(20) VALUE SPACES.
           05 QUANTITY       PIC 9(4)  VALUE ZERO.
           05 UNIT-PRICE     PIC 9(4)  VALUE ZERO.
           05 REORDER-POINT  PIC 9(4)  VALUE ZERO.
           05 SUPPLIER-CODE  PIC X(5)  VALUE SPACES.
       FD  SEQ-INVENT.
       01  SEQ-INVENT-RECORD.
           05 SEQ-PART-NUMBER    PIC 9(7)  VALUE ZERO.
           05 SEQ-PART-NAME      PIC X(20) VALUE SPACES.
           05 SEQ-QUANTITY       PIC 9(4)  VALUE ZERO.
           05 SEQ-UNIT-PRICE     PIC 9(4)  VALUE ZERO.
           05 SEQ-REORDER-POINT  PIC 9(4)  VALUE ZERO.
           05 SEQ-SUPPLIER-CODE  PIC X(5)  VALUE SPACES.
       FD  SUPPLIER-FILE.
       01  SUPPLIER-RECORD.
           05 SUPPLIER-CODE-IN   PIC X(5)  VALUE SPACES.
           05 SUPPLIER-NAME-IN   PIC X(20) VALUE SPACES.
       FD  SEQ-SUPPLIER.
       01  SEQ-SUPPLIER-RECORD.
           05 SEQ-SUPPLIER-CODE   PIC X(5)  VALUE SPACES.
           05 SEQ-SUPPLIER-NAME   PIC X(20) VALUE SPACES.
       
       WORKING-STORAGE SECTION.
       01 STATUS-FIELD	      PIC X(2)  VALUE SPACES.
       01 EOF-INVENT          PIC A(1)  VALUE SPACES.
       01 EOF-SUPPLI          PIC A(1)  VALUE SPACES.                   
           
       PROCEDURE DIVISION.
       
      *==============================================================
      * The top level of the program.
      *==============================================================
       100-CREATE-IND-FILES.
           PERFORM 200-INIT-CREATE-IND.
           PERFORM 201-CREATE-IND-INV
               UNTIL EOF-INVENT = "Y".
           PERFORM 202-CREATE-IND-SUP
               UNTIL EOF-SUPPLI = "Y".
           PERFORM 203-TERM-CREATE-IND.
           EXIT PROGRAM. 

      *==============================================================
      * Init create index files.
      *==============================================================
       200-INIT-CREATE-IND.
           PERFORM 300-OPEN-FILES.
           PERFORM 301-READ-SEQ-INV.
           PERFORM 302-READ-SEQ-SUP.
           
      *==============================================================
      * Main process to create inventory index file.
      *==============================================================
       201-CREATE-IND-INV.
           PERFORM 303-WR-IND-INV.
           PERFORM 301-READ-SEQ-INV.
       
      *==============================================================
      * Main process to create supplier index file.
      *============================================================== 
       202-CREATE-IND-SUP.
           PERFORM 304-WR-IND-SUP.
           PERFORM 302-READ-SEQ-SUP.
       
      *==============================================================
      * Terminate create index files.
      *============================================================== 
       203-TERM-CREATE-IND.
           CLOSE SEQ-INVENT SEQ-SUPPLIER
               INVENT-FILE SUPPLIER-FILE.
      
      *==============================================================
      * Open sequential and indexed files.
      *==============================================================
       300-OPEN-FILES.
           OPEN INPUT SEQ-INVENT SEQ-SUPPLIER.
           OPEN OUTPUT INVENT-FILE SUPPLIER-FILE.
      
      *==============================================================
      * Read sequential inventory file.
      *==============================================================
       301-READ-SEQ-INV.
           READ SEQ-INVENT
               AT END MOVE "Y" TO EOF-INVENT.
      
      *==============================================================
      * Read sequential supplier file.
      *==============================================================
       302-READ-SEQ-SUP.
           READ SEQ-SUPPLIER
               AT END MOVE "Y" TO EOF-SUPPLI.
      
      *==============================================================
      * Write indexed inventory file.
      *==============================================================     
       303-WR-IND-INV.
           WRITE INVENT-RECORD FROM SEQ-INVENT-RECORD
               INVALID KEY MOVE "ER" TO STATUS-FIELD.
      
      *==============================================================
      * Write indexed supplier file.
      *==============================================================         
       304-WR-IND-SUP.
           WRITE SUPPLIER-RECORD FROM SEQ-SUPPLIER-RECORD               
               INVALID KEY MOVE "ER" TO STATUS-FIELD.
           
       end program CONVERT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATEINV.
       AUTHOR.     George Yang.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENT-FILE
               ASSIGN TO "D:\COBOL\INVENT6.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PART-NUMBER
               FILE STATUS IS STATUS-FIELD.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENT-FILE.
       01  INVENT-RECORD.
           05 PART-NUMBER    PIC 9(7)  VALUE ZERO.
           05 PART-NAME      PIC X(20) VALUE SPACES.
           05 QUANTITY       PIC 9(4)  VALUE ZERO.
           05 UNIT-PRICE     PIC 9(4)  VALUE ZERO.
           05 REORDER-POINT  PIC 9(4)  VALUE ZERO.
           05 SUPPLIER-CODE  PIC X(5)  VALUE SPACES.
       
       WORKING-STORAGE SECTION.
       01 STATUS-FIELD	      PIC X(2)  VALUE SPACES.
       01 ENTRY-FLAG          PIC A(1)  VALUE SPACES.
       01 ENTRY-PROMPT        VALUE "ENTER A TRANSACTION? (Y OR N)".
       01 TRANSACTION-WS.
           05 PART-NUMBER-WS  PIC 9(7) VALUE ZERO.
           05 TRANS-CODE-WS   PIC A(1).
                   88 SALE    VALUE "S".
                   88 RECEIPT VALUE "R".
           05 TRANS-AMOUNT-WS PIC 9(4) VALUE ZERO.
       
       SCREEN SECTION.
       01 TRANSACTION-SCREEN.
           05 NUMBER-SECTION.
               10 VALUE "INVENTORY NUMBER:"   LINE 5 COL 10.
               10 PART-NUMBER-IN              LINE 5 COL 45
                   PIC 9(7) TO PART-NUMBER-WS.
           05 CODE-SECTION.
               10 VALUE "TRANSACTION CODE:"   LINE 7 COL 10.
               10 TRANS-CODE-IN               LINE 7 COL 45
                   PIC A(1) TO TRANS-CODE-WS.
           05 AMOUNT-SECTION.
               10 VALUE "TRANSACTION AMOUNT:" LINE 9 COL 10.
               10 TRANS-AMOUNT-IN             LINE 9 COL 45
                   PIC 9(4) TO TRANS-AMOUNT-WS.
           
      *==============================================================
      * The top level of the program.
      *==============================================================
       PROCEDURE DIVISION.
       100-UP-INV-FILE.
           PERFORM 200-INIT-UP-INV-FILE.
           PERFORM 201-UPDATE-INV-RECORD
               UNTIL ENTRY-FLAG = "N".
           PERFORM 202-TERM-UP-INV-FILE.
           EXIT PROGRAM.

      *==============================================================
      * Initiate updating inventory file.
      *==============================================================
       200-INIT-UP-INV-FILE.
           PERFORM 300-OPEN-INV-FILE.
           PERFORM 301-PROMPT-TRANS-ENTRY.
                                                                        
      *==============================================================   
      * Update an inventory record.
      *==============================================================
       201-UPDATE-INV-RECORD.
           PERFORM 302-PROMPT-TRANS-FIELDS.
           PERFORM 303-READ-INV-AMOUNT.
           EVALUATE TRUE
               WHEN SALE PERFORM 304-CAL-SALE-AMOUT
               WHEN RECEIPT PERFORM 305-CAL-RECEIPT-AMOUT
           END-EVALUATE.
           PERFORM 306-REWRITE-TRANS-AMOUT.
           PERFORM 301-PROMPT-TRANS-ENTRY.

      *==============================================================
      * Close the indexed inventory file.
      *============================================================== 
       202-TERM-UP-INV-FILE.
           CLOSE INVENT-FILE.
      
      *==============================================================
      * Open the indexed inventory file.
      *==============================================================
       300-OPEN-INV-FILE.
           OPEN I-O INVENT-FILE.
           
       
      *==============================================================
      * Ask the user if he/she wants to enter a transaction.
      *==============================================================
       301-PROMPT-TRANS-ENTRY.
           DISPLAY ENTRY-PROMPT
               LINE 2 COLUMN 10.
           ACCEPT ENTRY-FLAG
               LINE 2 COLUMN 45.
       
      *==============================================================
      * Display the screen section,
      * get the transaction data.
      *==============================================================
       302-PROMPT-TRANS-FIELDS.
           DISPLAY NUMBER-SECTION.
           ACCEPT  PART-NUMBER-IN.
           DISPLAY CODE-SECTION.
           ACCEPT TRANS-CODE-IN.
           DISPLAY AMOUNT-SECTION.
           ACCEPT TRANS-AMOUNT-IN.
       
      *==============================================================
      * Read the inventory record according to
      * the key from the transaction.
      *==============================================================
       303-READ-INV-AMOUNT.
           MOVE PART-NUMBER-WS TO PART-NUMBER.
           READ INVENT-FILE
               INVALID KEY     MOVE "ER" TO STATUS-FIELD
               NOT INVALID KEY MOVE "OK" TO STATUS-FIELD.
               
      *==============================================================
      * Decrease the amount if it is sale.
      *==============================================================
       304-CAL-SALE-AMOUT.
           SUBTRACT TRANS-AMOUNT-WS FROM QUANTITY. 
       
      *==============================================================
      * Increase the amount if it is receipt.
      *==============================================================
       305-CAL-RECEIPT-AMOUT.
           ADD TRANS-AMOUNT-WS TO QUANTITY.
       
      *==============================================================
      * Rewrite the record with the updated amount into 
      * the indexed inventory file.
      *==============================================================
       306-REWRITE-TRANS-AMOUT.
           MOVE PART-NUMBER-WS TO PART-NUMBER.
           REWRITE INVENT-RECORD
               INVALID KEY     MOVE "ER" TO STATUS-FIELD
               NOT INVALID KEY MOVE "OK" TO STATUS-FIELD.
       
       end program UPDATEINV.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY.
       AUTHOR.     Xiaoyang Miao.

       ENVIRONMENT DIVISION.
      *INPUT-OUTPUT SECTION DEFINES EXTERNAL FILE TO BE UESD.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENT-FILE-IN
               ASSIGN TO "D:\COBOL\INVENT6.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS PART-NUMBER-IN
               FILE STATUS IS STATUS-FIELD.
           SELECT SUPPLIER-FILE-IN
               ASSIGN TO "D:\COBOL\SUPPLIERI.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SUPPLIER-CODE-IN
               FILE STATUS IS STATUS-FIELD.
           SELECT INVENT-FILE-OUT
               ASSIGN TO "D:\COBOL\INVFILE4.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *FILE SECTION DESCRIBES THE DATA IN INPUT AND OUTPUT FILES.
       FILE SECTION.
       FD  INVENT-FILE-IN.
       COPY "D:\COBOL\INVENT_RECORD_IN.cpy".
           
       FD  INVENT-FILE-OUT.
       01  INVENT-RECORD-OUT     PIC X(45).

       FD  SUPPLIER-FILE-IN.
       01  SUPPLIER-RECORD-IN.
           05 SUPPLIER-CODE-IN   PIC X(5)      VALUE SPACES.
           05 SUPPLIER-NAME-IN   PIC X(20)     VALUE SPACES.
      
      *WORKING-STORAGE SECTION DESCRIBES THE DATA IN 
      *INVENT DETAIL OUT, AUDIT TRAIL OUT, 
      *INVENT REPORT HEADER, SUPPLIER TABLE,
      *FLAGS AND COUNTERS, SUB, CALCULATE VALUE.
       WORKING-STORAGE SECTION.
       01 INVENT-DETAIL-OUT.                                           
           05 PART-NUMBER-OUT PIC 9(7)        VALUE ZEROS.
           05 FILLER          PIC X(4)        VALUE SPACES.
           05 PART-NAME-OUT   PIC X(20)       VALUE SPACES.
           05 FILLER          PIC X(4)        VALUE SPACES.
           05 STOCK-VALUE-OUT PIC ZZZZZZZ9. 

       01 AUDIT-TRAIL-OUT.
           05  FILLER             PIC X(5)     VALUE "VALUE".
           05  FILLER             PIC X(2)     VALUE SPACES.
           05  INVENT-TOTAL-VALUE PIC ZZZZZZZZZZ9.
           05  FILLER             PIC X(2)     VALUE SPACES.
           05  FILLER             PIC X(4)     VALUE "READ".
           05  FILLER             PIC X(2)     VALUE SPACES.
           05  RECORDS-READ       PIC ZZZ9.
           05  FILLER             PIC X(2)     VALUE SPACES.
           05  FILLER             PIC X(7)     VALUE 'WRITTEN'.
           05  FILLER             PIC X(2)     VALUE SPACES.
           05  RECORDS-WRITTEN    PIC ZZZ9.
       
       01 INVENT-REPORT-HEADER.
           05 FILLER               PIC X(7)    VALUE "PARTNUM".
           05 FILLER               PIC X(4).
           05 FILLER               PIC X(20)   VALUE "PARTNAME".
           05 FILLER               PIC X(4).
           05 FILLER               PIC X(8)    VALUE "VALUE".
           
       01 SUPPLIER-TABLE.
           05 SUPPLIER-ITEM  OCCURS 7 TIMES.
               10 SUPPLIER-CODE  PIC X(5)  VALUE SPACES.
               10 SUPPLIER-NAME  PIC X(20) VALUE SPACES.
       
       01 SUB                    PIC 9(2)      value zero.
       
       01 FLAGS.
          05 EOF-INVENT          PIC X(3)         VALUE SPACES.
          05 EOF-SUPPLIER        PIC X(3)         VALUE SPACES.
          05 EOF-SEARCH          PIC X(3)         VALUE SPACES.
      
       01 CONUNTERS.
          05 READ-COUNTER-TMP    PIC 9(4)         VALUE ZERO.
          05 WRITTEN-COUNTER-TMP PIC 9(4)         VALUE ZERO.
          
       01 CALCULATE-VALUE.
          05 TOTAL-VALUE-TMP     PIC 9(10)        VALUE ZERO.
          05 STOCK-VALUE-TMP     PIC 9(8)         VALUE ZERO.

       01 STATUS-FIELD	      PIC X(2)  VALUE SPACES.
       
       PROCEDURE DIVISION.
      *THE TOP LEVEL OF THE PROGRAM.
       100-PRODUCE-INVENTORY-REORDER-REPORT.
           PERFORM  200-INIT-INVENTORY-REORDER-REPORT.
           PERFORM  200-PRODUCE-INVENTORY-REPORT-RECORD
                    UNTIL EOF-INVENT = "YES".
           PERFORM  200-TERMINATE-INVENTORY-REORDER-REPORT.
           EXIT PROGRAM.

      *THE INITIATION OF THE PROGRAM
      *INITIATE INVENTORY REPORT AND REORDER REPORT.
       200-INIT-INVENTORY-REORDER-REPORT.
           PERFORM  300-OPEN-IN-OUT-FILES.
           PERFORM  300-READ-INVENT-RECORD.
           PERFORM  300-WRITE-INVENTORY-REPORT-HEADER.   

      *CALCULATE AND WRITE REPORT RECORD.                              
       200-PRODUCE-INVENTORY-REPORT-RECORD.
           PERFORM 300-CAL-STOCK-VALUE.
           PERFORM 300-CAL-TOTAL-VALUE.
           PERFORM 300-WRITE-INVENT-DETAIL.
           PERFORM 300-READ-INVENT-RECORD.

      *THE TERMINATION OF THE PROGRAM.
      *DISPLAY AUDIT TRAIL, CLOSE FILES TO TERMINATE MAINLINE
       200-TERMINATE-INVENTORY-REORDER-REPORT.
           PERFORM  300-WRITE-AUDIT-TRAIL-OUT.
           PERFORM  300-CLOSE-IN-OUT-FILES.

      *OPEN INPUT AND OUTPUT FILES.
       300-OPEN-IN-OUT-FILES.
           OPEN INPUT  INVENT-FILE-IN  SUPPLIER-FILE-IN
           OPEN OUTPUT INVENT-FILE-OUT.
           
      *READ 1 RECORD FROM INPUT INVENTORY FILE,
      *SET EOF-INVENT IF REACH THE END OF THE INVENT FILE,
      *OTHERWISE INCREASE THE READ COUNTER.
       300-READ-INVENT-RECORD.
           READ INVENT-FILE-IN
               AT END MOVE "YES" TO EOF-INVENT
                   NOT AT END ADD 1 TO READ-COUNTER-TMP.

      *WRITE INVENTORY REPORT HEADER.
       300-WRITE-INVENTORY-REPORT-HEADER.  
           WRITE INVENT-RECORD-OUT FROM INVENT-REPORT-HEADER.
       
      *CALL A SUBPROGRAMME
      *GET AN ITEM'S STOCK VALUE BY 
      *MULTIPYING THE QUANTITY AND THE UNIT PRICE.
       300-CAL-STOCK-VALUE.
       CALL "CALCULAT"
           USING QUANTITY-IN, UNIT-PRICE-IN, STOCK-VALUE-TMP.

      *GET THE TOTAL INVENTORY VALUE BY ADDING ALL STOCK VALUES.       
       300-CAL-TOTAL-VALUE.
           ADD STOCK-VALUE-TMP TO TOTAL-VALUE-TMP.

      *COMPOSE THE INVENTORY OUTPUT RECORD,
      *WRITE IT TO THE INVENTORY OUTPUT FILE,
      *INCREASE THE WRITTEN-COUNTER.
       300-WRITE-INVENT-DETAIL.
           MOVE PART-NUMBER-IN TO PART-NUMBER-OUT.
           MOVE PART-NAME-IN TO PART-NAME-OUT.
           MOVE STOCK-VALUE-TMP TO STOCK-VALUE-OUT.
           MOVE INVENT-DETAIL-OUT TO INVENT-RECORD-OUT.
           WRITE INVENT-RECORD-OUT.
           ADD 1 TO WRITTEN-COUNTER-TMP.

      *WRITE AUDIT TRAIL TO THE INVENTORY OUTPUT FILE.                  
       300-WRITE-AUDIT-TRAIL-OUT.
           MOVE TOTAL-VALUE-TMP TO INVENT-TOTAL-VALUE.
           MOVE READ-COUNTER-TMP TO RECORDS-READ.
           MOVE WRITTEN-COUNTER-TMP TO RECORDS-WRITTEN.
           MOVE AUDIT-TRAIL-OUT TO INVENT-RECORD-OUT.
           WRITE INVENT-RECORD-OUT.

      *CLOSE INPUT OUTPUT FILES.
       300-CLOSE-IN-OUT-FILES.
           CLOSE INVENT-FILE-IN  SUPPLIER-FILE-IN 
                 INVENT-FILE-OUT.

       END PROGRAM INVENTORY.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULAT.
       AUTHOR.    Xiaoyang Miao.

       LINKAGE SECTION.
            01 LS-QUANTITY-IN       PIC 9(4)       VALUE ZERO.
            01 LS-UNIT-PRICE-IN     PIC 9(4)       VALUE ZERO.
            01 LS-STOCK-VALUE-TMP   PIC 9(8)       VALUE ZERO.
       
       PROCEDURE DIVISION USING
            LS-QUANTITY-IN, LS-UNIT-PRICE-IN, LS-STOCK-VALUE-TMP.
            MULTIPLY LS-QUANTITY-IN BY LS-UNIT-PRICE-IN
            GIVING LS-STOCK-VALUE-TMP.
            
       END PROGRAM  CALCULAT.
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REORDER.
       AUTHOR.     FANG.

      *IDENTIFY FILE IN AND FILE OUT 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENT-FILE-IN
               ASSIGN TO "D:\COBOL\INVENT6.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS PART-NUM-IN
               FILE STATUS IS STATUS-FIELD.
           SELECT SUPPLIER-FILE-IN
               ASSIGN TO"D:\COBOL\SUPPLIERI.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SUPP-CODE-IN
               FILE STATUS IS STATUS-FIELD.
           SELECT  REORDER-REPORT-OUT
               ASSIGN TO"D:\COBOL\REORDERFILE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL. 


      *FIELDS DECLARATION AND DEFINITION             
       DATA DIVISION.
       FILE SECTION.
       FD INVENT-FILE-IN.
       01 INVENTORY-RECORD.
           05 PART-NUM-IN          PIC 9(7)    VALUE ZEROS.
           05 PART-NAME-IN         PIC X(20)   VALUE SPACES.
           05 QTY-ON-HAND-IN       PIC 9(4)    VALUE ZEROS.        
           05 UNIT-PRICE-IN        PIC 9(4)    VALUE ZEROS.
           05 RE-ORDER-POINT-IN    PIC 9(4)    VALUE ZEROS.
           05 SUPPLIER-CODE-IN     PIC X(5)    VALUE SPACES.

       FD REORDER-REPORT-OUT.
       01 REORDER-REPORT-DATA     PIC X(70).   

       FD SUPPLIER-FILE-IN.
       01 SUPPLIER-RECORD-IN.
           05 SUPP-CODE-IN      PIC X(5)    VALUE SPACES.
           05 SUPP-NAME-IN      PIC X(20)   VALUE SPACES.



      *LOCAL DATA DECLARATION AND DEFINITION
       WORKING-STORAGE SECTION.

       01 REORDER-COLUMN-NAME.
           05 FILLER PIC X(15)   VALUE"INVENTORY-NUM".
           05 FILLER PIC X(15)   VALUE"INVENTORY-NAME".
           05 FILLER PIC X(25)   VALUE"QUANTITY-ON-HAND".
           05 FILLER PIC X(20)   VALUE"SUPPLIER-NAME".

       01 FLAGS.
           05 EOF-INVENT               PIC X(3)    VALUE "NO".
           05 EOF-SUPPLIER             PIC X(3)    VALUE "NO".
           05 EOF-QUERY                PIC X(3)    VALUE "NO".
           05 SUPPLIER-CODE-IN-SEARCH  PIC X(5)    VALUE SPACES.

       01 REORDER-REPORT-OUT-RECORD.
           05 INVENTORY-NUM-OUT        PIC 9(7)    VALUE ZEROS.
           05 FILLER                   PIC X(6)    VALUE SPACES.
           05 INVENTORY-NAME-OUT       PIC X(20)   VALUE ZEROS.
           05 FILLER                   PIC X(2)    VALUE SPACES.
           05 QTY-ON-HAND-OUT          PIC 9(4)    VALUE ZEROS.
           05 FILLER                   PIC X(12)   VALUE SPACES.
           05 SUPPLIER-NAME-OUT        PIC X(20)   VALUE SPACES.

       01 STATUS-FIELD	      PIC X(2)  VALUE SPACES.

      * PROGRAM LOGIC
       PROCEDURE DIVISION.
      * PROGRAM LOGIC CONTROL CENTER, THE HIGHEST LEVEL
       100-CREATE-INVENT-REORDER-REPORT.
           PERFORM 300-INIT-REORDER-REPORT.
           PERFORM 1000-WRITE-REORDER-HEADLINE-RECORD.
           PERFORM 300-SEARCH-INVENT-RECORD-WRITE-REORDER-RECORD        
               UNTIL EOF-INVENT = "YES".
           PERFORM 300-TERMINATE-REORDER-REPORT.           
           EXIT PROGRAM.  

      * SECOND LEVEL OF LOGIC CONTROL
      * OPEN READ INVENT DATA. WRITE COLUMN HEADERS.
      * OPEN FILES AND READ INVENTORY RECORD

       300-INIT-REORDER-REPORT.
           PERFORM 700-OPEN-ALL-FILES.

      * READ INVENT REPORT      
       300-SEARCH-INVENT-RECORD-WRITE-REORDER-RECORD.                   
           PERFORM 700-SEARCH-INV-RECORD-WRITE-REORDER-RECORD.

      *TERMINATE REORDER REPORT PROCESS 
       300-TERMINATE-REORDER-REPORT.
           PERFORM 700-CLOSE-ALL-FILES.

      *PROCEDURE THAT OPENS ALL FILES
       700-OPEN-ALL-FILES.
           OPEN INPUT  INVENT-FILE-IN.
           OPEN INPUT  SUPPLIER-FILE-IN.
           OPEN OUTPUT REORDER-REPORT-OUT.

      * READ INVENTORY RECORD IN
      * IF REORDER CONDITION SATISFIED
      * MOVE DATA TO OUTPUT RECORD
      * SEARCH THE SUPPLY FILE
      * WRITE THE REORDER REPORT RECORD
       700-SEARCH-INV-RECORD-WRITE-REORDER-RECORD.               
           READ INVENT-FILE-IN
               AT END MOVE "YES" TO EOF-INVENT
               NOT AT END
                   IF QTY-ON-HAND-IN < RE-ORDER-POINT-IN 
                       MOVE PART-NUM-IN       TO INVENTORY-NUM-OUT       
                       MOVE PART-NAME-IN      TO INVENTORY-NAME-OUT
                       MOVE QTY-ON-HAND-IN    TO QTY-ON-HAND-OUT
                       MOVE SUPPLIER-CODE-IN  TO SUPP-CODE-IN                   
                       PERFORM 1000-SEARCH-SUPPLIER-TABLE
                       PERFORM 1000-WRITE-REORDER-DETAIL-RECORD.
                  
      *PROCEDURE TO SEARCH THE SUPPLIER FILE 
      *WRITE SUPPLIER NAME OUT
       1000-SEARCH-SUPPLIER-TABLE. 
           READ SUPPLIER-FILE-IN
               INVALID KEY MOVE "ER" TO STATUS-FIELD
               NOT INVALID KEY MOVE SUPP-NAME-IN TO SUPPLIER-NAME-OUT
           END-READ.

      *WRITE REORDER RECORDER HEADER        
       1000-WRITE-REORDER-HEADLINE-RECORD.
           WRITE REORDER-REPORT-DATA 
               FROM REORDER-COLUMN-NAME.

      *WRITE REORDER RECORDER DETAIL        
       1000-WRITE-REORDER-DETAIL-RECORD.
           WRITE REORDER-REPORT-DATA 
                   FROM REORDER-REPORT-OUT-RECORD.
       
      *CLOSE ALL FILES
       700-CLOSE-ALL-FILES.
           CLOSE INVENT-FILE-IN.
           CLOSE SUPPLIER-FILE-IN.
           CLOSE REORDER-REPORT-OUT. 

       end program REORDER.

       end program CONTROL.
