       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATE as "UPDATE".
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
           05 VALUE "INVENTORY NUMBER:"   LINE 3 COL 10.
           05 PART-NUMBER-IN              LINE 3 COL 25
               PIC 9(7) TO PART-NUMBER-WS.
           05 VALUE "TRANSACTION CODE:"   LINE 5 COL 10.
           05 TRANS-CODE-IN               LINE 5 COL 25
               PIC A(1) TO TRANS-CODE-WS.
           05 VALUE "TRANSACTION AMOUNT:" LINE 7 COL 10.
           05 TRANS-AMOUNT-IN             LINE 7 COL 25
               PIC 9(4) TO TRANS-AMOUNT-WS.
           
      *==============================================================
      * The top level of the program.
      *==============================================================
       PROCEDURE DIVISION.
       100-UP-INV-FILE.
           PERFORM 200-INIT-UP-INV-FILE.
           PERFORM 201-UPDATE-INV-RECORD.
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
           302-PROMPT-TRANS-FIELDS.
           303-READ-INV-AMOUNT.
           IF STATUS-FIELD = "OK" AND SALE
               PERFORM 304-CAL-SALE-AMOUT
           ELSE IF STATUS-FIELD = "OK" AND RECEIPT.
               PERFORM 305-CAL-RECEIPT-AMOUT.
           IF STATUS-FIELD = "OK"
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
               LINE 16 COLUMN 10.
           ACCEPT ENTRY-FLAG
               LINE 16 COLUMN 25.
       
      *==============================================================
      * Display the screen section,
      * get the transaction data.
      *==============================================================
       302-PROMPT-TRANS-FIELDS.
           DISPLAY TRANSACTION-SCREEN.
           ACCEPT  TRANSACTION-SCREEN.
       
      *==============================================================
      * Read the inventory record according to
      * the key in the transaction.
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
       
       end program UPDATE.
