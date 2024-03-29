       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY as "INVENTORY".
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
           DISPLAY 'UPDATE1'.
           PERFORM  200-INIT-INVENTORY-REORDER-REPORT.
           PERFORM  200-PRODUCE-INVENTORY-REPORT-RECORD
                    UNTIL EOF-INVENT = "YES".
           PERFORM  200-TERMINATE-INVENTORY-REORDER-REPORT.
           STOP RUN.

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
       CALL ".\CALCULAT"
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
