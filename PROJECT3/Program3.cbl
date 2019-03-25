       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program3 as "Program3".
       AUTHOR.     George Yang.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENT-FILE-IN
               ASSIGN TO "D:\COBOL\INVFILE3.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SUPPLIER-FILE-IN
               ASSIGN TO "D:\COBOL\SUPPLIER.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INVENT-FILE-OUT
               ASSIGN TO "D:\COBOL\INVFILE4.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REORDER-FILE-OUT
               ASSIGN TO "D:\COBOL\REORDER.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENT-FILE-IN.
       01  INVENT-RECORD-IN.
           05 PART-NUMBER-IN    PIC 9(7)  VALUE ZERO.
           05 PART-NAME-IN      PIC X(20) VALUE SPACES.
           05 QUANTITY-IN       PIC 9(4)  VALUE ZERO.
           05 UNIT-PRICE-IN     PIC 9(4)  VALUE ZERO.
           05 REORDER-POINT-IN  PIC 9(4)  VALUE ZERO.
           05 SUPPLIER-CODE-INV PIC X(5)  VALUE SPACES.
       FD  SUPPLIER-FILE-IN.
       01  SUPPLIER-RECORD-IN.
           05 SUPPLIER-CODE-IN   PIC X(5)  VALUE SPACES.
           05 SUPPLIER-NAME-IN   PIC X(20) VALUE SPACES.
       FD  INVENT-FILE-OUT.
       01  INVENT-RECORD-OUT PIC X(50).
       FD  REORDER-FILE-OUT.
       01  REORDER-RECORD-OUT.
           05 PART-NAME-REORDER      PIC X(20) VALUE SPACES.
           05 FILLER                 PIC A(2)  VALUE SPACES.            
           05 SUPPLIER-NAME-REORDER  PIC X(20) VALUE SPACES.
       
       WORKING-STORAGE SECTION.
       01 EOF-INVENT          PIC A(3)  VALUE SPACES.
       01 EOF-SUPPLIER        PIC A(3)  VALUE SPACES.
       01 FOUND-FLAG          PIC A(3)  VALUE SPACES.
       01 SUB                 PIC 9(2).
       01 TOTAL-VALUE-TMP     PIC 9(10) VALUE ZERO.
       01 STOCK-VALUE-TMP     PIC 9(8) VALUE ZERO.
       01 READ-COUNTER-TMP    PIC 9(4) VALUE ZERO.
       01 WRITTEN-COUNTER-TMP PIC 9(4) VALUE ZERO.
       
       01 INVENT-REPORT-HD.
           05 FILLER  PIC A(7)  VALUE 'PART NO'.
           05 FILLER  PIC A(4)  VALUE SPACES.
           05 FILLER  PIC A(9)  VALUE 'PART NAME'.
           05 FILLER  PIC A(17) VALUE SPACES.
           05 FILLER  PIC A(11)  VALUE 'STOCK VALUE'.
       
       01 INVENT-DETAIL-OUT.                                           
           05 PART-NUMBER-OUT PIC ZZZZZZ9.
           05 FILLER          PIC A(4)      VALUE SPACES.
           05 PART-NAME-OUT   PIC X(20)     VALUE SPACES.
           05 FILLER          PIC A(4)      VALUE SPACES.
           05 STOCK-VALUE-OUT PIC ZZZZZZZ9.

       01 AUDIT-TRAIL.
           05  FILLER           PIC A(5)  VALUE 'VALUE'.
           05  FILLER           PIC A(2)  VALUE SPACES.
           05  TOTAL-VALUE      PIC $$$9.
           05  FILLER           PIC A(2)  VALUE SPACES.
           05  FILLER           PIC A(4)  VALUE 'READ'.
           05  FILLER           PIC A(2)  VALUE SPACES.
           05  READ-COUNTER     PIC ZZZ9  VALUE ZERO.
           05  FILLER           PIC A(2)  VALUE SPACES.
           05  FILLER           PIC A(7)  VALUE 'WRITTEN'.
           05  FILLER           PIC A(2)  VALUE SPACES.
           05  WRITTEN-COUNTER  PIC ZZZ9.
       
       01 SUPPLIER-TABLE.
           05 SUPPLIER-ITEM  OCCURS 10 TIMES.
               10 SUPPLIER-CODE  PIC X(5)  VALUE SPACES.
               10 SUPPLIER-NAME  PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
       
      *==============================================================
      * The top level of the program.
      *==============================================================
       100-CREATE-REPORTS.
           PERFORM  200-INITIATE-CREATE-REPORTS.
           PERFORM  201-CREATE-REPORT-RECORD
                    UNTIL EOF-INVENT = "YES".
           PERFORM  202-TERMINATE-CREATE-REPORTS.

           STOP RUN.

      *==============================================================
      * The initiation of the program.
      *==============================================================
       200-INITIATE-CREATE-REPORTS.
           PERFORM  300-OPEN-IN-OUT-FILES.
           PERFORM  301-READ-INV-FILE-IN.
           PERFORM  302-CREATE-SUP-TABLE.
           PERFORM  309-CREATE-INV-HEADER.

      *==============================================================
      * Calculate and write report record.
      *==============================================================
       201-CREATE-REPORT-RECORD.
           PERFORM 303-CAL-STOCK-VALUE.
           PERFORM 304-CAL-TOTAL-VALUE.
           PERFORM 305-WRITE-INV-FILE-OUT.
           IF QUANTITY-IN < REORDER-POINT-IN 
               PERFORM 306-WRITE-REORDER-OUT.
           PERFORM 301-READ-INV-FILE-IN.

      *==============================================================
      * The termination of the program.
      *==============================================================
       202-TERMINATE-CREATE-REPORTS.
           PERFORM  307-WRITE-AUDIT-TRAIL-OUT.
           PERFORM  308-CLOSE-IN-OUT-FILES.

      *==============================================================
      * Open input and output files.
      *==============================================================
       300-OPEN-IN-OUT-FILES.
           OPEN INPUT INVENT-FILE-IN SUPPLIER-FILE-IN
                OUTPUT INVENT-FILE-OUT REORDER-FILE-OUT.

      *==============================================================
      * Read 1 record from input inventory file,
      * set EOF-INVENT if reach the end of the file,
      * otherwise increase the READ-COUNTER.
      *==============================================================
       301-READ-INV-FILE-IN.
           READ INVENT-FILE-IN
               AT END
               MOVE "YES" TO EOF-INVENT
                   NOT AT END
                   ADD 1 TO READ-COUNTER-TMP.
       
      *==============================================================
      * Create supplier table from supplier input file.
      *==============================================================
       302-CREATE-SUP-TABLE.
           PERFORM 400-READ-SUP-REC 
               VARYING SUB
                   FROM 1 BY 1
                       UNTIL SUB > 7.
       
      *==============================================================
      * Get an item's stock value by multipying the quantity and
      * the unit price.
      *==============================================================
       303-CAL-STOCK-VALUE.
           MULTIPLY UNIT-PRICE-IN BY QUANTITY-IN
               GIVING STOCK-VALUE-TMP.
               
      *==============================================================
      * Get the total inventory value by adding all stock values.
      *==============================================================
       304-CAL-TOTAL-VALUE.
           ADD STOCK-VALUE-TMP
               TO TOTAL-VALUE-TMP.
               
      *==============================================================
      * Compose the inventory output record,
      * write it to the inventory output file,
      * increase the WRITTEN-COUNTER.
      *==============================================================
       305-WRITE-INV-FILE-OUT.
           MOVE PART-NUMBER-IN TO PART-NUMBER-OUT.
           MOVE PART-NAME-IN TO PART-NAME-OUT.
           MOVE STOCK-VALUE-TMP TO STOCK-VALUE-OUT.
           WRITE INVENT-RECORD-OUT FROM INVENT-DETAIL-OUT.
           ADD 1 TO WRITTEN-COUNTER-TMP.
       
      *==============================================================
      * Compose the re-order output record,
      * write it to the re-order output file,
      *==============================================================
       306-WRITE-REORDER-OUT.
           PERFORM 401-SEARCH-SUP-NAME
               VARYING SUB 
                   FROM 1 BY 1
                       UNTIL FOUND-FLAG = "YES" OR SUB > 7.
           PERFORM 402-WRITE-RECORD.
       
      *==============================================================
      * Write audit trail to the inventory output file.
      *==============================================================
       307-WRITE-AUDIT-TRAIL-OUT.
           MOVE TOTAL-VALUE-TMP TO TOTAL-VALUE.
           MOVE READ-COUNTER-TMP TO READ-COUNTER.
           MOVE WRITTEN-COUNTER-TMP TO WRITTEN-COUNTER.
           WRITE INVENT-RECORD-OUT FROM AUDIT-TRAIL.

      *==============================================================
      * Close input output files.
      *==============================================================
       308-CLOSE-IN-OUT-FILES.
           CLOSE INVENT-FILE-IN SUPPLIER-FILE-IN 
                 INVENT-FILE-OUT REORDER-FILE-OUT.
       
      *==============================================================
      * Write report header to inventory output file.
      *==============================================================
       309-CREATE-INV-HEADER.
           WRITE INVENT-RECORD-OUT FROM INVENT-REPORT-HD.
       
      *==============================================================
      * Read 1 record from supplier file,
      * move it to the supplier table.
      *==============================================================
       400-READ-SUP-REC.
           READ SUPPLIER-FILE-IN
               AT END MOVE "YES" TO EOF-SUPPLIER
               NOT AT END MOVE SUPPLIER-RECORD-IN 
                   TO SUPPLIER-ITEM (SUB).

      *==============================================================
      * Search the supplier name, based on the supplier code.
      *==============================================================
       401-SEARCH-SUP-NAME.
           MOVE "NO" TO FOUND-FLAG.
           IF SUPPLIER-CODE-INV = SUPPLIER-CODE (SUB)
               MOVE "YES" TO FOUND-FLAG
               MOVE SUPPLIER-NAME (SUB) TO SUPPLIER-NAME-REORDER.
                                                                        
      *==============================================================
      * Write a record into re-ordered file.
      *==============================================================
       402-WRITE-RECORD.
           MOVE PART-NAME-IN TO PART-NAME-REORDER.
           WRITE REORDER-RECORD-OUT.
           
       end program Program3.
