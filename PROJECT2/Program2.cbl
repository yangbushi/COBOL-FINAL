       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program2 as "Program2".
       AUTHOR.     George Yang.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENT-FILE-IN
               ASSIGN TO "D:\COBOL\INVFILE1.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INVENT-FILE-OUT
               ASSIGN TO "D:\COBOL\INVFILE2.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENT-FILE-IN.
       01  INVENT-RECORD-IN  PIC X(40).
       FD  INVENT-FILE-OUT.
       01  INVENT-RECORD-OUT PIC X(44).
       
       WORKING-STORAGE SECTION.
       01 INVENT-DETAIL-IN.
           05 PART-NUMBER-IN   PIC 9(7)  VALUE ZERO.
           05 PART-NAME-IN     PIC X(20) VALUE SPACES.
           05 QUANTITY-IN      PIC 9(4)  VALUE ZERO.
           05 UNIT-PRICE-IN    PIC 9(4)  VALUE ZERO.
           05 SUPPLIER-CODE-IN PIC X(5)  VALUE SPACES.
       
       01 INVENT-DETAIL-OUT.
           05 PART-NUMBER-OUT PIC 9(7)  VALUE ZERO.
           05 FILLER          PIC A(4)  VALUE SPACES.
           05 PART-NAME-OUT   PIC X(20) VALUE SPACES.
           05 FILLER          PIC A(4)  VALUE SPACES.
           05 STOCK-VALUE-OUT PIC 9(8)  VALUE ZERO.

       01 EOF-FLAG PIC A(3)  VALUE SPACES.
       
       01 AUDIT-TRAIL.
           05  VALUE-PROMPT     PIC A(5)  VALUE 'VALUE'.
           05  FILLER           PIC A(2)  VALUE SPACES.
           05  TOTAL-VALUE      PIC 9(10) VALUE ZERO.
           05  FILLER           PIC A(2)  VALUE SPACES.
           05  READ-PROMPT      PIC A(4)  VALUE 'READ'.
           05  FILLER           PIC A(2)  VALUE SPACES.
           05  READ-COUNTER     PIC 9(4)  VALUE ZERO.
           05  FILLER           PIC A(2)  VALUE SPACES.
           05  WRITTEN-PROMPT   PIC A(7)  VALUE 'WRITTEN'.
           05  FILLER           PIC A(2)  VALUE SPACES.
           05  WRITTEN-COUNTER  PIC 9(4)  VALUE ZERO.
       

       PROCEDURE DIVISION.
       
      *==============================================================
      * The top level of the program.
      *==============================================================
       100-CREATE-INVENTORY-FILE.
           PERFORM  200-INITIATE-CREATE-INV-FILE.
           PERFORM  201-CREATE-INV-RECORD
                    UNTIL EOF-FLAG = "YES".
           PERFORM  202-TERMINATE-CREATE-INV-FILE.

           STOP RUN.

      *==============================================================
      * The initiation of the program.
      *==============================================================
       200-INITIATE-CREATE-INV-FILE.
           PERFORM  300-OPEN-INVENTORY-FILES.
           PERFORM  301-READ-INV-FILE-IN.

      *==============================================================
      * Calculating and writing record.
      *==============================================================
       201-CREATE-INV-RECORD.
           PERFORM 302-CAL-STOCK-VALUE.
           PERFORM 303-CAL-TOTAL-VALUE.
           PERFORM 304-WRITE-INV-FILE-OUT.
           PERFORM 301-READ-INV-FILE-IN.

      *==============================================================
      * The termination of the program.
      *==============================================================
       202-TERMINATE-CREATE-INV-FILE.
           PERFORM  305-WRITE-AUDIT-TRAIL-OUT.
           PERFORM  306-CLOSE-INVENTORY-FILES.

      *==============================================================
      * Open input and output inventory files.
      *==============================================================
       300-OPEN-INVENTORY-FILES.
           OPEN INPUT INVENT-FILE-IN
                OUTPUT INVENT-FILE-OUT.

      *==============================================================
      * Read 1 record from input file,
      * set EOF-FLAG if reach the end of the file,
      * otherwise increase the READ-COUNTER.
      *==============================================================
       301-READ-INV-FILE-IN.
           READ INVENT-FILE-IN INTO INVENT-DETAIL-IN
               AT END
               MOVE "YES" TO EOF-FLAG
                   NOT AT END
                   ADD 1 TO READ-COUNTER.
                   
      *==============================================================
      * Get an item's stock value by multipying the quantity and
      * the unit price.
      *==============================================================
       302-CAL-STOCK-VALUE.
           MULTIPLY UNIT-PRICE-IN BY QUANTITY-IN
               GIVING STOCK-VALUE-OUT.
               
      *==============================================================
      * Get the total inventory value by adding all stock values.
      *==============================================================
       303-CAL-TOTAL-VALUE.
           ADD STOCK-VALUE-OUT
               TO TOTAL-VALUE.
               
      *==============================================================
      * Compose the output record,
      * write it to the output file,
      * increase the WRITTEN-COUNTER.
      *==============================================================
       304-WRITE-INV-FILE-OUT.
           MOVE PART-NUMBER-IN TO PART-NUMBER-OUT.
           MOVE PART-NAME-IN TO PART-NAME-OUT.
           MOVE INVENT-DETAIL-OUT TO INVENT-RECORD-OUT.
           WRITE INVENT-RECORD-OUT.
           ADD 1 TO WRITTEN-COUNTER.
       
      *==============================================================
      * Write audit trail to the output file.
      *==============================================================
       305-WRITE-AUDIT-TRAIL-OUT.
           MOVE AUDIT-TRAIL TO INVENT-RECORD-OUT.
           WRITE INVENT-RECORD-OUT.

      *==============================================================
      * Close input output files.
      *==============================================================
       306-CLOSE-INVENTORY-FILES.
           CLOSE INVENT-FILE-IN INVENT-FILE-OUT.
       

       end program Program2.
