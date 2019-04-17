       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERT as "CONVERT".
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
           
      *==============================================================
      * The top level of the program.
      *==============================================================
       PROCEDURE DIVISION.
       BEGIN.
      * 100-CREATE-IND-FILES.
           DISPLAY 'UPDATE0'.
           PERFORM 200-INIT-CREATE-IND.
           PERFORM 201-CREATE-IND-INV
               UNTIL EOF-INVENT = "Y".
           PERFORM 202-CREATE-IND-SUP
               UNTIL EOF-SUPPLI = "Y".
           PERFORM 203-TERM-CREATE-IND.
           EXIT PROGRAM. 

       200-INIT-CREATE-IND.
           PERFORM 300-OPEN-FILES.
           PERFORM 301-READ-SEQ-INV.
           PERFORM 302-READ-SEQ-SUP.
           
       201-CREATE-IND-INV.
           PERFORM 303-WR-IND-INV.
           PERFORM 301-READ-SEQ-INV.
           
       202-CREATE-IND-SUP.
           PERFORM 304-WR-IND-SUP.
           PERFORM 302-READ-SEQ-SUP.
       
       203-TERM-CREATE-IND.
           CLOSE SEQ-INVENT SEQ-SUPPLIER
               INVENT-FILE SUPPLIER-FILE.
       
       300-OPEN-FILES.
           OPEN INPUT SEQ-INVENT SEQ-SUPPLIER.
           OPEN OUTPUT INVENT-FILE SUPPLIER-FILE.
       
       301-READ-SEQ-INV.
           READ SEQ-INVENT
               AT END MOVE "Y" TO EOF-INVENT.
       
       302-READ-SEQ-SUP.
           READ SEQ-SUPPLIER
               AT END MOVE "Y" TO EOF-SUPPLI.
           
       303-WR-IND-INV.
           WRITE INVENT-RECORD FROM SEQ-INVENT-RECORD
               INVALID KEY MOVE "ER" TO STATUS-FIELD.
               
       304-WR-IND-SUP.
           WRITE SUPPLIER-RECORD FROM SEQ-SUPPLIER-RECORD               
               INVALID KEY MOVE "ER" TO STATUS-FIELD.
           
       end program CONVERT.
