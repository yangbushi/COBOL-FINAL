       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program1 as "Program1".
       AUTHOR. George Yang.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	       SELECT INVENT-FILE-OUT
               ASSIGN TO "D:\COBOL\INVFILE.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA  DIVISION.
       FILE SECTION.
	   FD  INVENT-FILE-OUT.
       01 INVENT-FILE-RECORD-OUT PIC 9(15).
       WORKING-STORAGE SECTION.
       01 INVENT-RECORD-OUT.
           05 PART-NUMBER PIC 9(7).
           05 QUANTITY PIC 9(4).
           05 UNIT-PRICE PIC 9(4).
       01 ENTRY-FLAG PIC A(1).
       
       
       PROCEDURE DIVISION.
      
      *Control of create inventory record file
       CREATE-INVENT-FILE-RTN.
	       PERFORM INIT-CREATE-FILE-RTN.
		   PERFORM WRITE-RECORD-RTN  
              UNTIL ENTRY-FLAG = "N".
		   PERFORM  TERM-CREATE-FILE-RTN.
		   STOP RUN.
      
      *Initiate create inventory record file
       INIT-CREATE-FILE-RTN.     
	       PERFORM OPEN-FILE-RTN.
		   PERFORM  PROMPT-ENTRY-RTN. 
      
      *Control of write an inventory record to file
       WRITE-RECORD-RTN.
           PERFORM  PROMPT-FIELD-RTN.
	       PERFORM  WRITE-RECORD-FILE-RTN.
	       PERFORM  PROMPT-ENTRY-RTN.
       
      *Open inventory record file 
       OPEN-FILE-RTN.
           OPEN OUTPUT INVENT-FILE-OUT.
      
      *Prompt user for record entry
       PROMPT-ENTRY-RTN.
           DISPLAY "Record to enter ( Y or N )"
               LINE 16 COLUMN 10.
           ACCEPT ENTRY-FLAG
               LINE 17 COLUMN 10.
      
      *Prompt user for inventory record detail 
       PROMPT-FIELD-RTN.
           DISPLAY "Enter Part Number ( 7 digits )"
               LINE 4 COLUMN 5.
           ACCEPT PART-NUMBER
               LINE 5 COLUMN 10.
           DISPLAY "Enter Quantity  ( 4 digits )"
               LINE 6 COLUMN 5.
           ACCEPT QUANTITY
               LINE 7 COLUMN 10.
           DISPLAY "Enter Unit price ( 4 digits )"
               LINE 8 COLUMN 5.
           ACCEPT UNIT-PRICE
               LINE 9 COLUMN 10.
      
      *Write inventory record to file
       WRITE-RECORD-FILE-RTN.
           MOVE INVENT-RECORD-OUT TO INVENT-FILE-RECORD-OUT.
           WRITE INVENT-FILE-RECORD-OUT.
      
      *Close inventory record file
       TERM-CREATE-FILE-RTN.
           CLOSE INVENT-FILE-OUT.
       
       end program Program1.
