       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTROL as "CONTROL".
       AUTHOR.     George Yang.

       PROCEDURE DIVISION.
       
      *==============================================================
      * The top level of the project
      * call 3 sub programs to 
      * 1. convert files to indexed files
      * 2. update invent file according to transactions
      * 3. generate report files
      *==============================================================
       100-CREATE-REPORTS.
           CALL ".\CONVERT".
           CALL ".\UPDATE".
           CALL ".\REPORT".

           STOP RUN.

       end program CONTROL.
