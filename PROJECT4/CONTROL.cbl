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
      * 100-CREATE-REPORTS.
       BEGIN.
      *     CALL ".\CONVERT".
           CALL ".\UPDATE".
           CALL ".\INVENTORY".
           CALL ".\REORDER".

           STOP RUN.

       end program CONTROL.
