       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULAT as "CALCULAT".
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