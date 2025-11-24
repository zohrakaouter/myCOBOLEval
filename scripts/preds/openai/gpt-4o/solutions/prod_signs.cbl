       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROD-SIGNS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-MAGNITUDE-SUM     PIC S9(10) VALUE 0.
       01 WS-PRODUCT-SIGNS     PIC S9(10) VALUE 1.
       01 WS-TEMP-VALUE        PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * You are given an array arr of integers and you need to return
      * sum of magnitudes of integers multiplied by product of all signs
      * of each number in the array, represented by 1, -1 or 0.
      * 
      * Example:
      * >>> prod_signs([1, 2, 2, -4]) == -9
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
               SET WS-TEMP-VALUE TO L-ARR(NI)
               EVALUATE TRUE
                 WHEN L-ARR(NI) > 0 
                     ADD L-ARR(NI) TO WS-MAGNITUDE-SUM
                     MULTIPLY 1 BY WS-PRODUCT-SIGNS
                 WHEN L-ARR(NI) < 0 
                     ADD -L-ARR(NI) TO WS-MAGNITUDE-SUM
                     MULTIPLY -1 BY WS-PRODUCT-SIGNS
                 WHEN L-ARR(NI) = 0
                     MULTIPLY 0 BY WS-PRODUCT-SIGNS
                     EXIT PERFORM
               END-EVALUATE
           END-PERFORM

           MULTIPLY WS-PRODUCT-SIGNS BY WS-MAGNITUDE-SUM GIVING RESULT

           GOBACK.

       END PROGRAM PROD-SIGNS.
