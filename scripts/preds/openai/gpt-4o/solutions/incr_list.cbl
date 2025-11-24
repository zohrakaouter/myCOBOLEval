       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCR-LIST.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-VALUE PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 3 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * Return list with elements incremented by 1.
      * >>> incr_list([1, 2, 3])
      * [2, 3, 4]
      * >>> incr_list([5, 3, 5])
      * [6, 4, 6]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCESS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 3
               MOVE L-L(NI) TO TEMP-VALUE
               ADD 1 TO TEMP-VALUE
               SET NJ TO NI
               MOVE TEMP-VALUE TO RESULT(NJ)
           END-PERFORM

           GOBACK.
       END PROGRAM INCR-LIST.
