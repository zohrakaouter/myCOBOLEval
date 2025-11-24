       IDENTIFICATION DIVISION.
       PROGRAM-ID. BELOW-THRESHOLD.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-VAR.
           05 ALL-BELOW PIC 9 VALUE 1.
           
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 L-T PIC S9(10).
           05 RESULT PIC 9.

      * Return True if all numbers in the list l are below threshold t.
      * >>> below_threshold([1, 2, 4, 10], 100)
      * True
      * >>> below_threshold([1, 20, 4, 10], 5)
      * False
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
           IF L-L(NI) NOT < L-T
               MOVE 0 TO ALL-BELOW
               EXIT PERFORM
           END-IF
       END-PERFORM

       MOVE ALL-BELOW TO RESULT

       GOBACK.

       END PROGRAM BELOW-THRESHOLD.
