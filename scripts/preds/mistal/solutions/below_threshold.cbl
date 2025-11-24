       IDENTIFICATION DIVISION.
       PROGRAM-ID. BELOW-THRESHOLD.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-RESULT-FLAG PIC X VALUE 'Y'.

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
               IF L-L(NI) >= L-T
                   MOVE 'N' TO WS-RESULT-FLAG
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-RESULT-FLAG = 'Y'
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.
       END PROGRAM BELOW-THRESHOLD.
