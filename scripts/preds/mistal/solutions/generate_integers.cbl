       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENERATE-INTEGERS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-START PIC S9(10).
           05 WS-END PIC S9(10).
           05 WS-TEMP PIC S9(10).
           05 WS-REMAINDER PIC S9(10).
           05 WS-INDEX PIC 9(3) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC S9(10).
           05 L-B PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * 
      * Given two positive integers a and b, return the even digits between a
      * and b, in ascending order.
      * 
      * For example:
      * generate_integers(2, 8) => [2, 4, 6, 8]
      * generate_integers(8, 2) => [2, 4, 6, 8]
      * generate_integers(10, 14) => []
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-A TO WS-START
           MOVE L-B TO WS-END

           IF WS-START > WS-END
               MOVE WS-END TO WS-TEMP
               MOVE WS-START TO WS-END
               MOVE WS-TEMP TO WS-START
           END-IF

           PERFORM VARYING WS-TEMP FROM WS-START BY 1
                   UNTIL WS-TEMP > WS-END
               DIVIDE WS-TEMP BY 2 GIVING WS-TEMP REMAINDER WS-REMAINDER
               IF WS-REMAINDER = 0
                   ADD 1 TO WS-INDEX
                   MOVE WS-TEMP TO RESULT(WS-INDEX)
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM GENERATE-INTEGERS.
