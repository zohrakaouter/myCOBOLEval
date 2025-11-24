       IDENTIFICATION DIVISION.
       PROGRAM-ID. HAS-CLOSE-ELEMENTS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 W-NUMBER1 COMP-2.
       01 W-NUMBER2 COMP-2.
       01 W-DIFFERENCE COMP-2.
       01 FOUND-RESULT-SWITCH PIC 9 VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 5 TIMES INDEXED BY NI COMP-2.
           05 L-THRESHOLD COMP-2.
           05 RESULT PIC 9.

      * Check if in given list of numbers, are any two numbers closer to each other than
      * given threshold.
      * >>> has_close_elements([1.0, 2.0, 3.0, 4.0, 5.0], 0.5)
      * False
      * >>> has_close_elements([1.0, 2.8, 3.0, 4.0, 5.0], 0.3)
      * True
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI >= 5
               SET FOUND-RESULT-SWITCH TO 0
               PERFORM VARYING NI2 FROM 1 BY 1 UNTIL NI2 >= 5
                   IF NI NOT EQUAL TO NI2
                       MOVE L-NUMBERS(NI) TO W-NUMBER1
                       MOVE L-NUMBERS(NI2) TO W-NUMBER2
                       COMPUTE W-DIFFERENCE = FUNCTION ABS(W-NUMBER1 - W-NUMBER2)
                       IF W-DIFFERENCE < L-THRESHOLD
                           SET RESULT TO TRUE
                           SET FOUND-RESULT-SWITCH TO 1
                           EXIT PERFORM
                       END-IF
                   END-IF
               END-PERFORM
               IF FOUND-RESULT-SWITCH = 1
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF FOUND-RESULT-SWITCH = 0
               SET RESULT TO FALSE
           END-IF
           GOBACK.
       END PROGRAM HAS-CLOSE-ELEMENTS.
