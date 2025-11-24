       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVEN-ODD-COUNT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-NUMBER PIC 9(10).
       01 WS-DIGIT  PIC X.
       01 EVEN-COUNT PIC 9(2) VALUE ZEROS.
       01 ODD-COUNT PIC 9(2) VALUE ZEROS.
       
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUM PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * Given an integer. return a list that has the number of even and odd digits respectively.
      * 
      * Example:
      * even_odd_count(-12) ==> [1, 1]
      * even_odd_count(123) ==> [1, 2]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
       * Convert negative number to positive for digit analysis
           IF L-NUM LESS THAN 0 THEN
               COMPUTE WS-NUMBER = L-NUM * -1
           ELSE
               MOVE L-NUM TO WS-NUMBER
           END-IF.

       * Initialize index for RESULT array
           SET NI TO 1

       * Count even and odd digits
           PERFORM VARYING WS-NUMBER FROM WS-NUMBER BY 0
               UNTIL WS-NUMBER = 0
               MOVE FUNCTION MOD(WS-NUMBER, 10) TO WS-DIGIT
               IF FUNCTION NUMVAL(WS-DIGIT) MOD 2 = 0 THEN
                  ADD 1 TO EVEN-COUNT
               ELSE
                  ADD 1 TO ODD-COUNT
               END-IF
               COMPUTE WS-NUMBER = FUNCTION INTEGER-PART(WS-NUMBER / 10)
           END-PERFORM

       * Store results in the RESULT array
           MOVE EVEN-COUNT TO RESULT(NI)
           ADD 1 TO NI
           MOVE ODD-COUNT TO RESULT(NI)
           
       * Mark the end of processing
           GOBACK.
       
       END PROGRAM EVEN-ODD-COUNT.
