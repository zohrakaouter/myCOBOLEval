       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNIQUE-DIGITS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-VARIABLES.
           05 I        PIC 9(2) VALUE 1.
           05 J        PIC 9(2) VALUE 1.
           05 K        PIC 9(2) VALUE 1.
           05 DIGIT    PIC 9(1).
           05 HAS-EVEN PIC 9(1) VALUE 0.
           05 IDX-X    PIC 9(2) VALUE 1.
           05 TEMP     PIC 9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-X OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * Given a list of positive integers x. return a sorted list of all
      * elements that hasn't any even digit.
      * 
      * Note: Returned list should be sorted in increasing order.
      * 
      * For example:
      * >>> unique_digits([15, 33, 1422, 1])
      * [1, 15, 33]
      * >>> unique_digits([152, 323, 1422, 10])
      * []
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCESS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI GREATER THAN 4
               MOVE L-X(NI) TO TEMP
               MOVE 0 TO HAS-EVEN
               PERFORM CHECK-DIGITS
               IF HAS-EVEN = 0
                  MOVE TEMP TO RESULT(IDX-X)
                  ADD 1 TO IDX-X
               END-IF
           END-PERFORM
           
           SORT RESULT USING J IN ASCENDING KEY RESULT(J)
               GIVING RESULT
               ON ASCENDING
           END-SORT

       CHECK-DIGITS.
           MOVE TEMP TO WS-TEMP
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > FUNCTION INTEGER-OF-DATE(WS-TEMP)
               COMPUTE DIGIT = FUNCTION MOD(WS-TEMP, 10)
               DIVIDE WS-TEMP BY 10 GIVING WS-TEMP
               IF FUNCTION MOD(DIGIT, 2) = 0
                   MOVE 1 TO HAS-EVEN
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       
       GOBACK.
       
       END PROGRAM UNIQUE-DIGITS.
