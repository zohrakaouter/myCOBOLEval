       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROLLING-MAX.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-MAX-VALUE PIC S9(10).
       01 WS-I PIC 9(4) VALUE 1.
       01 WS-J PIC 9(4) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * From a given list of integers, generate a list of rolling maximum element found until given moment
      * in the sequence.
      * >>> rolling_max([1, 2, 3, 2])
      * [1, 2, 3, 3]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
           SET WS-I TO 1
           SET WS-J TO 1
           
           IF WS-I = 1 THEN
               SET WS-MAX-VALUE TO L-NUMBERS(WS-I)
               MOVE L-NUMBERS(WS-I) TO RESULT(WS-J)
               ADD 1 TO WS-J
               ADD 1 TO WS-I
           END-IF
           
           PERFORM UNTIL WS-I > 4
               IF L-NUMBERS(WS-I) > WS-MAX-VALUE THEN
                   MOVE L-NUMBERS(WS-I) TO WS-MAX-VALUE
               END-IF
               MOVE WS-MAX-VALUE TO RESULT(WS-J)
               ADD 1 TO WS-J
               ADD 1 TO WS-I
           END-PERFORM

           GOBACK.

       END PROGRAM ROLLING-MAX.
