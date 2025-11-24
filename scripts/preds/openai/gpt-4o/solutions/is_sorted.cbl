       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-SORTED.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 PREV-NUM PIC S9(10) VALUE -1.
       01 CURRENT-NUM PIC S9(10).
       01 COUNTER-NUMERIC PIC 9(1) VALUE 0.
       01 VALID-SORT PIC 9 VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC 9.

      * '''
      * Given a list of numbers, return whether or not they are sorted
      * in ascending order. If list has more than 1 duplicate of the same
      * number, return False. Assume no negative numbers and only integers.
      * 
      * Examples
      * is_sorted([1, 2, 3, 4, 5]) ➞ True
      * is_sorted([1, 3, 2, 4, 5]) ➞ False
      * is_sorted([1, 2, 2, 2, 3]) ➞ False
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           SET NI TO 1
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 5
               MOVE L-LST(NI) TO CURRENT-NUM

               IF CURRENT-NUM = PREV-NUM
                   ADD 1 TO COUNTER-NUMERIC
               ELSE
                   MOVE 0 TO COUNTER-NUMERIC
               END-IF

               IF COUNTER-NUMERIC > 1
                   MOVE 0 TO VALID-SORT
                   EXIT PERFORM
               END-IF

               IF CURRENT-NUM < PREV-NUM
                   MOVE 0 TO VALID-SORT
                   EXIT PERFORM
               END-IF

               MOVE CURRENT-NUM TO PREV-NUM
           END-PERFORM

           MOVE VALID-SORT TO RESULT
           GOBACK
       END PROGRAM IS-SORTED.
