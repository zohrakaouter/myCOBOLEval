       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRANGE-SORT-LIST.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARS.
           05 WS-I PIC S9(10).
           05 WS-J PIC S9(10).
           05 WS-MIN-VAL PIC S9(10).
           05 WS-MAX-VAL PIC S9(10).
           05 WS-MIN-INDX PIC S9(10).
           05 WS-MAX-INDX PIC S9(10).
           05 WS-TEMP PIC S9(10).
           05 WS-COUNT PIC S9(10) VALUE 0.
           05 WS-SWITCH PIC X VALUE 'M'. *> 'M' for min, 'X' for max

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * '''
      * Given list of integers, return list in strange order.
      * Strange sorting, is when you start with the minimum value,
      * then maximum of the remaining integers, then minimum and so on.
      * 
      * Examples:
      * strange_sort_list([1, 2, 3, 4]) == [1, 4, 2, 3]
      * strange_sort_list([5, 5, 5, 5]) == [5, 5, 5, 5]
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
               SET WS-COUNT UP BY 1
               IF WS-SWITCH = 'M'
                   PERFORM FIND-MINIMUM
                   MOVE WS-MIN-VAL TO RESULT(WS-COUNT)
                   MOVE -1 TO L-LST(WS-MIN-INDX)
                   SET WS-SWITCH TO 'X'
               ELSE
                   PERFORM FIND-MAXIMUM
                   MOVE WS-MAX-VAL TO RESULT(WS-COUNT)
                   MOVE -1 TO L-LST(WS-MAX-INDX)
                   SET WS-SWITCH TO 'M'
               END-IF
           END-PERFORM.

           GOBACK.

       FIND-MINIMUM.
           MOVE 9999999999 TO WS-MIN-VAL
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               IF L-LST(WS-I) NOT = -1
                   IF L-LST(WS-I) < WS-MIN-VAL
                       MOVE L-LST(WS-I) TO WS-MIN-VAL
                       MOVE WS-I TO WS-MIN-INDX
                   END-IF
               END-IF
           END-PERFORM.

       FIND-MAXIMUM.
           MOVE -9999999999 TO WS-MAX-VAL
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               IF L-LST(WS-I) NOT = -1
                   IF L-LST(WS-I) > WS-MAX-VAL
                       MOVE L-LST(WS-I) TO WS-MAX-VAL
                       MOVE WS-I TO WS-MAX-INDX
                   END-IF
               END-IF
           END-PERFORM.
