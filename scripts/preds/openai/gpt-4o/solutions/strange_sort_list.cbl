       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRANGE-SORT-LIST.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-TEMP-VARS.
           05 WS-COUNT       PIC S9(10) VALUE 0.
           05 WS-SORTED      PIC X(01) VALUE "N".
           05 WS-SWITCH      PIC X(01) VALUE "Y".
           05 WS-TEMP        PIC S9(10).

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
           
       MAIN-PROCESS.
           PERFORM PREPARE-ARRAY
           PERFORM STRANGE-SORT
           PERFORM SHOW-RESULT
           .
           
       PREPARE-ARRAY.
           SET WS-COUNT TO LENGTH OF L-LST
           MOVE ALL ZEROS TO RESULT
           .

       STRANGE-SORT.
           PERFORM UNTIL WS-COUNT = 0
               SET WS-SORTED TO "Y"
               SET NJ TO 1
               PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > LENGTH OF L-LST
                   IF L-LST(NI) NOT EQUAL TO ZERO
                       IF WS-SWITCH = "Y"
                           IF L-LST(NI) < RESULT(NJ) OR NJ = 1
                               MOVE L-LST(NI) TO RESULT(NJ)
                               SET WS-SORTED TO "N"
                           END-IF
                       ELSE
                           IF L-LST(NI) > RESULT(NJ) OR NJ = 1
                               MOVE L-LST(NI) TO RESULT(NJ)
                               SET WS-SORTED TO "N"
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF WS-SORTED = "N"
                   PERFORM DELETE-USED-VALUE
                   SET NJ UP BY 1
                   IF WS-SWITCH = "Y"
                       SET WS-SWITCH TO "N"
                   ELSE
                       SET WS-SWITCH TO "Y"
                   END-IF
               END-IF
           END-PERFORM
           .

       DELETE-USED-VALUE.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > LENGTH OF L-LST
               IF L-LST(NI) = RESULT(NJ)
                   MOVE ZERO TO L-LST(NI)
                   SET WS-COUNT DOWN BY 1
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

       SHOW-RESULT.
           DISPLAY "RESULT: " 
           PERFORM VARYING NJ FROM 1 BY 1 UNTIL NJ > LENGTH OF RESULT
               IF RESULT(NJ) NOT = 0
                   DISPLAY RESULT(NJ)
               END-IF
           END-PERFORM
           .

       GOBACK.
       END PROGRAM STRANGE-SORT-LIST.
