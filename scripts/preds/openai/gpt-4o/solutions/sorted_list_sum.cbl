       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORTED-LIST-SUM.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-ITEMS.
           05 INPUT-LIST PIC X(100) OCCURS 100 TIMES.
           05 TEMP-ITEM PIC X(100) OCCURS 100 TIMES.
           05 RESULT-COUNT PIC 999 VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 4 TIMES INDEXED BY NI PIC X(4).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC X(100).

      * Write a function that accepts a list of strings as a parameter,
      * deletes the strings that have odd lengths from it,
      * and returns the resulted list with a sorted order,
      * The list is always a list of strings and never an array of numbers,
      * and it may contain duplicates.
      * The order of the list should be ascending by length of each word, and you
      * should return the list sorted by that rule.
      * If two words have the same length, sort the list alphabetically.
      * The function should return a list of strings in sorted order.
      * You may assume that all words will have the same length.
      * For example:
      * assert list_sort(["ab", "a", "aaa", "cd"]) => ["ab", "cd"]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

           LINKAGE SECTION.
           01 OUTPUT-ITEM PIC X(4).
       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-LOGIC.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
               CALL 'GET-PARAM-ITEM' USING NI OUTPUT-ITEM
               IF OUTPUT-ITEM IS NOT EQUAL TO SPACES
                  PERFORM PROCESS-ITEM
               END-IF
           END-PERFORM

           PERFORM SORT-RESULT-ARRAY

           GOBACK.

       GET-PARAM-ITEM SECTION.
       PROCEDURE DIVISION USING LINKED-ITEMS.
               MOVE L-LST(NI) TO OUTPUT-ITEM.
           EXIT SECTION.

       PROCESS-ITEM SECTION.
       PROCEDURE DIVISION USING LINKED-ITEMS.
               IF FUNCTION LENGTH(OUTPUT-ITEM) MOD 2 = 0
                   ADD 1 TO RESULT-COUNT
                   MOVE OUTPUT-ITEM TO TEMP-ITEM(RESULT-COUNT)
               END-IF
           EXIT SECTION.

       SORT-RESULT-ARRAY SECTION.
       PROCEDURE DIVISION USING LINKED-ITEMS.
               PERFORM VARYING NJ FROM 1 BY 1 UNTIL NJ > RESULT-COUNT
                   PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > RESULT-COUNT - NJ
                       IF FUNCTION LENGTH(TEMP-ITEM(NI)) > FUNCTION LENGTH(TEMP-ITEM(NI + 1))
                           MOVE TEMP-ITEM(NI) TO OUTPUT-ITEM
                           MOVE TEMP-ITEM(NI + 1) TO TEMP-ITEM(NI)
                           MOVE OUTPUT-ITEM TO TEMP-ITEM(NI + 1)
                       ELSE IF FUNCTION LENGTH(TEMP-ITEM(NI)) = FUNCTION LENGTH(TEMP-ITEM(NI + 1))
                           IF TEMP-ITEM(NI) > TEMP-ITEM(NI + 1)
                               MOVE TEMP-ITEM(NI) TO OUTPUT-ITEM
                               MOVE TEMP-ITEM(NI + 1) TO TEMP-ITEM(NI)
                               MOVE OUTPUT-ITEM TO TEMP-ITEM(NI + 1)
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM

               PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > RESULT-COUNT
                   MOVE TEMP-ITEM(NI) TO RESULT(NI)
               END-PERFORM
           EXIT SECTION.
