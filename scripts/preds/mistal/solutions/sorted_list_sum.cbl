       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORTED-LIST-SUM.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-WORK-AREAS.
           05 WS-I PIC 9(3) VALUE 0.
           05 WS-J PIC 9(3) VALUE 0.
           05 WS-K PIC 9(3) VALUE 0.
           05 WS-TEMP-LEN PIC 9(3) VALUE 0.
           05 WS-TEMP-WORD PIC X(100) VALUE SPACES.
           05 WS-SWAP-FLAG PIC X VALUE 'N'.
               88 SWAP-NEEDED VALUE 'Y'.
               88 NO-SWAP-NEEDED VALUE 'N'.
           05 WS-COUNT PIC 9(3) VALUE 0.
           05 WS-WORD-LEN PIC 9(3) VALUE 0.

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

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PARA.
           PERFORM FILTER-ODD-LENGTHS
           PERFORM SORT-LIST
           GOBACK.

       FILTER-ODD-LENGTHS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
               COMPUTE WS-WORD-LEN = FUNCTION LENGTH(L-LST(NI))
               IF FUNCTION MOD(WS-WORD-LEN, 2) = 0
                   ADD 1 TO WS-COUNT
                   MOVE L-LST(NI) TO RESULT(WS-COUNT)
               END-IF
           END-PERFORM.

       SORT-LIST.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT - 1
               PERFORM VARYING WS-J FROM WS-I + 1 BY 1 UNTIL WS-J > WS-COUNT
                   COMPUTE WS-TEMP-LEN = FUNCTION LENGTH(RESULT(WS-I))
                   IF FUNCTION LENGTH(RESULT(WS-J)) < WS-TEMP-LEN
                       MOVE 'Y' TO WS-SWAP-FLAG
                   ELSE IF FUNCTION LENGTH(RESULT(WS-J)) = WS-TEMP-LEN
                       IF RESULT(WS-J) < RESULT(WS-I)
                           MOVE 'Y' TO WS-SWAP-FLAG
                       END-IF
                   END-IF
                   IF SWAP-NEEDED
                       MOVE RESULT(WS-I) TO WS-TEMP-WORD
                       MOVE RESULT(WS-J) TO RESULT(WS-I)
                       MOVE WS-TEMP-WORD TO RESULT(WS-J)
                       MOVE 'N' TO WS-SWAP-FLAG
                   END-IF
               END-PERFORM
           END-PERFORM.

       END PROGRAM SORTED-LIST-SUM.
