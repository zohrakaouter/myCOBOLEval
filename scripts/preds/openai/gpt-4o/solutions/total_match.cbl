       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOTAL-MATCH.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-LST1-TOTAL-CHARS PIC 9(5) VALUE 0.
       01 WS-LST2-TOTAL-CHARS PIC 9(5) VALUE 0.
       01 LIST-INDEX PIC 9(3) VALUE 1.
       01 LIST-LENGTH1 PIC 9(3) VALUE 0.
       01 LIST-LENGTH2 PIC 9(3) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST1 OCCURS 2 TIMES INDEXED BY NI PIC X(2).
           05 L-LST2 OCCURS 2 TIMES INDEXED BY NJ PIC X(2).
           05 RESULT OCCURS 100 TIMES INDEXED BY NK PIC X(100).

      * '''
      * Write a function that accepts two lists of strings and returns the list that has
      * total number of chars in the all strings of the list less than the other list.
      * 
      * if the two lists have the same number of chars, return the first list.
      * 
      * Examples
      * total_match(['hi', 'admin'], ['hI', 'Hi', 'hi', 'hi']) ➞ ['hI', 'Hi', 'hi', 'hi']
      * total_match(['hi', 'admin'], ['hi', 'hi', 'admin', 'project']) ➞ ['hi', 'admin']
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCEDURE.

           MOVE ZERO TO WS-LST1-TOTAL-CHARS
                     WS-LST2-TOTAL-CHARS.

       * Calculate total characters in first list (L-LST1)
           PERFORM VARYING LIST-INDEX FROM 1 BY 1 UNTIL LIST-INDEX > 2
               IF L-LST1(LIST-INDEX) NOT EQUAL SPACES
                   COMPUTE WS-LST1-TOTAL-CHARS = WS-LST1-TOTAL-CHARS + LENGTH OF L-LST1(LIST-INDEX)
               END-IF
           END-PERFORM.

       * Calculate total characters in second list (L-LST2)
           PERFORM VARYING LIST-INDEX FROM 1 BY 1 UNTIL LIST-INDEX > 2
               IF L-LST2(LIST-INDEX) NOT EQUAL SPACES
                   COMPUTE WS-LST2-TOTAL-CHARS = WS-LST2-TOTAL-CHARS + LENGTH OF L-LST2(LIST-INDEX)
               END-IF
           END-PERFORM.

       * Decision-making to store the result
           IF WS-LST1-TOTAL-CHARS >= WS-LST2-TOTAL-CHARS THEN
               PERFORM VARYING LIST-INDEX FROM 1 BY 1 UNTIL LIST-INDEX > 2
                   MOVE L-LST2(LIST-INDEX) TO RESULT(LIST-INDEX)
               END-PERFORM
           ELSE
               PERFORM VARYING LIST-INDEX FROM 1 BY 1 UNTIL LIST-INDEX > 2
                   MOVE L-LST1(LIST-INDEX) TO RESULT(LIST-INDEX)
               END-PERFORM
           END-IF.

           GOBACK.

       END PROGRAM TOTAL-MATCH.
