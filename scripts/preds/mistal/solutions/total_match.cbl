       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOTAL-MATCH.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-COUNT1 PIC 9(5) VALUE 0.
           05 WS-COUNT2 PIC 9(5) VALUE 0.
           05 WS-I PIC 9(2) VALUE 0.
           05 WS-J PIC 9(2) VALUE 0.
           05 WS-K PIC 9(3) VALUE 0.
           05 WS-LEN1 PIC 9(3) VALUE 0.
           05 WS-LEN2 PIC 9(3) VALUE 0.

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

       MAIN-PARA.
           PERFORM COUNT-CHARS-LST1
           PERFORM COUNT-CHARS-LST2

           IF WS-COUNT2 > WS-COUNT1
               PERFORM MOVE-LST2-TO-RESULT
           ELSE
               PERFORM MOVE-LST1-TO-RESULT
           END-IF

           GOBACK.

       COUNT-CHARS-LST1.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 2
               COMPUTE WS-LEN1 = FUNCTION LENGTH(L-LST1(WS-I))
               ADD WS-LEN1 TO WS-COUNT1
           END-PERFORM.

       COUNT-CHARS-LST2.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 2
               COMPUTE WS-LEN2 = FUNCTION LENGTH(L-LST2(WS-J))
               ADD WS-LEN2 TO WS-COUNT2
           END-PERFORM.

       MOVE-LST1-TO-RESULT.
           MOVE SPACES TO RESULT
           MOVE L-LST1(1) TO RESULT(1:2)
           MOVE L-LST1(2) TO RESULT(3:2).

       MOVE-LST2-TO-RESULT.
           MOVE SPACES TO RESULT
           MOVE L-LST2(1) TO RESULT(1:2)
           MOVE L-LST2(2) TO RESULT(3:2).

       END PROGRAM TOTAL-MATCH.
