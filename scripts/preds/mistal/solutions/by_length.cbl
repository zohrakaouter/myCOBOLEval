       IDENTIFICATION DIVISION.
       PROGRAM-ID. BY-LENGTH.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-VARIABLES.
           05 WS-I PIC S9(10) VALUE 0.
           05 WS-J PIC S9(10) VALUE 0.
           05 WS-TEMP PIC S9(10) VALUE 0.
           05 WS-COUNT PIC S9(10) VALUE 0.
           05 WS-NUMBER-NAMES.
               10 WS-NUMBER-NAME OCCURS 9 TIMES PIC X(10) VALUE
                   "One" "Two" "Three" "Four" "Five"
                   "Six" "Seven" "Eight" "Nine".

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 8 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC X(100).

      * 
      * Given an array of integers, sort the integers that are between 1 and 9 inclusive,
      * reverse the resulting array, and then replace each digit by its corresponding name from
      * "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine".
      * 
      * For example:
      * arr = [2, 1, 1, 4, 5, 8, 2, 3]
      * -> sort arr -> [1, 1, 2, 2, 3, 4, 5, 8]
      * -> reverse arr -> [8, 5, 4, 3, 2, 2, 1, 1]
      * return ["Eight", "Five", "Four", "Three", "Two", "Two", "One", "One"]
      * 
      * 
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           *> First, count how many elements are between 1 and 9
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               IF L-ARR(WS-I) >= 1 AND L-ARR(WS-I) <= 9
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM

           *> Sort the array (using bubble sort)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 7
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8 - WS-I
                   IF L-ARR(WS-J) > L-ARR(WS-J + 1)
                       MOVE L-ARR(WS-J) TO WS-TEMP
                       MOVE L-ARR(WS-J + 1) TO L-ARR(WS-J)
                       MOVE WS-TEMP TO L-ARR(WS-J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM

           *> Reverse the array
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE L-ARR(WS-I) TO WS-TEMP
               MOVE L-ARR(9 - WS-I) TO L-ARR(WS-I)
               MOVE WS-TEMP TO L-ARR(9 - WS-I)
           END-PERFORM

           *> Convert numbers to their names
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               IF L-ARR(WS-I) >= 1 AND L-ARR(WS-I) <= 9
                   MOVE WS-NUMBER-NAME(L-ARR(WS-I)) TO RESULT(WS-I)
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM BY-LENGTH.
