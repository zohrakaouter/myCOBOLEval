       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMALLEST-CHANGE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARS.
           05 WS-I PIC S9(10) VALUE 0.
           05 WS-J PIC S9(10) VALUE 0.
           05 WS-CHANGES PIC S9(10) VALUE 0.
           05 WS-ARR-LEN PIC S9(10) VALUE 7.
           05 WS-MIDDLE PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 7 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * Given an array arr of integers, find the minimum number of elements that
      * need to be changed to make the array palindromic. A palindromic array is an array that
      * is read the same backwards and forwards. In one change, you can change one element to any other element.
      * 
      * For example:
      * smallest_change([1, 2, 3, 4, 3, 2, 2]) == 1
      * smallest_change([1, 2, 3, 4, 3, 2, 1]) == 0
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           COMPUTE WS-MIDDLE = FUNCTION INTEGER(FUNCTION MOD(WS-ARR-LEN, 2)) + 1
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-MIDDLE
               COMPUTE WS-J = WS-ARR-LEN - WS-I + 1
               IF L-ARR(WS-I) NOT EQUAL TO L-ARR(WS-J)
                   ADD 1 TO WS-CHANGES
               END-IF
           END-PERFORM
           MOVE WS-CHANGES TO RESULT
           GOBACK.
       END PROGRAM SMALLEST-CHANGE.
