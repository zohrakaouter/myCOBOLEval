       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXCHANGE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-ODD-COUNT PIC 9(3) VALUE 0.
           05 WS-EVEN-COUNT-IN-LST2 PIC 9(3) VALUE 0.
           05 I PIC 9(3).
           05 J PIC 9(3).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST1 OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 L-LST2 OCCURS 4 TIMES INDEXED BY NJ PIC S9(10).
           05 RESULT PIC X(100).

      * In this problem, you will implement a function that takes two lists of numbers,
      * and determines whether it is possible to perform an exchange of elements
      * between them to make lst1 a list of only even numbers.
      * There is no limit on the number of exchanged elements between lst1 and lst2.
      * If it is possible to exchange elements between the lst1 and lst2 to make
      * all the elements of lst1 to be even, return "YES".
      * Otherwise, return "NO".
      * For example:
      * exchange([1, 2, 3, 4], [1, 2, 3, 4]) => "YES"
      * exchange([1, 2, 3, 4], [1, 5, 3, 4]) => "NO"
      * It is assumed that the input lists will be non-empty.
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               IF FUNCTION MOD(L-LST1(I), 2) NOT EQUAL TO 0
                   ADD 1 TO WS-ODD-COUNT
               END-IF
           END-PERFORM

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
               IF FUNCTION MOD(L-LST2(J), 2) EQUAL TO 0
                   ADD 1 TO WS-EVEN-COUNT-IN-LST2
               END-IF
           END-PERFORM

           IF WS-ODD-COUNT <= WS-EVEN-COUNT-IN-LST2
               MOVE "YES" TO RESULT
           ELSE
               MOVE "NO" TO RESULT
           END-IF

           GOBACK.
       END PROGRAM EXCHANGE.
