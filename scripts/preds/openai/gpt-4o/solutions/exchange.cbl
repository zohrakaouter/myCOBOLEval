       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXCHANGE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-LST1.
           05 WS-ELEM-LST1 OCCURS 4 TIMES PIC 99.
       01 WS-LST2.
           05 WS-ELEM-LST2 OCCURS 4 TIMES PIC 99.

       01 WS-POS1 PIC 9(2) VALUE 1.
       01 WS-POS2 PIC 9(2) VALUE 1.

       01 FOUND-EVEN-INDICATOR PIC 9 VALUE 0.

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

       INITIALIZE PROCEDURE.
           PERFORM PREPARE-WS-LISTS.

       CHECK-EVEN-POSSIBILITY.
           MOVE 1 TO WS-POS1
           PERFORM VARYING WS-POS1 FROM 1 BY 1 UNTIL WS-POS1 > 4
               IF WS-ELEM-LST1(WS-POS1) MOD 2 <> 0
                   MOVE 1 TO FOUND-EVEN-INDICATOR
                   PERFORM VARYING WS-POS2 FROM 1 BY 1 UNTIL WS-POS2 > 4 OR FOUND-EVEN-INDICATOR = 0
                       IF WS-ELEM-LST2(WS-POS2) MOD 2 = 0
                           MOVE WS-ELEM-LST2(WS-POS2) TO WS-ELEM-LST1(WS-POS1)
                           MOVE 0 TO FOUND-EVEN-INDICATOR
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

       SET-RESULT.
           IF FOUND-EVEN-INDICATOR = 0
               MOVE "YES" TO RESULT
           ELSE
               MOVE "NO" TO RESULT
           END-IF

       PREPARE-WS-LISTS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
               MOVE L-LST1(NI) TO WS-ELEM-LST1(NI)
               MOVE L-LST2(NI) TO WS-ELEM-LST2(NI)
           END-PERFORM

       END-PROGRAM.
           PERFORM INITIALIZE PROCEDURE
           PERFORM CHECK-EVEN-POSSIBILITY
           PERFORM SET-RESULT
           GOBACK.
