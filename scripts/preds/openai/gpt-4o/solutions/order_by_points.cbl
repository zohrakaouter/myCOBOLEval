       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-BY-POINTS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-NUMS.
           05 WS-NUMS-ARR OCCURS 5 TIMES PIC S9(10).

       01 WS-RESULT-CNT          PIC 9(3) VALUE 0.
       01 WS-I                   PIC 9(3).
       01 WS-J                   PIC 9(3).
       01 WS-TEMP-INDEX          PIC 9(3).
       01 WS-SWAP-FLAG           PIC X VALUE 'N'.

       01 WS-SUMS.
           05 WS-SUMS-ARR OCCURS 5 TIMES PIC 9(10).
       
       01 WS-TEMP-NUM            PIC S9(10).
       01 WS-TEMP-SUM            PIC 9(10).
       
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMS OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * 
      * Write a function which sorts the given list of integers
      * in ascending order according to the sum of their digits.
      * Note: if there are several items with similar sum of their digits,
      * order them based on their index in original list.
      * 
      * For example:
      * >>> order_by_points([1, 11, -1, -11, -12]) == [-1, -11, 1, -12, 11]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       BEGIN.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE L-NUMS (WS-I) TO WS-NUMS-ARR (WS-I)
               MOVE FUNCTION NUMVAL-C(FUNCTION REPLACE (FUNCTION STRING
                    WS-NUMS-ARR (WS-I) USING '+-' BY '')) TO WS-TEMP-SUM
               MOVE 0 TO WS-SUMS-ARR (WS-I)

               PERFORM VARYING WS-TEMP-INDEX FROM 1 BY 1
                                    UNTIL WS-TEMP-INDEX > FUNCTION LENGTH
                                    (FUNCTION NUMVAL-C (WS-NUMS-ARR (WS-I)))
                   COMPUTE WS-SUMS-ARR (WS-I) = WS-SUMS-ARR (WS-I)
                        + FUNCTION NUMVAL (FUNCTION SUBSTR
                        (FUNCTION NUMVAL-C (WS-NUMS-ARR (WS-I)), WS-TEMP-INDEX, 1))
               END-PERFORM
           END-PERFORM

           PERFORM UNTIL WS-SWAP-FLAG = 'N'
                MOVE 'N' TO WS-SWAP-FLAG
                PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
                    IF WS-SUMS-ARR (WS-J) > WS-SUMS-ARR (WS-J + 1)
                        MOVE WS-NUMS-ARR (WS-J) TO WS-TEMP-NUM
                        MOVE WS-SUMS-ARR (WS-J) TO WS-TEMP-SUM
                        MOVE WS-NUMS-ARR (WS-J + 1) TO WS-NUMS-ARR (WS-J)
                        MOVE WS-SUMS-ARR (WS-J + 1) TO WS-SUMS-ARR (WS-J)
                        MOVE WS-TEMP-NUM TO WS-NUMS-ARR (WS-J + 1)
                        MOVE WS-TEMP-SUM TO WS-SUMS-ARR (WS-J + 1)
                        MOVE 'Y' TO WS-SWAP-FLAG
                    END-IF
                END-PERFORM
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               ADD 1 TO WS-RESULT-CNT
               MOVE WS-NUMS-ARR (WS-I) TO RESULT (WS-RESULT-CNT)
           END-PERFORM

           GOBACK.

       END PROGRAM ORDER-BY-POINTS.
