       IDENTIFICATION DIVISION.
       PROGRAM-ID. BELOW-ZERO.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 BALANCE PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-OPERATIONS OCCURS 8 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC 9.

      * You're given a list of deposit and withdrawal operations on a bank account that starts with
      * zero balance. Your task is to detect if at any point the balance of account fallls below zero, and
      * at that point function should return True. Otherwise it should return False.
      * >>> below_zero([1, 2, 3, 4, 5, 6, 7, 8])
      * False
      * >>> below_zero([1, 2, -4, 5, 6, 7, 8, 9])
      * True
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 8
               COMPUTE BALANCE = BALANCE + L-OPERATIONS(NI)
               IF BALANCE < 0
                   MOVE 1 TO RESULT
                   GOBACK
               END-IF
           END-PERFORM
           MOVE 0 TO RESULT
           GOBACK
           .
       END PROGRAM BELOW-ZERO.
