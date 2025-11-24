       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHOOSE-NUM.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARS.
           05 WS-MAX-EVEN PIC S9(10) VALUE -1.
           05 WS-TEMP PIC S9(10).
           05 WS-I PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-X PIC S9(10).
           05 L-Y PIC S9(10).
           05 RESULT PIC S9(10).

      * This function takes two positive numbers x and y and returns the
      * biggest even integer number that is in the range [x, y] inclusive. If
      * there's no such number, then the function should return -1.
      * 
      * For example:
      * choose_num(12, 15) = 14
      * choose_num(13, 12) = -1
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           IF L-X > L-Y
               MOVE -1 TO RESULT
               GOBACK
           END-IF

           MOVE L-Y TO WS-TEMP
           PERFORM VARYING WS-I FROM L-X BY 1 UNTIL WS-I > WS-TEMP
               IF FUNCTION MOD(WS-I, 2) = 0
                   MOVE WS-I TO WS-MAX-EVEN
               END-IF
           END-PERFORM

           MOVE WS-MAX-EVEN TO RESULT
           GOBACK
           .

       END PROGRAM CHOOSE-NUM.
