       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHOOSE-NUM.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-TEMP-X PIC S9(10).
       01 WS-TEMP-Y PIC S9(10).
       01 WS-BIGGEST-EVEN PIC S9(10) VALUE -1.
       01 I PIC S9(10).

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
           
           IF L-X > L-Y THEN
               MOVE L-X TO WS-TEMP-X
               MOVE L-Y TO WS-TEMP-Y
           ELSE
               MOVE L-Y TO WS-TEMP-X
               MOVE L-X TO WS-TEMP-Y
           END-IF

           PERFORM VARYING I FROM WS-TEMP-X BY -1 UNTIL I < WS-TEMP-Y
               IF I MOD 2 = 0 THEN
                   MOVE I TO WS-BIGGEST-EVEN
                   EXIT PERFORM
               END-IF
           END-PERFORM

           MOVE WS-BIGGEST-EVEN TO RESULT

           GOBACK.

       END PROGRAM CHOOSE-NUM.
