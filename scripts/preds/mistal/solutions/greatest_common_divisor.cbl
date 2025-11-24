       IDENTIFICATION DIVISION.
       PROGRAM-ID. GREATEST-COMMON-DIVISOR.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-A PIC S9(10).
       01 WS-B PIC S9(10).
       01 WS-TEMP PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC S9(10).
           05 L-B PIC S9(10).
           05 RESULT PIC S9(10).

      * Return a greatest common divisor of two integers a and b
      * >>> greatest_common_divisor(3, 5)
      * 1
      * >>> greatest_common_divisor(25, 15)
      * 5
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-A TO WS-A
           MOVE L-B TO WS-B

           PERFORM UNTIL WS-B = 0
               COMPUTE WS-TEMP = WS-B
               COMPUTE WS-B = FUNCTION MOD(WS-A, WS-B)
               MOVE WS-TEMP TO WS-A
           END-PERFORM

           MOVE WS-A TO RESULT

           GOBACK.
       END PROGRAM GREATEST-COMMON-DIVISOR.
