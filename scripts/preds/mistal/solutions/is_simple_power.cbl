       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-SIMPLE-POWER.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-POWER      PIC S9(10) VALUE 0.
           05 WS-TEMP       PIC S9(10) VALUE 1.
           05 WS-REM        PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-X PIC S9(10).
           05 L-N PIC S9(10).
           05 RESULT PIC 9.

      * Your task is to write a function that returns true if a number x is a simple
      * power of n and false in other cases.
      * x is a simple power of n if n**int=x
      * For example:
      * is_simple_power(1, 4) => true
      * is_simple_power(2, 2) => true
      * is_simple_power(8, 2) => true
      * is_simple_power(3, 2) => false
      * is_simple_power(3, 1) => false
      * is_simple_power(5, 3) => false
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       MAIN-PARAGRAPH.
           IF L-X = 1
               MOVE 1 TO RESULT
               GOBACK
           END-IF

           IF L-N = 0 OR L-N = 1 OR L-N = -1
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           PERFORM VARYING WS-POWER FROM 1 BY 1
                   UNTIL WS-TEMP > L-X OR WS-TEMP < 0
               COMPUTE WS-TEMP = FUNCTION POWER(L-N, WS-POWER)
               IF WS-TEMP = L-X
                   MOVE 1 TO RESULT
                   GOBACK
               END-IF
           END-PERFORM

           MOVE 0 TO RESULT
           GOBACK.
       END PROGRAM IS-SIMPLE-POWER.
