       IDENTIFICATION DIVISION.
       PROGRAM-ID. ISCUBE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01  WS-VARIABLES.
           05 WS-NUMBER PIC S9(10).
           05 WS-CUBE-ROOT PIC S9(10) COMP.
           05 WS-CUBE PIC S9(10) COMP.
           05 WS-REM PIC S9(10) COMP.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC S9(10).
           05 RESULT PIC 9.

      * '''
      * Write a function that takes an integer a and returns True
      * if this ingeger is a cube of some integer number.
      * Note: you may assume the input is always valid.
      * Examples:
      * iscube(1) ==> True
      * iscube(2) ==> False
      * iscube(-1) ==> True
      * iscube(64) ==> True
      * iscube(0) ==> True
      * iscube(180) ==> False
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-A TO WS-NUMBER.

           IF WS-NUMBER = 0 THEN
               MOVE 1 TO RESULT
               GOBACK
           END-IF.

           COMPUTE WS-CUBE-ROOT = FUNCTION CUBEROOT(WS-NUMBER).
           COMPUTE WS-CUBE = WS-CUBE-ROOT * WS-CUBE-ROOT * WS-CUBE-ROOT.

           IF WS-CUBE = WS-NUMBER THEN
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF.

           GOBACK.
       END PROGRAM ISCUBE.
