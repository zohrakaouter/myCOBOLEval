       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-MUSIC.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 WS-INPUT-LENGTH        PIC S9(10) COMP-3.
       77 WS-I                   PIC S9(10) COMP-3 VALUE ZERO.
       77 WS-J                   PIC S9(10) COMP-3 VALUE ZERO.
       77 WS-K                   PIC S9(10) COMP-3 VALUE ONE.
       77 WS-TEMP-NOTE           PIC X(3).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-MUSIC-STRING PIC X(100).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * Input to this function is a string representing musical notes in a special ASCII format.
      * Your task is to parse this string and return list of integers corresponding to how many beats does each
      * not last.
      * 
      * Here is a legend:
      * 'o' - whole note, lasts four beats
      * 'o|' - half note, lasts two beats
      * '.|' - quater note, lasts one beat
      * 
      * >>> parse_music('o o| .| o| o| .| .| .| .| o o')
      * [4, 2, 1, 2, 2, 1, 1, 1, 1, 4, 4]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCEDURE.
           PERFORM INITIALIZE-VARS
           PERFORM PARSE-MUSIC-STRING
           GOBACK.

       INITIALIZE-VARS.
           UNSTRING L-MUSIC-STRING DELIMITED BY SPACE INTO WS-TEMP-NOTE
                WITH POINTER WS-I
           END-UNSTRING
           EVALUATE TRUE
               WHEN WS-I NOT = 0
                   COMPUTE WS-INPUT-LENGTH = WS-I
           END-EVALUATE
           SET WS-I TO ZERO
           SET WS-J TO ZERO
           SET NI TO 1.

       PARSE-MUSIC-STRING.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-INPUT-LENGTH
               UNSTRING L-MUSIC-STRING DELIMITED BY SPACE INTO WS-TEMP-NOTE
                   WITH POINTER WS-I
               END-UNSTRING

               IF WS-TEMP-NOTE = 'o'
                   MOVE 4 TO RESULT(NI)
                   SET NI UP BY 1
               ELSE IF WS-TEMP-NOTE = 'o|'
                   MOVE 2 TO RESULT(NI)
                   SET NI UP BY 1
               ELSE IF WS-TEMP-NOTE = '.|'
                   MOVE 1 TO RESULT(NI)
                   SET NI UP BY 1
               END-IF
           END-PERFORM.

       END PROGRAM PARSE-MUSIC.
