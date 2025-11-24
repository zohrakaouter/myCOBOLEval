       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-MUSIC.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-COUNTERS.
           05 WS-INDEX PIC 9(3) VALUE 0.
           05 WS-RESULT-INDEX PIC 9(3) VALUE 0.
           05 WS-LENGTH PIC 9(3).
           05 WS-I PIC 9(3).

       01 WS-NOTE-TYPE.
           05 WS-NOTE PIC X(2).

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

           COMPUTE WS-LENGTH = FUNCTION LENGTH(L-MUSIC-STRING)

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LENGTH
               MOVE SPACES TO WS-NOTE
               UNSTRING L-MUSIC-STRING DELIMITED BY SPACE
                   INTO WS-NOTE
                   WITH POINTER WS-INDEX
                   ON OVERFLOW CONTINUE
               END-UNSTRING

               IF WS-NOTE(1:1) = 'o' AND WS-NOTE(2:1) = SPACE OR LOW-VALUE
                   ADD 1 TO WS-RESULT-INDEX
                   MOVE 4 TO RESULT(WS-RESULT-INDEX)
               ELSE IF WS-NOTE(1:2) = 'o|'
                   ADD 1 TO WS-RESULT-INDEX
                   MOVE 2 TO RESULT(WS-RESULT-INDEX)
               ELSE IF WS-NOTE(1:2) = '.|'
                   ADD 1 TO WS-RESULT-INDEX
                   MOVE 1 TO RESULT(WS-RESULT-INDEX)
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM PARSE-MUSIC.
