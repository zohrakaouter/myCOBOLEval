       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-NUMBERS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-WORK-AREAS.
           05 WS-NUMBER-TABLE.
               10 WS-NUMBER-ENTRY OCCURS 10 TIMES.
                   15 WS-NUMBER-NAME PIC X(6).
                   15 WS-NUMBER-VALUE PIC 9(2).
           05 WS-INPUT-TABLE.
               10 WS-INPUT-ENTRY OCCURS 20 TIMES.
                   15 WS-INPUT-NAME PIC X(6).
                   15 WS-INPUT-VALUE PIC 9(2).
           05 WS-TEMP-ENTRY.
               10 WS-TEMP-NAME PIC X(6).
               10 WS-TEMP-VALUE PIC 9(2).
           05 WS-COUNT PIC 9(2) VALUE 0.
           05 WS-I PIC 9(2).
           05 WS-J PIC 9(2).
           05 WS-K PIC 9(2).
           05 WS-TEMP-LEN PIC 9(3).
           05 WS-TEMP-STR PIC X(6).
           05 WS-SPACE-COUNT PIC 9(3) VALUE 0.
           05 WS-START-POS PIC 9(3) VALUE 1.
           05 WS-END-POS PIC 9(3) VALUE 1.

       01 WS-NUMBER-DEFINITIONS.
           05 FILLER PIC X(6) VALUE 'zero'.
           05 FILLER PIC X(6) VALUE 'one'.
           05 FILLER PIC X(6) VALUE 'two'.
           05 FILLER PIC X(6) VALUE 'three'.
           05 FILLER PIC X(6) VALUE 'four'.
           05 FILLER PIC X(6) VALUE 'five'.
           05 FILLER PIC X(6) VALUE 'six'.
           05 FILLER PIC X(6) VALUE 'seven'.
           05 FILLER PIC X(6) VALUE 'eight'.
           05 FILLER PIC X(6) VALUE 'nine'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS PIC X(100).
           05 RESULT PIC X(100).

      * Input is a space-delimited string of numberals from 'zero' to 'nine'.
      * Valid choices are 'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight' and 'nine'.
      * Return the string with numbers sorted from smallest to largest
      * >>> sort_numbers('three one five')
      * 'one three five'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PARAGRAPH.
           PERFORM INITIALIZE-NUMBER-TABLE
           PERFORM PARSE-INPUT-STRING
           PERFORM SORT-NUMBERS
           PERFORM BUILD-RESULT-STRING
           GOBACK.

       INITIALIZE-NUMBER-TABLE.
           MOVE 0 TO WS-I
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               MOVE WS-NUMBER-DEFINITIONS(WS-I) TO WS-NUMBER-NAME(WS-I)
               COMPUTE WS-NUMBER-VALUE(WS-I) = WS-I - 1
           END-PERFORM.

       PARSE-INPUT-STRING.
           MOVE 0 TO WS-COUNT
           MOVE 1 TO WS-START-POS
           MOVE 0 TO WS-SPACE-COUNT

           INSPECT L-NUMBERS TALLYING WS-SPACE-COUNT FOR ALL SPACES

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > FUNCTION LENGTH(L-NUMBERS)
               IF L-NUMBERS(WS-I:1) = SPACE OR WS-I = FUNCTION LENGTH(L-NUMBERS)
                   IF WS-I > WS-START-POS
                       ADD 1 TO WS-COUNT
                       COMPUTE WS-END-POS = WS-I - 1
                       IF WS-I = FUNCTION LENGTH(L-NUMBERS) AND L-NUMBERS(WS-I:1) <> SPACE
                           COMPUTE WS-END-POS = WS-I
                       END-IF
                       COMPUTE WS-TEMP-LEN = WS-END-POS - WS-START-POS + 1
                       MOVE L-NUMBERS(WS-START-POS:WS-TEMP-LEN) TO WS-INPUT-NAME(WS-COUNT)

                       PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 10
                           IF WS-INPUT-NAME(WS-COUNT) = WS-NUMBER-NAME(WS-J)
                               MOVE WS-NUMBER-VALUE(WS-J) TO WS-INPUT-VALUE(WS-COUNT)
                               EXIT PERFORM
                           END-IF
                       END-PERFORM
                   END-IF
                   COMPUTE WS-START-POS = WS-I + 1
               END-IF
           END-PERFORM.

       SORT-NUMBERS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= WS-COUNT
               PERFORM VARYING WS-J FROM WS-I + 1 BY 1 UNTIL WS-J > WS-COUNT
                   IF WS-INPUT-VALUE(WS-I) > WS-INPUT-VALUE(WS-J)
                       MOVE WS-INPUT-NAME(WS-I) TO WS-TEMP-NAME
                       MOVE WS-INPUT-VALUE(WS-I) TO WS-TEMP-VALUE
                       MOVE WS-INPUT-NAME(WS-J) TO WS-INPUT-NAME(WS-I)
                       MOVE WS-INPUT-VALUE(WS-J) TO WS-INPUT-VALUE(WS-I)
                       MOVE WS-TEMP-NAME TO WS-INPUT-NAME(WS-J)
                       MOVE WS-TEMP-VALUE TO WS-INPUT-VALUE(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       BUILD-RESULT-STRING.
           MOVE SPACES TO RESULT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT
               IF WS-I > 1
                   STRING RESULT DELIMITED BY SIZE
                          ' ' DELIMITED BY SIZE
                          WS-INPUT-NAME(WS-I) DELIMITED BY SPACE
                   INTO RESULT
               ELSE
                   MOVE WS-INPUT-NAME(WS-I) TO RESULT
               END-IF
           END-PERFORM.
