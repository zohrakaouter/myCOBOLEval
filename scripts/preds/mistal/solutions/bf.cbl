       IDENTIFICATION DIVISION.
       PROGRAM-ID. BF.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 PLANETS.
           05 FILLER PIC X(10) VALUE "Mercury".
           05 FILLER PIC X(10) VALUE "Venus".
           05 FILLER PIC X(10) VALUE "Earth".
           05 FILLER PIC X(10) VALUE "Mars".
           05 FILLER PIC X(10) VALUE "Jupiter".
           05 FILLER PIC X(10) VALUE "Saturn".
           05 FILLER PIC X(10) VALUE "Uranus".
           05 FILLER PIC X(10) VALUE "Neptune".
       01 PLANET-REDEFINES REDEFINES PLANETS.
           05 PLANET OCCURS 8 TIMES PIC X(10).
       01 I PIC 99.
       01 J PIC 99.
       01 PLANET1-IND PIC 99 VALUE 0.
       01 PLANET2-IND PIC 99 VALUE 0.
       01 TEMP-IND PIC 99.
       01 FOUND-FLAG PIC X VALUE 'N'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-PLANET1 PIC X(100).
           05 L-PLANET2 PIC X(100).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC X(100).

      * '''
      * There are eight planets in our solar system: the closerst to the Sun
      * is Mercury, the next one is Venus, then Earth, Mars, Jupiter, Saturn,
      * Uranus, Neptune.
      * Write a function that takes two planet names as strings planet1 and planet2.
      * The function should return a tuple containing all planets whose orbits are
      * located between the orbit of planet1 and the orbit of planet2, sorted by
      * the proximity to the sun.
      * The function should return an empty tuple if planet1 or planet2
      * are not correct planet names.
      * Examples
      * bf("Jupiter", "Neptune") ==> ("Saturn", "Uranus")
      * bf("Earth", "Mercury") ==> ("Venus")
      * bf("Mercury", "Uranus") ==> ("Venus", "Earth", "Mars", "Jupiter", "Saturn")
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
               IF L-PLANET1 = PLANET(I)
                   MOVE I TO PLANET1-IND
               END-IF
               IF L-PLANET2 = PLANET(I)
                   MOVE I TO PLANET2-IND
               END-IF
           END-PERFORM

           IF PLANET1-IND = 0 OR PLANET2-IND = 0
               MOVE SPACES TO RESULT
               GOBACK
           END-IF

           IF PLANET1-IND > PLANET2-IND
               MOVE PLANET2-IND TO TEMP-IND
               MOVE PLANET1-IND TO PLANET2-IND
               MOVE TEMP-IND TO PLANET1-IND
           END-IF

           SET NI TO 1
           PERFORM VARYING I FROM PLANET1-IND + 1 BY 1
                   UNTIL I >= PLANET2-IND
               MOVE PLANET(I) TO RESULT(NI)
               SET NI UP BY 1
           END-PERFORM

           GOBACK.
       END PROGRAM BF.
