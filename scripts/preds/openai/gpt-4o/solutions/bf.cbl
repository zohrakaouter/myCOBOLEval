       IDENTIFICATION DIVISION.
       PROGRAM-ID. BF.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 PLANET-NAMES.
           05 PLANET PIC X(100) OCCURS 8 TIMES.
               88 MERCURY VALUE 'Mercury'.
               88 VENUS VALUE 'Venus'.
               88 EARTH VALUE 'Earth'.
               88 MARS VALUE 'Mars'.
               88 JUPITER VALUE 'Jupiter'.
               88 SATURN VALUE 'Saturn'.
               88 URANUS VALUE 'Uranus'.
               88 NEPTUNE VALUE 'Neptune'.

       01 PLANET-INDEXES.
           05 PLANET1-IDX PIC 99 COMP-5.
           05 PLANET2-IDX PIC 99 COMP-5.

       01 LIMIT-INDEXES.
           05 START-IDX PIC 99 COMP-5.
           05 END-IDX PIC 99 COMP-5.
           05 TEMP-IDX PIC 99 COMP-5.

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

       0001-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-VALIDATE-PLANETS
           IF PLANET1-IDX > 0 AND PLANET2-IDX > 0
              PERFORM 3000-DETERMINE-PLANETS-BETWEEN
           END-IF
           GOBACK.

       1000-INITIALIZE.
           MOVE 'Mercury' TO PLANET(1)
           MOVE 'Venus' TO PLANET(2)
           MOVE 'Earth' TO PLANET(3)
           MOVE 'Mars' TO PLANET(4)
           MOVE 'Jupiter' TO PLANET(5)
           MOVE 'Saturn' TO PLANET(6)
           MOVE 'Uranus' TO PLANET(7)
           MOVE 'Neptune' TO PLANET(8).

       2000-VALIDATE-PLANETS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 8 OR PLANET1-IDX > 0
               IF PLANET(NI) = L-PLANET1 THEN
                   MOVE NI TO PLANET1-IDX
               END-IF
           END-PERFORM
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 8 OR PLANET2-IDX > 0
               IF PLANET(NI) = L-PLANET2 THEN
                   MOVE NI TO PLANET2-IDX
               END-IF
           END-PERFORM.

       3000-DETERMINE-PLANETS-BETWEEN.
           IF PLANET1-IDX < PLANET2-IDX THEN
               MOVE PLANET1-IDX TO START-IDX
               MOVE PLANET2-IDX TO END-IDX
           ELSE
               MOVE PLANET2-IDX TO START-IDX
               MOVE PLANET1-IDX TO END-IDX
           END-IF
           ADD 1 TO START-IDX
           SUBTRACT 1 FROM END-IDX
           IF END-IDX > START-IDX THEN
               PERFORM VARYING TEMP-IDX FROM START-IDX BY 1 UNTIL TEMP-IDX > END-IDX
                   MOVE PLANET(TEMP-IDX) TO RESULT(NI)
                   ADD 1 TO NI
               END-PERFORM
           END-IF.

       END PROGRAM BF.
