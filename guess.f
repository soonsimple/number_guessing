C     ******************************************************************
C
C     GUESS
C
C     ORIGINALLY WRITTEN BY WALT KOETKE IN FOCAL, AND PORTED TO BASIC BY
C     DAVID H. AHL. CONVERTED TO FORTRAN BY PHILIPP ENGEL.
C      
C     Here, I converted it to Modern Fortran free style.
C     soonsimple, 4-24-2022
C
C     ******************************************************************
      PROGRAM GUESS
      INTEGER INPUT
      INTEGER IATTEM, IGUESS, IMAX, ISECRE, LIMIT

      CALL SRAND(TIME())

      PRINT 100
      LIMIT = INPUT()
      IMAX = INT(LOG(REAL(LIMIT)) / LOG(2.0)) + 1

      PRINT 200, LIMIT
      ISECRE = 1 + INT(RAND(0) * LIMIT)
      IGUESS = 0
      IATTEM = 0

   10 CONTINUE
      IATTEM = IATTEM + 1
      IGUESS = INPUT()
      IF (IGUESS .LT. ISECRE) PRINT 300
      IF (IGUESS .GT. ISECRE) PRINT 400
      IF (IGUESS .NE. ISECRE) GOTO 10

      PRINT 500, IATTEM

      IF (IATTEM .LT. IMAX) THEN
        PRINT 600
      ELSE IF (IATTEM .EQ. IMAX) THEN
        PRINT 700
      ELSE
        PRINT 800, IMAX
      END IF

  100 FORMAT (' THIS IS A NUMBER GUESSING GAME. I WILL THINK',/,
     &' OF A NUMBER BETWEEN 1 AND ANY LIMIT YOU WANT.',/,
     &' THEN YOU HAVE TO GUESS WHAT IT IS.',/,/,
     &' WHAT LIMIT DO YOU WANT? ',$)
  200 FORMAT (' I AM THINKING OF A NUMBER BETWEEN 1 AND ',I8,' ...',/,
     &' NOW YOU TRY TO GUESS WHAT IT IS: ',$)
  300 FORMAT (' TOO LOW. TRY A BIGGER ANSWER: ',$)
  400 FORMAT (' TOO HIGH. TRY A SMALLER ANSWER: ',$)
  500 FORMAT (' THAT''S IT! YOU GOT IT IN ',I2,' TRIES.')
  600 FORMAT (' VERY GOOD.')
  700 FORMAT (' GOOD.')
  800 FORMAT (' YOU SHOULD HAVE BEEN ABLE TO GET IT IN ONLY ',I3,'.')
      END
C     ******************************************************************
      INTEGER FUNCTION INPUT()
C
C     RETURNS A POSITIVE INTEGER GREATER THAN 1 FROM USER INPUT.
C
      INTEGER ISTAT

   10 CONTINUE
      READ (*, 100, IOSTAT=ISTAT) INPUT
  100 FORMAT (I8)
      IF (ISTAT .NE. 0 .OR. INPUT .LE. 1) THEN
        PRINT 200
        GOTO 10
      END IF
  200 FORMAT (' INVALID INPUT. TRY AGAIN:')
      END
