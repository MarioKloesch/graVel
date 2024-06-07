C     graVel± Program to derive the unsteady virtual velocity functions of bedload and decelerating bedload tracers from bedload tracer data
C     Copyright (C) 2024 Mario Kloesch and Helmut Habersack

C     This program is free software: you can redistribute it and/or modify
C     it under the terms of the GNU General Public License as published by
C     the Free Software Foundation, either version 3 of the License, or
C     (at your option) any later version.

C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.

C     You should have received a copy of the GNU General Public License
C     along with this program.  If not, see <http://www.gnu.org/licenses/>.

      
      PROGRAM graVel_plusminus
      
C     --------------------------declarations---------------------------
      IMPLICIT NONE
      INTEGER COUNT,COUNT2,EOF,I,J,M,N,MMAX,NMAX,STEPNR,Y
      LOGICAL EX
      PARAMETER (MMAX=10000000)
      PARAMETER (NMAX=100000)
      DOUBLE PRECISION A,AO,B,BAO,BO,BMIN,BMAX,BRANGE(NMAX,5),BSTART,
     *  BSTRT0,BSTEP0,BSTEP,D,EXP,INTINS,INTEGR(NMAX),PLUMIN,R2,R2OLD,
     *  R2BOLD,SCALC(NMAX),SHEAR(MMAX,2),SHICBO,SHICRI,SHICR0,SHICRO,
     *  SHIELD(MMAX,2),STEP,SHIMIN,SHIMAX,SUMX2,SUMXY,SUMY2,
     *  TIMESP(NMAX,4)
      CHARACTER CEL*5,FNAME*40,FNAME2*40,T,S

C     --------------------------formatting-----------------------------
   36 FORMAT (A51,A21)
   37 FORMAT (A44,A22)
   38 FORMAT (A30,A56,A45)
   40 FORMAT (A55,A4)
   35 FORMAT (A43,A16,F6.4,A1)
     
C     -------------------------start screen----------------------------
      PRINT*,' '
      PRINT*,'                                                        '
      PRINT*,'                                                        '
      PRINT*,'                        graVel+-                        '
      PRINT*,'                                                        '
      PRINT*,'       Mario Kloesch and Helmut Habersack (2024)        '
      PRINT*,'                                                        '
      PRINT*,'        _________________     _________________         '
      PRINT*,'       |                 |   |                 |        '
      PRINT*,'       |~ ~ ~ ~ ~ ~ ~ ~ ~|   |~ ~ ~ ~ ~ ~ ~ ~ ~|        '
      PRINT*,'       |                 |   |                 |        '
      PRINT*,'       |                 |   |                 |        '
      PRINT*,'       |      - 0        |   |      - O        |        '
      PRINT*,'       |oOoOooOoOooOoOooO|   |oOoOooOoOooOoOooO|        '
      PRINT*,'       | o O O o O o O o |   | o O 0 o O o O o |        '
      PRINT*,'       |_________________|   |_________________|        '
      PRINT*,'                                                        '
      PRINT*,'                                                        '
      PRINT*,'                    BOKU University                     '
      PRINT*,'University of Natural Resources and Life Sciences, Vienna
     *'
      PRINT*,' Institute of Hydraulic Engineering and River Research  '
      PRINT*,'         Am Brigittenauer Sporn 3, 1120 Vienna          '
      PRINT*,'            Email: mario.kloesch@boku.ac.at             '
      PRINT*,'               Tel:   +43 1 47654-81913                 '
      PRINT*,'                                                        '
      PRINT*,'                                                        '
      PRINT*,'                                                        '
      PRINT*,'                                                        '
      PRINT*, 'Press RETURN to continue'
      READ*
      PRINT*,'                                                        '
      PRINT*,'                                                        '
      PRINT*,'                                                        '
      PRINT*,'graVel+-                                                '
      PRINT*,'Program to derive the unsteady virtual velocity functions
     *'
      PRINT*,'of bedload and decelerating bedload tracers from bedload'
      PRINT*,'tracer data                                             '
      PRINT*,'                                                        '
      PRINT*,'Copyright (C) 2024 Mario Kloesch and Helmut Habersack   '
      PRINT*,'                                                        '
      PRINT*,'This program is free software: you can redistribute it  '
      PRINT*,'and/or modify it under the terms of the GNU General     '
      PRINT*,'Public License as published by the Free Software        '
      PRINT*,'Foundation, either version 3 of the License, or (at your'
      PRINT*,'option) any later version.                              '
      PRINT*,'                                                        '
      PRINT*,'This program is distributed in the hope that it will be '
      PRINT*,'useful, but WITHOUT ANY WARRANTY; without even the      '
      PRINT*,'implied warranty of MERCHANTABILITY or FITNESS FOR A    '
      PRINT*,'PARTICULAR PURPOSE.  See the GNU General Public License '
      PRINT*,'for more details.                                       '
      PRINT*,'                                                        '
      PRINT*,'You should have received a copy of the GNU General      '
      PRINT*,'Public License along with this program.  If not, see    '
      PRINT*,'<http://www.gnu.org/licenses/>.                         '
      PRINT*,'                                                        '
      PRINT*,'                                                        '
      PRINT*,'                                                        '
      PRINT*,'                                                        '
      PRINT*,'Press RETURN to continue                                '
      READ*
      PRINT*,' '

C     ------------------------read input data--------------------------

      PRINT 40, ' 1. Enter grain size of investigated tracer size class
     * ','in m'      
      READ*,D
      PRINT*,' '
      PRINT 36, ' 2. Enter name of file containing a time series of ',
     *'shear stress in N m-2'
   66 CONTINUE
      READ*, FNAME
      INQUIRE(FILE= FNAME, EXIST=EX)
      IF (EX.EQV..FALSE.) THEN 
        PRINT*, 'File does not exist. Please type in filename again.'
        GO TO 66
      END IF
      OPEN (30, file=FNAME)
      I=0
      M=0
      DO
       I=I+1
       READ(30,*,IOSTAT=EOF) (SHEAR(I,J), J=1,2)
       IF (EOF.GT.0) THEN
         PRINT*, 'Check input data. Something is wrong.'
       ELSE IF (EOF.LT.0) THEN
         EXIT
       ELSE
         M=M+1
       END IF
      END DO
      CLOSE (30)
      PRINT*,' '

      PRINT 37, ' 3. Enter name of file containing time spans',
     * ' and transport lengths'
   77 CONTINUE
      READ*, FNAME2
      INQUIRE(FILE= FNAME2, EXIST=EX)
      IF (EX.EQV..FALSE.) THEN 
        PRINT*, 'File does not exist. Please type in filename again.'
        GOTO 77
      END IF
      OPEN (31, file=FNAME2)
      I=0
      N=0
      DO
        I=I+1
        READ(31,*,IOSTAT=EOF) (TIMESP(I,J), J=1,4)
        IF (EOF.GT.0) THEN
          PRINT*, 'Check input data. Something is wrong.'
        ELSE IF (EOF.LT.0) THEN
          EXIT
        ELSE
          N=N+1
        END IF
      END DO
      CLOSE (31)
      PRINT*,' '

C     -----------------------data preparation--------------------------

      DO I=1,M
        SHIELD(I,1)=SHEAR(I,1)
        SHIELD(I,2)=SHEAR(I,2)/(1650*9.81*D)
      END DO

      SHIMIN=SHIELD(1,2)
      SHIMAX=SHIELD(1,2)
      
      DO I=1,M
        IF (SHIELD(I,2).GT.SHIMAX) THEN
          SHIMAX=SHIELD(I,2)
        END IF
        IF (SHIELD(I,2).LT.SHIMIN) THEN
          SHIMIN=SHIELD(I,2)
        END IF
      END DO

C     ---------------------read regression settings---------------------

      PRINT*, '4. Select formula exponent EXP in the formula ',
     *'Vu,s={a*[tau*/(1+bs)-tauc*)]}^EXP'
      PRINT*, 'Suggestion: EXP=1.5'
      READ*,EXP
      PRINT*,' '
      PRINT*, '5. Please select:'
      PRINT*, 'Do you want to check for deceleration or acceleration?'
      PRINT 38, ' The celeration coefficient is
     * ',' positive at deceleration and negative at acceleration, 
     * ','and will approach 0 with the wrong selection.'
      PRINT*, 'Please type "decel" or "accel":'
      READ*,CEL      
      IF (CEL.EQ.'decel') THEN
        PLUMIN=1.
      ELSE IF (CEL.EQ.'accel') THEN
        PLUMIN=-1.
      END IF
      PRINT*,' '
      PRINT *, '6. Please select:'
      PRINT *, 'a.....automatic assessment of celeration coefficient b'
      PRINT *, 'b.....analysis of a defined range of a*b values'
      PRINT *, 'Please type a or b:'
      READ*,T
      PRINT*,' '

      PRINT *, '7. Enter value of critical Shields stress for starting '
     *,'inner regression loop (suggestion: 0.0000, test also higher ',
     *'start values):'
      PRINT 35, ' (Note: The maximum Shields stress in your ',
     *'time series is: ', SHIMAX,')'
      READ*, SHICR0
      PRINT*,' '
      PRINT *, '8. Please select:'
      PRINT *, 'a) The selected critical Shields value should be used '
     *,'as a start value in every inner loop (this may help to avoid '
     *,'running into secondary maxima of raw R2)'
      PRINT *, 'b) The selected critical Shields value should be used '
     *,'as a start value only in the first loop and then continue ',
     *'with the last result as a start value (this is faster)'
      PRINT *, 'Please type a or b:'
      READ*,S
      PRINT*,' '
      IF (T.EQ.'a') THEN
        IF (CEL.EQ.'decel') THEN
        PRINT '(A27,A35)', ' 9. Select start value for ',
     *    'a*b in 1/s (suggestion: 0.0000001):'
        ELSE IF (CEL.EQ.'accel') THEN
        PRINT '(A27,A36)', '9.  Select start value for ',
     *    'a*b in 1/s (suggestion: -0.0000001):'
        END IF
          READ*,BSTRT0
          BSTART=ABS(BSTRT0)
          BSTART=LOG10(BSTART)
      PRINT*,' '
        PRINT '(A35,A22)', ' 10. Select log10 search increment for ',
     *    'a*b (suggestion: 0.1):'
          READ*,BSTEP0
      ELSE IF (T.EQ.'b') THEN
        IF (CEL.EQ.'decel') THEN
          PRINT *, '9. Select minimum value for log(a*b)',
     *     ' (suggestion: -10):'
        ELSE IF (CEL.EQ.'accel') THEN
          PRINT *, '9. Select minimum value for log(a*(-b))',
     *     ' (suggestion: -10):'
        END IF
        READ*,BMIN
      PRINT*,' '
        IF (CEL.EQ.'decel') THEN
          PRINT *, '10. Select maximum value for log(a*b)',
     *      ' (suggestion: -3):'
        ELSE IF (CEL.EQ.'accel') THEN
          PRINT *, '10. Select maximum value for log(a*(-b))',
     *      ' (suggestion: -3):'
        END IF
        READ*,BMAX
      PRINT*,' '
        PRINT *, '11. Select number of log10 increments to be analysed:'
        READ*,STEPNR
      END IF
      PRINT*,' '
      
      
C     -----------------------regression loop---------------------------

      R2=0.
      Y=1
      BAO=0.
      BO=0.
      BSTEP=BSTEP0

      IF (T.EQ.'a') THEN
        B=BSTART-BSTEP
      ELSE IF (T.EQ.'b') THEN
        BSTEP=(BMAX-BMIN)/REAL(STEPNR)
        B=BMIN-BSTEP
      END IF
      STEP=(SHIMAX-SHIMIN)/50.
      SHICRI=SHICR0+STEP
      
      DO 
      
        B=B+BSTEP
        R2BOLD=R2
        R2OLD=0.
      
        AO=0.
        SHICRO=0.
        DO
          SHICRI=SHICRI-STEP
          DO I=1,N
          INTEGR(I)=0.
          INTINS=0.
            DO J=1,M
              IF (SHIELD(J,1).GE.TIMESP(I,3)) THEN
              INTEGR(I)=INTEGR(I)+
     *        (MAX(SHIELD(J,2)/(1.+PLUMIN*INTEGR(I)*10.**B)-SHICRI,0.))
     *          **EXP
     *          *(SHIELD(J+1,1)-SHIELD(J-1,1))/2.*86400.
                IF ((SHIELD(J,1).LE.TIMESP(I,1)).AND.
     *          (SHIELD(J+1,1).GT.TIMESP(I,1))) THEN
                  INTINS=INTEGR(I)
                ENDIF
                IF (SHIELD(J,1).GE.TIMESP(I,2)) THEN
                  INTEGR(I)=INTEGR(I)-INTINS
                  EXIT
                ENDIF
              END IF
            END DO
          END DO
        
           SUMXY=0.
           SUMX2=0.
           SUMY2=0.
          DO I=1,N
            SUMXY=SUMXY+INTEGR(I)*TIMESP(I,4)
            SUMX2=SUMX2+INTEGR(I)*INTEGR(I)
C            PRINT*,'SUMX2=',SUMX2
            SUMY2=SUMY2+TIMESP(I,4)*TIMESP(I,4)
          END DO
          
          A=SUMXY/SUMX2
          R2=(SUMXY**2./(SUMX2*SUMY2))
          IF (CEL.EQ.'decel') THEN
            PRINT '(A9,G13.7,A9,G12.6,A5,F16.14)',
     *      'log(a*b)=',B,'tau*c=',SHICRI,'R2=',R2
          ELSE IF (CEL.EQ.'accel') THEN
            PRINT '(A12,G13.7,A9,G12.6,A5,F16.14)',
     *      'log(a*(-b))=',B,'tau*c=',SHICRI,'R2=',R2
          END IF
          DO I=1,N
            SCALC(I)=INTEGR(I)*A
          END DO
          
          IF(ABS(R2-R2OLD).EQ.0.) THEN
            EXIT
          ELSE IF ((ABS((A-AO)/AO).LT.0.000001). AND .
     *      (ABS((SHICRI-SHICRO)/SHICRO).LT.0.000001)) THEN  
            EXIT
          ELSE IF (R2.LT.R2OLD) THEN 
            STEP=STEP/(-2.)
            COUNT=1        
          ELSE IF (COUNT.EQ.1) THEN
            STEP=STEP/2.
            COUNT=0
          END IF          
          R2OLD=R2
          AO=A
          SHICRO=SHICRI
        END DO
        PRINT*,''
        IF (CEL.EQ.'decel') THEN
          PRINT '(A9,G13.7,A7,G12.6,A5,E12.6,A5,E12.6,A6,F16.14)',
     *    'log(a*b)=',B,'tau*c=',SHICRI,'a=',A,'b=',PLUMIN*10.**B/A,
     *    'R2=',R2
        ELSE IF (CEL.EQ.'accel') THEN
          PRINT '(A12,G13.7,A7,G12.6,A5,E12.6,A5,E12.6,A6,F16.14)',
     *    'log(a*(-b))=',B,'tau*c=',SHICRI,'a=',A,'b=',PLUMIN*10.**B/A,
     *    'R2=',R2
        END IF
        PRINT*,''
        IF (T.EQ.'a') THEN
          IF (ABS(B-BO).LT.0.000001) THEN
            EXIT
          ELSE IF ((ABS((A-BAO)/BAO).LT.0.000001).AND.
     *      (ABS((SHICRI-SHICBO)/SHICBO).LT.0.000001).AND.
     *      (ABS(R2-R2BOLD).EQ.0.)) THEN  
            EXIT
          ELSE IF (R2.LT.R2BOLD) THEN 
            BSTEP=-BSTEP/2.
            COUNT2=1
          ELSE IF (COUNT2.EQ.1) THEN
            BSTEP=BSTEP/2.
          COUNT2=0
          END IF
          BAO=A
          BO=B
          SHICBO=SHICRI
        ELSE IF (T.EQ.'b') THEN
          BRANGE (Y,1) = PLUMIN*10.**B/A
          BRANGE (Y,2) = SHICRI 
          BRANGE (Y,3) = A
          BRANGE (Y,4) = R2
          BRANGE (Y,5) = B
          Y=Y+1
          IF (BMAX-B.LT.BSTEP/2.) THEN
            EXIT
          END IF
        END IF
        STEP=(SHIMAX-SHIMIN)/100.
        IF (S.EQ.'a') THEN
          SHICRI=SHICR0+STEP
        ELSE IF (S.EQ.'b') THEN
          SHICRI=SHICRI+STEP
        END IF
      END DO

C     -----------------------------------------------------------------


C     -------------------write result file-----------------------------

      OPEN (31, file='graVel±_result.txt')
      WRITE (31,*) 'program graVel± result file'
      WRITE (31,*) ' '
      WRITE (31,*) 'Underlying function:'
      WRITE (31,*) 'Vu,s={a*[tau*/(1+bs)-tauc*)]}^EXP'
      WRITE (31,*) ' '
      WRITE (31,'(A36,A25)') ' Vu,s......unsteady virtual velocity 
     *','subject to slowdown (m/s)'
      WRITE (31,*) 'a.......velocity coefficient (m/s)'
      WRITE (31,*) 'b.......slowdown coefficient (1/m)'
      WRITE (31,*) 's.......travelled distance (m)'      
      WRITE (31,*) 'tau*....Shields stress (-)'
      WRITE (31,*) 'tauc*...critical Shields stress (-)'
      WRITE (31,*) ' '
      WRITE (31,*) 'Used input data:'
      WRITE (31,*) ' '
      WRITE (31,'(A27,F6.4,A2)') ' Investigated tracer size: ',D,' m'
      WRITE (31,*) 'Used file with time series of shear stress: ',
     *  FNAME
      WRITE (31,*)'Used file with time spans and travel distances: ',
     *    FNAME2
      
      WRITE (31,*) ' '
      WRITE (31,*) 'Formula and regression settings:'
      WRITE (31,*) ' '
      WRITE (31,'(A28,F5.3)') ' Used formula exponent EXP: ', EXP
      IF (CEL.EQ.'decel') THEN
        WRITE (31,*) 'Investigation for: ','tracer deceleration'
      ELSE IF (CEL.EQ.'accel') THEN
        WRITE (31,*) 'Investigation for: ','tracer acceleration'
      END IF
      IF (T.EQ.'a') THEN
        WRITE (31,*) 'Automatic assessment of celeration coefficient b'
        WRITE (31,'(A31,G10.3)') ' Selected start value for a*b: ', 
     *    BSTRT0
        IF (CEL.EQ.'decel') THEN
          WRITE (31,'(A29,A10,G11.4)') ' Selected start interval for ',
     *      'log(a*b): ',BSTEP0
        ELSE IF (CEL.EQ.'accel') THEN
          WRITE (31,'(A28,A13,G10.4)') 'Selected start interval for ',
     *      'log(a*(-b)): ', BSTEP0
        END IF
      ELSE IF (T.EQ.'b') THEN
        WRITE (31,*) 'Analysis of a defined range of a*b values'
        IF (CEL.EQ.'decel') THEN
          WRITE (31,'(A35,F7.3)') ' Selected min. value for log(a*b): '
     *    ,BMIN
          WRITE (31,'(A35,F7.3)') ' Selected max. value for log(a*b): '
     *    ,BMAX
        ELSE IF (CEL.EQ.'accel') THEN
          WRITE (31,'(A25,A13,F7.3)') ' Selected min. value for ',
     *    'log(a*(-b)): ',BMIN
          WRITE (31,'(A24,A14,F7.3)') ' Selected max. value for',
     *    ' log(a*(-b)): ',BMAX
        END IF
      END IF
      WRITE (31,'(A30,A22,F6.4)') ' The maximum Shields stress in',
     *  ' your time series is: ',SHIMAX
      IF (S.EQ.'a') THEN
        WRITE (31,'(A31,A44,F6.4)') ' Every inner loop was selected ',
     *    'to start with a critical Shields stress of: ', SHICR0
      ELSE IF (S.EQ.'b') THEN
        WRITE (31,'(A32,A41,F6.4)') ' The first loop was selected to ',
     *    'start with a critical Shields stress of: ', SHICR0
        WRITE (31,*) 'Subsequent loops were selected to start with ',
     *    'the last result.'
      END IF
      WRITE (31,*) ' '

      WRITE (31,*) 'Results:'
      WRITE (31,*) ' '
      IF (T.EQ.'a') THEN
        WRITE (31,'(A8,G12.6,A4)') 'tau*c = ', SHICRI, ' (-)'
        WRITE (31,'(A4,G12.6,A6)') 'a = ',A, ' (m/s)'
        WRITE (31,'(A4,G12.6,A6)') 'b = ',PLUMIN*10.**B/A, ' (1/m)'
        WRITE (31,'(A11,G12.6)') 'r2 (raw) = ',R2
        WRITE (31,*) ' '
         WRITE (31,'(A23,A27,A35,A26,A27)')' time of pre-survey (d)',
     *   'time of post-survey (d)','integrated (tau*-tau*c)^EXP (s)',
     *   'travelled distance (m)','calculated distance (m)'
          DO I=1,N
           WRITE (31,'(A1,F22.5,F27.5,F35.3,F26.3,F27.3)') ' ',
     *     TIMESP(I,1),TIMESP(I,2),INTEGR(I),TIMESP(I,4),SCALC(I)
          END DO
        PRINT*, ' '
        PRINT*, 'Results:'
        WRITE (31,*) ' '
        PRINT'(A8,G12.6,A4)', 'tau*c = ', SHICRI, ' (-)'
        PRINT'(A4,G12.6,A6)', 'a = ',A, ' (m/s)'
        PRINT'(A4,G12.6,A6)', 'b = ',PLUMIN*10.**B/A, ' (1/m)'
        PRINT'(A11,G12.6)', 'r2 (raw) = ',R2
      ELSE IF (T.EQ.'b') THEN
        IF (CEL.EQ.'decel') THEN
          DO I=1,STEPNR
           WRITE (31,'(A10,F8.2,A6,G10.3,A10,E9.3,A6,E9.3,A12,F16.14)')
     *       ' log(a*b)=',BRANGE(I,5),'    b=',BRANGE(I,1),
     *       'tau*c=',BRANGE(I,2),'a=',BRANGE(I,3),' R2 (raw)=', 
     *       BRANGE(I,4)
          END DO
        ELSE IF (CEL.EQ.'accel') THEN
          DO I=1,STEPNR+1
           WRITE (31,'(A13,F8.2,A6,G10.3,A10,E9.3,A6,E9.3,A12,F16.14)')
     *       ' log(a*(-b))=',BRANGE(I,5),'    b=',
     *       BRANGE(I,1),'tau*c=',BRANGE(I,2),'a=',BRANGE(I,3),
     *       ' R2 (raw)=', BRANGE(I,4)
          END DO
        END IF
      END IF
      
      WRITE (31,*) ' '
      WRITE (31,'(A11)') 'References:'
      WRITE (31,*) ' '
      WRITE (31,'(A26)') 'Citation of this software:'
      WRITE (31,'(A60,A45,A52,A50)')
     *'Kloesch, M., & Habersack, H. (2024). graVel, a code set for ',
     *'deriving the unsteady virtual grain velocity ',
     *'from bedload tracer studies - gravel± (Version 1.0) ',
     *'[Software]. https://doi.org/xx.xxxx/zenodo.xxxxxxx'
      WRITE (31,*) ' '
      WRITE (31,'(A36)') 'Citation of related journal article:'
      WRITE (31,'(A63,A60,A56,A35,A36)')
     *'Kloesch, M., Pessenlehner, S., Gmeiner, Ph., and Habersack, H. ',
     *'(2024). Tracer velocity vs. bedload velocity: Derivation of ',
     *'the unsteady virtual bedload velocity from decelerating ',
     *'tracers. Water Resources Research. ',
     *'https://doi.org/10.1029/2023WR034823'
      CLOSE (31)      
      PRINT*,'Results written to the file graVel+-_result.txt'
      PRINT*, 'Press RETURN to close program'
      READ*
C     -----------------------------------------------------------------
      END PROGRAM
