C-------------------------------------------------------------------------------
C
C  File name.: HAGBR.FOR                               ARCO Oil and Gas, Inc.
C  Subroutine: HAGBR
C
C  Date......: 16-Apr-87                               Author: Richard Stoisits
C
C  Modified..: Ramana V. Palisetti
C  Date......: 06/08/95
C
C  Purpose...: Calculate Liquid holdup and pressure gradient
C              using the Hagedorn and Brown Correlation.
C              The acceleration pressure gradient is calculated
C              with the Duns and Ros equation.
C
C  Arguments.: HAGBR ( ANG, DIA, ED, P, VM, HLNS, DENG, DENL, GVIS,
C                      VISL, XNL, XNLV, XNGV, XND, HL, REYN, FRGR,
C                      ELGR, ACCGR, DPDL, IREG, ICRIT, KREG )
C
C     ANG            - REAL*4      - Angle of flow from horizontal. (deg-F)
C
C     DIA            - REAL*4      - Inside pipe diameter. (ft)
C
C     ED             - REAL*4      - Relative pipe roughness
C
C     P              - REAL*4      - Pressure (psia)
C
C     VM             - REAL*4      - Superficial mixture velocity. (ft/sec)
C
C     HLNS           - REAL*4      - No slip liquid holdup
C
C     DENG           - REAL*4      - Gas density. (lbm/ft**3)
C
C     DENL           - REAL*4      - Liquid density. (lbm/ft**3)
C
C     GVIS           - REAL*4      - Viscosity of Gas
C
C     VISL           - REAL*4      - Liquid viscosity. (cp)
C
C     XNL            - REAL*4      - Dimensionless liquid viscosity number
C
C     XNLV           - REAL*4      - Dimensionless liquid velocity number
C
C     XNGV           - REAL*4      - Dimensionless gas velocity number
C
C     XND            - REAL*4      - Dimensionless Diameter Number
C
C     HL             - REAL*4      - Liquid Holdup
C
C     REYN           - REAL*4      - Reynold's Number
C
C     FRGR           - REAL*4      - Friction Gradient
C
C     ELGR           - REAL*4      - Elevation Gradient
C
C     ACCGR          - REAL*4      - Acceleration Gradient
C
C     DPDL           - REAL*4      - Pressure drop per foot ( psi per foot )
C
C     IREG           - INTEGER*4   - Flow regiment flag
C
C                                    Values are: 1  LIQUID
C                                                2  GAS
C                                                3  DISTRIBUTED
C                                                4  INTERMITTENT
C                                                5  SEGREGATED
C                                                6  TRANSITION
C
C     KREG           - INTEGER*4   - 1 - Use Griffith & Wallis bubble flow
C
C     ICRIT          - INTEGER*4   - Critical Flow flag
C
C                                    Values are: 0 - Not Critical
C                                                1 - Critical Flow
C
C
C  Returned Value(s):
C
C
C
C  Subroutines Referenced:      FRFACT         -  ARCO
C
C-------------------------------------------------------------------------------


C-------------------------------------------------------------------------------
C                          Start of Subroutine
C-------------------------------------------------------------------------------


      SUBROUTINE  HAGBR ( ANG, DIA, ED, P, VM, HLNS, DENG, DENL, GVIS,
     1                    VISL, XNL, XNLV, XNGV, XND, HL, REYN, FRGR,
     1                    ELGR, ACCGR, DPDL, IREG, ICRIT, KREG )

C-----------------------------------------------------------------------
C             >>>>>>>>>   Variable Declarations   <<<<<<<<<<<
C-----------------------------------------------------------------------

      double precision XHL(12), YHL(12), XCNL(10), YCNL(10), XPSI(12)
      double precision YPSI(12), XHLL(12), XCNLL(10), YCNLL(10)
      double precision ANG, DIA, ED, P, VM, HLNS, DENG, DENL, GVIS
      double precision VISL, XNL, XNLV, XNGV, XND, HL, REYN, FRGR
      double precision ELGR, ACCGR, DPDL
      double precision A, VSL, VSG, DENNS, DENS, XLB, REYNB, FF, VS
      double precision EKK, XX, CNL, PSI, FLAGR, VISS, HGNS

      integer  IREG, ICRIT, K, KREG

C------------------------------------------------------------------------
C     ...DATA ARRAYS FOR LIQUID HOLDUP CORRELATION.
C------------------------------------------------------------------------

      DATA XHL/
     1.2,.5,1.,2.,5.,10.,20.,50.,100.,200.,300.,1000./

      DATA YHL/
     1.04,.09,.15,.18,.25,.34,.44,.65,.82,.92,.96,1./

      DATA XCNL/
     1.002,.005,.01,.02,.03,.06,.1,.15,.2,.4/

      DATA YCNL/
     1.0019,.0022,.0024,.0028,.0033,.0047,.0064,.008,.009,.0115/

      DATA XPSI/
     1.01,.02,.025,.03,.035,.04,.045,.05,.06,.07,.08,.09/

      DATA YPSI/
     11.,1.1,1.23,1.4,1.53,1.6,1.65,1.68,1.74,1.78,1.8,1.83/

C-----------------------------------------------------------------------
C                 >>>>>>>>>   Start Code   <<<<<<<<<<<
C
C     ...CONVERT INCLINATION ANGLE TO RADIANS.
C-----------------------------------------------------------------------
      !WRITE(*,*) XHL
      A = ANG * 3.1416 / 180.
    

C------------------------------------------------------------------------
C     ...CALCULATE SUPERFICIAL VELOCITIES.
C------------------------------------------------------------------------

      VSL = VM * HLNS
      VSG = VM - VSL
      
      !write(*,*) VSL, VSG

C------------------------------------------------------------------------
C     ...CHECK FOR SINGLE PHASE GAS OR LIQUID FLOW.
C------------------------------------------------------------------------

      IF (HLNS.LE.0.999) GOTO 1
      HL    = 1.
      DENNS = DENL
      IREG  = 1
      GOTO 6

1     CONTINUE
      IF (HLNS.GE.0.001) GOTO 2
      HL    = 0.
      DENNS = DENG
      IREG  = 2
      GOTO 6

C------------------------------------------------------------------------
C     ...CHECK FOR BUBBLE FLOW.
C------------------------------------------------------------------------

2     CONTINUE
      XLB = 1.071 - 0.2218 * VM**2 / DIA

      IF (XLB.LT.0.13) XLB = 0.13

      HGNS = 1. - HLNS
      IF (HGNS.GT.XLB) GOTO 3

      IREG = 3

C ...CHECK TO SEE IF GRIFFITH & WALLIS CORRELATION IS TO BE USED
      IF (KREG.EQ.0) GO TO 3
      
C ...GRIFFITH & WALLIS BUBBLE FLOW CALCULATION
      VS   = 0.8
      HL   = 1. - 0.5 * (1. + VM/VS - SQRT((1.+VM/VS)**2.-4.*VSG/VS))

      IF (HL.LT.HLNS) HL=HLNS

      DENS  = DENL * HL + DENG * (1.-HL)
      REYNB = 1488. * DENL * (VSL/HL) * DIA / VISL

      CALL FRFACT ( REYNB, ED, FF )

C------------------------------------------------------------------------
C     ...CALCULATE ELEVATION AND FRICTION GRADIENTS AND ACCELERATION TERM
C        FOR BUBBLE FLOW.
C------------------------------------------------------------------------

      ELGR = DENS * COS(A)/144.
      FRGR = FF * DENL * (VSL/HL)**2 / ( 2. * 32.2 * DIA * 144. )
      EKK  = 0.
      ICRIT = 0.
      GOTO 7


C ...SLUG FLOW EXISTS OR GRIFFITH & WALLIS NOT SELECTED
C------------------------------------------------------------------------
C     ...PREPARE HOLDUP CORRELATION ARRAYS FOR INTERPOLATION.
C------------------------------------------------------------------------

3     CONTINUE
      DO 4 K=1,10
         XCNLL(K) = LOG(XCNL(K))
         YCNLL(K) = LOG(YCNL(K))
4     CONTINUE

      DO 5 K=1,12
5        XHLL(K) = LOG(1.E-05*XHL(K))

      IREG  = 4

C------------------------------------------------------------------------
C     ...CALCULATE LIQUID HOLDUP.
C------------------------------------------------------------------------

      XX   = LOG(XNL)
      CNL  = EXP ( FLAGR (XCNLL,YCNLL,XX,2,10) )
      XX   = LOG( XNLV * CNL / (XNGV**0.575 * XND) * (P/14.7)**0.1)
      
      HL   = FLAGR (XHLL,YHL,XX,2,12)
      !!!write(*,*) HL
      
      XX   = XNGV * XNL**0.38 / XND**2.14

      PSI  = FLAGR (XPSI,YPSI,XX,2,12)
      !!!write(*,*) PSI

      IF (PSI.LT.1.) PSI=1.

      HL  = HL * PSI
      !!!write(*,*) HL

      IF (HL.LT.0.) HL=0.
      IF (HL.GT.1.) HL=1.
      !!!write(*,*) HL, HLNS
      IF (HL.GT.HLNS) GOTO 6

      HL = HLNS

C------------------------------------------------------------------------
C     ...CALCULATE NO-SLIP AND SLIP MIXTURE DENSITIES.
C------------------------------------------------------------------------

6     CONTINUE
      !write(*,*) "here"
      
      !!!write(*,*) HL, IREG, DENNS
      
      DENNS = DENL * HLNS + DENG * (1.-HLNS)
      !!!write(*,*) DENNS, HLNS, DENG

      DENS  = DENL * HL + DENG * (1.-HL)
      !!!write(*,*) DENS, HL, DENG

C------------------------------------------------------------------------
C     ...CALCULATE FRICTION FACTOR.
C------------------------------------------------------------------------

      IF (HL.LE.0.) THEN
          VISS=GVIS
          GOTO 989
      ENDIF

      IF (HL.GE.1.) THEN
          VISS=VISL
          GOTO 989
      ENDIF

      VISS = VISL**HL * GVIS**(1.-HL)
      !!!write(*,*) VISS, HL, GVIS

989   CONTINUE
      REYN = 1488. * DENNS * VM * DIA / VISS
      !!!write(*,*) REYN
      
      CALL FRFACT (REYN,ED,FF)
      !!!write(*,*) FF

C------------------------------------------------------------------------
C     ...CALCULATE ELEVATION, FRICTION, ACCELERATION, AND TOTAL PRESSURE
C        GRADIENTS.
C------------------------------------------------------------------------

      ELGR = DENS * COS(A) / 144.
      FRGR = FF * DENNS**2 * VM**2 / (2. * 32.2 * DIA * DENS * 144. )
      VSG  = VM * (1.-HLNS)
      EKK  = DENS * VM * VSG / ( 32.2 * P * 144. )
      !!!write(*,*) DENS, VM, VSG, P
      !!!write(*,*) ELGR, FRGR, EKK
      
      ICRIT= 0

      IF (EKK.GT.0.95) ICRIT=1
      IF (EKK.GT.0.95) EKK=.95
      
      !!!write(*,*) EKK

7     CONTINUE
      DPDL  =  -( ELGR + FRGR ) / ( 1. - EKK )
      !!!write(*,*) DPDL, (1.-EKK)
      !!!write(*,*) ELGR+FRGR
      ACCGR =  - DPDL * EKK

      RETURN
      END


C-------------------------------------------------------------------------------
C
C  File name.: FRFACT.FOR                              ARCO Oil and Gas, Inc.
C  Subroutine: FRFACT
C
C  Date......: 16-Apr-87                               Author: Richard Stoisits
C
C  Purpose...: Calculate the Moody Friction Factor using
C              either the Laminar Flow or the Colebrook
C              Equations.  If flow is not Laminar
C              (REY < 2000), use a Blasius Friction Factor for
C              the first guess in the Colebrook equation.
C
C  Arguments.: FRFACT ( REY, ED, FF )
C
C     REY            - REAL*4      - Reynold's Number
C
C     ED             - REAL*4      - Relative pipe roughness
C
C
C  Returned Value(s):
C
C     FF             - REAL*4      - Flow Friction Factor
C
C
C
C  Subroutines Referenced:      NONE
C
C-------------------------------------------------------------------------------


C-------------------------------------------------------------------------------
C                          Start of Subroutine
C-------------------------------------------------------------------------------

      SUBROUTINE FRFACT ( REY, ED, FF )

C-----------------------------------------------------------------------
C             >>>>>>>>>   Variable Declarations   <<<<<<<<<<<
C-----------------------------------------------------------------------

       double precision  REY, ED, FF, FGI, DIFF, DEN

       integer   I

C-----------------------------------------------------------------------
C                 >>>>>>>>>   Start Code   <<<<<<<<<<<
C-----------------------------------------------------------------------

      IF (REY.GT.2000.) GOTO 1

C------------------------------------------------------------------------
C     ...LAMINAR FLOW FRICTION FACTOR.
C------------------------------------------------------------------------

      FF = 64./REY
      GOTO 8

C------------------------------------------------------------------------
C     ...CALCULATE MOODY FRICTION FACTOR WITH SMOOTH PIPE BLASIUS
C        EQUATION FOR FIRST GUESS IN COLEBROOK EQUATION.
C------------------------------------------------------------------------


1     CONTINUE
      FGI = 0.0056 + 0.5 / REY**0.32

C------------------------------------------------------------------------
C     ...SET COUNTER. COLEBROOK EQUATION IS ITERATIVE. IF CONVERGENCE
C        IS NOT ATTAINED IN 10 ITERATIONS AN INFINITE LOOP WILL
C        PROBABLY OCCUR. SET FRICTION FACTOR EQUAL TO THE VALUE
C        DETERMINED IN THE 10 TH ITERATION AND USE WITH CAUTION.
C------------------------------------------------------------------------

      I = 1

5     CONTINUE
      DEN = 1.14 - 2. * LOG10( ED + 9.34 / (REY*SQRT(FGI)))
      FF  = ( 1. / DEN )**2
      DIFF= ABS(FGI-FF)

      IF (DIFF.LE.0.0001) GOTO 8

      FGI = (FGI+FF)/2.
      I   = I + 1

      IF (I.LT.10) GOTO 5

      FF = FGI
      

8     CONTINUE
      write(*,*) "inside FRFACT"
      write(*,*) FF, FGI
      RETURN
      END


C-------------------------------------------------------------------------------
C
C  File name.: FLAGR.FOR                               ARCO Oil and Gas, Inc.
C  Function..: FLAGR
C
C  Date......: 10-Apr-87                               Author: Richard Stoisits
C
C  Purpose...:
C
C  Arguments.: FLAGR ( X, Y, XARG, IDEG, NPTS )
C
C     X              - REAL*4      - The array of independent variable data
C                                    points
C
C     Y              - REAL*4      - The array of dependent variable data
C                                    points
C
C     XARG           - REAL*4      - The argument for which an interpolated
C                                    value is desired
C
C     IDEG           - INTEGER*4   - Degree of interpolating polynomial
C
C                                    Values are: 1 - linear
C                                                2 - quadratic, etc
C
C     NPTS           - INTEGER*4   - The number of data points in x and y
C
C
C  Returned Value(s):
C
C    NONE
C
C
C  Subroutines Referenced:      NONE
C
C-------------------------------------------------------------------------------


C-------------------------------------------------------------------------------
C                          Start of Subroutine
C-------------------------------------------------------------------------------

      FUNCTION  FLAGR ( X, Y, XARG, IDEG, NPTS )

C-----------------------------------------------------------------------
C             >>>>>>>>>   Variable Declarations   <<<<<<<<<<<
C-----------------------------------------------------------------------

      integer   NPTS, L, N, N1, MAX, MIN, IDEG, I, J

      double precision  X(NPTS), Y(NPTS), XARG, FLAGR, FACTOR, YEST, TERM

C-----------------------------------------------------------------------
C                 >>>>>>>>>   Start Code   <<<<<<<<<<<
C-----------------------------------------------------------------------
C
C     ...INTERPOLATION ROUTINE SIMILAR TO FLAGR IN APPLIED NUMERICAL
C        METHODS BY CARNAHAN, LUTHER AND WILKES, JOHN WILEY AND SONS,
C        PG. 31.
C
C     ...FLAGR USES THE LAGRANGE FORMULA TO EVALUATE THE INTERPOLATING
C        POLYNOMIAL OF DEGREE "IDEG" FOR ARGUMENT "XARG" USING THE DATA
C        VALUES X(MIN).....X(MAX) AND Y(MIN).....Y(MAX) WHERE M
C        MIN = MAX-IDEG. THE "X(I)" VALUES ARE NOT NECESSARILY EVENLY SPACED
C        AND CAN BE IN EITHER INCREASING OR DECREASING ORDER.
C
C        X    = ARRAY OF INDEPENDENT VARIABLE DATA POINTS.
C        Y    = ARRAY OF DEPENDENT VARIABLE DATA POINTS.
C        XARG = ARGUMENT FOR WHICH AN INTERPOLATED VALUE IS DESIRED.
C        IDEG = DEGREE OF INTERPOLATING POLYNOMIAL (1=LINEAR,2=QUADRATIC, ETC).
C        NPTS = NUMBER OF DATA POINTS IN X AND Y.
C------------------------------------------------------------------------

      N  = IABS(NPTS)
      N1 = IDEG + 1
      L  = 1
      IF (X(2).GT.X(1)) GOTO 1
      L  = 2

C------------------------------------------------------------------------
C     ...CHECK TO BE SURE THAT XARG IS WITHIN RANGE OF X(I) VALUES
C        FOR INTERPOLATION PURPOSES. IF IT IS NOT, SET FLAGR EQUAL
C        TO THE APPROPRIATE TERMINAL VALUE (Y(1) OR Y(N)) AND RETURN.
C        NOTE THAT THIS PRECLUDES EXTRAPOLATION OF DATA.
C------------------------------------------------------------------------

1     CONTINUE
      GOTO (2,3) L

2     CONTINUE
      IF (XARG.LE.X(1)) GOTO 4
      IF (XARG.GE.X(N)) GOTO 5
      GOTO 6

3     CONTINUE
      IF (XARG.GE.X(1)) GOTO 4
      IF (XARG.LE.X(N)) GOTO 5
      GOTO 6

4     CONTINUE
      FLAGR = Y(1)
      RETURN

5     CONTINUE
      FLAGR = Y(N)
      RETURN

C------------------------------------------------------------------------
C     ...DETERMINE VALUE OF MAX.
C------------------------------------------------------------------------

6     CONTINUE
      GOTO (10,20) L

C------------------------------------------------------------------------
C     ...DATA ARE IN ORDER OF INCREASING VALUES OF X.
C------------------------------------------------------------------------

10    CONTINUE
      DO 11 MAX=N1,N
         IF (XARG.LT.X(MAX)) GOTO 12
11    CONTINUE

C------------------------------------------------------------------------
C     ...DATA ARE IN ORDER OF DECREASING VALUES OF X.
C------------------------------------------------------------------------

20    CONTINUE
      DO 21 MAX=N1,N
         IF (XARG.GT.X(MAX)) GOTO 12
21    CONTINUE

C------------------------------------------------------------------------
C     ...COMPUTE VALUE OF FACTOR.
C------------------------------------------------------------------------

12    CONTINUE
      MIN   = MAX - IDEG
      FACTOR= 1.

      DO 7 I=MIN,MAX
         IF (XARG.NE.X(I)) GOTO 71
         FLAGR = Y(I)
         RETURN

71       CONTINUE
         FACTOR = FACTOR * ( XARG - X(I) )
7     CONTINUE

C------------------------------------------------------------------------
C     ...EVALUATE INTERPOLATING POLYNOMIAL.
C------------------------------------------------------------------------

      YEST = 0.

      DO 9 I=MIN,MAX

         TERM = Y(I) * FACTOR / ( XARG - X(I) )

         DO 8 J=MIN,MAX
            IF (I.NE.J) TERM = TERM / ( X(I) -X(J) )
8        CONTINUE

         YEST = YEST + TERM

9     CONTINUE
      write(*,*) "inside FLAGR"
      FLAGR = YEST

      RETURN
      END

