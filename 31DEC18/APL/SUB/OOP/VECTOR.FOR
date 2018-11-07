C   Vector class
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "VECTOR" class library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Public functions
C
C      RVNEG (IN, OUT, N)
C         Real Vector negate.
C      CVNEG (IN, OUT, N)
C         Complex Vector negate
C      RVBNEG (IN, VALIN, OUT, VALOUT, N)
C         Real Vector negate with blanking
C      CVBNEG (IN, VALIN, OUT, VALOUT, N)
C         Complex Vector negate with blanking
C      RVADD (IN1, IN2, OUT, N)
C         Real Vector add
C      CVADD (IN1, IN2, OUT, N)
C         Complex Vector add
C      RVBADD (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C         Real Vector add with blanking
C      CVBADD (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C         Complex Vector add with blanking
C      RVSUB (IN1, IN2, OUT, N)
C         Real Vector subtract
C      CVSUB (IN1, IN2, OUT, N)
C         Complex Vector subtract
C      RVBSUB (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C         Real Vector subtract with blanking
C      CVBSUB (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C         Complex Vector subtract with blanking
C      RVMUL (IN1, IN2, OUT, N)
C         Real Vector multiply
C      CVMUL (IN1, IN2, OUT, N)
C         Complex Vector multiply
C      RVBMUL (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C         Real Vector multiply with blanking
C      CVBMUL (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C         Complex Vector multiply with blanking
C      RVDIV (IN1, IN2, OUT, N)
C         Real Vector divide first by second
C      CVDIV (IN1, IN2, OUT, N)
C         Complex Vector divide first by second
C      RVBDIV (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C         Real Vector divide first by second with blanking
C      CVBDIV (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C         Complex Vector divide first by second with blanking
C      RVSCL (IN1, FACTOR, OUT, N)
C         Real Scale vector with real scalar
C      CVSCL (IN1, FACTOR, OUT, N)
C         Complex Scale vector with complex scalar
C      RVBSCL (IN1, VALIN1, FACTOR, OUT, VALOUT, N)
C         Real Scale vector with real scalar with blanking
C      CVBSCL (IN1, VALIN1, FACTOR, OUT, VALOUT, N)
C         Complex Scale vector with complex scalar with blanking
C      RVCOPY (IN, OUT, N)
C         Copy real vector
C      CVCOPY (IN, OUT, N)
C         Copy complex vector
C      RVFILL (SCALAR, OUT, N)
C         Real vector fill
C      CVFILL (SCALAR, OUT, N)
C         Complex vector fill
C      RSVADD (IN, SCALAR, OUT, N)
C         Real scalar vector add
C      CSVADD (IN, SCALAR, OUT, N)
C         Complex scalar vector add
C      RSVADB (IN, VALIN, SCALAR, OUT, VALOUT, N)
C         Real scalar vector add with blanking
C      CSVADB (IN, VALIN, SCALAR, OUT, VALOUT, N)
C         Complex scalar vector add with blanking
C      RVBBLS (IN, VALIN, N, NWIN, WIN)
C         Real vector baseline fit and subtract with blanking
C      RVBLS (IN, N, NWIN, WIN)
C         Real vector baseline fit and subtract
C      RVBBSC (IN, VALIN, OUT, VALOUT, N, CONVF)
C         Real vector convolution with blanking
C      RVBSC (IN, OUT, N, CONVF)
C         Real vector convolution
C-----------------------------------------------------------------------
      SUBROUTINE RVNEG (IN, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Vector negate.
C   Inputs:
C      IN     R(*)  Input vector
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) = - IN(LOOP)
 100  CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVNEG (IN, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Vector negate
C   Inputs:
C      IN     CX(*)  Input vector
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) = - IN(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVBNEG (IN, VALIN, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Vector negate with blanking
C   Inputs:
C      IN     R(*)  Input vector
C      VALIN  L(*)  True if corresponding element of IN is valid.
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C      VALOUT L(*)  True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN(N), OUT(N)
      LOGICAL   VALIN(N), VALOUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(LOOP) = VALIN(LOOP)
         IF (VALOUT(LOOP)) OUT(LOOP) = - IN(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVBNEG (IN, VALIN, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Vector negate with blanking
C   Inputs:
C      IN     CX(*)  Input vector
C      VALIN  L(*)   True if corresponding element of IN is valid.
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C      VALOUT L(*)   True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN(N), OUT(N)
      LOGICAL   VALIN(2,N), VALOUT(2,N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(1,LOOP) = VALIN(1,LOOP)
         VALOUT(2,LOOP) = VALOUT(1,LOOP)
         IF (VALOUT(1,LOOP)) OUT(LOOP) = - IN(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVADD (IN1, IN2, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Vector add
C   Inputs:
C      IN1    R(*)  First Input vector
C      IN2    R(*)  Second Input vector
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), IN2(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN1(LOOP) + IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVADD (IN1, IN2, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Vector add
C   Inputs:
C      IN1    CX(*)  First Input vector
C      IN2    CX(*)  Second Input vector
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN1(N), IN2(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN1(LOOP) + IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVBADD (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Vector add with blanking
C   Inputs:
C      IN1    R(*)  First Input vector
C      VALIN1 L(*)  True if corresponding element of IN1 is valid.
C      IN2    R(*)  Second Input vector
C      VALIN2 L(*)  True if corresponding element of IN2 is valid.
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C      VALOUT L(*)  True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), IN2(N), OUT(N)
      LOGICAL   VALIN1(N), VALIN2(N), VALOUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(LOOP) = VALIN1(LOOP) .AND. VALIN2(LOOP)
         IF (VALOUT(LOOP)) OUT(LOOP) =  IN1(LOOP) + IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVBADD (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Vector add with blanking
C   Inputs:
C      IN1    CX(*)  First Input vector
C      VALIN1 L(*)   True if corresponding element of IN1 is valid.
C      IN2    CX(*)  Second Input vector
C      VALIN2 L(*)   True if corresponding element of IN2 is valid.
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C      VALOUT L(*)   True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN1(N), IN2(N), OUT(N)
      LOGICAL   VALIN1(2,N), VALIN2(2,N), VALOUT(2,N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(1,LOOP) = VALIN1(1,LOOP) .AND. VALIN2(1,LOOP)
         VALOUT(2,LOOP) = VALOUT(1,LOOP)
         IF (VALOUT(1, LOOP)) OUT(LOOP) = IN1(LOOP) + IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVSUB (IN1, IN2, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Vector subtract
C   Inputs:
C      IN1    R(*)  First Input vector
C      IN2    R(*)  Second Input vector
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), IN2(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN1(LOOP) - IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVSUB (IN1, IN2, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Vector subtract
C   Inputs:
C      IN1    CX(*)  First Input vector
C      IN2    CX(*)  Second Input vector
C      N      I     Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN1(N), IN2(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN1(LOOP) - IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVBSUB (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Vector subtract with blanking
C   Inputs:
C      IN1    R(*)  First Input vector
C      VALIN1 L(*)  True if corresponding element of IN1 is valid.
C      IN2    R(*)  Second Input vector
C      VALIN2 L(*)  True if corresponding element of IN2 is valid.
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C      VALOUT L(*)  True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), IN2(N), OUT(N)
      LOGICAL   VALIN1(N), VALIN2(N), VALOUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(LOOP) = VALIN1(LOOP) .AND. VALIN2(LOOP)
         IF (VALOUT(LOOP)) OUT(LOOP) =  IN1(LOOP) - IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVBSUB (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Vector subtract with blanking
C   Inputs:
C      IN1    CX(*)  First Input vector
C      VALIN1 L(*)   True if corresponding element of IN1 is valid.
C      IN2    CX(*)  Second Input vector
C      VALIN2 L(*)   True if corresponding element of IN2 is valid.
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C      VALOUT L(*)   True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN1(N), IN2(N), OUT(N)
      LOGICAL   VALIN1(2,N), VALIN2(2,N), VALOUT(2,N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(1,LOOP) = VALIN1(1,LOOP) .AND. VALIN2(1,LOOP)
         VALOUT(2,LOOP) = VALOUT(1,LOOP)
         IF (VALOUT(1, LOOP)) OUT(LOOP) = IN1(LOOP) - IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVMUL (IN1, IN2, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Vector multiply
C   Inputs:
C      IN1    R(*)  First Input vector
C      IN2    R(*)  Second Input vector
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), IN2(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN1(LOOP) * IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVMUL (IN1, IN2, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Vector multiply
C   Inputs:
C      IN1    CX(*)  First Input vector
C      IN2    CX(*)  Second Input vector
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN1(N), IN2(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN1(LOOP) * IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVBMUL (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Vector multiply with blanking
C   Inputs:
C      IN1    R(*)  First Input vector
C      VALIN1 L(*)  True if corresponding element of IN1 is valid.
C      IN2    R(*)  Second Input vector
C      VALIN2 L(*)  True if corresponding element of IN2 is valid.
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C      VALOUT L(*)  True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), IN2(N), OUT(N)
      LOGICAL   VALIN1(N), VALIN2(N), VALOUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(LOOP) = VALIN1(LOOP) .AND. VALIN2(LOOP)
         IF (VALOUT(LOOP)) OUT(LOOP) =  IN1(LOOP) * IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVBMUL (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Vector multiply with blanking
C   Inputs:
C      IN1    CX(*)  First Input vector
C      VALIN1 L(*)   True if corresponding element of IN1 is valid.
C      IN2    CX(*)  Second Input vector
C      VALIN2 L(*)   True if corresponding element of IN2 is valid.
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C      VALOUT L(*)   True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN1(N), IN2(N), OUT(N)
      LOGICAL   VALIN1(2,N), VALIN2(2,N), VALOUT(2,N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(1,LOOP) = VALIN1(1,LOOP) .AND. VALIN2(1,LOOP)
         VALOUT(2,LOOP) = VALOUT(1,LOOP)
         IF (VALOUT(1,LOOP)) OUT(LOOP) = IN1(LOOP) * IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVDIV (IN1, IN2, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Vector divide first by second
C   Inputs:
C      IN1    R(*)  First Input vector
C      IN2    R(*)  Second Input vector
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), IN2(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN1(LOOP)  / IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVDIV (IN1, IN2, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Vector divide first by second
C   Inputs:
C      IN1    CX(*)  First Input vector
C      IN2    CX(*)  Second Input vector
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN1(N), IN2(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN1(LOOP)  / IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVBDIV (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Vector divide first by second with blanking
C   Inputs:
C      IN1    R(*)  First Input vector
C      VALIN1 L(*)  True if corresponding element of IN1 is valid.
C      IN2    R(*)  Second Input vector
C      VALIN2 L(*)  True if corresponding element of IN2 is valid.
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C      VALOUT L(*)  True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), IN2(N), OUT(N)
      LOGICAL   VALIN1(N), VALIN2(N), VALOUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(LOOP) = VALIN1(LOOP) .AND. VALIN2(LOOP)
         IF (VALOUT(LOOP)) OUT(LOOP) =  IN1(LOOP)  / IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVBDIV (IN1, VALIN1, IN2, VALIN2, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Vector divide first by second with blanking
C   Inputs:
C      IN1    CX(*)  First Input vector
C      VALIN1 L(*)   True if corresponding element of IN1 is valid.
C      IN2    CX(*)  Second Input vector
C      VALIN2 L(*)   True if corresponding element of IN2 is valid.
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C      VALOUT L(*)   True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN1(N), IN2(N), OUT(N)
      LOGICAL   VALIN1(2,N), VALIN2(2,N), VALOUT(2,N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(1,LOOP) = VALIN1(1,LOOP) .AND. VALIN2(1,LOOP)
         VALOUT(2,LOOP) = VALOUT(1,LOOP)
         IF (VALOUT(1, LOOP)) OUT(LOOP) = IN1(LOOP)  / IN2(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVSCL (IN1, FACTOR, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Scale vector with real
C   Inputs:
C      IN1    R(*)  Input vector
C      FACTOR R     Scaling factor
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), FACTOR, OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN1(LOOP) * FACTOR
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVSCL (IN1, FACTOR, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Scale vector with scalar complex
C   Inputs:
C      IN1    CX(*)  Input vector
C      FACTOR CX     Scaling factor
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN1(N), FACTOR, OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN1(LOOP) * FACTOR
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVBSCL (IN1, VALIN1, FACTOR, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real Scale vector with real with blanking
C   Inputs:
C      IN1    R(*)  Input vector
C      VALIN1 L(*)  True if corresponding element of IN1 is valid.
C      FACTOR R     Scaling factor
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C      VALOUT L(*)  True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN1(N), FACTOR, OUT(N)
      LOGICAL   VALIN1(N), VALOUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(LOOP) = VALIN1(LOOP)
         IF (VALOUT(LOOP)) OUT(LOOP) =  IN1(LOOP) * FACTOR
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVBSCL (IN1, VALIN1, FACTOR, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Scale vector with real with blanking
C   Inputs:
C      IN1    CX(*)  Input vector
C      VALIN1 L(*)   True if corresponding element of IN1 is valid.
C      FACTOR R      Scaling factor
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C      VALOUT L(*)   True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN1(N), FACTOR, OUT(N)
      LOGICAL   VALIN1(2,N), VALOUT(2,N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(1,LOOP) = VALIN1(1,LOOP)
         VALOUT(2,LOOP) = VALOUT(1,LOOP)
         IF (VALOUT(1,LOOP)) OUT(LOOP) = IN1(LOOP) * FACTOR
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVCOPY (IN, OUT, N)
C-----------------------------------------------------------------------
C     Public
C     Real Vector copy.
C     Inputs:
C        IN     R(*)  Input vector
C        N      I     Number of elements
C     Outputs:
C        OUT    R(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) = IN(LOOP)
 100  CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVCOPY (IN, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex Vector copy
C   Inputs:
C      IN     CX(*)  Input vector
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN(N), OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) = IN(LOOP)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVFILL (SCALAR, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real vector fill
C   Inputs:
C      SCALAR R     Value to fill OUT
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      SCALAR, OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) = SCALAR
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CVFILL (SCALAR, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex vector fill
C   Inputs:
C      SCALAR CX     Value to fill OUT
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   SCALAR, OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) = SCALAR
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RSVADD (IN, SCALAR, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real scalar vector add
C   Inputs:
C      IN     R(*)  Input vector
C      SCALAR R     scalar to add
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN(N), SCALAR, OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN(LOOP) + SCALAR
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CSVADD (IN, SCALAR, OUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex scalar vector add
C   Inputs:
C      IN     CX(*)  Input vector
C      SCALAR CX     scalar to add
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN(N), SCALAR, OUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         OUT(LOOP) =  IN(LOOP) + SCALAR
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RSVADB (IN, VALIN, SCALAR, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Real scalar vector add with blanking
C   Inputs:
C      IN     R(*)  Input vector
C      VALIN  L(*)  True if corresponding element of IN is valid.
C      SCALAR R     scalar to add
C      N      I     Number of elements
C   Outputs:
C      OUT    R(*)  Output vector
C      VALOUT L(*)  True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      IN(N), SCALAR, OUT(N)
      LOGICAL   VALIN(N), VALOUT(N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(LOOP) = VALIN(LOOP)
         IF (VALOUT(LOOP)) OUT(LOOP) =  IN(LOOP) + SCALAR
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CSVADB (IN, VALIN, SCALAR, OUT, VALOUT, N)
C-----------------------------------------------------------------------
C   Public
C   Complex scalar vector add with blanking
C   Inputs:
C      IN     CX(*)  Input vector
C      VALIN  L(2,*) True if corresponding element of IN is valid.
C      SCALAR R      scalar to add
C      N      I      Number of elements
C   Outputs:
C      OUT    CX(*)  Output vector
C      VALOUT L(2,*) True if corresponding element of OUT is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   IN(N), SCALAR, OUT(N)
      LOGICAL   VALIN(2,N), VALOUT(2,N)
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(1,LOOP) = VALIN(1,LOOP)
         VALOUT(2,LOOP) = VALOUT(1,LOOP)
         IF (VALOUT(1,LOOP)) OUT(LOOP) = IN(LOOP) + SCALAR
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVBLS (VALU, N, NORD, NWIN, WIN)
C-----------------------------------------------------------------------
C   Public
C   Real vector baseline fit and subtract
C   Inputs:
C      VALU   R(*)  Input vector
C      N      I     Number of elements
C      NORD   I     Baseline order : 0, 1 only
C      NWIN   I     Number baseline windows
C      WIN    I(2,*)  Windows
C   Outputs:
C      VALU   R(*)  Corrected vector
C-----------------------------------------------------------------------
      INTEGER   N, NORD, NWIN, WIN(2,*)
      REAL      VALU(N)
C
      INTEGER   LOOP, L1, L2, IW, NSUM
      REAL      BSUM, BSUMI, ISUM, ISUMI, L0, BO, BS
C-----------------------------------------------------------------------
      BSUM = 0.0
      BSUMI = 0.0
      ISUM = 0.0
      ISUMI = 0.0
      NSUM = 0
      L0 = N / 2.0
      DO 30 IW = 1,NWIN
         IF ((WIN(1,IW).LE.N) .AND. (WIN(2,IW).GE.1)) THEN
            L1 = MAX (1, WIN(1,IW))
            L2 = MIN (N, WIN(2,IW))
            DO 20 LOOP = L1,L2
               BSUM = BSUM + VALU(LOOP)
               NSUM = NSUM + 1
               IF (NORD.GT.0) THEN
                  BSUMI = BSUMI + VALU(LOOP) * (LOOP - L0)
                  ISUM = ISUM + (LOOP - L0)
                  ISUMI = ISUMI + (LOOP - L0) * (LOOP - L0)
                  END IF
 20            CONTINUE
            END IF
 30      CONTINUE
C                                       subtract baseline
      IF (NSUM.GT.0) THEN
         IF ((NORD.LE.0) .OR. (NSUM.EQ.1)) THEN
            BO = BSUM / NSUM
            BS = 0.0
         ELSE
            BS = (BSUM * ISUM - NSUM * BSUMI) /
     *         (ISUM * ISUM - NSUM * ISUMI)
            BO = (ISUM * BSUMI - ISUMI * BSUM) /
     *         (ISUM * ISUM - NSUM * ISUMI)
            END IF
         DO 50 LOOP = 1,N
            VALU(LOOP) = VALU(LOOP) - (BO + BS * (LOOP - L0))
 50         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE RVBBLS (VALU, VALID, N, NORD, NWIN, WIN)
C-----------------------------------------------------------------------
C   Public
C   Real vector baseline fit and subtract with blanking
C   Inputs:
C      VALU   R(*)  Input vector
C      VALID  L(*)  True if corresponding element of IN is valid.
C      N      I     Number of elements
C      NORD   I     Baseline order : 0, 1 only
C      NWIN   I     Number baseline windows
C      WIN    I(2,*)  Windows
C   Outputs:
C      VALU   R(*)  Corrected vector
C      VALID  L(*)  True if corresponding element of IN still valid.
C-----------------------------------------------------------------------
      INTEGER   N, NORD, NWIN, WIN(2,*)
      REAL      VALU(N)
      LOGICAL   VALID(N)
C
      INTEGER   LOOP, L1, L2, IW, NSUM
      REAL      BSUM, BSUMI, ISUM, ISUMI, L0, BO, BS
C-----------------------------------------------------------------------
      BSUM = 0.0
      BSUMI = 0.0
      ISUM = 0.0
      ISUMI = 0.0
      NSUM = 0
      L0 = N / 2.0
      DO 30 IW = 1,NWIN
         IF ((WIN(1,IW).LE.N) .AND. (WIN(2,IW).GE.1)) THEN
            L1 = MAX (1, WIN(1,IW))
            L2 = MIN (N, WIN(2,IW))
            DO 20 LOOP = L1,L2
               IF (VALID(LOOP)) THEN
                  BSUM = BSUM + VALU(LOOP)
                  NSUM = NSUM + 1
                  IF (NORD.GT.0) THEN
                     BSUMI = BSUMI + VALU(LOOP) * (LOOP - L0)
                     ISUM = ISUM + (LOOP - L0)
                     ISUMI = ISUMI + (LOOP - L0) * (LOOP - L0)
                     END IF
                  END IF
 20            CONTINUE
            END IF
 30      CONTINUE
C                                       blank all if none
      IF (NSUM.LE.0) THEN
         DO 40 LOOP = 1,N
            VALID(LOOP) = .FALSE.
 40         CONTINUE
C                                       subtract baseline
      ELSE
         IF ((NORD.LE.0) .OR. (NSUM.EQ.1)) THEN
            BO = BSUM / NSUM
            BS = 0.0
         ELSE
            BS = (BSUM * ISUM - NSUM * BSUMI) /
     *         (ISUM * ISUM - NSUM * ISUMI)
            BO = (ISUM * BSUMI - ISUMI * BSUM) /
     *         (ISUM * ISUM - NSUM * ISUMI)
            END IF
         DO 50 LOOP = 1,N
            IF (VALID(LOOP)) VALU(LOOP) = VALU(LOOP) - (BO+BS*(LOOP-L0))
 50         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE RVBSC (IN, OUT, N, IC, CONVF)
C-----------------------------------------------------------------------
C   Public function
C   Real vector convolution with NO blanking
C   Inputs:
C      IN       R(*)   Input vector
C      N        I      Number points in IN
C      CONVF    R(*)   Convolving function (at least 2N+1)
C      IC       I      Subscript of center of CONVF (>= N+1)
C   Outputs:
C      OUT      R(*)   Convolution of IN with CONVF
C-----------------------------------------------------------------------
      INTEGER   N, IC
      REAL      IN(N), OUT(N), CONVF(*)
C
      INTEGER   LOOP, I, J
      REAL      BSUM
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         BSUM = 0.0
         DO 20 I = 1,N
            J = I - LOOP + IC
            BSUM = BSUM + IN(I) * CONVF(J)
 20         CONTINUE
         OUT(LOOP) = BSUM
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RVBBSC (IN, VALIN, OUT, VALOUT, N, IC, CONVF)
C-----------------------------------------------------------------------
C   Public function
C   Real vector convolution with blanking
C   Inputs:
C      IN       R(*)   Input vector
C      VALIN    L(*)   T => input point valid
C      N        I      Number points in IN
C      CONVF    R(*)   Convolving function (at least 2N+1)
C      IC       I      Subscript of center of CONVF > N+1
C   Outputs:
C      OUT      R(*)   Convolution of IN with CONVF
C      VALOUT   L(*)   T => output pixel is valid
C-----------------------------------------------------------------------
      INTEGER   N, IC
      REAL      IN(N), OUT(N), CONVF(*)
      LOGICAL   VALIN(N), VALOUT(N)
C
      INTEGER   LOOP, I, NSUM, J
      REAL      BSUM
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALOUT(LOOP) = VALIN(LOOP)
         OUT(LOOP) = IN(LOOP)
         IF (VALOUT(LOOP)) THEN
            BSUM = 0.0
            NSUM = 0
            DO 20 I = 1,N
               IF (VALIN(I)) THEN
                  J = I - LOOP + IC
                  BSUM = BSUM + IN(I) * CONVF(J)
                  NSUM = NSUM + 1
                  END IF
 20            CONTINUE
            IF (NSUM.LE.0) THEN
               VALOUT(LOOP) = .FALSE.
            ELSE
               OUT(LOOP) = BSUM
               END IF
            END IF
 100     CONTINUE
C
 999  RETURN
      END
