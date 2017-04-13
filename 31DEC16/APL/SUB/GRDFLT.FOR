      SUBROUTINE GRDFLT (CTYPX, CTYPY, XPARM, YPARM)
C-----------------------------------------------------------------------
C! Sets default gridding convolution functions.
C# UV Map Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999
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
C   GRDFLT determines default parameters for the convolution functions
C   If no convolving type is chosen, a Spheriodal is picked.  Otherwise
C   any unspecified values ( = 0.0) will be set to some value.
C   Input/output:
C      CTYPX   I       Convolution types for X and Y direction
C      CTYPY   I          1 = pill box
C                         2 = exponential
C                         3 = sinc
C                         4 = exponential * sinc
C                         5 = spheroidal function
C                         6 = exponential * BesselJ1(x) / x
C                         11 - 16 as 1-6 but evaluated for circular
C                         abs(type) is used
C      XPARM   R(10)   Parameters for the convolution fns.
C      YPARM   R(10)      (1) = support radius (cells)
C-----------------------------------------------------------------------
      INTEGER   CTYPX, CTYPY
      REAL      XPARM(10), YPARM(10)
C
      CHARACTER CHTYPS(6)*16
      INTEGER   NUMPRM(6), I, NKML, K, LFORM, LSIGN, CODE, DONMSG
      LOGICAL   SAME
      INCLUDE 'INCS:DMSG.INC'
      SAVE DONMSG
      DATA NUMPRM /1, 3, 2, 4, 2, 4/
      DATA CHTYPS /'PILL BOX', 'EXPONENTIAL', 'SIN(X) / X',
     *   'EXP * SIN(X)/X', 'SPHEROIDAL', 'EXP * BESJ1(X)/X'/
      DATA DONMSG /0/
C-----------------------------------------------------------------------
C                                       Default = type 5
      LSIGN = 1
      IF (CTYPX.LT.0) LSIGN = -1
      CTYPX = ABS (CTYPX)
      LFORM = 0
      IF (CTYPX.GT.10) LFORM = 1
      CTYPX = CTYPX - 10*LFORM
C                                       Pillbox
      IF (CTYPX.EQ.1) THEN
         IF (XPARM(1).LE.0.0) XPARM(1) = 0.5
C                                       Exponential.
      ELSE IF (CTYPX.EQ.2) THEN
         IF (XPARM(1).LE.0.0) XPARM(1) = 3.0
         IF (XPARM(2).LE.0.0) XPARM(2) = 1.00
         IF (XPARM(3).LE.0.0) XPARM(3) = 2.00
C                                       Sinc.
      ELSE IF (CTYPX.EQ.3) THEN
         IF (XPARM(1).LE.0.0) XPARM(1) = 3.0
         IF (XPARM(2).LE.0.0) XPARM(2) = 1.14
C                                       Exponential * sinc
      ELSE IF (CTYPX.EQ.4) THEN
         IF (XPARM(1).LE.0.0) XPARM(1) = 3.0
         IF (XPARM(2).LE.0.0) XPARM(2) = 1.55
         IF (XPARM(3).LE.0.0) XPARM(3) = 2.52
         IF (XPARM(4).LE.0.0) XPARM(4) = 2.00
C                                       spheroidal function
      ELSE IF (CTYPX.EQ.5) THEN
         CTYPX = 5
         IF (XPARM(1).LE.0.0) XPARM(1) = 3.0
         IF (XPARM(2).LE.0.0) XPARM(2) = 1.0
C                                       Exponential * BessJ1(x)/x
      ELSE IF (CTYPX.EQ.6) THEN
         IF (XPARM(1).LE.0.0) XPARM(1) = 3.0
         IF (XPARM(2).LE.0.0) XPARM(2) = 1.55
         IF (XPARM(3).LE.0.0) XPARM(3) = 2.52
         IF (XPARM(4).LE.0.0) XPARM(4) = 2.00
C                                       spheroidal function
      ELSE
         CTYPX = 5
         XPARM(1) = 3.0
         XPARM(2) = 1.0
         END IF
      CTYPX = (CTYPX + 10 * LFORM) * LSIGN
      IF (LFORM.EQ.0) THEN
         XPARM(5) = 100.
         YPARM(5) = 100.
         END IF
      IF ((LFORM.EQ.1) .AND. (XPARM(5).LE.4.0)) XPARM(5) = 20.
C                                       Y defaults to X
      LSIGN = 1
      IF (CTYPY.LT.0) LSIGN = -1
      CTYPY = LSIGN * CTYPY
      IF ((CTYPY.LE.0) .OR. (CTYPY.GT.5) .OR. (LFORM.EQ.1)) THEN
         CTYPY = CTYPX * LSIGN
         CALL RCOPY (10, XPARM, YPARM)
C                                       Pillbox
      ELSE IF (CTYPY.EQ.1) THEN
         IF (YPARM(1).LE.0.0) YPARM(1) = 0.5
C                                       Exponential
      ELSE IF (CTYPY.EQ.2) THEN
         IF (YPARM(1).LE.0.0) YPARM(1) = 3.0
         IF (YPARM(2).LE.0.0) YPARM(2) = 1.0
         IF (YPARM(3).LE.0.0) YPARM(3) = 2.0
C                                       Sinc
      ELSE IF (CTYPY.EQ.3) THEN
         IF (YPARM(1).LE.0.0) YPARM(1) = 3.0
         IF (YPARM(2).LE.0.0) YPARM(2) = 1.14
C                                       Exponential * sinc
      ELSE IF (CTYPY.EQ.4) THEN
         IF (YPARM(1).LE.0.0) YPARM(1) = 3.0
         IF (YPARM(2).LE.0.0) YPARM(2) = 1.55
         IF (YPARM(3).LE.0.0) YPARM(3) = 2.52
         IF (YPARM(4).LE.0.0) YPARM(4) = 2.00
C                                       Exponential * BessJ1(x)/x
      ELSE IF (CTYPY.EQ.6) THEN
         IF (YPARM(1).LE.0.0) YPARM(1) = 3.0
         IF (YPARM(2).LE.0.0) YPARM(2) = 1.55
         IF (YPARM(3).LE.0.0) YPARM(3) = 2.52
         IF (YPARM(4).LE.0.0) YPARM(4) = 2.00
C                                       Spheriodal function
      ELSE IF (CTYPY.EQ.5) THEN
         IF (YPARM(1).LE.0.0) YPARM(1) = 3.0
         IF (YPARM(2).LE.0.0) YPARM(2) = 1.0
         END IF
      CTYPY = LSIGN * CTYPY
C                                       Print parameters chosen.
      CODE = 100*CTYPY + CTYPX
      IF (DONMSG.NE.CODE) THEN
            IF (LFORM.EQ.0) THEN
               SAME = CTYPX.EQ.CTYPY
               DO 10 I = 1,5
                  SAME = SAME .AND. (ABS(XPARM(I)-YPARM(I)).LT.1.E-4)
 10               CONTINUE
            NKML = NUMPRM(CTYPX)
            IF (SAME) THEN
               WRITE (MSGTXT,1000) 'X and Y', CHTYPS(CTYPX)
               CALL MSGWRT (2)
               WRITE (MSGTXT,1001) 'X and Y', (XPARM(K), K = 1,NKML)
               CALL MSGWRT (2)
            ELSE
               WRITE (MSGTXT,1000) 'X', CHTYPS(CTYPX)
               CALL MSGWRT (2)
               WRITE (MSGTXT,1001) 'X', (XPARM(K), K = 1,NKML)
               CALL MSGWRT (2)
               NKML = NUMPRM(CTYPY)
               WRITE (MSGTXT,1000) 'Y', CHTYPS(CTYPY)
               CALL MSGWRT (2)
               WRITE (MSGTXT,1001) 'Y', (YPARM(K), K = 1,NKML)
               CALL MSGWRT (2)
               END IF
C                                       Circular
         ELSE
            NKML = NUMPRM(CTYPX-10)
            WRITE (MSGTXT,1000) 'Radial', CHTYPS(CTYPX-10)
            CALL MSGWRT (2)
            WRITE (MSGTXT,1001) 'Radial', (XPARM(K), K = 1,NKML)
            CALL MSGWRT (2)
            END IF
C                                       Sample interval
         I = XPARM(5) + 0.1
         WRITE (MSGTXT,1005) I
         CALL MSGWRT (2)
         END IF
      DONMSG = CODE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRDFLT: ',A,' convolution type = ',A)
 1001 FORMAT ('GRDFLT: ',A,' parms =',4F8.4)
 1005 FORMAT ('GRDFLT: convolution function sampled every 1/',I3,
     *   ' of a cell')
      END
