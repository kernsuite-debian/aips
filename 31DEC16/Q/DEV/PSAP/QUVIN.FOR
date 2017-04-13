      SUBROUTINE QUVIN (UV, VIS, WORK, L, M, LROW, NS, INCS, NF, INCF,
     *   CNT, FLAG)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Interpolate visibility model from a grid.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2000, 2006, 2012
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
C   Vector Version
C   Interpolates model visibility values from a grid, corrects the
C   model for the phase of the z*w term and adds the model to the data.
C   Inputs:
C      UV    I  Base address of U,V vector
C      VIS   I  Base address of visibility
C      WORK  I  Base address of work vector length CNT*4 words
C      L     I  Length of visibility record
C               interpolation fns tabulated every 1/200 cell.
C      M     I  Width of interpolation fn support size,  also
C               the number of rows kept in AP (must be odd)
C      LROW  I  length of a row (V)
C      NS    I  Number of correlators (Stokes' parm.) to process.
C      INCS  I  Increment between correlators.
C      NF    I  Number of frequencies to process.
C      INCF  I  Increment between frequencies.
C      CNT   I  Number of visibility points.
C      FLAG  I  1 => add model, -1 => add i (SQRT(-1)) times
C               model.
C   Also expects necessary constants in following AP locations:
C          0 = U scaling to cells.
C          1 = V scaling to cells
C          2 = DXC = -2 * Pi * delta RA (in cells)
C          3 = DYC = -2 * Pi * delta DEC (cells)
C          4 = DZC = -2 * Pi * delta Z (in radians) for 1st
C              frequency channel.
C          5 = Used as work space.
C          6 = Used as work space.
C          7 = ROW = lowest central row of the interpolation (INTEGER)
C          8 = G = Address of base address of grid (INTEGER)
C          9 = CX = Address of base address of row interpolation
C              function (INTEGER)
C          10 = Used as work space.
C          11 = Used as work space.
C          15 = Factor for data (0 -> return only model, 1 data+-model)
C          16...15+NS = an array of correlator factors.
C              To subtract the model these should be negative.
C          20...20+NF-2 = differential frequency scaling table
C               for channels after the first one.
C               FREQ(n) = (1+MD(19+n-1)) * FREQ(n-1) for n>1
C-----------------------------------------------------------------------
      INTEGER   UV, VIS, WORK, L, M, LROW, NS, INCS, NF, INCF, CNT,
     *   FLAG
C
      INTEGER   IIF, UV1, UV2, IWORK1, IWORK2, VVIS, WORK1, VVIS1,
     *   JLOOP, FVIS, LOOP, IVAL1, IVAL2, IVAL3
      LONGINT   IUV, IW, IUV1, IW1, IW2, CORR, IV
      DOUBLE PRECISION FAC0, FAC1, FAC2, FAC3, FAC4, FACC, VALUE
      LOGICAL   SHIFT
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF ((CNT.LE.0) .OR. (NS.LE.0) .OR. (NF.LE.0)) GO TO 999
C                                       Setup
      UV1 = UV + 1
      UV2 = UV1 + 1
      WORK1 = WORK + 1
      IWORK1 = WORK + 2
      IWORK2 = IWORK1 + 1
      FVIS = VIS
      FAC0 = APCORE(PSAPOF)
      FAC1 = APCORE(1+PSAPOF)
      FAC2 = APCORE(2+PSAPOF)
      FAC3 = APCORE(3+PSAPOF)
      FAC4 = APCORE(4+PSAPOF)
C                                       Check if shift wanted
      SHIFT = (ABS (FAC2).GT.1.0E-20) .OR. (ABS (FAC3).GT.1.0E-20)
C                                       Setup U,V,W
      IUV = UV + PSAPOF
      IUV1 = UV1 + PSAPOF
      IW = WORK + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,CNT
C                                       Save u,v
         APCORE(IW) = APCORE(IUV)
         APCORE(IW+1) = APCORE(IUV+1)
C                                       Scale u,v to cells.
         APCORE(IUV) = APCORE(IUV) * FAC0
         APCORE(IUV1) = APCORE(IUV1) * FAC1
C                                       Update pointers
         IUV = IUV + L
         IUV1 = IUV1 + L
         IW = IW + 4
 100     CONTINUE
C                                       Loop over frequency
      DO 800 IIF = 1,NF
         VVIS = FVIS
         CORR = 15 + PSAPOF
         IF (IIF.GT.1) THEN
C                                       Scale u and v
            VALUE = APCORE(18+PSAPOF+IIF)
            FAC4 = FAC4 + FAC4 * VALUE
            IUV = UV + PSAPOF
            DO 110 LOOP = 1,CNT
               APCORE(IUV) = APCORE (IUV) + VALUE * APCORE(IUV)
               APCORE(IUV+1) = APCORE (IUV+1) + VALUE * APCORE(IUV+1)
               IUV = IUV + L
 110           CONTINUE
            END IF
C                                       Shift position (z*w) term.
         IF (SHIFT) THEN
C                                       Fill array for Shift
            IUV = UV + PSAPOF
            IW2 = IWORK1 + 1 + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 120 LOOP = 1,CNT
C                                       Set phase for shift
               APCORE(IW2) = APCORE(IUV) * FAC2 +
     *                       APCORE(IUV+1) * FAC3 +
     *                       APCORE(IUV+2) * FAC4
               IUV = IUV + L
               IW2 = IW2 + 4
 120           CONTINUE
C                                       Calc Cos and Sin of phase
            CALL QRECT1 (IWORK1, 4, IWORK1, 4, CNT)
         ELSE
C                                       Else, No shift
            IW1 = IWORK1 + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 130 LOOP = 1,CNT
               APCORE(IW1) = 1.0
               APCORE(IW1+1) = 0.0
               IW1 = IW1 + 4
 130           CONTINUE
C                                       end if shift
            END IF
C                                       Interpolate from grid.
         IVAL1 = APCORI(8+PSAPOF)
         IVAL2 = APCORI(9+PSAPOF)
         IVAL3 = APCORI(7+PSAPOF)
         CALL QINTG (UV, IWORK1, IWORK1, IVAL1, IVAL2, M, LROW, L, CNT,
     *      IVAL3)
C                                       Model values left in IWORK1
C                                       Conjugate if necessary.
         IF (FLAG.LE.0) THEN
            IW1 = IWORK1 + 1 + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 300 LOOP = 1,CNT
               APCORE(IW1) = -APCORE(IW1)
               IW1 = IW1 + 4
 300           CONTINUE
            END IF
C                                       Loop over correlators.
         DO 600 JLOOP = 1,NS
C                                       Add component parts.
            FACC = APCORE(CORR+1)
            IV = VVIS + PSAPOF
            IW1 = IWORK1 + PSAPOF
C                                       just model, drop data
            IF (APCORE(15+PSAPOF).EQ.0.0) THEN
               DO 500 LOOP = 1,CNT
                  APCORE(IV) = APCORE(IW1) * FACC
                  APCORE(IV+1) = APCORE(IW1+1) * FACC
                  IV = IV + L
                  IW1 = IW1 + 4
 500              CONTINUE
C                                       data +- model
            ELSE
      INCLUDE 'INCS:ZVND.INC'
               DO 510 LOOP = 1,CNT
                  APCORE(IV) = APCORE(IV) + APCORE(IW1) * FACC
                  APCORE(IV+1) = APCORE(IV+1) + APCORE(IW1+1) * FACC
                  IV = IV + L
                  IW1 = IW1 + 4
 510              CONTINUE
               END IF
C                                       Update pointers.
            CORR = CORR + 1
            VVIS = VVIS + INCS
            VVIS1 = VVIS + 1
 600        CONTINUE
C                                       Update pointers for next freq.
         FVIS = FVIS + INCF
 800     CONTINUE
C                                       Restore u,v
      IW = WORK + PSAPOF
      IUV = UV + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
      DO 850 LOOP = 1,CNT
         APCORE(IUV) = APCORE(IW)
         APCORE(IUV+1) = APCORE(IW+1)
         IW = IW + 4
         IUV = IUV + L
 850     CONTINUE
C
 999  RETURN
      END
