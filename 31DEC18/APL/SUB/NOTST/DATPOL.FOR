      SUBROUTINE DATPOL (IA1, IA2, TIME, VIS, IERR)
C-----------------------------------------------------------------------
C! Apply polarization corrections to data.
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2007, 2010, 2012-2014, 2017-2018
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
C   Applies polarization correction to data.  This operation is only
C   really defined if KNCOR = 4.  However, it will also apply the
C   parallel hand corrections if KNCOR = 2.
C   Inputs:
C      IA1     I      First antenna number
C      IA2     I      Second antenna number
C      TIME    R      Time in days; used for parallactic angle.
C   Inputs from common in DSEL.INC
C      PARTIM  R      Time of current parallactic angles.
C      PARAGL  R(2,*) Cos and sin of the parallactic angles of
C                     antennas.
C      PARSOU  I      Source ID for current parallactic angles.
C      IFR     R(*)   Ionospheric Faraday RM for each antenna
C      LAMBDA  R(*)   Wavelength of each channel and IF
C      STNPST  C*8    Polarization model type NOW IGNORED
C      SOLTYP  I      Model type numeric code NOW USED (from POLSET)
C      POLCAL  R(2,*) Polarization correction
C                     Values in order:
C                     By baseline
C                        By IF (EIF-BIF+1)
C                           A 4x4 complex matrix to be multiplied by
C                               the observed polarization vector
C                               (RR,LL,RL,LR or XX,YY,XY,YX) to produce
C                                the corrected data.
C                    Indexing scheme: an entry defined by ant1<ant2
C                    starts in element:
C      (((ant1-1)*numant-((ant1+1)*ant1)/2 + ant2) - 1) + 1
C   Input/output:
C      VIS     R(*)  Input visibility array in form of "RR,LL,LR,RL"
C              or "XX,YY,XY,YY"  on output = "RR,LL,RL,LR"
C   NOTE: Uses AIPS LUN 49.
C     Output:
C      IERR         I    Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IA1, IA2, IERR
      REAL      TIME, VIS(*)
C
      INTEGER   IIF, IPOL, IFQ, I, LUN, INCPX, IOFF, JOFF, INDEX, JNDEX,
     *   BLNDX, LIMIT, LENTRY, BLPNT, IPNT, PNT(8), OFF(32), OFF2(8),
     *   STORDR, STINC, LOFF, JRL, JLR
      REAL      GR, GI, TR, TI, VTEMP(2,16), XTEMP(2,16), VTEMPX(32),
     *   DPANG, GR1, GI1, TINC, PAINC
      LOGICAL   DOORI, WFLAG
      INCLUDE 'INCS:PUVD.INC'
      REAL      PANGO(MAXANT), PDBG(1000)
      LONGINT   PP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DPDC.INC'
      EQUIVALENCE (VTEMP, VTEMPX)
C
      SAVE OFF, OFF2, PANGO, DOORI, INCPX, STINC
C
      DATA PNT /1,2,1,2,1,2,1,2/
      DATA PANGO /MAXANT*100.0/
      DATA LUN /49/
C                                       Time increment (days) to update
C                                       correction matrices (0.1 sec.)
      DATA TINC /1.15E-6/
C                                       Parallac. angle change(rad) to
C                                       update corr. matrices (0 deg)
      DATA PAINC /0.0/
C-----------------------------------------------------------------------
C                                       Orientation-ellipticity?
C                                       DOORI = STNPST.EQ.'ORI-ELP '
      DOORI = SOLTYP.EQ.2
      IF (BCHANS.LE.0) BCHANS = BCHAN
      IF (ECHANS.LE.0) ECHANS = ECHAN
C                                       This routine has no defined
C                                       function at all unless
C                                       KNCOR = 2 or 4.
      IF ((KNCOR.NE.2) .AND. (KNCOR.NE.4)) GO TO 999
C                                       See if parallactic angles
C                                       current
      IF ((CURSOU.NE.PARSOU) .OR. (ABS(TIME-PARTIM).GT.TINC)) THEN
C                                       Source info
         IF ((CURSOU.NE.PARSOU) .OR. (CURSOU.NE.IDSOUR)) THEN
            CALL GETSOU (CURSOU, IUDISK, IUCNO, CATUV, LUN, IERR)
            IF (IERR.NE.0) GO TO 999
            PANGO(1) = 1000.0
            END IF
         PARSOU = CURSOU
C                                       Parallactic angles
         CALL PARANG (TIME, PANGLE)
         DPANG = 0.0
         DO 10 I = 1,NSTNS
            PARAGL(1,I) = COS (PANGLE(I))
            PARAGL(2,I) = -SIN (PANGLE(I))
            DPANG = MAX (DPANG, ABS (PANGO(I)-PANGLE(I)))
 10         CONTINUE
C                                       Update par. angle time
         PARTIM = TIME
C                                       Set visibility increment
         INCPX = CATUV(KINAX)
C                                       Compressed data expanded
         IF (INCPX.EQ.1) INCPX = 3
C                                       Find order of Stokes axis in
C                                       input data.
         CALL AXEFND (8, 'STOKES  ', CATUV(KIDIM), CATUV(KHCTP), STORDR,
     *      IERR)
         IF ((IERR.NE.0) .OR. (STORDR.LT.1)) THEN
            MSGTXT = 'DATPOL: STOKES AXIS NOT FOUND'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Find Stokes Increment
         STINC = INCPX
         IF (STORDR.GT.1) STINC = STINC * MAX (1, CATUV(KINAX+1))
         IF (STORDR.GT.2) STINC = STINC * MAX (1, CATUV(KINAX+2))
         IF (STORDR.GT.3) STINC = STINC * MAX (1, CATUV(KINAX+3))
         IF (STORDR.GT.4) STINC = STINC * MAX (1, CATUV(KINAX+4))
C                                       Offset array for vis data
         LIMIT = 8 * KNCOR
         IPNT = -1
         DO 50 IPOL = 1,LIMIT,8
            OFF(IPOL) = IPNT + 1
            OFF(IPOL+1) = IPNT + 2
            OFF(IPOL+2) = IPNT + 1
            OFF(IPOL+3) = IPNT + 2
            OFF(IPOL+4) = IPNT + 1
            OFF(IPOL+5) = IPNT + 2
            OFF(IPOL+6) = IPNT + 1
            OFF(IPOL+7) = IPNT + 2
            IPNT = IPNT + STINC
 50         CONTINUE
         LIMIT = 2 * KNCOR
         IPNT = -1
         DO 60 IPOL = 1,LIMIT,2
            OFF2(IPOL) = IPNT + 1
            OFF2(IPOL+1) = IPNT + 2
            IPNT = IPNT + STINC
 60         CONTINUE
C                                       Time for a new set of matrices
C                                       for linear feeds? Check change
C                                       in parallactic angle.
         IF ((SOLTYP.GT.1) .AND. (DPANG.GT.PAINC)) THEN

C                                       Save parallactic angles
            DO 70 I = 1,NSTNS
               PANGO(I) = PANGLE(I)
 70            CONTINUE
C                                       Use central channel.
            IFQ = (BCHAN + ECHAN)/2
C                                       Get matrices for
C                                       orientation-ellipticity model.
            IF (DOORI) THEN
               CALL ORIPOL
C                                       Get matrices for lin. pol.
            ELSE IF (SOLTYP.EQ.3) THEN
               CALL LXYPOL (IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Get  matrices for VLBI
            ELSE IF (SOLTYP.EQ.4) THEN
               CALL VLBPOL
               END IF
            END IF
         END IF
C                                       Set baseline index
      BLNDX = ((IA1-1)*NSTNS) - (((IA1-1)*IA1)/2) + IA2
      LENTRY = 32 * PCLIF  * PCLCH
C                                       Loop thru IF
      DO 400 IIF = BIF,EIF
         IOFF = (IIF-1) * KNCIF
         BLPNT = LENTRY * (BLNDX-1) + (IIF-BIF)*32*PCLCH + 1
C                                       Loop thru channels
         DO 300 IFQ = BCHANS,ECHANS
            JOFF = ((IFQ-1) * KNCF + IOFF) * INCPX + 1
C                                       Loop thru polarization
C                                       Deal with case of missing
C                                       parallel poln; use one present
C                                       for correction.
            WFLAG = .FALSE.
C                                       1st par. poln missing
            IF (VIS(JOFF+2).LE.0.0) THEN
               VIS(JOFF) = VIS(JOFF+OFF2(3))
               VIS(JOFF+1) = VIS(JOFF+OFF2(4))
               VIS(JOFF+2) = 0.0
               END IF
C                                       2nd par. poln missing
            IF (VIS(JOFF+OFF2(3)+2).LE.0.0) THEN
               VIS(JOFF+OFF2(3)) = VIS(JOFF)
               VIS(JOFF+OFF2(4)) = VIS(JOFF+1)
               VIS(JOFF+OFF2(3)+2) = 0.0
C                                       Flag all if neither parallel
C                                       poln present.
               IF (VIS(JOFF+2).LE.0.0) WFLAG = .TRUE.
               END IF
C                                       Check for missing cross-hand
C                                       data
            IF (KNCOR.GT.2) THEN
               JRL = JOFF + OFF2(5)
               JLR = JOFF + OFF2(7)
C                                       Check if both missing
               IF ((VIS(JRL+2).LE.0.0).AND.(VIS(JLR+2).LE.0.0)) THEN
C                                       Zero cross-hand data used in
C                                       correction
                  CALL RFILL (2, 0.0, VIS(JRL))
                  CALL RFILL (2, 0.0, VIS(JLR))
C                                       1st cross-hand missing
               ELSE IF (VIS(JRL+2).LE.0.0) THEN
C                                       Use RL=conjg(LR) approx.
                  VIS(JRL) = VIS(JLR)
                  VIS(JRL+1) = -VIS(JLR+1)
C                                       2nd cross-hand missing
               ELSE IF (VIS(JLR+2).LE.0.0) THEN
C                                       Use LR=conjg(RL) approx.
                  VIS(JLR) = VIS(JRL)
                  VIS(JLR+1) = -VIS(JRL+1)
                  END IF
               END IF
C                                       If DOPOL > 2 check for
C                                       any missing correlations
            IF ((MOD(DOPOL,10).EQ.3) .OR. (MOD(DOPOL,10).EQ.8)) THEN
               LIMIT = 2 * KNCOR
               DO 100 IPOL = 1, LIMIT, 2
                  IF (VIS(JOFF+OFF2(IPOL)+2).LE.0.0) WFLAG = .TRUE.
100               CONTINUE
               END IF
C                                       Check for blanked IFR
            IF ((IFR(IA1).EQ.FBLANK) .OR. (IFR(IA2).EQ.FBLANK)) THEN
               WFLAG = .TRUE.
               END IF
C                                       Flag all output data if
C                                       both par. hands missing or
C                                       (DOPOL > 2) and any polzn.
C                                       correlations are missing.
C                                       Also flag output data if
C                                       IFR corrections are blanked
            IF (WFLAG) THEN
               LIMIT = 2 * KNCOR
               DO 120 IPOL = 1, LIMIT, 2
                  VIS(JOFF+OFF2(IPOL)+2) = 0.0
120               CONTINUE
               END IF
C                                       Save old data
            LIMIT = 8 * KNCOR
            DO 150 IPOL = 1,LIMIT
               JNDEX = JOFF + OFF(IPOL)
               VTEMPX(IPOL) = VIS(JNDEX)
 150           CONTINUE
C                                       Clear XTEMP (in case KNCOR < 4)
            CALL RFILL (2*16, 0.0, XTEMP)
C                                       Matrix x vector multiply
            PP = BLPNT + PPOLCL
            LIMIT = 4 * KNCOR
            CALL RCOPY (2*LIMIT, POLCAL(PP), PDBG)
            DO 200 IPOL = 1,LIMIT
               IF ((POLCAL(PP).NE.FBLANK) .AND.
     *            (POLCAL(PP+1).NE.FBLANK)) THEN
                  XTEMP(1,IPOL) = VTEMP(1,IPOL) * POLCAL(PP) -
     *               VTEMP(2,IPOL) * POLCAL(PP+1)
                  XTEMP(2,IPOL) = VTEMP(1,IPOL) * POLCAL(PP+1) +
     *               VTEMP(2,IPOL) * POLCAL(PP)
               ELSE
                  XTEMP(1,IPOL) = FBLANK
                  XTEMP(2,IPOL) = FBLANK
                  END IF
               PP = PP + 2
 200           CONTINUE
            IF (PDVER.GT.0) BLPNT = BLPNT + 32
C                                       sum
            INDEX = 1
            LIMIT = KNCOR * 2
            DO 250 IPOL = 1,LIMIT
               INDEX = ((IPOL-1)/2) + 1
               IPNT = PNT(IPOL)
               JNDEX = JOFF + OFF2(IPOL)
               IF ((XTEMP(IPNT,INDEX).NE.FBLANK) .AND.
     *            (XTEMP(IPNT,INDEX+4).NE.FBLANK) .AND.
     *            (XTEMP(IPNT,INDEX+8).NE.FBLANK) .AND.
     *            (XTEMP(IPNT,INDEX+12).NE.FBLANK)) THEN
                  VIS(JNDEX) = XTEMP(IPNT,INDEX) + XTEMP(IPNT,INDEX+4) +
     *               XTEMP(IPNT,INDEX+8) + XTEMP(IPNT,INDEX+12)
               ELSE
                  VIS(JNDEX) = 0.0
                  VIS(JOFF+OFF2(IPOL)+2) = 0.0
                  END IF
 250           CONTINUE
C                                       Done if 'ORI-ELI'
            IF (DOORI) GO TO 300
C                                       Done if not 4 polarizations
            IF (KNCOR.LT.4) GO TO 300
C                                       special for linears
C                                       DGHEAD makes DOPOL>0
C                                       be >10 for linear input
            IF (DOPOL.GT.10) GO TO 300
            IF (ICOR0.LT.-4) GO TO 300
C                                       PARALLACTIC angle
            GR = PARAGL(1,IA1) * PARAGL(1,IA2) - PARAGL(2,IA1) *
     *         PARAGL(2,IA2)
            GI = PARAGL(1,IA1) * PARAGL(2,IA2) + PARAGL(2,IA1) *
     *         PARAGL(1,IA2)
C                                       Correct RL,LR for parallactic
C                                       angle and ionospheric Faraday
C                                       rotation:
            LOFF = (IIF - 1) * NLAMDA + IFQ
            GR1 = GR * COS (LAMBDA(LOFF)**2 * (IFR(IA1) + IFR(IA2))) -
     *         GI * SIN (LAMBDA(LOFF)**2 * (IFR(IA1) + IFR(IA2)))
            GI1 = GI * COS (LAMBDA(LOFF)**2 * (IFR(IA1) + IFR(IA2))) +
     *         GR * SIN (LAMBDA(LOFF)**2 * (IFR(IA1) + IFR(IA2)))
C                                       Correct RL
            IPNT = 5
            TR = VIS(JOFF+OFF2(IPNT))
            TI = VIS(JOFF+OFF2(IPNT+1))
            VIS(JOFF+OFF2(IPNT)) = TR * GR1 + TI * GI1
            VIS(JOFF+OFF2(IPNT+1)) = TI * GR1 - TR * GI1
C                                       Correct LR
            IPNT = 7
            TR = VIS(JOFF+OFF2(IPNT))
            TI = VIS(JOFF+OFF2(IPNT+1))
            VIS(JOFF+OFF2(IPNT)) = TR * GR1 - TI * GI1
            VIS(JOFF+OFF2(IPNT+1)) =  TI * GR1 + TR * GI1
 300        CONTINUE
 400     CONTINUE
C
 999  RETURN
      END
