      SUBROUTINE GETCTL (CATD, FACT2, IRET)
C-----------------------------------------------------------------------
C! Determine Stokes' type of Clean map and other modeling info.
C# Map UV Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999, 2003, 2014
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
C   GETCTL determines the stokes' type of the CLEAN map and determines
C   necessary control information to subtract the model from the proper
C   visibilities.
C   Input:
C      CATD    D(128)  UV data catalog header
C   Input: via COMMON,
C      KLNBLK  I(256)  CLEAN map catalog header
C   Output:
C      FACT2   R(2)    Factor for each correlator.
C      IRET    I       Return error code.  0 => OK
C                                       1 => data incompatible
C                                       2 => improper CLEAN stokes.
C   Output: via common
C      VOFF    I    Vis. offset (words) of first correlator
C      NSTOK   I    Number of correlators to subtract (1 or 2)
C      MSGTXT  C    First character is type of polarization processed.
C                   Message suppressed if MSGSUP  >= 31900.
C-----------------------------------------------------------------------
      DOUBLE PRECISION CATD(128)
      REAL      FACT2(*)
      INTEGER   IRET
C
      CHARACTER STOKES*8, KSTOKE(4)*1, CHTMP*2
      HOLLERITH CLNH(256)
      INTEGER   ISOFF, IERR, DROUND
      LOGICAL   ISUV
      REAL      CLNR(256)
      DOUBLE PRECISION CLND(128)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      EQUIVALENCE  (KLNBLK, CLNR, CLNH, CLND)
      DATA STOKES, KSTOKE /'STOKES  ', 'I','Q','U','V'/
C-----------------------------------------------------------------------
C                                       See if "CLEAN" file really uv
C                                       data.
      CALL H2CHR (2, KHPTYO, CLNH(KHPTY), CHTMP)
      ISUV = CHTMP.EQ.'UV'
C                                       Determine CLEAN stokes type
      IRET = 3
      ISOFF = 0
      CALL AXEFND (8, STOKES, KICTPN, CLNH(KHCTP), ISOFF, IERR)
      KSTOK = DROUND (CLND(KDCRV+ISOFF))
      IF (ISUV) KSTOK = - DROUND (CLND(KDCRV+ISOFF))
C                                       Improper stokes type.
      IF ((IERR.NE.0) .OR. (KSTOK.GT.4) .OR. (KSTOK.EQ.0) .OR.
     *   (KSTOK.LT.-6) .OR. (KSTOK.EQ.-3) .OR. (KSTOK.EQ.-4)) THEN
         IF (IERR.NE.0) KSTOK = 0
         WRITE (MSGTXT,1020) KSTOK
         GO TO 990
         END IF
      IRET = 2
C                                       Write message about stokes.
C                                       model is Rpol
      IF (KSTOK.EQ.-1) THEN
C                                       but data are Lpol
         IF (ICOR0.LE.-2) GO TO 330
         IF (NCOR.EQ.1) THEN
            MSGTXT = 'Rpol model processed'
         ELSE
            MSGTXT = 'Ipol processed from Rpol model'
            END IF
C                                       model is Lpol
      ELSE IF (KSTOK.EQ.-2) THEN
C                                       but data are Rpol
         IF ((ICOR0.EQ.-1) .AND. (NCOR.EQ.1)) GO TO 330
         IF (NCOR.EQ.1) THEN
            MSGTXT = 'Lpol model processed'
         ELSE
            MSGTXT = 'Ipol processed from Lpol model'
            END IF
C                                       model is XXpol
      ELSE IF (KSTOK.EQ.-5) THEN
C                                       but data are YYpol
         IF (ICOR0.LE.-6) GO TO 330
         IF (NCOR.EQ.1) THEN
            MSGTXT = 'XXpol model processed'
         ELSE
            MSGTXT = 'Ipol processed from XXpol model'
            END IF
C                                       model is YYpol
      ELSE IF (KSTOK.EQ.-6) THEN
C                                       but data are XXpol
         IF ((ICOR0.EQ.-5) .AND. (NCOR.EQ.1)) GO TO 330
         IF (NCOR.EQ.1) THEN
            MSGTXT = 'YYpol model processed'
         ELSE
            MSGTXT = 'Ipol processed from YYpol model'
            END IF
C                                       Stokes'
      ELSE
         WRITE (MSGTXT,1021) KSTOKE(KSTOK)
         END IF
      IF (MSGSUP.LT.31900) CALL MSGWRT (3)
      KSTOK = MAX(1, KSTOK)
C                                       Initialize output
      FACT2(2) = 0.0
C                                       True stokes uv data.
      IF (CATD(KDCRV+JLOCS).GT.0.0D0) THEN
         FACT2(1) = 1.0
         NSTOK = 1
         IF (ICOR0.GT.KSTOK) GO TO 330
         VOFF = (KSTOK - ICOR0) * INCS
C                                       Correlator (RR,LL etc) stokes
      ELSE IF (ICOR0.GT.-5) THEN
         NSTOK = 2
C                                       IPOL
         IF (KSTOK.EQ.1) THEN
            FACT2(1) = 1.0
            FACT2(2) = 1.0
            IF (DOALL4) THEN
               NSTOK = NCOR
               FACT2(3) = 1.0
               FACT2(4) = 1.0
               END IF
            IF (ABS (ICOR0).GT.2) GO TO 330
            VOFF = 0
            NSTOK = MIN (NSTOK, NCOR)
            IF (ABS(ICOR0).EQ.2) NSTOK = 1
C                                       If only 1 stokes, second fact=0
            IF (NSTOK.LE.1) FACT2(2) = 0.0
C                                       QPOL
         ELSE IF (KSTOK.EQ.2) THEN
            FACT2(1) = 1.0
            FACT2(2) = 1.0
            IF (ABS (ICOR0).GT.3) GO TO 330
            VOFF = (3 - ABS (ICOR0)) * INCS
C                                       UPOL
         ELSE IF (KSTOK.EQ.3) THEN
            FACT2(1) = 1.0
            FACT2(2) = -1.0
            IF (ABS (ICOR0).GT.3) GO TO 330
            VOFF = (3 - ABS (ICOR0)) * INCS
C                                       VPOL
         ELSE IF (KSTOK.EQ.4) THEN
            FACT2(1) = 1.0
            FACT2(2) = -1.0
            IF (ABS (ICOR0).GT.1) GO TO 330
            VOFF = 0
            END IF
C                                       Correlator (XX, YY etc) stokes
      ELSE
         NSTOK = 2
C                                       IPOL
         IF (KSTOK.EQ.1) THEN
            FACT2(1) = 1.0
            FACT2(2) = 1.0
            IF (DOALL4) THEN
               NSTOK = NCOR
               FACT2(3) = 1.0
               FACT2(4) = 1.0
               END IF
            IF (ABS (ICOR0).GT.6) GO TO 330
            VOFF = 0
            NSTOK = MIN (NSTOK, NCOR)
            IF (ABS(ICOR0).EQ.6) NSTOK = 1
C                                       If only 1 stokes, second fact=0
            IF (NSTOK.LE.1) FACT2(2) = 0.0
C                                       QPOL, UPOL, VPOL
C                                       don't know how
         ELSE
            GO TO 330
            END IF
         END IF
      IRET = 0
      GO TO 999
C                                       error messages
 330  MSGTXT = 'GETCTL: MAP STOKES TYPE INCOMPATIBLE WITH UV DATA'
      CALL MSGWRT (8)
      WRITE (MSGTXT,1330) KSTOK, ICOR0, NCOR
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('UNKNOWN CLEAN MAP STOKES TYPE =',I4)
 1021 FORMAT (A1,' Polarization model processed')
 1330 FORMAT ('KSTOK=',I5,', ICOR0=',I5,', NCOR=',I5)
      END

