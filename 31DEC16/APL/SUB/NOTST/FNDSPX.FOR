      SUBROUTINE FNDSPX (DISK, CNO, SRC, FREQID, CATBLK, NTERM, SPIX,
     *   IERR)
C-----------------------------------------------------------------------
C! find spectral index of a source from SU table
C# UV Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2013-2014, 2017
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
C   FNDSPC fits the spectral index of the specified source to the fluxes
C   in the source table.
C   Finds the source info into DSOU.INC and the freq info
C   Requires UVPGET to have been called.
C   Inputs:
C      DISK     I      Disk number
C      CNO      I      Catalog number
C      SRC      I      Source number
C      FREQID   I      Freq ID number
C      CATBLK   I(*)   header
C      NTERM    I      =2 => return curvature in SPIX(3)
C   Outputs:
C      SPIX     R(3)   Spectral index: (1) Flux at 1 GHz
C                         (2) spectral index
C                         (3) curvature (wrt 1 GHz) or zero
C      IERR     I      Error code: 0 okay, 1 no answer, 2 IO troubles,
C                      3 input troubles
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, SRC, FREQID, CATBLK(256), NTERM, IERR
      REAL      SPIX(*)
C
      INCLUDE 'INCS:DCHND.INC'
      INTEGER   IRET, LUN, LUNTMP, JIF, NIF, TABUFF(512), VER, I,
     *   CATI(256)
      DOUBLE PRECISION REFREQ, CATD(128), X(MAXIF), Y(MAXIF), BFIT(6),
     *   SX, SY, SXX, SXY, SX3, SX4, SXXY, DEN
      REAL      CATR(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATI, CATD, CATR)
C-----------------------------------------------------------------------
      IERR = 2
      SPIX(1) = 0.0
      SPIX(2) = 0.0
      IF (NTERM.EQ.2) SPIX(3) = 0.0
      LUN = LUNTMP (1)
C                                       find source
      CALL GETSOU (SRC, DISK, CNO, CATBLK, LUN, IRET)
      IF (IRET.EQ.11) THEN
         IERR = 3
         MSGTXT = 'CANNOT FIND SPECIFIED SOURCE IN SU TABLE'
         GO TO 990
      ELSE IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINDING SOURCE IN SU TABLE'
         GO TO 990
         END IF
C                                       count valid fluxes
      NIF = 0
      DO 10 JIF = 1,MAXIF
         IF ((FLUX(1,JIF).GT.0.0) .AND. (FLUX(1,JIF).NE.FBLANK)) THEN
            NIF = NIF + 1
            END IF
 10      CONTINUE
 20   JIF = NIF
C                                       get frequencies
      CALL FNDEXT ('FQ', CATBLK, VER)
      JIF = NIF
      FQCHND = FREQID
      CALL CHNDAT ('READ', TABUFF, DISK, CNO, VER, CATBLK, LUN, NIF,
     *   FOFF, ISBAND, FINC, BNDCOD, FQCHND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING FREQUENCY TABLE'
         GO TO 990
         END IF
      IF ((JIF.LT.2) .OR. (NIF.LT.2)) THEN
         IERR = 1
         MSGTXT = 'NOT ENOUGH FLUXES IN SU TABLE TO FIT SPECTRAL INDEX'
         GO TO 990
         END IF
C                                       values
      CALL COPY (256, CATBLK, CATI)
      REFREQ = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *   (CATI(KINAX+JLOCF)/2.0 - CATR(KRCRP+JLOCF))
      JIF = 0
      SX = 0.0D0
      SY = 0.0D0
      SXX = 0.0D0
      SXY = 0.0D0
      SX3 = 0.0D0
      SX4 = 0.0D0
      SXXY = 0.0D0
      DO 50 I = 1,NIF
         IF ((FLUX(1,I).NE.0.0) .AND. (FLUX(1,I).NE.FBLANK)) THEN
            JIF = JIF + 1
            Y(JIF) = LOG10 (FLUX(1,I))
            X(JIF) = LOG10 (REFREQ+FOFF(I)) - 9.0D0
            SX  = SX  + X(JIF)
            SXX = SXX + X(JIF)*X(JIF)
            SY  = SY  + Y(JIF)
            SXY = SXY + X(JIF)*Y(JIF)
            SX3 = SX3 + X(JIF)*X(JIF)*X(JIF)
            SX4 = SX4 + X(JIF)*X(JIF)*X(JIF)*X(JIF)
            SXXY = SXXY + X(JIF)*X(JIF)*Y(JIF)
            END IF
 50      CONTINUE
C                                       curvature
      IF (NTERM.EQ.2) THEN
         DEN = SX * SX - JIF * SXX
         IF ((JIF.EQ.2) .AND. (DEN.NE.0.0D0)) THEN
            BFIT(1) = (SX * SXY - SXX * SY) / DEN
            BFIT(2) = (SX * SY - JIF * SXY) / DEN
            BFIT(3) = 0.0D0
         ELSE IF (JIF.GE.3) THEN
            DEN = (SX*SXX-JIF*SX3) * (SXX*SX3-SX*SX4) -
     *         (SXX*SXX-JIF*SX4) * (SXX*SXX-SX*SX3)
            BFIT(1) = ((SXX*SXY-SX3*SY) * (SXX*SX3-SX*SX4) -
     *         (SXX*SXXY-SX4*SY) * (SXX*SXX-SX*SX3)) / DEN
            BFIT(2) = ((SXX*SXXY-SX4*SY) * (SX*SXX-JIF*SX3) -
     *         (SXX*SXY-SX3*SY) * (SXX*SXX-JIF*SX4)) / DEN
            BFIT(3) = ((SX*SY-JIF*SXY) * (SXX*SX-JIF*SX3) -
     *         (SXX*SY-JIF*SXXY) * (SX*SX-JIF*SXX)) /
     *         ((SX*SXX-JIF*SX3) * (SXX*SX-JIF*SX3) -
     *         (SX*SX-JIF*SXX) * (SXX*SXX-JIF*SX4))
         ELSE
            GO TO 990
            END IF
         IERR = 0
         SPIX(1) = 10.D0 ** BFIT(1)
         SPIX(2) = BFIT(2)
         SPIX(3) = BFIT(3)
         WRITE (MSGTXT,1050) SPIX(1), BFIT(2), BFIT(3), SRC
         CALL MSGWRT (3)
C                                       simple spectral index
      ELSE
         DEN = SX * SX - JIF * SXX
         IF ((JIF.GE.2) .AND. (DEN.NE.0.0D0)) THEN
            BFIT(1) = (SX * SXY - SXX * SY) / DEN
            BFIT(2) = (SX * SY - JIF * SXY) / DEN
         ELSE
            GO TO 990
            END IF
         IERR = 0
         SPIX(1) = 10.0D0 ** BFIT(1)
         SPIX(2) = BFIT(2)
         WRITE (MSGTXT,1060) SPIX(1), BFIT(2), SRC
         CALL MSGWRT (3)
         END IF
      GO TO 999
C
 990  MSGTXT = 'TOO LITTLE DATA FOR SPECTRAL FIT OR SINGULAR'
      IERR = 1
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FNDSPX: ERROR',I4,' ON ',A)
 1050 FORMAT ('FNDSPX: F1 spix spcurv',F8.3,2F8.4,' for source',I5)
 1060 FORMAT ('FNDSPX: flux 1GHz & spectral index',2F7.3,
     *   ' for source',I5)
      END
