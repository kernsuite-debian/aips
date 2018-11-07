      SUBROUTINE SNAMBG (DISKIN, CNOIN, SNINP, SNOUT, TCENTE, IERR)
C-----------------------------------------------------------------------
C! Solve 2PI ambiguity of the SN table phase for each antenna and source.
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2004, 2015
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
C   SNAMBG reads the input SN table, calculates phase for each row IF
C   and stoke and then solves 2 PI ambiguity for each antenna and source.
C   The value of the phase (in degrees) is recorded into the REAL column
C   of the output SN table. IMAG column of the output SN table is  zero.
C
C   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C   ! Having used the output SN table should be deleted because of !
C   ! the 'strange' REAL and IMAG                                  !
C   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C   INPUT:
C      DISKIN    I        Disk number of the file
C      CNOIN     I        Catalog slot number.
C      SNINP     I        Input SN table number
C      SNOUT     I        Output SN table number
C      TCENTE    D        Edge time in days.
C                         2 PI jumps accumulated until this time
C                         are subtructed of the phase accumulated
C                         since the beginning.
C                         So the phase is concerved at the
C                         vicinity of TCENTE
C   Output:
C      IERR      I        Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISKIN, CNOIN, SNINP, SNOUT, IERR
      DOUBLE PRECISION TCENTE
      INTEGER   BUFFER(512), BUFOUT(512), LUN, LUNOUT, ISNRNO, ISNOUT,
     *   NUMREC, BUFF1(256), BUFF2(256),
     *   NUMNOD, ISN, JIF, JS, SOUN, FREQN, MODENO, LANT, IASIFP,
     *   NUMANT, NUMPOL, NUMIF, ISUB
      LOGICAL   ISAPL
      REAL      RANOD(25), DECNOD(25), GMMOD, MBDELY(2), TIMINT, IFRM,
     *   DISP(2), DDISP(2)
      DOUBLE PRECISION CTIM
C
      INTEGER   SNKOLS(MAXSNC), SNNUMV(MAXSNC)
      INCLUDE 'INCS:PSNTAB.INC'
C
      REAL      REAL(2,MAXIF), IMAG(2,MAXIF), DELAY(2,MAXIF),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF)
C
      INTEGER   REFA(2,MAXIF), MXSO, MMXPOL, MPROD
C                                       maximum number of sources in
C                                       the SN table
      PARAMETER (MXSO = 50)
C                                       maximum number of polarization
C                                       in the SN table
      PARAMETER (MMXPOL = 2)
      PARAMETER (MPROD = MAXANT*MXSO*MAXIF*MMXPOL)
      LOGICAL   FIRST(MPROD)
      REAL VALCUR, VALORI, VALOLD(MPROD), DIFF
      INTEGER JUMP(MPROD), JUMPC(MPROD)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
C
      DATA LUN, LUNOUT /29,28/
C-----------------------------------------------------------------------
C                                       start reading the input SN table
C                                       to find jumps accumulated to the
C                                       time center of the SN table
C
C                                       Open input SN table
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNINP, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
C                                       initiate the TWOPI jumps
C                                       for each antenna, source, IF,
C                                       and polarisation
      DO 9 IASIFP = 1, MPROD
         JUMP(IASIFP) = 0
         FIRST(IASIFP) = .TRUE.
    9    CONTINUE
C
      DO 50 ISNRNO = 1, NUMREC
C                                       read SN table
         ISN = ISNRNO
         CALL TABSN ('READ', BUFFER, ISN, SNKOLS, SNNUMV, NUMPOL, CTIM,
     *      TIMINT, SOUN, LANT, ISUB, FREQN, IFRM, MODENO, MBDELY, DISP,
     *      DDISP, REAL, IMAG, DELAY, RATE, WEIGHT, REFA, IERR)
         DO 15 JIF = 1,NUMIF
            DO 14 JS = 1,NUMPOL
               IASIFP = JS + NUMPOL*(JIF-1) +
     *            NUMPOL*NUMIF*(SOUN-1) + NUMPOL*NUMIF*MXSO*(LANT-1)
               IF (IASIFP .GT. MPROD) THEN
                  IERR = 1
                  WRITE (MSGTXT,1100) MPROD
                  GO TO 990
                  END IF
C
               IF ((REAL(JS,JIF).EQ.FBLANK) .OR.
     *            (IMAG(JS,JIF).EQ.FBLANK)) THEN
C                  REAL(JS,JIF) = FBLANK
                  GO TO 14
               ELSE
                  IF (FIRST(IASIFP)) THEN
                     VALOLD(IASIFP) = ATAN2(IMAG(JS,JIF), REAL(JS,JIF))
                     FIRST(IASIFP) = .FALSE.
                     END IF
                  END IF
               VALORI = ATAN2(IMAG(JS,JIF), REAL(JS,JIF))
               VALCUR = VALORI + JUMP(IASIFP)*TWOPI
               DIFF = VALCUR - VALOLD(IASIFP)
               IF (DIFF .GT. PI) JUMP(IASIFP) = JUMP(IASIFP) - 1
               IF (DIFF .LT. -PI) JUMP(IASIFP) = JUMP(IASIFP) + 1
               VALCUR = VALORI + JUMP(IASIFP)*TWOPI
               VALOLD(IASIFP) = VALCUR
C                                       jumps accumulated up to TCENTE
               IF (CTIM .LT. TCENTE) JUMPC(IASIFP) = JUMP(IASIFP)
C
   14          CONTINUE
   15       CONTINUE
   50    CONTINUE
C                                       Close input SN table.
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IERR)
C
C                                       end reading the input SN table
C                                       to find jumps accumulated to the
C                                       time: TCENTE of the SN table
C
C
C                                       copy input table to the output
C                                       table
      CALL TABCOP ('SN', SNINP, SNOUT, LUN, LUNOUT, DISKIN, DISKIN,
     *      CNOIN, CNOIN, CATBLK, BUFF1, BUFF2, IERR)
C                                       Open input SN table
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNINP, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open output SN table
      CALL SNINI ('WRIT', BUFOUT, DISKIN, CNOIN, SNOUT, CATBLK,
     *   LUNOUT, ISNOUT, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF,
     *   NUMNOD, GMMOD, RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) GO TO 999

C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
C                                       initiate the TWOPI jumps
C                                       for each antenna, source, IF,
C                                       and polarisation
      DO 90 IASIFP = 1, MPROD
         JUMP(IASIFP) = 0
         FIRST(IASIFP) = .TRUE.
   90    CONTINUE
C
      DO 500 ISNRNO = 1, NUMREC
C                                       read SN table
         ISN = ISNRNO
         CALL TABSN ('READ', BUFFER, ISN, SNKOLS, SNNUMV, NUMPOL, CTIM,
     *      TIMINT, SOUN, LANT, ISUB, FREQN, IFRM, MODENO, MBDELY, DISP,
     *      DDISP, REAL, IMAG, DELAY, RATE, WEIGHT, REFA, IERR)
         DO 150 JIF = 1,NUMIF
            DO 140 JS = 1,NUMPOL
               IASIFP = JS + NUMPOL*(JIF-1) +
     *            NUMPOL*NUMIF*(SOUN-1) + NUMPOL*NUMIF*MXSO*(LANT-1)
               IF (IASIFP .GT. MPROD) THEN
                  IERR = 1
                  WRITE (MSGTXT,1100) MPROD
                  GO TO 990
                  END IF
C
               IF ((REAL(JS,JIF).EQ.FBLANK) .OR.
     *            (IMAG(JS,JIF).EQ.FBLANK)) THEN
C                  REAL(JS,JIF) = FBLANK
                  GO TO 140
               ELSE
                  IF (FIRST(IASIFP)) THEN
                     VALOLD(IASIFP) = ATAN2(IMAG(JS,JIF), REAL(JS,JIF))
                     FIRST(IASIFP) = .FALSE.
                     END IF
                  END IF
               VALORI = ATAN2(IMAG(JS,JIF), REAL(JS,JIF))
               VALCUR = VALORI + JUMP(IASIFP)*TWOPI
               DIFF = VALCUR - VALOLD(IASIFP)
               IF (DIFF .GT. PI) JUMP(IASIFP) = JUMP(IASIFP) - 1
               IF (DIFF .LT. -PI) JUMP(IASIFP) = JUMP(IASIFP) + 1
               VALCUR = VALORI + JUMP(IASIFP)*TWOPI
               VALOLD(IASIFP) = VALCUR
C                                       output: corrected REAL, IMAG
C                                       accumulated jumps subtruacted
C                                       REAL is in degrees
               REAL(JS,JIF) = (VALCUR - JUMPC(IASIFP)*TWOPI) * RAD2DG
               IMAG(JS,JIF) = 0
C
 140           CONTINUE
 150        CONTINUE


C                                       record the output SN table
         ISN = ISNRNO
         CALL TABSN ('WRIT', BUFOUT, ISN, SNKOLS, SNNUMV, NUMPOL, CTIM,
     *      TIMINT, SOUN, LANT, ISUB, FREQN, IFRM, MODENO, MBDELY, DISP,
     *      DDISP, REAL, IMAG, DELAY, RATE, WEIGHT, REFA, IERR)

  500    CONTINUE
C                                       Close input SN table.
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IERR)
C                                       Close output SN table.
      CALL TABIO ('CLOS', 0, ISNRNO, BUFOUT, BUFOUT, IERR)

 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('NANT*NSOU*NIF*NPOL exceedes maximum:', I8)
      END
