      SUBROUTINE SNAPP (DISK, CNO, VER, SNOUT, CATBLK, BUFFER, OUTBUF,
     *   REFUSE, REFUSS, REREF, IRET)
C-----------------------------------------------------------------------
C! Append SN tables and keep track of reference antennas.
C# EXT-appl Calibration UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2008, 2011, 2014, 2016
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
C   Subroutine to append SN tables and to keep track of the reference
C   antennas used.
C   Input:
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VER      I        SN file version must be antenna-time sorted.
C      SNOUT    I        Output SN table number.
C      CATBLK   I(256)   Catalog header block.
C   Input/Output:
C      BUFFER   I(*)     Buffer for TABIO use.
C      OUTBUF   I(*)     2nd buffer for TABIO use.
C      REFUSE   I(*)     Count of the number times each antenna has
C                        been used as a reference antenna.
C      REREF    L        T -> there are non-zero imaginary, delay, rate
C   Output:
C      IRET     I        Return code 0=OK, else failed.
C                           -1 => no data found
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISK, CNO, VER, SNOUT, CATBLK(256), BUFFER(*),
     *   OUTBUF(*), REFUSE(*), REFUSS(MAXANT,*), IRET
      LOGICAL   REREF
C
      INCLUDE 'INCS:PSNTAB.INC'
      INTEGER   IRCODE, RECI(1024), KOLS(MAXSNC), NUMV(MAXSNC), LUN,
     *   LUN2, NUMANT, NUMPOL, NUMIF, NUMNOD, REFA, RF1K, RF2K, WT1K,
     *   WT2K, WT1KOL, RF1KOL, WT2KOL, RF2KOL, NUMREC, LOOPIF, ISNRNO,
     *   LOOPR, OUTRNO, SUBA, LUNTMP, SUBKOL, IM1KOL, IM2KOL, DE1KOL,
     *   DE2KOL, RA1KOL, RA2KOL, IM1K, IM2K, DE1K, DE2K, RA1K, RA2K
      LOGICAL   ISAPPL, APPEND
      REAL      GMMOD, RANOD(25), DECNOD(25), RECR(1024)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (RECI, RECR)
      DATA IRCODE /0/
C-----------------------------------------------------------------------
      APPEND = VER.NE.SNOUT
C                                       Open input SN table
C                                       Reformat ?
      LUN = LUNTMP (1)
      CALL SNREFM (DISK, CNO, VER, CATBLK, LUN, IRET)
C                                       Rescale
      LUN = LUNTMP (1)
      CALL SNRESC (DISK, CNO, VER, CATBLK, LUN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       open input
      LUN = LUNTMP (1)
      CALL SNINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, ISNRNO,
     *   KOLS, NUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD, RANOD,
     *   DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get number of records in table
      OUTRNO = 0
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 190
C                                       Pointers to SN table
      WT1KOL = KOLS(SNRWE1)
      RF1KOL = KOLS(SNIRF1)
      IM1KOL = KOLS(SNRIM1)
      DE1KOL = KOLS(SNRDE1)
      RA1KOL = KOLS(SNRRA1)
      WT2KOL = KOLS(SNRWE2)
      RF2KOL = KOLS(SNIRF2)
      IM2KOL = KOLS(SNRIM2)
      DE2KOL = KOLS(SNRDE2)
      RA2KOL = KOLS(SNRRA2)
      SUBKOL = KOLS(SNISUB)
C                                       Appending: open output
      IF (APPEND) THEN
         LUN2 = LUNTMP (1)
         CALL SNINI ('WRIT', OUTBUF, DISK, CNO, SNOUT, CATBLK, LUN2,
     *      OUTRNO, KOLS, NUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *      RANOD, DECNOD, ISAPPL, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Clear sort order
         OUTBUF(43) = 0
         OUTBUF(44) = 0
         END IF
      IF (NUMPOL.EQ.1) THEN
         RF2KOL = RF1KOL
         WT2KOL = WT1KOL
         END IF
C                                       Append SN VER to SNOUT
      DO 150 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECI, BUFFER, IRET)
         IF (IRET.LT.0) GO TO 150
         IF (IRET.NE.0) GO TO 900
C                                       Count reference antennas.
C                                       Loop over IF
         RF1K = RF1KOL
         RF2K = RF2KOL
         WT1K = WT1KOL
         WT2K = WT2KOL
         IM1K = IM1KOL
         IM2K = IM2KOL
         DE1K = DE1KOL
         DE2K = DE2KOL
         RA1K = RA1KOL
         RA2K = RA2KOL
         DO 100 LOOPIF = 1,NUMIF
            REFA = 0
            IF (RECR(WT1K).GT.0.0) REFA = RECI(RF1K)
            SUBA = RECI(SUBKOL)
            IF (REFA.GT.0) THEN
               REFUSE(REFA) = REFUSE(REFA) + 1
               IF ((SUBA.GT.0) .AND. (SUBA.LE.100)) REFUSS(REFA,SUBA) =
     *            REFUSS(REFA,SUBA) + 1
               END IF
            IF (.NOT.REREF) THEN
               IF ((RECR(IM1K).NE.0.0) .AND. (RECR(IM1K).NE.FBLANK))
     *            REREF = .TRUE.
               IF ((RECR(DE1K).NE.0.0) .AND. (RECR(DE1K).NE.FBLANK))
     *            REREF = .TRUE.
               IF ((RECR(RA1K).NE.0.0) .AND. (RECR(RA1K).NE.FBLANK))
     *            REREF = .TRUE.
               END IF
            IF (NUMPOL.GT.1) THEN
               REFA = 0
               IF (RECR(WT2K).GT.0.0) REFA = RECI(RF2K)
               IF (REFA.GT.0) THEN
                  REFUSE(REFA) = REFUSE(REFA) + 1
                  IF ((SUBA.GT.0) .AND. (SUBA.LE.100)) REFUSS(REFA,SUBA)
     *               = REFUSS(REFA,SUBA) + 1
                  END IF
               IF (.NOT.REREF) THEN
                  IF ((RECR(IM2K).NE.0.0) .AND. (RECR(IM2K).NE.FBLANK))
     *               REREF = .TRUE.
                  IF ((RECR(DE2K).NE.0.0) .AND. (RECR(DE2K).NE.FBLANK))
     *               REREF = .TRUE.
                  IF ((RECR(RA2K).NE.0.0) .AND. (RECR(RA2K).NE.FBLANK))
     *               REREF = .TRUE.
                  END IF
               IM2K = IM2K + 1
               DE2K = DE2K + 1
               RA2K = RA2K + 1
               RF2K = RF2K + 1
               WT2K = WT2K + 1
               END IF
            IM1K = IM1K + 1
            DE1K = DE1K + 1
            RA1K = RA1K + 1
            RF1K = RF1K + 1
            WT1K = WT1K + 1
 100        CONTINUE
C                                       Write if necessary
         IF (APPEND) THEN
            CALL TABIO ('WRIT', IRCODE, OUTRNO, RECI, OUTBUF, IRET)
            IF (IRET.NE.0) GO TO 900
            OUTRNO = OUTRNO + 1
            END IF
 150     CONTINUE
C                                       Close tables
      IF (APPEND) THEN
         CALL TABIO ('CLOS', IRCODE, LOOPR, RECI, OUTBUF, IRET)
         IF (IRET.NE.0) GO TO 900
         END IF
 190  CALL TABIO ('CLOS', IRCODE, LOOPR, RECI, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 900
      IF ((NUMREC.LE.0) .OR. (OUTRNO.LE.0)) IRET = -1
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('SNAPP: TABIO ERROR',I3,' CONCATENATING SOLUTIONS')
      END
