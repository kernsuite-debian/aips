      SUBROUTINE IMTAB (OPCODE, BUFFER, NUMBND, IERR)
C-----------------------------------------------------------------------
C! I/O to interferometer model tables (following init. by IMINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2009
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
C   Does I/O to interferometer model tables. Usually used after setup by
C   IMINI.
C   Inputs:
C      OPCODE   C*4        Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(512)     I/O buffer and related storage, also defines
C                          file if open. Should have been returned by
C                          IMINI.
C      NUMBND   I          # band's (IF's) in data
C   Inputs via common:
C      IIMRNO   I          Next scan number to read or write.
C      IMKOLS   I(MAXIMC)  The column pointer array in order:
C                          TIME, TIME INTERVAL, SOURCE ID, ANTENNA NO.,
C                          ARRAY, FREQID, I.FAR.ROT, FREQ. VAR,
C                          PDELAY1, GDELAY1, PRATE1, GRATE1, DISP1,
C                          DDISP1,
C                          the following are only present if NOPOLZ = 2
C                          PDELAY2, GDELAY2, PRATE2, GRATE2, DISP2,
C                          DDISP2,
C      IMNUMV   I(MAXIMC)  Element count in each column.
C      NOPOLZ   I          # polzns in table
C      NPOLY    I          Order of polynomial for geometric model
C   Input/output: (written to or read from model file)
C      TIME     D          Time of centre of interval since 0hr on
C                          reference day (days)
C      TINT     R          Interval over which calibration constants
C                          apply (days)
C      ISRC     I          Source ID number
C      NOSTA    I          Station number
C      IARRAY   I          Array number
C      IFQID    I          Freq ID number
C      IFR      R          Ionospheric Faraday Rotation (Rad/m**2)
C      FREQVR   R(MAXIF)   Time variable freq. offset from ref + FR
C                          value + SO value
C      PDELA1   D(MAXIF,MAXPOL)
C                          Total phase delay (seconds) at ref. time
C      GDELA1   D(MAXPOL)  Total group delay (seconds) at ref. time
C      PRATE1   D(MAXIF,MAXPOL)
C                          Phase delay rate (sec/sec)
C      GRATE1   D(MAXPOL)  Group delay rate (sec/sec)
C      DISP1    R          Dispersive portion of delay for polzn 1
C                          (seconds/m^2 - scales as lambda^2)
C      DDISP1   R          Time derivative of DISP1 (sec/sec/m^2)
C    The follwing are only used if NOPOLZ = 2.
C      PDELA2   D(MAXIF,MAXPOL)
C                          Total phase delay (seconds) at ref. time
C      GDELA2   D(MAXPOL)  Total group delay (seconds) at ref. time
C      PRATE2   D(MAXIF,MAXPOL)
C                          Phase delay rate (sec/sec)
C      GRATE2   D(MAXPOL)  Group delay rate (sec/sec)
C      DISP2    R          Dispersive portion of delay for polzn 1
C                          (seconds/m^2 - scaoles as lambda^2)
C      DDISP2   R          Time derivative of DISP1 (sec/sec/m^2)
C   Output:
C      IIMRNO   I          Next row number.
C      IERR     I          Error code, 0=>OK else TABIO error.
C                          Note: -1=> read but record deselected.
C   Usage NOTE: use the include 'DIMV.INC' for the declarations in
C   IMINI and IMTAB.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IERR, NUMBND
C
      LOGICAL   DOREAD, DOCLOS
      INTEGER   I, K, IOFF
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIMV.INC'
      INTEGER   RECI(MAXREC)
      REAL      RECORD(MAXREC)
      HOLLERITH RECH(MAXREC)
      DOUBLE PRECISION RECD(MAXRCD)
      EQUIVALENCE (RECD, RECORD, RECI, RECH)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
C                                       Time
         RECD(IMKOLS(KTIM)) = TIME
C                                       Interval
         RECORD(IMKOLS(KINT)) = TINT
C                                       Source ID
         RECI(IMKOLS(KSOU)) = ISRC
C                                       Antenna number
         RECI(IMKOLS(KANO)) = NOSTA
C                                       Array number
         RECI(IMKOLS(KARR)) = IARRAY
C                                       Freqid number
         RECI(IMKOLS(KFRE)) = IFQID
C                                       IFR
         RECORD(IMKOLS(KIFR)) = IFR
C                                       Dispersive delay
         RECORD(IMKOLS(KDSP1)) = DISP1
C                                       Dispersive delay rate
         RECORD(IMKOLS(KDDT1)) = DDISP1
C                                       2nd polzn
         IF (NOPOLZ.EQ.2) THEN
C                                       Dispersive delay
            RECORD(IMKOLS(KDSP2)) = DISP2
C                                       Dispersive delay rate
            RECORD(IMKOLS(KDDT2)) = DDISP2
            END IF
C                                       Variables that vary with
C                                       NPOLY
         DO 100 I = 1, NPOLY
C                                       Group delay
            RECD(IMKOLS(KGDL1)+I-1) = GDELA1(I)
C                                       Group delay rate
            RECD(IMKOLS(KGRT1)+I-1) = GRATE1(I)
C                                       2nd polzn
            IF (NOPOLZ.EQ.2) THEN
C                                       Group delay
               RECD(IMKOLS(KGDL2)+I-1) = GDELA2(I)
C                                       Group delay rate
               RECD(IMKOLS(KGRT2)+I-1) = GRATE2(I)
               END IF
 100        CONTINUE
C                                       Variables that vary over IF's
         DO 150 I = 1, NUMBND
C                                       Time variable freq offset
            RECORD(IMKOLS(KFVR)+I-1) = FREQVR(I)
 150        CONTINUE
C                                       Variables that vary with IF and
C                                       NPOLY
         DO 170 I = 1, NUMBND
            IOFF = (I-1) * NPOLY
            DO 160 K = 1, NPOLY
C                                       Phase delay
               RECD(IMKOLS(KPDL1)+IOFF+K-1) = PDELA1(I,K)
C                                       Phase delay rate
               RECD(IMKOLS(KPRT1)+IOFF+K-1) = PRATE1(I,K)
C                                       2nd polzn
               IF (NOPOLZ.EQ.2) THEN
C                                       Phase delay
                  RECD(IMKOLS(KPDL2)+IOFF+K-1) = PDELA2(I,K)
C                                       Phase delay rate
                  RECD(IMKOLS(KPRT2)+IOFF+K-1) = PRATE2(I,K)
                  END IF
 160           CONTINUE
 170        CONTINUE
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IIMRNO, RECORD, BUFFER, IERR)
      IIMRNO = IIMRNO + 1
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABIO', 'IMTAB', IERR)
         GO TO 990
         END IF
      IF (IERR.LT.0) GO TO 50
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
C                                       Time
         TIME = RECD(IMKOLS(KTIM))
C                                       Interval
         TINT = RECORD(IMKOLS(KINT))
C                                       Source ID
         ISRC = RECI(IMKOLS(KSOU))
C                                       Antenna number
         NOSTA = RECI(IMKOLS(KANO))
C                                       Array number
         IARRAY = RECI(IMKOLS(KARR))
C                                       Freqid number
         IFQID = RECI(IMKOLS(KFRE))
C                                       IFR
         IFR = RECORD(IMKOLS(KIFR))
C                                       Dispersive delay
         DISP1 = RECORD(IMKOLS(KDSP1))
C                                       Dispersive delay rate
         DDISP1 = RECORD(IMKOLS(KDDT1))
C                                       2nd polzn
         IF (NOPOLZ.EQ.2) THEN
C                                       Dispersive delay
            DISP2 = RECORD(IMKOLS(KDSP2))
C                                       Dispersive delay rate
            DDISP2 = RECORD(IMKOLS(KDDT2))
            END IF
C                                       Variables that vary with
C                                       NPOLY
         DO 200 I = 1, NPOLY
C                                       Group delay
            GDELA1(I) = RECD(IMKOLS(KGDL1)+I-1)
C                                       Group delay rate
            GRATE1(I) = RECD(IMKOLS(KGRT1)+I-1)
C                                       2nd polzn
            IF (NOPOLZ.EQ.2) THEN
C                                       Group delay
               GDELA2(I) = RECD(IMKOLS(KGDL2)+I-1)
C                                       Group delay rate
               GRATE2(I) = RECD(IMKOLS(KGRT2)+I-1)
               END IF
 200        CONTINUE
C                                       Variables that vary over IF's
         DO 250 I = 1, NUMBND
C                                       Time variable freq offset
            FREQVR(I) = RECORD(IMKOLS(KFVR)+I-1)
 250        CONTINUE
C                                       Variables that vary with IF and
C                                       NPOLY
         DO 270 I = 1, NUMBND
            IOFF = (I-1) * NPOLY
            DO 260 K = 1, NPOLY
C                                       Phase delay
               PDELA1(I,K) = RECD(IMKOLS(KPDL1)+IOFF+K-1)
C                                       Phase delay rate
               PRATE1(I,K) = RECD(IMKOLS(KPRT1)+IOFF+K-1)
C                                       2nd polzn
               IF (NOPOLZ.EQ.2) THEN
C                                       Phase delay
                  PDELA2(I,K) = RECD(IMKOLS(KPDL2)+IOFF+K-1)
C                                       Phase delay rate
                  PRATE2(I,K) = RECD(IMKOLS(KPRT2)+IOFF+K-1)
                  END IF
 260           CONTINUE
 270        CONTINUE
         END IF
         GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IIMRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'IMTAB', IERR)
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('IMTAB: TABIO ERROR',I5)
      END
