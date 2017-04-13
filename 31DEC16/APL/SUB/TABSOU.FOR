      SUBROUTINE TABSOU (OPCODE, BUFFER, ISURNO, SUKOLS, SUNUMV,
     *   IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *   DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *   PMRA, PMDEC, IERR)
C-----------------------------------------------------------------------
C! Does I/O to Source (SU) table opened by SOUINI
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2009, 2012-2015
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
C   Does I/O to SOURCE extention tables. Usually used after setup by
C   SOUINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'READ' = read entry from table.
C                        'WRIT' = write entry in table.
C                        'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        SOUINI
C      ISURNO   I        Next record number to read or write.
C      SUKOLS   I(21)    The column pointer array in order, ID. NO.,
C                        SOURCE, QUAL, CALCODE, IFLUX, QFLUX, UFLUX,
C                        VFLUX, FREQO, BANDWIDTH, RAEPO, DECEPO, EPOCH,
C                        RAAPP, DECAPP, LSRVEL, LRESTF, PMRA, PMDEC
C      SUNUMV   I(21)    Element count in each column.
C   Input/output: (written to or read from SOURCE file)
C      IDSOU    I        Source ID as defined in the SOURCE table.
C      SOUNAM   C*16     Source name
C      QUAL     I        Source qualifier.
C      CALCOD   C*4      Calibrator code
C      FLUX     R(4,*)   Total flux density I, Q, U, V pol, (Jy)
C                        1 set per IF.
C      FREQO    D(*)     Frequency offset (Hz) from IF nominal.
C      BANDW    D        Bandwidth (Hz)
C      RAEPO    D        Right ascension at mean EPOCH (degrees)
C      DECEPO   D        Declination at mean EPOCH (degrees)
C      EPOCH    D        Equinox for position in yr. since year 0.0
C      RAAPP    D        Apparent Right ascension (degrees)
C      DECAPP   D        Apparent Declination(degrees)
C      RAOBS    D        Antenna pointing Right ascension (degrees)
C      DECOBS   D        Antenna pointing Declination(degrees)
C      LSRVEL   D(*)     LSR velocity (m/sec) of each IF
C      LRESTF   D(*)     Line rest frequency (Hz) of each IF
C      PMRA     D        Proper motion (deg/day) in RA
C      PMDEC    D        Proper motion (deg/day) in declination
C   Output:
C      ISURNO   I        Next record number.
C      IERR     I        Error code, 0=>OK else TABIO error.
C                           Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4, CALCOD*4, SOUNAM*16
      INTEGER   BUFFER(512), ISURNO, SUKOLS(MAXSUC), SUNUMV(MAXSUC),
     *   IDSOU, QUAL, IERR
      REAL      FLUX(4,*)
      DOUBLE PRECISION FREQO(*), BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL(*), LRESTF(*), PMRA, PMDEC
C
      DOUBLE PRECISION RECD(14+5*MAXIF)
      REAL      RECORD(27+10*MAXIF)
      HOLLERITH RECH(27+10*MAXIF)
      INTEGER   RECI(27+10*MAXIF), LOOP, LIMIT, IDKOL, SOUKOL, QKOL,
     *   CCKOL, IFKOL, QFKOL, UFKOL, VFKOL, FRKOL, BANKOL, RAEKOL,
     *   DCEKOL, EPOKOL, RAAKOL, DCAKOL, RAOKOL, DCOKOL, LSRKOL, LRFKOL,
     *   PMRKOL, PMDKOL
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECD, RECORD, RECI, RECH)
C-----------------------------------------------------------------------
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, ISURNO, RECORD, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 990
C                                       illegal opcode
      ELSE IF ((OPCODE.NE.'READ') .AND. (OPCODE.NE.'WRIT')) THEN
         MSGTXT = 'TABSOU: INVALID OPCODE= ''' // OPCODE // ''''
         GO TO 995
C                                       read/write
      ELSE
C                                       set pointers
         IDKOL = SUKOLS(1)
         SOUKOL = SUKOLS(2)
         QKOL = SUKOLS(3)
         CCKOL = SUKOLS(4)
         IFKOL = SUKOLS(5)
         QFKOL = SUKOLS(6)
         UFKOL = SUKOLS(7)
         VFKOL = SUKOLS(8)
         FRKOL = SUKOLS(9)
         BANKOL = SUKOLS(10)
         RAEKOL = SUKOLS(11)
         DCEKOL = SUKOLS(12)
         EPOKOL = SUKOLS(13)
         RAAKOL = SUKOLS(14)
         DCAKOL = SUKOLS(15)
         RAOKOL = SUKOLS(16)
         DCOKOL = SUKOLS(17)
         LSRKOL = SUKOLS(18)
         LRFKOL = SUKOLS(19)
         PMRKOL = SUKOLS(20)
         PMDKOL = SUKOLS(21)
C                                       If write fill RECORD
         IF (OPCODE.EQ.'WRIT') THEN
            RECI(IDKOL) = IDSOU
            CALL CHR2H (SUNUMV(2), SOUNAM, 1, RECH(SOUKOL))
            RECI(QKOL) = QUAL
            IF (CALCOD.EQ.'NONE') THEN
               CALL CHR2H (4, '    ', 1, RECH(CCKOL))
            ELSE
               CALL CHR2H (4, CALCOD, 1, RECH(CCKOL))
               END IF
            RECD(BANKOL) = BANDW
            RECD(RAEKOL) = RAEPO
            RECD(DCEKOL) = DECEPO
            RECD(EPOKOL) = EPOCH
            RECD(RAAKOL) = RAAPP
            RECD(DCAKOL) = DECAPP
            IF (RAOKOL.GT.0) RECD(RAOKOL) = RAOBS
            IF (DCOKOL.GT.0) RECD(DCOKOL) = DECOBS
C                                       IF dependent values.
            LIMIT = SUNUMV(18)
            DO 10 LOOP = 1,LIMIT
               IF (SUNUMV(9).GE.LOOP) RECD(FRKOL+LOOP-1) = FREQO(LOOP)
               RECD(LSRKOL+LOOP-1) = LSRVEL(LOOP)
               IF (SUNUMV(19).GE.LOOP)
     *            RECD(LRFKOL+LOOP-1) = LRESTF(LOOP)
               IF (SUNUMV(5).GE.LOOP) THEN
                  RECORD(IFKOL+LOOP-1) = FLUX(1,LOOP)
                  RECORD(QFKOL+LOOP-1) = FLUX(2,LOOP)
                  RECORD(UFKOL+LOOP-1) = FLUX(3,LOOP)
                  RECORD(VFKOL+LOOP-1) = FLUX(4,LOOP)
                  END IF
 10            CONTINUE
 15         RECD(PMRKOL) = PMRA
            RECD(PMDKOL) = PMDEC
            END IF
C                                       Process record.
         CALL TABIO (OPCODE, 0, ISURNO, RECORD, BUFFER, IERR)
         ISURNO = ISURNO + 1
         IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
         IF (OPCODE.EQ.'READ') THEN
            IDSOU = RECI(IDKOL)
            CALL H2CHR (SUNUMV(2), 1, RECH(SOUKOL), SOUNAM)
            QUAL = RECI(QKOL)
            CALL H2CHR (4, 1, RECH(CCKOL), CALCOD)
            IF (CALCOD.EQ.'NONE') CALCOD = ' '
            BANDW = RECD(BANKOL)
            RAEPO = RECD(RAEKOL)
            DECEPO = RECD(DCEKOL)
            EPOCH = RECD(EPOKOL)
            RAAPP = RECD(RAAKOL)
            DECAPP = RECD(DCAKOL)
            IF (RAOKOL.GT.0) THEN
               RAOBS = RECD(RAOKOL)
            ELSE
               RAOBS = RAEPO
               END IF
            IF (DCOKOL.GT.0) THEN
               DECOBS = RECD(DCOKOL)
            ELSE
               DECOBS = DECEPO
               END IF
C                                       IF dependent values.
            LIMIT = SUNUMV(18)
            DO 70 LOOP = 1,LIMIT
               FREQO(LOOP) = 0.0D0
               IF (SUNUMV(9).GE.LOOP) FREQO(LOOP) = RECD(FRKOL+LOOP-1)
               LSRVEL(LOOP) = RECD(LSRKOL+LOOP-1)
               LRESTF(LOOP) = 0.0D0
               IF (SUNUMV(19).GE.LOOP)
     *            LRESTF(LOOP) = RECD(LRFKOL+LOOP-1)
               IF (SUNUMV(5).GE.LOOP) THEN
                  FLUX(1,LOOP) = RECORD(IFKOL+LOOP-1)
                  FLUX(2,LOOP) = RECORD(QFKOL+LOOP-1)
                  FLUX(3,LOOP) = RECORD(UFKOL+LOOP-1)
                  FLUX(4,LOOP) = RECORD(VFKOL+LOOP-1)
                  END IF
 70            CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABSOU: TABIO ERROR',I3)
      END
